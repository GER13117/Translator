#lang racket

(require db)
(require net/url)
(require json)
(require "login.rkt")
(require "helper_funcs.rkt")
(provide isPrepositon getPreposition)


(define (isPrepositon ele) ;checks if the given word is a Prepositon, by checking if it exists in the prepositions table
  (cond
    [(query-maybe-value mdbc (string-append "SELECT ger_prep FROM prepositions WHERE eng_prep=" "'" ele "'LIMIT 1"))#t]
    [else #f]))

(define (getPreposition preposition pos wordTypeList input) ;function for building the preposition and its surroundings (Artikel etc.)
  (define nextNoun #f)
  (define nextPronoun #f)
  (define nextObject #f)
  (define ger_preposition #f)
  (define nextName #f)
  (cond
    [(string? (getNext "noun" pos wordTypeList input))( ;checks if there is a noun behind the preposition and sets values accordingly
                                    (lambda ()
                                      (set! nextNoun (getNext "noun" pos wordTypeList input))
                                      (set! nextObject nextNoun)
                                      (set! ger_preposition (query-value mdbc (string-append "SELECT ger_prep FROM prepositions join nouns on prepositions.usecase = nouns.sense WHERE eng_prep='" preposition "' AND eng_noun ='" nextNoun "'")))))]
    [(string? (getNext "pronoun" pos wordTypeList input))( ;checks if there is a pronoun behind the preposition and sets values accordingly
                                       (lambda ()
                                         (set! nextPronoun (getNext "pronoun" pos wordTypeList input))
                                         (set! nextObject nextPronoun)
                                         (set! ger_preposition (query-value mdbc (string-append "SELECT ger_prep FROM prepositions WHERE eng_prep='" preposition "' AND usecase ='person'")))))]
    [else ( ;if none of the above options is correct, the following word is probably a name. So values will be set accordingly
           (lambda ()
             (set! nextName (getNext "name" pos wordTypeList input))
             (set! nextObject nextName)
             (set! ger_preposition (query-value mdbc (string-append "SELECT ger_prep FROM prepositions WHERE eng_prep='" preposition "' AND usecase ='person'")))))])

  (define nextObjectQuery ;The function returns a snippet of the Query needed to translate the preposition accordingly
    (cond
      [(string? nextName)(string-append " pronouns WHERE eng_pronoun='" nextName "'")] ;if name is defined ther cant be a noun or pronoun
      [(and (string? nextNoun) (string? nextPronoun)) ;if there is a pronoun and a noun it checks which comes first and defines accordingly
       (cond
         [(< (index-of input nextPronoun) (index-of input nextNoun))(string-append " pronouns WHERE eng_pronoun='" nextPronoun "'")]
         [else (string-append " nouns WHERE eng_noun='" nextNoun "'")])]
      [else (cond ;if there is just a noun or a pronoun
              [(string? nextNoun)(string-append " nouns WHERE eng_noun='" nextNoun "'")]
              [else (string-append " pronouns WHERE eng_pronoun='" nextPronoun "'")])]))
  

  (define (getGenderOfName name) ;calls an api which returnes a probable gender of name, NOT REALLY NEEDED (names dont come with articles)
    (hash-ref
     (call/input-url (string->url (string-append "https://api.genderize.io/?name=" name))
                                 get-pure-port
                                 (compose string->jsexpr port->string)) 'gender))
  
  (define (senseQuery) ;templete for finding the sense of a noun, pronoun or name. A name is not in a list so its "sense" is set to person. The sense together with "usecase" of a preposition is used to finde the correct preposition
    (cond
      [(query-maybe-value mdbc (string-append "SELECT sense FROM" nextObjectQuery))(query-value mdbc (string-append "SELECT sense FROM" nextObjectQuery))]
      [else "person"]))

  (define (numerusQuery) ;queries for the numerus of a noun, pronoun or name.
    (cond
      [(query-maybe-value mdbc (string-append "SELECT numerus FROM" nextObjectQuery))(query-value mdbc (string-append "SELECT numerus FROM" nextObjectQuery))]
      [else "singular"]))

  (define (genderQuery) ;queries for the gender of a noun, pronoun or name. For name the getGenderofName (with the api) is used
    (cond
      [(query-maybe-value mdbc (string-append "SELECT gender FROM" nextObjectQuery))(query-value mdbc (string-append "SELECT gender FROM" nextObjectQuery))]
      [else (getGenderOfName nextName)])) 
  
  (cond
    [(not (string-ci=? "article" (list-ref wordTypeList (+ 1 pos)))) ;To have a correct sentence an artcile is required. To check if there was already an article in the inputed sentence, so it would already get translated.
        (string-append ger_preposition                               ;If thats not the case the function uses a combination of usecase, numerus and sense to determine the correct article.
                            (cond                                    ;It than gets appeded to ger_prepsition to get a usable answer
                              [(or (string-ci=? "bigplace" (senseQuery))(string-ci=? "time" (senseQuery))(string-ci=? "person" (senseQuery)))""] ;Länder / Städte, Zeiten und personen haben keinen Artikel
                              [(or (string-ci=? "object" (senseQuery))(string-ci=? "smallplace" (senseQuery))) ;Objekte und kleine Orte (Schule, Straßen) schon
                               (cond
                                 [(string-ci=? "plural" (numerusQuery))" den"]
                                 [else
                                  (cond
                                    [(string-ci=? "male" (genderQuery))" dem"]
                                    [(string-ci=? "female" (genderQuery))" der"]
                                    [(string-ci=? "neutral" (genderQuery))" dem"])])]
                              ))]
    [else ger_preposition]))

