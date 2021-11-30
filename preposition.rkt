#lang racket

(require db)
(require net/url)
(require json)
(require "login.rkt")
(require "helper_funcs.rkt")
(provide isPrepositon)
(provide getPreposition)


(define (isPrepositon ele)
  (cond
    [(query-maybe-value mdbc (string-append "SELECT ger_prep FROM prepositions WHERE eng_prep=" "'" ele "'LIMIT 1"))#t]
    [else #f]))

;TODO: Genitiv und Akkusativ hinzufügen
(define (getPreposition preposition pos wordTypeList input)
  (define nextNoun #f)                                         ;Das ist nicht schön
  (define nextPronoun #f)                                    ;Das auch nicht
  (define nextObject #f)
  (define ger_preposition #f)                               ;Das auch absolut nicht
  (define nextName #f)
  (cond
    [(string? (getNext "noun" pos wordTypeList input))(
                                    (lambda ()
                                      (set! nextNoun (getNext "noun" pos wordTypeList input))
                                      (set! nextObject nextNoun)
                                      (set! ger_preposition (query-value mdbc (string-append "SELECT ger_prep FROM prepositions join nouns on prepositions.usecase = nouns.sense WHERE eng_prep='" preposition "' AND eng_noun ='" nextNoun "'")))))]
    [(string? (getNext "pronoun" pos wordTypeList input))(
                                       (lambda ()
                                         (set! nextPronoun (getNext "pronoun" pos wordTypeList input))
                                         (set! nextObject nextPronoun)
                                         (set! ger_preposition (query-value mdbc (string-append "SELECT ger_prep FROM prepositions WHERE eng_prep='" preposition "' AND usecase ='person'")))))]
    [else (
           (lambda ()
             (set! nextName (getNext "name" pos wordTypeList input))
             (set! nextObject nextName)
             (set! ger_preposition (query-value mdbc (string-append "SELECT ger_prep FROM prepositions WHERE eng_prep='" preposition "' AND usecase ='person'")))))]) 


  (define nextObjectQuery
    (cond
      [(string? nextName)(string-append " pronouns WHERE eng_pronoun='" nextName "'")] ;An sich nicht nötig es ist aber das false später nötig
      [(and (string? nextNoun) (string? nextPronoun))
       (cond
         [(< (index-of input nextPronoun) (index-of input nextNoun))(string-append " pronouns WHERE eng_pronoun='" nextPronoun "'")]
         [else (string-append " nouns WHERE eng_noun='" nextNoun "'")])]
      [else (cond
              [(string? nextNoun)(string-append " nouns WHERE eng_noun='" nextNoun "'")]
              [else (string-append " pronouns WHERE eng_pronoun='" nextPronoun "'")])]))
  

  (define (getGenderOfName name) ;<==ist wahrscheinlich unnötig aber lustig
    (hash-ref
     (call/input-url (string->url (string-append "https://api.genderize.io/?name=" name))
                                 get-pure-port
                                 (compose string->jsexpr port->string)) 'gender))
  
  (define (senseQuery)
    (cond
      [(query-maybe-value mdbc (string-append "SELECT sense FROM" nextObjectQuery))(query-value mdbc (string-append "SELECT sense FROM" nextObjectQuery))]
      [else "person"]))

  (define (numerusQuery)
    (cond
      [(query-maybe-value mdbc (string-append "SELECT numerus FROM" nextObjectQuery))(query-value mdbc (string-append "SELECT numerus FROM" nextObjectQuery))]
      [else "singular"]))

  (define (genderQuery)
    (cond
      [(query-maybe-value mdbc (string-append "SELECT gender FROM" nextObjectQuery))(query-value mdbc (string-append "SELECT gender FROM" nextObjectQuery))]
      [else (getGenderOfName nextName)])) 
  
  (cond
    [(not (string-ci=? "article" (list-ref wordTypeList (+ 1 pos))))
        (string-append ger_preposition
                            (cond
                              [(or (string-ci=? "bigplace" (senseQuery))(string-ci=? "person" (senseQuery)))""] ;Länder haben keinen Artikel
                              [(string-ci=? "smallplace" (senseQuery))
                               (cond
                                 [(string-ci=? "plural" (numerusQuery))" den"]
                                 [else
                                  (cond
                                    [(string-ci=? "male" (genderQuery))" dem"]
                                    [(string-ci=? "female" (genderQuery))" der"]
                                    [(string-ci=? "neutral" (genderQuery))" dem"])])]
                              ))]
    [else ger_preposition]))

