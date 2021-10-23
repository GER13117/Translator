#lang racket
(require db)

;TODO: Funktion um regelmäßige Verben in Datenbank zu schreiben Link regelmäßige deutsche verben: https://de.wiktionary.org/wiki/Verzeichnis:Deutsch/Regelm%C3%A4%C3%9Fige_Verben
;TODO: .!? am Ende von Sätzen
;TODO: How to store the results

(require "login.rkt")


;===========================================|Unused|================================================|
(define (populateVerbTable lst)
  (cond
    [(> (length lst) 0)((query-exec mdbc (string-append "INSERT INTO verbs(eng_verb, ich, du, ersiees, wir, ihr, sie) " "VALUES " "('" ;Die Funkton funktioniert so nicht mehr
                                                        (first lst) "','"
                                                        (string-append (second lst) "e") "','"
                                                        (string-append (second lst) "st") "','"
                                                        (string-append (second lst) "t") "','"
                                                        (string-append (second lst) "en") "','"
                                                        (string-append (second lst) "t") "','"
                                                        (string-append (second lst) "en") "')"))
                        (populateVerbTable (cddr lst)))]
    [else " "]))

;(populateVerbTable '("believe" "glaub" "hope" "hoff"))



;==========================================|WordToWord|==============================================|
(define (checkForCorrectReturn ele)
  (query-maybe-value mdbc (string-append "SELECT german FROM wordtoword WHERE english=" "'" (symbol->string ele) "'")))

(define (wordToWordQuery engword)
  (query-value mdbc (string-append "SELECT german FROM wordtoword WHERE english=" "'" (symbol->string engword) "'")))


(define (wordByWordSQL lst)
  (for-each (lambda (ele)
              (cond
                [(checkForCorrectReturn ele) (display (string-append(wordToWordQuery ele) " "))]
                [else (display (string-append (symbol->string ele) " "))])) lst))

(define (transSQL input)
  (wordByWordSQL input) (newline)
  (transSQL (read (open-input-string (string-append "(" (read-line) ")")))))

;(transSQL (read (open-input-string (string-append "(" (read-line) ")"))))



;|========================================|Grammarbased|=============================================|

;|-------------------------------------<|Helper-Functions|>------------------------------------------|

(define (slist->string slst) ;Convert one list into a whole string
  (string-join (map symbol->string slst) " "))

;|-----------------------------------------<|Articles|>----------------------------------------------|

(define (isArticle ele)
  (cond
    [(query-maybe-value mdbc (string-append "SELECT ger_article FROM articles WHERE eng_article=" "'" (symbol->string ele) "'" "AND gender='male'"))#t]
    [else #f]))

(define (getArticle article noun)
  (query-value mdbc (string-append "SELECT ger_article FROM articles WHERE eng_article=" "'" (symbol->string article)
                                            "'" "AND gender='" (query-value mdbc (string-append "SELECT gender FROM nouns WHERE eng_noun='"
                                                                                                (symbol->string noun) "'")) "'")))

;|------------------------------------------<|Nouns|>------------------------------------------------|

(define (isNoun ele)
  (cond
    [(query-maybe-value mdbc (string-append "SELECT translation FROM nouns WHERE eng_noun=" "'" (symbol->string ele) "'"))#t]
    [else #f]))
(define (getNoun noun)
  (query-value mdbc (string-append "SELECT translation FROM nouns WHERE eng_noun=" "'" (symbol->string noun) "'")))
  

;|-----------------------------------------<|Pronouns|>----------------------------------------------|

(define (isPronoun ele)
  (cond
    [(query-maybe-value mdbc (string-append "SELECT ger_pronoun FROM pronouns WHERE eng_pronoun=" "'" (symbol->string ele) "'"))#t]
    [else #f]))

(define (getPronoun pronoun)
  (query-value mdbc (string-append "SELECT ger_pronoun FROM pronouns WHERE eng_pronoun=" "'" (symbol->string pronoun) "'")))

;|------------------------------------------<|Verbs|>------------------------------------------------|

(define (isVerb ele) ;TODO: Make functional: he she it s / es muss mit, (-ing)
  #t)

(define (regVerbQuery ele_eng)
  (cond
    [(query-maybe-value mdbc (string-append "SELECT ger_wortstamm FROM verbs WHERE eng_verb=" "'" (symbol->string ele_eng) "'"))
     (query-value mdbc (string-append "SELECT ger_wortstamm FROM verbs WHERE eng_verb=" "'" (symbol->string ele_eng) "'"))]
    [else (symbol->string ele_eng)]))

(define (getPerson ele)
  ;Logik: Check ob Personalpronomen oder Nomen -> Person herausfinden
  (cond
    [(query-maybe-value mdbc (string-append "SELECT ger_pronoun FROM pronouns WHERE eng_pronoun=" "'" (symbol->string ele) "'"))
     (cond
       [(or (eq? ele 'I) (eq? ele 'i))'ich]
       [(or (eq? ele 'We) (eq? ele 'we) (eq? ele 'They) (eq? ele 'they) (eq? ele 'You) (eq? ele 'you))'wirSieSie]
       [(or (eq? ele 'He) (eq? ele 'he) (eq? ele 'She) (eq? ele 'she) (eq? ele 'It) (eq? ele 'it))'erSieEs])]
    [else 'erSieEs]))                                                          ;TODO: Gibt es andere Pronomen die als hinweis genutzt werden können
                                                                               ;TODO: Du (you) fixen
(define (getVerbHelper person verb)
  (cond
    [(query-maybe-value mdbc (string-append "SELECT ger_verb FROM irregular_verbs WHERE eng_verb=" "'" (symbol->string verb) "'"))
     (query-value mdbc (string-append "SELECT ger_verb FROM irregular_verbs WHERE eng_verb=" "'" (symbol->string verb) "'"))]
    [(query-maybe-value mdbc (string-append "SELECT ger_wortstamm FROM verbs WHERE eng_verb=" "'" (symbol->string verb) "'"))
     (cond
       [(eq? person 'ich)(string-append (regVerbQuery verb) "e")]
       [(eq? person 'erSieEs)(string-append (regVerbQuery verb) "t")]
       [(eq? person 'wirSieSie)(string-append (regVerbQuery verb) "en")]
       [(eq? person 'du)(string-append (regVerbQuery verb) "st")])]
    [else (symbol->string verb)])) ;TODO: Ändern, sodass das Verb mit übersetzung der Datenbank hinzugefügt wird


(define (getVerb subj verb) ;TODO: Make dynamically if Adjective infront or noun in front
  (getVerbHelper (getPerson subj) verb))



;|----------------------------------------<|Adjectives|>---------------------------------------------|

(define (isAdjective)
  #t)
;(define (getCase subj))


(define (getAdjective foo bar)
  (displayln("AAAAAAAAAAAAA")))
;|-----------------------------------------<|Unsorted|>----------------------------------------------|
(define (checkForQuestion ele)
  (cond
    [(query-maybe-value mdbc (string-append "SELECT eng_verb FROM verbs WHERE eng_verb=" "'" (symbol->string ele) "'")) #t]
    [(query-maybe-value mdbc (string-append "SELECT eng_question_word FROM verbs WHERE eng_verb=" "'" (symbol->string ele) "'")) #t]
    [else #f]))

;|--------------------------------------<|Main Functions|>-------------------------------------------|
(define (sentenceLoop lst (pos 0))
  (cond
    [(< pos (length lst))
       (cond
         [(isArticle (list-ref lst pos))(display(getArticle (list-ref lst pos) (list-ref lst (+ pos 1))))]
         [(isNoun (list-ref lst pos))(display (getNoun (list-ref lst pos)))]
         [(isPronoun (list-ref lst pos))(display (getPronoun (list-ref lst pos)))]
         [(isVerb (list-ref lst pos))(display (getVerb (list-ref lst (- pos 1)) (list-ref lst pos)))] ;TODO: Make dynamically if Adjective infront or noun in front
         [(isAdjective (list-ref lst pos))"Adjective"]
         [else (list-ref lst pos)])
       (display " ")
       (sentenceLoop lst (+ pos 1))
       ]
    [else (display ".")]))

(sentenceLoop '(The program do shit))




(define (transSenWithArticle lst)
  (display (query-value mdbc (string-append "SELECT ger_article FROM articles WHERE eng_article=" "'" (symbol->string (first lst))
                                            "'" "AND gender='" (query-value mdbc (string-append "SELECT gender FROM nouns WHERE eng_noun='"
                                                                                                (symbol->string (third lst)) "'")) "'"))) ;Das ist so nicht wirklich anwendbar
  (cond
    [(query-maybe-value mdbc (string-append "SELECT translation FROM nouns WHERE eng_noun=" "'" (symbol->string (second lst)) "'"))
     (query-value mdbc (string-append "SELECT translation FROM nouns WHERE eng_noun=" "'" (symbol->string (second lst)) "'"))]
    
    [(query-maybe-value mdbc (string-append "SELECT ger_adj FROM adjectives WHERE eng_adj=" "'" (symbol->string (second lst)) "'")) ;DAS BRINGT SO NOCH NICHTS
     (query-value mdbc (string-append "SELECT ger_adj FROM adjectives WHERE eng_adj=" "'" (symbol->string (second lst)) "'"))]
    [else (displayln "I dont know")]))



(define (grammarTranslate lst)
  (cond
    [(query-maybe-value mdbc (string-append "SELECT ger_text FROM phrases WHERE eng_text=" "'" (slist->string lst) "'")) ;check for redewendung / already translated phrase
     (query-value mdbc (string-append "SELECT ger_text FROM phrases WHERE eng_text=" "'" (slist->string lst) "'"))]
    [(or (eq? (first lst) 'The) (eq? (first lst) 'A) (eq? (first lst) 'An))(transSenWithArticle lst)] ;check if a sentence starts with an article
    
    [(display "Are you have been stupid")])
  )
  ;check for redewendung => give the translation back
  ;check for article => check for / get noun  => get gender of noun / currect form of article => check for verb => get the correct translation of the verb => check for präpostion => check for / get nomen        => get currect version of the präposition
  ;                                                                                                                                                                                => check for / get pronomen
  ;                  => check for adjective ||
  
  ;check for pronomen




;|===========================================|Tests|=================================================|
;(displayln(grammarTranslate '(The beautiful fish swims through the sea)))
;(displayln(grammarTranslate '(The art is beautiful)))


(define (verbTest verb)
  (define pronouns '(I you he she it we you they))
  (for-each (lambda (ele)
              (displayln (getVerb ele verb))) pronouns))
;(verbTest 'jump)




