#lang racket
(require db)

;TODO: Funktion um regelmäßige Verben in Datenbank zu schreiben Link regelmäßige deutsche verben: https://de.wiktionary.org/wiki/Verzeichnis:Deutsch/Regelm%C3%A4%C3%9Fige_Verben
;TODO: .!? am Ende von Sätzen
;TODO: How to store the results

;TODO: Use list-ref instead of first second etc. sentences can be translated more frealy. For a verb you would just look "infront" or "behind", instead of having fixed positions

;For MySQL or MariaDB Database on your local machine

(define mdbc ;function ro connect to mariaDB Database on raspberryPi
  (mysql-connect #:server "translator.ddns.net" ;dyndns to fix the problem of a changing ip-address
                 #:port 3306
                 #:database "vocabdb"
                 #:user "user1"
                 #:password "password")) ;<====Change this before publishing


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

;|------------------------------------------<|Verbs|>------------------------------------------------|

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
       [(or (eq? ele 'I) (eq? ele 'i))('ich)]
       [(or (eq? ele 'You) (eq? ele 'you) (eq? ele 'We) (eq? ele 'we))('wirSieDu)]                                                                ;Das geht so nicht: Ihr geht langsam vs Du gehst langsam vs Sie (Höflichkeitsform) gehen langsam
       [(or (eq? ele 'He) (eq? ele 'he) (eq? ele 'She) (eq? ele 'she) (eq? ele 'It) (eq? ele 'it) (eq? ele 'You) (eq? ele 'you))('erSieEsIhr)])]
    [else 'erSieEsIhr]))                                                                                                ;TODO: Überprüfen ob das funtionieren kann

(define (getVerbHelper person verb)
  (cond
    [(query-maybe-value mdbc (string-append "SELECT ger_verb FROM irregular_verbs WHERE eng_verb=" "'" (symbol->string verb) "'"))
     (query-value mdbc (string-append "SELECT ger_verb FROM irregular_verbs WHERE eng_verb=" "'" (symbol->string verb) "'"))]
    [else ;Das funktioniert nur bedingt
     (cond
       [(eq? person 'ich)(string-append (regVerbQuery verb) "e")]
       [(eq? person 'erSieEsIhr)(string-append (regVerbQuery verb) "t")]
       [(eq? person 'wirSieDu)(string-append (regVerbQuery verb) "en")])]))


(define (getVerb subj verb)
  (getVerbHelper (getPerson subj) verb))


;|----------------------------------------<|Adjectives|>---------------------------------------------|



;|-----------------------------------------<|Unsorted|>----------------------------------------------|
(define (checkForQuestion ele)
  (cond
    [(query-maybe-value mdbc (string-append "SELECT eng_verb FROM verbs WHERE eng_verb=" "'" (symbol->string ele) "'")) #t]
    [(query-maybe-value mdbc (string-append "SELECT eng_question_word FROM verbs WHERE eng_verb=" "'" (symbol->string ele) "'")) #t]
    [else #f]))

;|--------------------------------------<|Main Functions|>-------------------------------------------|

(define (transSenWithArticle lst)
  (display (query-value mdbc (string-append "SELECT ger_article FROM articles WHERE eng_article=" "'" (symbol->string (first lst)) "'" "AND gender='" (query-value mdbc (string-append "SELECT gender FROM nouns WHERE eng_noun='" (symbol->string (third lst)) "'")) "'"))) ;Das ist so nicht wirklich anwendbar
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

(displayln(grammarTranslate '(The beautiful fish swims through the sea)))




