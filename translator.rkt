#lang racket
(require db)

;TODO: Funktion um regelmäßige Verben in Datenbank zu schreiben Link regelmäßige deutsche verben: https://de.wiktionary.org/wiki/Verzeichnis:Deutsch/Regelm%C3%A4%C3%9Fige_Verben
;TODO: .!? am Ende von Sätzen
;TODO: How to store the results

(require "login.rkt")

;============================================|Unused|================================================|



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

(define caseOfNoun '())

;|-------------------------------------<|Helper-Functions|>------------------------------------------|

(define (slist->string slst) ;Convert one list into a whole string
  (string-join (map slst) " "))

;|-----------------------------------------<|Articles|>----------------------------------------------|

(define (isArticle ele) ;TODO: He walks the way --> Er geht den Weg (NICHT Er geht der Weg)
  (cond
    [(query-maybe-value mdbc (string-append "SELECT ger_article FROM articles WHERE eng_article=" "'" ele "'" "AND gender='male'"))#t]
    [else #f]))

(define (getArticle article noun)
  (query-value mdbc (string-append "SELECT ger_article FROM articles join nouns on articles.gender = nouns.gender WHERE eng_noun='" noun "'AND eng_article='" article "'")))

;|------------------------------------------<|Nouns|>------------------------------------------------|
(define (isNoun ele)
  (cond
    [(query-maybe-value mdbc (string-append "SELECT ger_noun FROM nouns WHERE eng_noun=" "'" ele "'"))]
    [(query-maybe-value mdbc (string-append "SELECT ger_noun FROM nouns WHERE eng_noun='" (string-trim ele "'s" #:left? #f) "'"))]
    [(query-maybe-value mdbc (string-append "SELECT ger_noun FROM nouns WHERE eng_noun='" (string-trim ele "s'" #:left? #f) "'"))]
    [else #f]))
(define (getNoun noun)
  (query-value mdbc (string-append "SELECT ger_noun FROM nouns WHERE eng_noun=" "'" noun "'")))
  

;|-----------------------------------------<|Pronouns|>----------------------------------------------|

(define (isPronoun ele)
  (cond
    [(query-maybe-value mdbc (string-append "SELECT ger_pronoun FROM pronouns WHERE eng_pronoun=" "'" ele "'"))#t]
    [else #f]))

(define (getPronoun pronoun)
  (query-value mdbc (string-append "SELECT ger_pronoun FROM pronouns WHERE eng_pronoun=" "'" pronoun "'")))

;|------------------------------------------<|Verbs|>------------------------------------------------|

(define (isVerb ele) ;TODO: Make functional: (-ing) ;TODO: REIHENFOLGE OPTMIEREN
  (cond
    [(query-maybe-value mdbc (string-append "SELECT ger_verb FROM irregular_verbs WHERE eng_verb=" "'"  ele "'"))"iVerb"]
    [(query-maybe-value mdbc (string-append "SELECT ger_wortstamm FROM verbs WHERE eng_verb=" "'" ele "'"))"rVerb"]
    [(query-maybe-value mdbc (string-append "SELECT ger_wortstamm FROM verbs WHERE eng_verb=" "'" (string-trim ele "s" #:left? #f) "'"))"rVerbS"]
    [(query-maybe-value mdbc (string-append "SELECT ger_wortstamm FROM verbs WHERE eng_verb=" "'" (string-trim ele "es" #:left? #f) "'"))"rVerbES"]
    [else #f]))

(define (regVerbQuery verb_eng)
  (query-value mdbc (string-append "SELECT ger_wortstamm FROM verbs WHERE eng_verb=" "'" verb_eng "'")))

(define (iregVerbQuery verb_eng)
  (query-value mdbc (string-append "SELECT ger_verb FROM irregular_verbs WHERE eng_verb=" "'" verb_eng "'")))

(define (getPerson ele)
  ;Logik: Check ob Personalpronomen oder Nomen -> Person herausfinden
  (cond
    [(query-maybe-value mdbc (string-append "SELECT ger_pronoun FROM pronouns WHERE eng_pronoun=" "'" ele "'"))
     (cond
       [(string-ci=? ele "I")'ich]
       [(or (string-ci=? ele "We") (string-ci=? ele "They") (string-ci=? ele "You"))'wirSieSie]
       [(or (string-ci=? ele "He") (string-ci=? ele "She") (string-ci=? ele "It"))'erSieEs])]
    [else 'erSieEs]))                                                          ;TODO: Gibt es andere Pronomen die als hinweis genutzt werden können
                                                                               ;TODO: Du (you) fixen
(define (getVerbHelper person verb)
     (cond
       [(eq? person 'ich)(string-append (regVerbQuery verb) "e")]
       [(eq? person 'erSieEs)(string-append (regVerbQuery verb) "t")]
       [(eq? person 'wirSieSie)(string-append (regVerbQuery verb) "en")]
       [(eq? person 'du)(string-append (regVerbQuery verb) "st")]))


(define (getVerb subj verb form) ;TODO: Make dynamically if Adjective infront or noun in front
  (cond
    [(eq? form "rVerbES")(getVerbHelper (getPerson subj) (string-trim verb "es" #:left? #f))]
    [(eq? form "rVerbS")(getVerbHelper (getPerson subj) (string-trim verb "s" #:left? #f))]
    [(eq? form "iVerb")(iregVerbQuery verb)]
    [else (getVerbHelper (getPerson subj) verb)]))



;|----------------------------------------<|Adjectives|>---------------------------------------------|

(define (isAdjective adj)
  #f)
;(define (getCase subj))


(define (getAdjective foo bar)
  (displayln("AAAAAAAAAAAAA")))
;|-----------------------------------------<|Unsorted|>----------------------------------------------|
(define (checkForQuestion ele)
  (cond
    [(query-maybe-value mdbc (string-append "SELECT eng_verb FROM verbs WHERE eng_verb=" "'" ele "'")) #t]
    [(query-maybe-value mdbc (string-append "SELECT eng_question_word FROM verbs WHERE eng_verb=" "'" ele "'")) #t]
    [else #f]))

;|--------------------------------------<|Main Functions|>-------------------------------------------|
(define (sentenceLoop lst (pos 0))
  (cond
    [(< pos (length lst))
       (cond
         [(isArticle (list-ref lst pos))(display(getArticle (list-ref lst pos) (list-ref lst (+ pos 1))))]
         [(isNoun (list-ref lst pos))(display (getNoun (list-ref lst pos)))]
         [(isPronoun (list-ref lst pos))(display (getPronoun (list-ref lst pos)))]
         [(string? (isVerb (list-ref lst pos)))(display (getVerb (list-ref lst (- pos 1)) (list-ref lst pos) (isVerb (list-ref lst pos))))] ;TODO: Make dynamically if Adjective infront or noun in front
         [(isAdjective (list-ref lst pos))"Adjective"]
         [else (display (list-ref lst pos))])
       (display " ")
       (sentenceLoop lst (+ pos 1))
       ]
    [else (display ".")]))

(sentenceLoop '("The" "fish" "swims" "in" "the" "lake"))

;|===========================================|Tests|=================================================|

(define (verbTest verb)
  (define pronouns '(I you he she it we you they))
  (for-each (lambda (ele)
              (displayln (getVerb ele verb))) pronouns))
;(verbTest 'jump)




