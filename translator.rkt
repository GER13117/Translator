#lang racket
(require db)

;TODO: Funktion um regelmäßige Verben in Datenbank zu schreiben Link regelmäßige deutsche verben: https://de.wiktionary.org/wiki/Verzeichnis:Deutsch/Regelm%C3%A4%C3%9Fige_Verben
;TODO: .!? am Ende von Sätzen
;TODO: How to store the results

(require "login.rkt")



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
(define input '())
(define wordTypeList '())

;define getWordTypeList

(define (splitListAtPos x y (z '()))
  (cond
    [(> x 0)(splitListAtPos (- x 1) (rest y) (cons (first y) z ))]
        [else (reverse z) ]))
  

(define (getCase noun pos)
  (cond
   [(member 'verb (splitListAtPos wordTypeList pos))
    (cond
      [(#t)"Objekt"])]
   [else "nominativ"]))
;wenn im Satzteil vor dem gegbenen (Pro)Nomen ein Verb vorhanden ist --> Nominativ (Subjekt)
  ;sonst --> Obejekt




;|-----------------------------------------<|Articles|>----------------------------------------------|

(define (isArticle ele) ;TODO: He walks the way --> Er geht den Weg (NICHT Er geht der Weg)
  (cond
    [(query-maybe-value mdbc (string-append "SELECT ger_article FROM articles WHERE eng_article=" "'" ele "'" "AND gender='male'"))#t]
    [else #f]))

(define (getArticle article noun pos)
  (cond
    [(string-ci=? "nominativ" (getCase noun pos))
                  (query-value mdbc (string-append "SELECT ger_article FROM articles join nouns on articles.gender = nouns.gender WHERE eng_noun='" noun "'AND eng_article='" article "'"))]))

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
; Nominativ wenn kein Verb davor
; Akkusativ und Dativ wenn Präposition davor: Teilweise über Präposition bestimmbar


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
(define (sentenceLoop input (translation '()) (pos 0))
  (cond
    [(< pos (length input))
     (cond
       [(isArticle (list-ref input pos))(sentenceLoop input (cons (getArticle (list-ref input pos) (list-ref input (+ pos 1)) pos) translation) (+ 1 pos))] ;TODO: What todo if article is recognized but no noun
       [(isNoun (list-ref input pos))(sentenceLoop input(cons (getNoun (list-ref input pos)) translation)(+ 1 pos))]
       [(isPronoun (list-ref input pos))(sentenceLoop input(cons (getPronoun (list-ref input pos)) translation)(+ 1 pos))]
       [(string? (isVerb (list-ref input pos)))(sentenceLoop input(cons (getVerb (list-ref input (- pos 1)) (list-ref input pos) (isVerb (list-ref input pos))) translation)(+ 1 pos))]
       [(isAdjective (list-ref input pos))(sentenceLoop input (cons "Adjective" translation)(+ 1 pos))]
       [else (sentenceLoop input (cons (list-ref input pos) translation) (+ 1 pos))])]
    [else (reverse translation)]))



(sentenceLoop '("The" "fish" "swims"))


;|============================================|Server|================================================|

(define (translate request)
  (define data (request-post-data/raw request))
  (set! input (string-split (bytes->string/utf-8 data) " "))
  (set! wordTypeList (getWordTypeList input))
  (define str (string-join (sentenceLoop input) " "))
  (displayln str)     ;REMOVE WHEN WORKING
  (http-response str))


(require web-server/servlet) 
(require web-server/servlet-env)

(define (http-response content)  
  (response/full
    200                  ; HTTP response code.
    #"OK"                ; HTTP response message.
    (current-seconds)    ; Timestamp.
    TEXT/HTML-MIME-TYPE  ; MIME type for content.
    '()                  ; Additional HTTP headers.
    (list                ; Content (in bytes) to send to the browser.
     (string->bytes/utf-8 content))))


;; URL routing table (URL dispatcher).
(define-values (dispatch generate-url)
  (dispatch-rules
    [("example-post") #:method "post" translate]
    [else (error "There is no procedure to handle the url.")]))

(define (request-handler request)
  (dispatch request))

;; Start the server.
(serve/servlet
  request-handler
  #:launch-browser? #f
  #:quit? #f
  #:listen-ip "127.0.0.1"
  #:port 8001
  #:servlet-regexp #rx"")


;|===========================================|Tests|=================================================|

(define (verbTest verb)
  (define pronouns '(I you he she it we you they))
  (for-each (lambda (ele)
              (displayln (getVerb ele verb))) pronouns))
;(verbTest 'jump)




