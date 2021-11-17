#lang racket
(require db)

;Autoren: Okke und manchmal Johann


;TODO: Funktion um regelmäßige Verben in Datenbank zu schreiben Link regelmäßige deutsche verben: https://de.wiktionary.org/wiki/Verzeichnis:Deutsch/Regelm%C3%A4%C3%9Fige_Verben
;TODO: .!? am Ende von Sätzen
;TODO: How to store the results
;TODO: Usefull error messages for the users: The word which has problem, (the problem)

;TODO: Verben als Auslöser für Dativ
(require "login.rkt")



;==========================================|WordToWord|==============================================| ;UNUSED RIGHT NOW
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
(define (index-of lst ele) ;in alten (Schul-)Versionen nicht in base
  (let loop ((lst lst)
             (idx 0))
    (cond ((empty? lst) #f)
          ((equal? (first lst) ele) idx)
          (else (loop (rest lst) (add1 idx))))))


(define input '()) ;Global list storing the input
(define wordTypeList '()) ;Global list storing the wordtypes


(define (translate request) ;main function: uses different web-handlers to receive and send data
  (define data (request-post-data/raw request))
  (set! input (string-split (regexp-replace #rx"'" (bytes->string/utf-8 data) "''")" "))                 ;?????REGEX: 'nt --> not, 're --> are, ('s --> is)
  (set! wordTypeList (getWordTypeList input))
  (define str "Oops")
  (cond
    [(eq? 1 (length input))(set! str "TEST")]
    [else (set! str (regexp-replace #rx"''" (string-join (sentenceLoop input) " ")"'"))])
  (displayln str)     ;REMOVE WHEN WORKING
  (http-response str))

;TODO: Alle Wortarten / Wörter aus Tabellen müssen verwendet werden können --> schreiben von Get uns Is für die restlichen wortarten
(define (getWordTypeList input (typeList '()) (pos 0)) ;gets the wordtype of the inputed words by using the "is" functions of the different wordTypes.
  (cond                                                ;returns a list
    [(< pos (length input))
     (cond
       [(isArticle (list-ref input pos))(getWordTypeList input (cons "article" typeList) (+ 1 pos))] ;TODO: What todo if article is recognized but no noun (ALLGEMEIN: Catchen von Fehlern)
       [(isNoun (list-ref input pos))(getWordTypeList input (cons "noun" typeList)(+ 1 pos))]
       [(isPronoun (list-ref input pos))(getWordTypeList input(cons "pronoun" typeList)(+ 1 pos))]
       [(string? (isVerb (list-ref input pos)))(getWordTypeList input (cons "verb" typeList) (+ 1 pos))]
       [(isAdjective (list-ref input pos))(getWordTypeList input (cons "adjective" typeList)(+ 1 pos))]
       [(isPrepositon (list-ref input pos))(getWordTypeList input (cons "preposition" typeList)(+ 1 pos))]
       [else (getWordTypeList input (cons "noun" typeList) (+ 1 pos))])]
    [else (reverse typeList)]))

(define (splitListAtPos index lst (res_lst '()))
  (cond
    [(> index 0) (splitListAtPos (- index 1) (rest lst) (cons (first lst) res_lst ))]
        [else (reverse res_lst) ]))

(define (getCase noun pos)
  (cond
   [(member 'verb (splitListAtPos pos wordTypeList ))
    (cond
      [(member 'preposition (splitListAtPos pos wordTypeList)(
        [(#f)"genitiv"]
        [(string-ci=? "dativ" query-value (string-append "SELECT gram_case FROM prepositions WHERE eng_prep='" (list-ref input pos) "'LIMIT 1"))"dativ"] ;TODO: Make the table usable even with multiple meanings of a preposition
        [(#f)"akkusativ"]
      ))])]
   [else "nominativ"]))
;wenn im Satzteil vor dem gegbenen (Pro)Nomen ein Verb vorhanden ist --> Nominativ (Subjekt)
  ;sonst --> Obejekt

;TODO: Wenn für Nomen gesucht wird auch Pronomen beachten
(define (getNext wordType startPos)
  (list-ref input (+ startPos 1 (index-of (drop wordTypeList (+ 1 startPos)) wordType))))

;|---------------------------------------<|Prepositions|>--------------------------------------------|

;TODO: Make the table usable even with multiple meanings of a preposition
(define (isPrepositon ele)
  (cond
    [(query-maybe-value mdbc (string-append "SELECT ger_prep FROM prepositions WHERE eng_prep=" "'" ele "'LIMIT 1"))#t]
    [else #f]))

;TODO: Genitiv und Akkusativ hinzufügen
(define (getPreposition preposition pos)
  (define nextNoun (getNext "noun" pos))
  (define ger_preposition (query-value mdbc (string-append "SELECT ger_prep FROM prepositions join nouns on prepositions.usecase = nouns.sense WHERE eng_prep='" preposition "' AND eng_noun ='" nextNoun "'")))
  (cond
    [(not (string-ci=? "article" (list-ref wordTypeList (+ 1 pos))))
      (cond
        [(regexp-match? #rx"^[a-z](.*[aeiou])?$" ger_preposition)
         (string-append ger_preposition (cond
                                          ;Genitiv
                                          [(string-ci=? (query-value mdbc (string-append "SELECT gram_case FROM prepositions WHERE ger_prep='" ger_preposition "'")) "dativ")
                                           (cond
                                             [(string-ci=? "plural" (query-value mdbc (string-append "SELECT numerus FROM nouns WHERE eng_noun='" nextNoun "'")))"n"]
                                             [else
                                              (cond
                                                [(string-ci=? "male" (query-value mdbc (string-append "SELECT gender FROM nouns WHERE eng_noun='" nextNoun "'")))"m"]
                                                [(string-ci=? "female" (query-value mdbc (string-append "SELECT gender FROM nouns WHERE eng_noun='" nextNoun "'")))"r"]
                                                [(string-ci=? "neutral" (query-value mdbc (string-append "SELECT gender FROM nouns WHERE eng_noun='" nextNoun "'")))"m"])])]
                                          ;Akkusativ
                                          ))]
        [else (string-append ger_preposition (cond
                                               [(string-ci=? "bigplace" (query-value mdbc (string-append "SELECT sense FROM nouns WHERE eng_noun ='" nextNoun "'")))""] ;Länder haben keinen Artikel
                                               [else (cond                                                                                                              ;TODO: Condition für smallplace anstatt von else: Es ist nicht small place wenn nicht bigplace
                                                       [(string-ci=? "plural" (query-value mdbc (string-append "SELECT numerus FROM nouns WHERE eng_noun='" nextNoun "'")))" den"]
                                                       [else
                                                        (cond
                                                          [(string-ci=? "male" (query-value mdbc (string-append "SELECT gender FROM nouns WHERE eng_noun='" nextNoun "'")))" dem"]
                                                          [(string-ci=? "female" (query-value mdbc (string-append "SELECT gender FROM nouns WHERE eng_noun='" nextNoun "'")))" der"]
                                                          [(string-ci=? "neutral" (query-value mdbc (string-append "SELECT gender FROM nouns WHERE eng_noun='" nextNoun "'")))" dem"])])]))])]
    [else ger_preposition]));TODO: Funktion auch für Pronomen verwendbar machen:
                                  ;Findung von ger_preposition: nextNoun und proNoun
;                                                               --> condition um gucken welche Positon kleiner ist
;                                                               --> festlegen von nextObject (als Ersatz für nextNoun) und ger_preposition


;|-----------------------------------------<|Articles|>----------------------------------------------|

(define (isArticle ele) ;TODO: He walks the way --> Er geht den Weg (NICHT Er geht der Weg)
  (cond
    [(query-maybe-value mdbc (string-append "SELECT ger_article FROM articles WHERE eng_article=" "'" ele "'" "AND gender='male'"))#t]
    [else #f]))

;TODO: Bedingungen für alle Fälle (logik von getPreposition übernehmen --> 
(define (getArticle article noun pos)
  (cond
    [(string-ci=? "nominativ" (getCase noun pos))
                  (query-value mdbc (string-append "SELECT ger_article FROM articles join nouns on articles.gender = nouns.gender WHERE eng_noun='" noun "'AND eng_article='" article "'"))]))

;|------------------------------------------<|Nouns|>------------------------------------------------|
(define (isNoun ele)
  (cond
    [(query-maybe-value mdbc (string-append "SELECT ger_noun FROM nouns WHERE eng_noun=" "'" ele "'"))]
    [(query-maybe-value mdbc (string-append "SELECT ger_noun FROM nouns WHERE eng_noun='" (string-trim ele "''s" #:left? #f) "'"))] ;TODO: Für genitiv nutzen
    [(query-maybe-value mdbc (string-append "SELECT ger_noun FROM nouns WHERE eng_noun='" (string-trim ele "s''" #:left? #f) "'"))]
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
;TODO: Make functional: (-ing)
;TODO: REIHENFOLGE OPTMIEREN
(define (isVerb ele) 
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
  (cond
    [(query-maybe-value mdbc (string-append "SELECT ger_pronoun FROM pronouns WHERE eng_pronoun=" "'" ele "'"))
     (cond
       [(string-ci=? ele "I")'ich]
       [(or (string-ci=? ele "We") (string-ci=? ele "They"))'wirSie]
       [(or (string-ci=? ele "He") (string-ci=? ele "She") (string-ci=? ele "It"))'erSieEs]
       [(string-ci=? ele "You")'du])]
    [else 'erSieEs]))                                                          ;TODO: Gibt es andere Pronomen die als hinweis genutzt werden können

(define (getVerbHelper person verb)
     (cond
       [(eq? person 'ich)(string-append (regVerbQuery verb) "e")]
       [(eq? person 'erSieEs)(string-append (regVerbQuery verb) "t")]
       [(eq? person 'wirSie)(string-append (regVerbQuery verb) "en")]
       [(eq? person 'du)(string-append (regVerbQuery verb) "st")]))


(define (getVerb subj verb form) ;TODO: Make dynamically if Adjective infront or noun in front --> implentieren von getNext-Funtktion (Anstatt von subj die position des Verbs übergeben)
  (cond
    [(eq? form "rVerbES")(getVerbHelper (getPerson subj) (string-trim verb "es" #:left? #f))]
    [(eq? form "rVerbS")(getVerbHelper (getPerson subj) (string-trim verb "s" #:left? #f))]
    [(eq? form "iVerb")(iregVerbQuery verb)]
    [else (getVerbHelper (getPerson subj) verb)]))



;|----------------------------------------<|Adjectives|>---------------------------------------------|
; Akkusativ und Dativ wenn Präposition davor: Teilweise über Präposition bestimmbar

;TODO: Komplette Logik für Adjektive:
;     - Fälle
;     - Geschlecht
;     - numerus
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

;|------------------------------------<|Translation Interator|>--------------------------------------|
(define (sentenceLoop input(translation '()) (pos 0))
  (cond
    [(< pos (length input))
     (cond
       [(isArticle (list-ref input pos))(sentenceLoop input (cons (getArticle (list-ref input pos) (list-ref input (+ pos 1)) pos) translation) (+ 1 pos))] ;TODO: Position des Artikels übergeben --> zur dynamischen Erkennung von Nomen durch getNext
       [(isNoun (list-ref input pos))(sentenceLoop input  (cons (getNoun (list-ref input pos)) translation)(+ 1 pos))]
       [(isPronoun (list-ref input pos))(sentenceLoop input  (cons (getPronoun (list-ref input pos)) translation)(+ 1 pos))]
       [(string? (isVerb (list-ref input pos)))(sentenceLoop input  (cons (getVerb (list-ref input (- pos 1)) (list-ref input pos) (isVerb (list-ref input pos))) translation)(+ 1 pos))]
       [(isAdjective (list-ref input pos))(sentenceLoop input  (cons "Adjective" translation)(+ 1 pos))]
       [(isPrepositon (list-ref input pos))(sentenceLoop input (cons (getPreposition (list-ref input pos) pos) translation)(+ 1 pos))]
       [else (sentenceLoop input  (cons (list-ref input pos) translation) (+ 1 pos))])]
    [else (reverse translation)]))


;|============================================|Server|================================================|


(require web-server/servlet) 
(require web-server/servlet-env)

(define (http-response content)  
  (response/full
    200                  ; HTTP response code.
    #"OK"                ; HTTP response message.
    (current-seconds)    ; Timestamp.
    TEXT/HTML-MIME-TYPE  ; MIME type for content.
    '()                  ; Additional HTTP headers.
    (list                ; Content (in bytes) to send to the client.
     (string->bytes/utf-8 content))))


;; URL routing table (URL dispatcher).
(define-values (dispatch generate-url)
  (dispatch-rules
    [("translate") #:method "post" translate]
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




