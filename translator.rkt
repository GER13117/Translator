#lang racket
(require db)
(require "login.rkt")
(require "preposition.rkt")
(require "adjectives.rkt")
(require "article.rkt")
(require "helper_funcs.rkt")
(require "numerals.rkt")
(require "interjections.rkt")
(require "pronouns.rkt")
;Autoren: Okke und manchmal Johann

;MAINMODULE

;==========================================|WordToWord|==============================================|
(define (checkForCorrectReturn ele)
  (query-maybe-value mdbc (string-append "SELECT ger_word FROM wordtoword WHERE eng_word=" "'" ele "'")))

(define (checkForGerCorrectReturn ele)
  (query-maybe-value mdbc (string-append "SELECT eng_word FROM wordtoword WHERE ger_word=" "'" ele "'")))

(define (wordToWordQuery engword)
  (query-value mdbc (string-append "SELECT ger_word FROM wordtoword WHERE eng_word=" "'" engword "'")))

(define (gerToEng gerword)
  (query-value mdbc (string-append "SELECT eng_word FROM wordtoword WHERE ger_word=" "'" gerword "'")))

(define (singleWordQuery ele) ;function for querying a single-word-input
  (cond
    [(checkForCorrectReturn ele) (wordToWordQuery ele)]
    [else ele]))


(define (wordByWordSQL lst) ;used for translating german sentences to english
 (map (lambda (ele)
              (cond
                [(checkForGerCorrectReturn ele) (string-append(gerToEng ele) " ")]
                [else (string-append ele " ")])) lst))

;|========================================|Grammarbased|=============================================|


(define input '()) ;Global list storing the input
(define wordTypeList '()) ;Global list storing the wordtypes


(define (translate request) ;main function: uses different web-handlers to receive and send data
  (define data (request-post-data/raw request))
  (set! input (map(lambda (e)(string-split e " "))
                  (map string-trim(string-split (regexp-replace #rx"[\n\r]"(regexp-replace #rx"'" (bytes->string/utf-8 data) "°")"") #px"[\\.!?]+"))))
  
  (displayln (string-append "INPUT: " (bytes->string/utf-8 data))) ;For debugging: Showing what the Server received from the client
  
  (set! wordTypeList (map (lambda (e)
                            (getWordTypeList e)) input)) ;sets the wordTypeList storing the wordtypes
  
  (define str "Oops")
  (cond
    [(string=? "g->e" (first (flatten input)))(set! str (string-join (wordByWordSQL(rest (flatten input)))""))]
    [else
     (cond
       [(and (eq? 1 (length input)) (eq? (length (car input)) 1))(set! str (singleWordQuery(car (flatten input))))]
       [else (set! str
                  (regexp-replace #rx"°"
                                 (string-join (flatten (map
                                                        (lambda (inputPart wordTypeListPart)
                                                          (sentenceLoop inputPart wordTypeListPart)) input wordTypeList)) " ")"'"))])]) ;Long function for translating the sentence. Different sentences are stored in seperate lists. The function loops through them and trabslates them seperatly. Flatten making the list 1D again. It gets converted to a string.
  
  ;(displayln (string-append "OUTPUT: " str)) ;for debugging: displays the output
  (http-response str)) ;calls function to respond


(define (getWordTypeList input (typeList '()) (pos 0)) ;gets the wordtype of the inputed words by using the "is" functions of the different wordTypes.
  (cond                                                ;returns a list
    [(< pos (length input))
     (cond
       [(isArticle (list-ref input pos))(getWordTypeList input (cons "article" typeList) (+ 1 pos))] ;example: checks if word is an article ---> true? --> calls  getWordTypeList recusive and moves "article" into typeList
       [(isNoun (list-ref input pos))(getWordTypeList input (cons "noun" typeList)(+ 1 pos))]
       [(isPronoun (list-ref input pos))(getWordTypeList input(cons "pronoun" typeList)(+ 1 pos))]
       [(string? (isVerb (list-ref input pos)))(getWordTypeList input (cons "verb" typeList) (+ 1 pos))]
       [(isAdjective (list-ref input pos))(getWordTypeList input (cons "adjective" typeList)(+ 1 pos))]
       [(isPrepositon (list-ref input pos))(getWordTypeList input (cons "preposition" typeList)(+ 1 pos))]
       [(isNumeral (list-ref input pos))(getWordTypeList input (cons "numeral" typeList)(+ 1 pos))]
       [(isInterjection (list-ref input pos))(getWordTypeList input (cons "interjection" typeList)(+ 1 pos))]
       [else (getWordTypeList input (cons "name" typeList) (+ 1 pos))])]
    [else (reverse typeList)]))                      ;typeList gets returned reversed because cons places the word-types at the beginning of the list not at the end

;|------------------------------------------<|Nouns|>------------------------------------------------|
(define (isNoun ele) ;checks if given Word is a noun. It "ignores" s' and 's at the end of a noun
  (cond
    [(query-maybe-value mdbc (string-append "SELECT ger_noun FROM nouns WHERE eng_noun=" "'" ele "'"))]
    [(query-maybe-value mdbc (string-append "SELECT ger_noun FROM nouns WHERE eng_noun='" (string-trim ele "°s" #:left? #f) "'"))]
    [(query-maybe-value mdbc (string-append "SELECT ger_noun FROM nouns WHERE eng_noun='" (string-trim ele "s°" #:left? #f) "'"))]
    [else #f]))


(define (getNoun noun) ;gets the noun. It looks if the noun end with a specific ending and appends a char accordingly
  (cond
    [(regexp-match? #rx"°s$" noun)(cond
                                    [(regexp-match? #rx"^[a-z](.*[aeiou])?$" (string-trim noun "°s" #:left? #f))(string-append (query-value mdbc (string-append "SELECT ger_noun FROM nouns WHERE eng_noun='" (string-trim noun "°s" #:left? #f) "'"))"s")] ;if noun German ends with a vocal --> append "s"
                                    [else (string-append (query-value mdbc (string-append "SELECT ger_noun FROM nouns WHERE eng_noun='" (string-trim noun "°s" #:left? #f) "'"))"es")])] ;if noun does not end with a vocal --> append "es"
    [(regexp-match? #rx"s°$" noun)(query-value mdbc (string-append "SELECT ger_noun FROM nouns WHERE eng_noun='" (string-trim noun "s°" #:left? #f) "'"))] ; Grammatically I think there is no such thing like an "s" for genitves in plural
    [else (query-value mdbc (string-append "SELECT ger_noun FROM nouns WHERE eng_noun=" "'" noun "'"))])) ;if erverything is normal
; in order to perfectly translate noun, a word-to-word translation is not sufficiant: Die Flosse von den kleinen FischeN <---> Die Flosse von den kleinen Fische

;|-----------------------------------------<|Pronouns|>----------------------------------------------|
(define (isPronoun ele) ;checks if the given word is a pronoun. It does this by checking if it is in the interjection table.
  (cond
    [(query-maybe-value mdbc (string-append "SELECT ger_pronoun FROM pronouns WHERE eng_pronoun=" "'" ele "'"))#t]
    [else #f]))

(define (getPronoun pronoun)  ;does a basic wordtoword translation of a pronoun. This is totally sufficiant
  (query-value mdbc (string-append "SELECT ger_pronoun FROM pronouns WHERE eng_pronoun=" "'" pronoun "'")))

;|------------------------------------------<|Verbs|>------------------------------------------------|

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

(define (getPerson ele) ;gets the person of a noun / pronoun (ich, du, er, sie, es, etc.) by checking if its pronoun and what type of pronoun
  (cond
    [(query-maybe-value mdbc (string-append "SELECT ger_pronoun FROM pronouns WHERE eng_pronoun=" "'" ele "'"))
     (cond
       [(string-ci=? ele "I")'ich]
       [(or (string-ci=? ele "We") (string-ci=? ele "They"))'wirSie]
       [(or (string-ci=? ele "He") (string-ci=? ele "She") (string-ci=? ele "It"))'erSieEs]
       [(string-ci=? ele "You")'du])]
    [else 'erSieEs]))

(define (getVerbHelper person verb) ;function for building regular  Verbs, it translates "you" as "sie" because it isn's possible know which "you" is ment.
  (cond
    [(eq? person 'ich)(string-append (regVerbQuery verb) "e")]
    [(eq? person 'erSieEs)(string-append (regVerbQuery verb) "t")]
    [(or (eq? person 'du)(eq? person 'wirSie))(string-append (regVerbQuery verb) "en")]))


(define (getVerb subj verb form) ;translates the verb: with the given value the function determines if the verb is regular, is in third person or is inregular.
  (cond
    [(eq? form "rVerbES")(getVerbHelper (getPerson subj) (string-trim verb "es" #:left? #f))]
    [(eq? form "rVerbS")(getVerbHelper (getPerson subj) (string-trim verb "s" #:left? #f))]
    [(eq? form "iVerb")(iregVerbQuery verb)]
    [else (getVerbHelper (getPerson subj) verb)]))

;|------------------------------------<|Translation Interator|>--------------------------------------|
(define (sentenceLoop input wordTypeListPart (translation '()) (pos 0)) ;iterates through the input-sentence / the current subslist of of the input-list, and returnes it translated.
  (cond
    [(< pos (length input))
     (cond
       [(isArticle (list-ref input pos))(sentenceLoop input wordTypeListPart (cons (getArticle (list-ref input pos) pos wordTypeListPart input) translation) (+ 1 pos))]
       [(isNoun (list-ref input pos))(sentenceLoop input wordTypeListPart  (cons (getNoun (list-ref input pos)) translation)(+ 1 pos))]
       [(isPronoun (list-ref input pos))(sentenceLoop input wordTypeListPart  (cons (getPronoun (list-ref input pos)) translation)(+ 1 pos))]
       [(string? (isVerb (list-ref input pos)))(sentenceLoop input wordTypeListPart  (cons (getVerb (list-ref input (- pos 1)) (list-ref input pos) (isVerb (list-ref input pos))) translation)(+ 1 pos))] ;ACHTUNG: Könnte vielleicht zu Fehlern führen
       [(isAdjective (list-ref input pos))(sentenceLoop input wordTypeListPart  (cons (getAdjective (list-ref input pos) pos wordTypeListPart input )translation)(+ 1 pos))]
       [(isPrepositon (list-ref input pos))(sentenceLoop input wordTypeListPart (cons (getPreposition (list-ref input pos) pos wordTypeListPart input) translation)(+ 1 pos))]
       [(isNumeral (list-ref input pos))(sentenceLoop input wordTypeListPart (cons (getNumeral (list-ref input pos)) translation)(+ 1 pos))]
       [(isInterjection (list-ref input pos))(sentenceLoop input wordTypeListPart (cons (getInterjection (list-ref input pos)) translation)(+ 1 pos))]
       [else (sentenceLoop input wordTypeListPart  (cons (list-ref input pos) translation) (+ 1 pos))])]
    [else (reverse translation)])) ; with reverse the output has the same order as the input --> grammatically incorrect sometimes

;|============================================|Server|================================================|
(require web-server/servlet) 
(require web-server/servlet-env)

(define (http-response content)  ;function simplifying the response to the client
  (response/full
   200                                             ;HTTP response code.
   #"OK"                                        ;HTTP response message.
   (current-seconds)                    ;Timestamp.
   TEXT/HTML-MIME-TYPE       ; MIME type for content.
   '()                                               ; Additional HTTP headers.
   (list                                            ;Content (in bytes) to send to the client.
    (string->bytes/utf-8 content))))


;; URL routing table (URL dispatcher).
(define-values (dispatch generate-url)
  (dispatch-rules
   [("translate") #:method "post" translate] ;translate requests need to be postet to /translate
   [else (error "There is no procedure to handle the url.")]))

(define (request-handler request)
  (dispatch request))

;; Start the server.
(serve/servlet
 request-handler
 #:launch-browser? #f  ;disables auto-launcing the brower (#t would cause errors on headless systems)
 #:quit? #f
 #:listen-ip "127.0.0.1" ;#f wenn man von Geräten außerhalb
 #:port 8001                  ;opens the port 8001
 #:servlet-regexp #rx"")