#lang racket

(require db)
(require "login.rkt")
(require "helper_funcs.rkt")
(require "pronoun+.rkt")

(provide getAdjective)
(provide isAdjective)

;|----------------------------------------<|Adjectives|>---------------------------------------------|
; Akkusativ und Dativ wenn Präposition davor: Teilweise über Präposition bestimmbar

;Sorry Johann, dass ich bei dir rumgepfuscht habe. Du hattes klammer vergessen xoxo

;TODO: Komplette Logik für Adjektive:
;     - Fälle
;     - Geschlecht
;     - numerus
;Genitiv: the ball of the small boy -> der Ball des kleinEN Jungen

(define (AdjectiveQuery adjective_eng)
  (query-value mdbc (string-append "SELECT ger_adj FROM adjectives WHERE eng_adj=" "'" adjective_eng "'"))
  )
;funktioniert wie regVerbQuery, muss ggf. auf die Tabellen angepasst werden

(define (getAdjective adjective pos wordTypeList input)
  (displayln "sdfsdfd")
  (cond
    [(member "of" (take input pos)) (string-append (AdjectiveQuery adjective) "en")] ;wenn bis zu drei Wörter davor ein "of" steht -> Genitiv und "en" wird drangehängt
    [(string=? (getCase (getNext "noun" pos wordTypeList input) pos wordTypeList input) "dativ")    ;Dativ                                                                   ;Dativ
     (cond
       [(weakDeclination pos input) (string-append (AdjectiveQuery adjective) "en")]   ;schwache Deklination (der,die,das)
       [else                                                                           ;starke Deklination
        (cond
          [(eq? (query-value mdbc (string-append "SELECT numerus FROM nouns WHERE eng_noun=" "'"(getNext "noun" pos wordTypeList input)"'")) "plural") (string-append (AdjectiveQuery adjective) "en")]
          [else "sonenschwachsinn"]
          )
        ]
       )
     ]
    )
  )
        
(define (weakDeclination pos input)
  (not
   (eq?
    (or
     (member (list-ref input (- pos 1)) '("der" "die" "das"))
     (isPronoun (list-ref input (- pos 1)))
     )
    #f
    )
   )
  #t
  )

;schaut, ob schwach dekliniert werden muss    
    

(define (isAdjective ele)
  (cond
    [(query-maybe-value mdbc (string-append "SELECT ger_adj FROM adjectives WHERE eng_adj=" "'" ele "'LIMIT 1"))#t]
    [else #f]))

;(define (getCase subj))