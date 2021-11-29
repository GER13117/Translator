#lang racket

(require db)
(require "login.rkt")

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
  (cond
    [(ofBefore pos input) (string-append (AdjectiveQuery adjective) "en")] ;wenn bis zu drei Wörter davor ein "of" steht -> Genitiv und "en" wird drangehängt
    [(eq? (getCase adjective) "dativ")
     (cond
       [(weakDeclination pos input) (string-append (AdjectiveQuery adjective) "en")] ;bei schwacher Deklination
       [else                                                                         ;bei starker Deklination
        (cond
          [(eq? (nextNoun pos input) plural) (string-append (AdjectiveQuery adjective) "en")] ;bei Plural
          )
        ]
       )
     ]
    )
  )
;nextNoun -> holt sich das nächste Nomen
        
       




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
  )

;schaut, ob schwach dekliniert werden muss

(define (ofBefore pos input)
  (cond
    [(eq? (list-ref input (- pos 1)) "of")]
    [(eq? (list-ref input (- pos 2)) "of")]
    [(eq? (list-ref input (- pos 3)) "of")]
    [else #f]))
;schaut, ob eins der drei Wörter vor der pos "of" ist

(define (isAdjective ele)
  (cond
    [(query-maybe-value mdbc (string-append "SELECT ger_adj FROM adjectives WHERE eng_adj=" "'" ele "'LIMIT 1"))#t]
    [else #f]))

;(define (getCase subj))