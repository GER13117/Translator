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
  (query-value mdbc (string-append "SELECT ger_adj FROM adjectives WHERE eng_adj=" "'" adjective_eng "'")))
   ;funktioniert wie regVerbQuery, muss ggf. auf die Tabellen angepasst werden

(define (getAdjective adj pos wordTypeList input)
  (cond
    [(eq? (wordsbefore pos input) #t) (string-append (AdjectiveQuery adj) "en")]
    [else (display "ist kein Genitiv")])
  )
   ;wenn bis zu drei Wörter davor ein "of" steht -> Genitiv und "en" wird drangehängt

(define (wordsbefore pos input)
  (cond
    [eq? (list-ref input (- pos 1)) "of"]
    [eq? (list-ref input (- pos 2)) "of"]
    [eq? (list-ref input (- pos 3)) "of"]
    [else #f]))

(define (isAdjective ele)
  (cond
    [(query-maybe-value mdbc (string-append "SELECT ger_adj FROM adjectives WHERE eng_adj=" "'" ele "'LIMIT 1"))#t]
    [else #f]))

;(define (getCase subj))