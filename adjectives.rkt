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
  (query-value mdbc (string-append "SELECT ger_wortstamm FROM adjectives WHERE eng_adjective=" "'" adjective_eng "'")))
   ;funktioniert wie regVerbQuery, muss ggf. auf die Tabellen angepasst werden

(define (getAdjective adj pos wordTypeList input)
  (cond
    [(eq? wordsbefor #t) (string-append (AdjectiveQuery adj) "en")]
    [else (display "ist kein Genitiv")])
  )
   ;wenn bis zu drei Wörter davor ein "of" steht -> Genitiv und "en" wird drangehängt

(define (wordsbefor pos)
  (cond
    [eq? (list-ref input (- pos 1)) "of"]
    [eq? (list-ref input (- pos 2)) "of"]
    [eq? (list-ref input (- pos 3)) "of"]
    [else #f]))



(define (isAdjective adj)
  #f)
;(define (getCase subj))