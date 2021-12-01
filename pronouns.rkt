#lang racket

(require db)
(require "login.rkt")
(provide isPronoun getPronoun) ;provides isPronoun and getPronoun for usage in other files

(define (isPronoun ele) ;checks if the given word is a pronoun. It does this by checking if it is in the pronoun table.
  (cond
    [(query-maybe-value mdbc (string-append "SELECT ger_pronoun FROM pronouns WHERE eng_pronoun=" "'" ele "'"))#t]
    [else #f]))

(define (getPronoun pronoun) ;does a basic wordtoword translation of a pronoun. This is totally sufficiant
  (query-value mdbc (string-append "SELECT ger_pronoun FROM pronouns WHERE eng_pronoun=" "'" pronoun "'")))