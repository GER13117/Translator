#lang racket
(require "login.rkt")
(provide isNumeral getNumeral) ;provides iisNumeral and getNumeral for usage in other files

(require db)
(define (isNumeral ele) ;checks if the given word is a numeral. It does this by checking if it is in the numeral table.
  (cond
    [(query-maybe-value mdbc (string-append "SELECT ger_num FROM numerals WHERE eng_num=" "'" ele "'"))#t]
    [else #f]))


(define (getNumeral num) ;does a basic wordtoword translation of a numeral. This is totally sufficiant
  (query-value mdbc (string-append "SELECT ger_num FROM numerals WHERE eng_num=" "'" num "'")))