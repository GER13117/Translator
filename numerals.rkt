#lang racket
(require "login.rkt")
(provide isNumeral getNumeral)

(require db)
(define (isNumeral ele)
  (cond
    [(query-maybe-value mdbc (string-append "SELECT ger_num FROM numerals WHERE eng_num=" "'" ele "'"))#t]
    [else #f]))
(define (getNumeral num)
  (query-value mdbc (string-append "SELECT ger_num FROM numerals WHERE eng_num=" "'" num "'")))