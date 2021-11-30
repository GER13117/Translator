#lang racket
(require "login.rkt")
(provide isInterjection getInterjection)

(require db)
(define (isInterjection ele)
  (cond
    [(query-maybe-value mdbc (string-append "SELECT ger_interj FROM interjections WHERE eng_interj=" "'" ele "'"))#t]
    [else #f]))
(define (getInterjection interj)
  (query-value mdbc (string-append "SELECT ger_interj FROM interjections WHERE eng_interj=" "'" interj "'")))