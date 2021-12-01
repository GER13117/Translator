#lang racket
(require "login.rkt")
(provide isInterjection getInterjection) ;provides isInterjection getInterjection for usage in other files
(require db)


(define (isInterjection ele) ;checks if the given word is an interjection. It does this by checking if it is in the interjection table.
  (cond
    [(query-maybe-value mdbc (string-append "SELECT ger_interj FROM interjections WHERE eng_interj=" "'" ele "'"))#t]
    [else #f]))

(define (getInterjection interj) ;does a basic wordtoword translation of an interjection. This is totally sufficiant
  (query-value mdbc (string-append "SELECT ger_interj FROM interjections WHERE eng_interj=" "'" interj "'")))