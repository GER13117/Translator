#lang racket
(require db)
(require "login.rkt")
(provide getNext index-of getCase)

(define (index-of lst ele) ;in alten (Schul-)Versionen nicht in base
  (let loop ((lst lst)
             (idx 0))
    (cond ((empty? lst) #f)
         ((equal? (first lst) ele) idx)
         (else (loop (rest lst) (add1 idx))))))

(define (getNext wordType startPos wordTypeList input)
  (cond
    [(not (boolean? (index-of (drop wordTypeList (+ 1 startPos)) wordType)))
     (list-ref input (+ startPos 1 (index-of (drop wordTypeList (+ 1 startPos)) wordType)))]
    [else #f]))

(define (getPrevious wordType startPos wordTypeList input)
  (cond
    [(not (boolean? (index-of (take wordTypeList startPos) wordType)))
     (list-ref input(index-of (take wordTypeList startPos) wordType))]
    [else #f]))



;TODO: Make this work properly
(define (getCase noun pos wordTypeList input)
  (cond
     [(or (regexp-match? #rx"(''s)|(s'')$" noun)(list? (member  "of" (take input pos)))) "genitiv"]
    [(member "verb" (take wordTypeList pos))
     (cond
       [(member "preposition" (take wordTypeList pos))
        (cond
          [(or (regexp-match? #rx"(''s)|(s'')$" noun)(string? (member  "of" (take input pos)))) "genitiv"]
          [(string-ci=? "dativ" (query-value mdbc (string-append "SELECT gram_case FROM prepositions WHERE eng_prep='" (getPrevious "preposition" pos wordTypeList input) "' LIMIT 1")))"dativ"]
          [(string-ci=? "akkusativ" (query-value mdbc (string-append "SELECT gram_case FROM prepositions WHERE eng_prep='" (getPrevious "preposition" pos wordTypeList input) "' LIMIT 1")))"akkusativ"]
          [else "genitiv"]
          )]
       [else "genitiv"])]
    [else "nominativ"]))
