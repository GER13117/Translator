#lang racket
(require db)
(require "login.rkt")
(provide getNext getPrevious index-of getCase)

(define (index-of lst ele) ;implementation of index-of, because on older racket versions its not included in base
  (let loop ((lst lst)
             (idx 0))
    (cond ((empty? lst) #f)
         ((equal? (first lst) ele) idx)
         (else (loop (rest lst) (add1 idx))))))

(define (getNext wordType startPos wordTypeList input) ;returns the next word of a specific type. If there is no such word it returns false
  (cond
    [(not (boolean? (index-of (drop wordTypeList (+ 1 startPos)) wordType)))
     (list-ref input (+ startPos 1 (index-of (drop wordTypeList (+ 1 startPos)) wordType)))]
    [else #f]))

(define (getPrevious wordType startPos wordTypeList input)  ;returns the closest word of a specific type in front of a specific index. If there is no such word it returns false
  (cond
    [(not (boolean? (index-of (take wordTypeList startPos) wordType)))
     (list-ref input(index-of (take wordTypeList startPos) wordType))]
    [else #f]))



(define (getCase noun pos wordTypeList input) ;gets the case of a noun by checking multiple things. e.g. 's or the normal case of a preposition (to => Dativ)
  (cond
    [(or (regexp-match? #rx"(°s)|(s°)$" noun)(list? (member  "of" (take input pos)))) "genitiv"]
    [(string-ci=? "dativ" (query-value mdbc (string-append "SELECT gram_case FROM prepositions WHERE eng_prep='" (getPrevious "preposition" pos wordTypeList input) "' LIMIT 1")))"dativ"]
    [(member "verb" (take wordTypeList pos))
     (cond
       [(member "preposition" (take wordTypeList pos))
        (cond
          [(or (regexp-match? #rx"(°s)|(s°)$" noun)(string? (member  "of" (take input pos)))) "genitiv"]
          [(string-ci=? "dativ" (query-value mdbc (string-append "SELECT gram_case FROM prepositions WHERE eng_prep='" (getPrevious "preposition" pos wordTypeList input) "' LIMIT 1")))"dativ"]
          [(string-ci=? "akkusativ" (query-value mdbc (string-append "SELECT gram_case FROM prepositions WHERE eng_prep='" (getPrevious "preposition" pos wordTypeList input) "' LIMIT 1")))"akkusativ"]
          [else "genitiv"]
          )]
       [else "genitiv"])]
    [else "nominativ"]))
