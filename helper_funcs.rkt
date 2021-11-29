#lang racket
(provide getNext)
(provide index-of)

(define (getNext wordType startPos wordTypeList input)
  (cond
    [(not (boolean? (index-of (drop wordTypeList (+ 1 startPos)) wordType)))
     (list-ref input (+ startPos 1 (index-of (drop wordTypeList (+ 1 startPos)) wordType)))]
    [else #f]))



(define (index-of lst ele) ;in alten (Schul-)Versionen nicht in base
  (let loop ((lst lst)
             (idx 0))
    (cond ((empty? lst) #f)
          ((equal? (first lst) ele) idx)
          (else (loop (rest lst) (add1 idx))))))

