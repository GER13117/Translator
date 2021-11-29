#lang racket
(require db)
(require "login.rkt")
(provide getNext)
(provide index-of)
(provide splitListAtPos)
(provide getCase)

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


(define (splitListAtPos index lst (res_lst '()))
  (cond
    [(> index 0) (splitListAtPos (- index 1) (rest lst) (cons (first lst) res_lst ))]
        [else (reverse res_lst) ]))

(define (getCase noun pos wordTypeList input)
  (cond
   [(member 'verb (splitListAtPos pos wordTypeList ))
    (cond
      [(member 'preposition (splitListAtPos pos wordTypeList)(
        [(or (regexp-match? #rx"''s$" noun) (regexp-match? #rx"s''$" noun))"genitiv"] ;TODO: wenn of davor
        [(string-ci=? "dativ" query-value (string-append "SELECT gram_case FROM prepositions WHERE eng_prep='" (list-ref input pos) "'LIMIT 1"))"dativ"] ;TODO: Make the table usable even with multiple meanings of a preposition
        [(string-ci=? "akkusativ" query-value (string-append "SELECT gram_case FROM prepositions WHERE eng_prep='" (list-ref input pos) "'LIMIT 1"))"akkusativ"]
      ))])]
   [else "nominativ"]))
