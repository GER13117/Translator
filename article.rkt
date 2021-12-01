;|-----------------------------------------<|Articles|>----------------------------------------------|
#lang racket
(require db)
(require "login.rkt")
(require "helper_funcs.rkt")

(provide isArticle)
(provide getArticle)


; PLURAL UND SINGULAR

(define (isArticle ele) ;TODO: He walks the way --> Er geht den Weg (NICHT Er geht der Weg)
  (cond
    [(query-maybe-value mdbc (string-append "SELECT ger_article FROM articles WHERE eng_article=" "'" ele "'" "AND gender='male' LIMIT 1"))#t]
    [else #f]))

;TODO: Was tun wenn Nomen unbekannt
(define (getArticle article pos wordTypeList input)
  (cond
    [(= 0 pos)(query-value mdbc (string-append "SELECT ger_article FROM articles join nouns on articles.gender = nouns.gender AND articles.numerus = nouns.numerus WHERE eng_noun='" (getNext "noun" pos wordTypeList input) "'AND eng_article='" article "'AND `case`='nominativ'"))]
    [else 
     (cond
       [(string-ci=? "nominativ" (getCase (getNext "noun" pos wordTypeList input) (+ 1 pos) wordTypeList input))
        (query-value mdbc (string-append "SELECT ger_article FROM articles join nouns on articles.gender = nouns.gender  AND articles.numerus = nouns.numerus WHERE eng_noun='" (getNext "noun" pos wordTypeList input) "'AND eng_article='" article "'AND `case`='nominativ'"))]
       [(string-ci=? "dativ" (getCase (getNext "noun" pos wordTypeList input) (+ 1 pos) wordTypeList input))
        (query-value mdbc (string-append "SELECT ger_article FROM articles join nouns on articles.gender = nouns.gender AND articles.numerus = nouns.numerus WHERE eng_noun='" (getNext "noun" pos wordTypeList input) "'AND eng_article='" article "'AND `case`='dativ'"))]
       [(string-ci=? "akkusativ" (getCase (getNext "noun" pos wordTypeList input) (+ 1 pos) wordTypeList input))
        (query-value mdbc (string-append "SELECT ger_article FROM articles join nouns on articles.gender = nouns.gender AND articles.numerus = nouns.numerus WHERE eng_noun='" (getNext "noun" pos wordTypeList input) "'AND eng_article='" article "'AND `case`='akkusativ'"))]
       [(string-ci=? "genitiv" (getCase (getNext "noun" pos wordTypeList input) (+ 1 pos) wordTypeList input))
        (query-value mdbc (string-append "SELECT ger_article FROM articles join nouns on articles.gender = nouns.gender AND articles.numerus = nouns.numerus WHERE eng_noun='" (getNext "noun" pos wordTypeList input) "'AND eng_article='" article "'AND `case`='genitiv'"))]
       )]))