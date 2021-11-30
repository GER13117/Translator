;|-----------------------------------------<|Articles|>----------------------------------------------|
#lang racket
(require db)
(require "login.rkt")
(require "helper_funcs.rkt")

(provide isArticle)
(provide getArticle)


(define (isArticle ele) ;TODO: He walks the way --> Er geht den Weg (NICHT Er geht der Weg)
  (cond
    [(query-maybe-value mdbc (string-append "SELECT ger_article FROM articles WHERE eng_article=" "'" ele "'" "AND gender='male'"))#t]
    [else #f]))

;TODO: Bedingungen für alle Fälle (logik von getPreposition übernehmen --> 
(define (getArticle article pos wordTypeList input)
  (cond
    [(string-ci=? "nominativ" (getCase (getNext "noun" pos wordTypeList input) (+ 1 pos) wordTypeList input))
                  (query-value mdbc (string-append "SELECT ger_article FROM articles join nouns on articles.gender = nouns.gender WHERE eng_noun='" (getNext "noun" pos wordTypeList input) "'AND eng_article='" article "'AND `case`='nominativ'"))]
    [(string-ci=? "dativ" (getCase (getNext "noun" pos wordTypeList input) (+ 1 pos) wordTypeList input))
                  (query-value mdbc (string-append "SELECT ger_article FROM articles join nouns on articles.gender = nouns.gender WHERE eng_noun='" (getNext "noun" pos wordTypeList input) "'AND eng_article='" article "'AND `case`='dativ'"))]
    [(string-ci=? "akkusativ" (getCase (getNext "noun" pos wordTypeList input) (+ 1 pos) wordTypeList input))
                  (query-value mdbc (string-append "SELECT ger_article FROM articles join nouns on articles.gender = nouns.gender WHERE eng_noun='" (getNext "noun" pos wordTypeList input) "'AND eng_article='" article "'AND `case`='nominativ'"))]
    ;Cases für Akkusativ, Genitiv
    ))
