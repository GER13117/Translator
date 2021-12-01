;|-----------------------------------------<|Module for translating articles|>----------------------------------------------|
#lang racket
(require db)
(require "login.rkt")
(require "helper_funcs.rkt")

(provide isArticle getArticle) ;provide isArticle and getArticle to be used in other modules

(define (isArticle ele) ;checks if the given word is an article, if so it returns true
  (cond
    [(query-maybe-value mdbc (string-append "SELECT ger_article FROM articles WHERE eng_article=" "'" ele "'" "AND gender='male' LIMIT 1"))#t]
    [else #f]))

(define (nextNoun pos wordTypeList input) ;this function prevents errors when no noun is given by setting nextNoun to "fish" (some random noun)
  (cond
    [(string? (getNext "noun" pos wordTypeList input)) (getNext "noun" pos wordTypeList input)]
    [else "fish"]))

(define (getArticle article pos wordTypeList input) ;gets / translates the article. It peforms multiple checks in order to determine the case of the noun the article is used for. Than it peforms the correct Query to the database
  (cond
    [(= 0 pos)(query-value mdbc (string-append "SELECT ger_article FROM articles join nouns on articles.gender = nouns.gender AND articles.numerus = nouns.numerus WHERE eng_noun='" (nextNoun pos wordTypeList input) "'AND eng_article='" article "'AND `case`='nominativ'"))]
    [else 
     (cond
       [(string-ci=? "nominativ" (getCase (getNext "noun" pos wordTypeList input) (+ 1 pos) wordTypeList input))
        (query-value mdbc (string-append "SELECT ger_article FROM articles join nouns on articles.gender = nouns.gender  AND articles.numerus = nouns.numerus WHERE eng_noun='" (nextNoun pos wordTypeList input) "'AND eng_article='" article "'AND `case`='nominativ'"))]
       [(string-ci=? "dativ" (getCase (getNext "noun" pos wordTypeList input) (+ 1 pos) wordTypeList input))
        (query-value mdbc (string-append "SELECT ger_article FROM articles join nouns on articles.gender = nouns.gender AND articles.numerus = nouns.numerus WHERE eng_noun='" (nextNoun pos wordTypeList input) "'AND eng_article='" article "'AND `case`='dativ'"))]
       [(string-ci=? "akkusativ" (getCase (getNext "noun" pos wordTypeList input) (+ 1 pos) wordTypeList input))
        (query-value mdbc (string-append "SELECT ger_article FROM articles join nouns on articles.gender = nouns.gender AND articles.numerus = nouns.numerus WHERE eng_noun='" (nextNoun pos wordTypeList input) "'AND eng_article='" article "'AND `case`='akkusativ'"))]
       [(string-ci=? "genitiv" (getCase (getNext "noun" pos wordTypeList input) (+ 1 pos) wordTypeList input))
        (query-value mdbc (string-append "SELECT ger_article FROM articles join nouns on articles.gender = nouns.gender AND articles.numerus = nouns.numerus WHERE eng_noun='" (nextNoun pos wordTypeList input) "'AND eng_article='" article "'AND `case`='genitiv'"))]
       )]))