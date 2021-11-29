#lang racket

(require db)
(require "login.rkt")
(provide isPrepositon)
(provide getPreposition)
(provide getNext)

;TODO: Wenn für Nomen gesucht wird auch Pronomen beachten
(define (getNext wordType startPos wordTypeList input)
  (cond
    [(not (boolean? (index-of (drop wordTypeList (+ 1 startPos)) wordType)))
     (list-ref input (+ startPos 1 (index-of (drop wordTypeList (+ 1 startPos)) wordType)))]
    [else #f]))


; FUNKTIONIERT EXTREM BESCHISSEN: Die Implementierung von Pronomen lief nicht gut: pronomen müssen gesondert behandelt werden --> "person" als usecase in die Datenbank einplfegen --> Case zur Bildung von prepositionen für "person"

;BUGS:
;- Fehler wenn pronomen aber kein nomen vorhanden --> list-ref returned ein komisches #f, dass weder als string noch als boolean verwendet werden kann --> LÖSUNG: ????
;- ger_preposotion muss dynamisch (passend ob pronomen oder nomen festgelegt werden)

;TODO:
;- Namen (unbekannte Worte) sollen als Person behandelt werden --> WIE  AUCH IMMER


(define (isPrepositon ele)
  (cond
    [(query-maybe-value mdbc (string-append "SELECT ger_prep FROM prepositions WHERE eng_prep=" "'" ele "'LIMIT 1"))#t]
    [else #f]))

;TODO: Genitiv und Akkusativ hinzufügen
(define (getPreposition preposition pos wordTypeList input)
  (define nextNoun #f)                                         ;Das ist nicht schön
  (define nextPronoun #f)                                    ;Das auch nicht
  (define ger_preposition #f)                               ;Das auch absolut nicht
  (cond
    [(string? (getNext "noun" pos))(
                                    (lambda ()
                                      (set! nextNoun (getNext "noun" pos))
                                      (set! ger_preposition (query-value mdbc (string-append "SELECT ger_prep FROM prepositions join nouns on prepositions.usecase = nouns.sense WHERE eng_prep='" preposition "' AND eng_noun ='" nextNoun "'")))))]
    [(string? (getNext "pronoun" pos))(
                                       (lambda ()
                                         (set! nextPronoun (getNext "pronoun" pos))
                                         (set! ger_preposition (query-value mdbc (string-append "SELECT ger_prep FROM prepositions WHERE eng_prep='" preposition "' AND usecase ='""smallplace" "'")))))])      ;TODO: smallplace durch person


  (define nextObjectQuery
    (cond
      [(and (string? nextNoun) (string? nextPronoun))
       (cond
         [(< (index-of input nextPronoun) (index-of input nextNoun))(string-append " pronouns WHERE eng_pronoun'" nextPronoun "'")]
         [else (string-append " nouns WHERE eng_noun='" nextNoun "'")])]
      [else (cond
              [(string? nextNoun)(string-append " nouns WHERE eng_noun='" nextNoun "'")]
              [else (string-append " pronouns WHERE eng_pronoun='" nextPronoun "'")])]))
  
;(set! ger_preposition (query-value mdbc (string-append "SELECT ger_prep FROM prepositions join nouns on prepositions.usecase = nouns.sense WHERE eng_prep='" preposition "' AND eng_noun ='" nextNoun "'")))
;(set! ger_preposition (query-value mdbc (string-append "SELECT ger_prep FROM prepositions WHERE eng_prep='" preposition "' AND usecase ='""smallplace" "'")))
  
  (cond
    [(not (string-ci=? "article" (list-ref wordTypeList (+ 1 pos))))
      (cond
        [(regexp-match? #rx"^[a-z](.*[aeiou])?$" ger_preposition)
         (string-append ger_preposition (cond
                                          ;Genitiv
                                          [(string-ci=? (query-value mdbc (string-append "SELECT gram_case FROM prepositions WHERE ger_prep='" ger_preposition "'")) "dativ")
                                           (cond
                                             [(string-ci=? "plural" (query-value mdbc (string-append "SELECT numerus FROM"nextObjectQuery)))"n"]
                                             [else
                                              (cond
                                                [(string-ci=? "male" (query-value mdbc (string-append "SELECT gender FROM" nextObjectQuery)))"m"]
                                                [(string-ci=? "female" (query-value mdbc (string-append "SELECT gender FROM"nextObjectQuery)))"r"]
                                                [(string-ci=? "neutral" (query-value mdbc (string-append "SELECT gender FROM" nextObjectQuery)))"m"])])]
                                          ;Akkusativ
                                          ))]
        [else (string-append ger_preposition (cond
                                               [(string-ci=? "bigplace" (query-value mdbc (string-append "SELECT sense FROM" nextObjectQuery)))""] ;Länder haben keinen Artikel
                                               [else (cond                                                                                                              ;TODO: Condition für smallplace anstatt von else: Es ist nicht small place wenn nicht bigplace
                                                       [(string-ci=? "plural" (query-value mdbc (string-append "SELECT numerus FROM"nextObjectQuery)))" den"]
                                                       [else
                                                        (cond
                                                          [(string-ci=? "male" (query-value mdbc (string-append "SELECT gender FROM" nextObjectQuery)))" dem"]
                                                          [(string-ci=? "female" (query-value mdbc (string-append "SELECT gender FROM" nextObjectQuery)))" der"]
                                                          [(string-ci=? "neutral" (query-value mdbc (string-append "SELECT gender FROM" nextObjectQuery)))" dem"])])]))])]
    [else ger_preposition]));TODO: Funktion auch für Pronomen verwendbar machen:
                                  ;Findung von ger_preposition: nextNoun und proNoun
;                                                               --> condition um gucken welche Positon kleiner ist
;                                                               --> festlegen von nextObject (als Ersatz für nextNoun) und ger_preposition

