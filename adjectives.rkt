#lang racket

(require db)
(require "login.rkt")
(require "helper_funcs.rkt")
(require "pronouns.rkt")

(provide getAdjective)
(provide isAdjective)


(define (AdjectiveQuery adjective_eng)
  (query-value mdbc (string-append "SELECT ger_adj FROM adjectives WHERE eng_adj=" "'" adjective_eng "'"))
  )
;übersetzt das englische Wort nach Deutsch z.B. small ->klein

(define (getAdjective adjective pos wordTypeList input)
  (display "AdjektiveTriggered")
  (cond
    [(member "of" (take input pos)) (string-append (AdjectiveQuery adjective) "en")]
    
    [(string=? (getCase (getNext "noun" pos wordTypeList input) pos wordTypeList input) "nominativ") ;Nominativ
     (cond
       [(weakDeclination pos input)                                                                  ;schwache Deklination
        (cond
          [(string=? (query-value mdbc (string-append "SELECT numerus FROM nouns WHERE eng_noun=" "'"(getNext "noun" pos wordTypeList input)"'")) "singular") (string-append (AdjectiveQuery adjective) "e")] ;Nomen im Singular
          [(string=? (query-value mdbc (string-append "SELECT numerus FROM nouns WHERE eng_noun=" "'"(getNext "noun" pos wordTypeList input)"'")) "plural") (string-append (AdjectiveQuery adjective) "en")]  ;Nomen im Plural
          [else "bullshiot"]
          )
        ]
       [else                                                                                         ;starke Deklination
        (cond
          [(string=? (query-value mdbc (string-append "SELECT numerus FROM nouns WHERE eng_noun=" "'"(getNext "noun" pos wordTypeList input)"'")) "plural") (string-append (AdjectiveQuery adjective) "e")]    ;Nomen im Plural
          [(string=? (query-value mdbc (string-append "SELECT gender FROM nouns WHERE eng_noun=" "'"(getNext "noun" pos wordTypeList input)"'")) "female") (string-append (AdjectiveQuery adjective) "e")]     ;Nomen ist weiblich
          [(string=? (query-value mdbc (string-append "SELECT gender FROM nouns WHERE eng_noun=" "'"(getNext "noun" pos wordTypeList input)"'")) "male") (string-append (AdjectiveQuery adjective) "er")]      ;Nomen ist männlich
          [(string=? (query-value mdbc (string-append "SELECT gender FROM nouns WHERE eng_noun=" "'"(getNext "noun" pos wordTypeList input)"'")) "neutral") (string-append (AdjectiveQuery adjective) "es")]   ;Nomen ist neutral
          [else "blubb"]
          )
        ]
       )
     ]
    [(string=? (getCase (getNext "noun" pos wordTypeList input) pos wordTypeList input) "dativ")    ;Dativ
     (cond
       [(weakDeclination pos input) (string-append (AdjectiveQuery adjective) "en")]                ;schwache Deklination
       [else                                                                                        ;starke Deklination
        (cond
          [(string=? (query-value mdbc (string-append "SELECT numerus FROM nouns WHERE eng_noun=" "'"(getNext "noun" pos wordTypeList input)"'")) "plural") (string-append (AdjectiveQuery adjective) "en")];Nomen, auf das sich das Adjektiv bezieht, im Plural
          [(string=? (query-value mdbc (string-append "SELECT gender FROM nouns WHERE eng_noun=" "'"(getNext "noun" pos wordTypeList input)"'")) "female") (string-append (AdjectiveQuery adjective) "er")] ;Nomen ist weiblich
          [else (string-append (AdjectiveQuery adjective) "em")]                                    ;Nomen ist neutral oder männlich
          ) 
        ]
       )
     ]
    [(string=? (getCase (getNext "noun" pos wordTypeList input) pos wordTypeList input) "akkusativ");Akkusativ
     (cond
       [(weakDeclination pos input)                                                                  ;schwache Deklination
        (cond
          [(string=? (query-value mdbc (string-append "SELECT numerus FROM nouns WHERE eng_noun=" "'"(getNext "noun" pos wordTypeList input)"'")) "plural") (string-append (AdjectiveQuery adjective) "en")]  ;Nomen im Plural
          [(string=? (query-value mdbc (string-append "SELECT gender FROM nouns WHERE eng_noun=" "'"(getNext "noun" pos wordTypeList input)"'")) "male") (string-append (AdjectiveQuery adjective) "en")]     ;Nomen ist männlich
          [(string=? (query-value mdbc (string-append "SELECT gender FROM nouns WHERE eng_noun=" "'"(getNext "noun" pos wordTypeList input)"'")) "female") (string-append (AdjectiveQuery adjective) "e")]    ;Nomen ist weiblich
          [else (string-append (AdjectiveQuery adjective) "e")]                                                                                                                                               ;Nomen ist neutral
          )
        ]
       [else                                                                                         ;starke Deklination
        (cond
          [(string=? (query-value mdbc (string-append "SELECT numerus FROM nouns WHERE eng_noun=" "'"(getNext "noun" pos wordTypeList input)"'")) "plural") (string-append (AdjectiveQuery adjective) "e")]   ;Nomen im Plural
          [(string=? (query-value mdbc (string-append "SELECT gender FROM nouns WHERE eng_noun=" "'"(getNext "noun" pos wordTypeList input)"'")) "female") (string-append (AdjectiveQuery adjective) "e")]    ;Nomen ist weiblich
          [(string=? (query-value mdbc (string-append "SELECT gender FROM nouns WHERE eng_noun=" "'"(getNext "noun" pos wordTypeList input)"'")) "male") (string-append (AdjectiveQuery adjective) "en")]     ;Nomen ist männlich
          [else (string-append (AdjectiveQuery adjective) "es")]                                                                                                                                              ;Nomen ist neutral
          )
        ]
       )
     ]
    [else "omegagigablubb"]
    )
  )
;die Funktion passt die Endung der Adjektive je nach Kasus, Numerus und Genus an

        
(define (weakDeclination pos input)
  (cond
    [(> pos 0)
     (not
      (eq?
       (or
        (member (list-ref input (- pos 1)) '("der" "die" "das" "the"))
        (isPronoun (list-ref input (- pos 1)))
        )
       #f
       )
      )
     ]
    [else #f]
    )
  )

;checkt, ob vor dem Adjektiv ein bestimmter Artikel oder ein Pronomen steht, wenn ja wird #t zurückgegeben  
    

(define (isAdjective ele)
  (cond
    [(query-maybe-value mdbc (string-append "SELECT ger_adj FROM adjectives WHERE eng_adj=" "'" ele "'LIMIT 1"))#t]
    [else #f]))

;checkt, ob es sich bei dem Wort um ein Adjektiv handelt. Hierfür wird die Tabelle mit Adjektiven durchsucht
