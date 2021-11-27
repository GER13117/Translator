#lang racket

(require json)
(require net/url)
(require http/request)


(define (httpPost str_input)
  (bytes->string/utf-8 (call/output-request
                        "1.1"
                        "POST"
                        "http://localhost:8001/translate"
                        (string->bytes/utf-8 str_input)
                        #f
                        empty
                        read-entity/bytes)))



(define (userInput str_input)
  (displayln (httpPost str_input))
  (userInput (read-line)))

(userInput (read-line))