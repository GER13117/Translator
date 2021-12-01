#lang racket

(require json)
(require net/url)
(require http/request)
(display
 "Welcome to our english-to-german simple-present Translator!
______________________________________________________________________________ \n
To start the translator just type in the english sentence you want to translate and press enter. Longer texts take a longer time to be translated, be patient! \n\n")

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