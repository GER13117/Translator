#lang racket

(require json)
(require net/url)
(require http/request)
(display
 "Welcome to our Calculator
By now it just features a few things.
Things like adjectives aren't working by now. But feel free to contribute.
______________________________________________________________________________ \n
To start the calculator just type in the english sentence you want to translate. If you are lucky you might get a translation. Longer texts take a longer time to be translated, be patient! \n\n")

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