#lang racket
(require web-server/servlet
         web-server/servlet-env)

(require json)
(require net/url)
(require http/request)


 
(define (my-app req)
  (response/jsexpr
   (bytes->jsexpr (call/input-request
                   "1.1"
                   "GET"
                   "http://192.168.180.34/api/"
                   empty
                   read-entity/bytes))))
 
(serve/servlet my-app
               #:listen-ip #f
               #:servlet-path "/hello.rkt")



;IO-Server

(require web-server/servlet) 
(require web-server/servlet-env)

(define (http-response content)  
  (response/full
    200                  ; HTTP response code.
    #"OK"                ; HTTP response message.
    (current-seconds)    ; Timestamp.
    TEXT/HTML-MIME-TYPE  ; MIME type for content.
    '()                  ; Additional HTTP headers.
    (list                ; Content (in bytes) to send to the browser.
      (string->bytes/utf-8 content))))

(define (show-time-page request)
  (http-response (number->string (current-seconds))))

(define (greeting-page request)  
  (http-response (list-ref '("Hi" "Hello") (random 2))))

(define (example-post request)
  (define data (request-post-data/raw request))
  (define str (format "got post data: ~v" data))
  (displayln str)
  (http-response str))

;; URL routing table (URL dispatcher).
(define-values (dispatch generate-url)
  (dispatch-rules
    [("time") show-time-page]
    [("hello") greeting-page]
    [("example-post") #:method "post" example-post] ; <=== NEW
    [else (error "There is no procedure to handle the url.")]))

(define (request-handler request)
  (dispatch request))

;; Start the server.
(serve/servlet
  request-handler
  #:launch-browser? #f
  #:quit? #f
  #:listen-ip "127.0.0.1"
  #:port 8001
  #:servlet-regexp #rx"")
