#lang racket

(require json)
(require net/url)
(require http/request)

;(bytes->jsexpr (call/input-request
;  "1.1"
;  "GET"
;  "http://192.168.180.34/api/"
;  empty
;  read-entity/bytes))

;(bytes->jsexpr (call/output-request
;  "1.1"
;  "PUT"
;  "http://192.168.180.34/api/api-search.php"
;  (jsexpr->bytes (hasheq 'search "one"))
;  #f
;  empty
;  read-entity/bytes))


(call/output-request
  "1.1"
  "POST"
  "http://localhost:8001/example-post"
  #"I am stupid"
  #f
  empty
  read-entity/bytes)