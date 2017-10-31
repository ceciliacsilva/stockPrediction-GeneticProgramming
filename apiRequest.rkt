#lang racket

(require net/http-client)
(require json)

(provide (all-defined-out))

(define (http-request-get host)
  (let ( (connectionRequest (http-conn-open host)) )
    (lambda (endPoint [listQueryString '()])
      (let* ( (queryString (list->queryString listQueryString))
              (url (string-append endPoint "?" queryString)) )
        (let-values ( ((responseStatus responseHeader responseBody)
                       (http-conn-sendrecv! connectionRequest url)) )
          (port->string responseBody)) 
        ))
    )
  )

(define (list->queryString listQueryString)
  (string-join 
   (map (lambda(pair)
                (string-join pair "=")) listQueryString)
   "&")
  )

(define (json-serialize jsonResponse)
  (match jsonResponse
    ( (hash-table ( 'datatable
                    (hash-table ('columns c)
                                ('data d))))
      (let ( (columnsJson (map (lambda(a) (hash-ref a 'name)) c)) )
        (values columnsJson
                (for/list ( (infosDia (in-list d)) )
                  (map (lambda(name value) (list name value))
                       columnsJson
                       infosDia)
                  )
                );;endValues
        )
      ))
  )

(define http->responseJson (http-request-get "quantprice.herokuapp.com"))
(define jsonResponse (http->responseJson "/api/v1.1/scoop/period" '(("tickers" "MSFT") ("begin" "2017-10-19"))))