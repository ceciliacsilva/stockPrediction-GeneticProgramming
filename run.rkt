#lang racket

(require "geneticOperation.rkt")
(require "expression.rkt")
(require "config.rkt")
(require "main.rkt")

(require "apiRequest.rkt")
(require net/http-client)
(require json)

(define (run-herokuapp fields)
  (define-values (jsonColumns jsonData) (json-serialize (string->jsexpr jsonResponse)))
  (define *gp1* (make-gp 50 (length fields) *operators* 3 0.6 0.4 3 0.7 3 100 100))
  
  (let ( (listPrice
          (map (lambda(a)
                 (for/list ( (field (in-list fields)) )
                   (match field
                     ( (list name mFator)
                       (* (cadr (assoc name a)) mFator) )
                     ( name
                       (cadr (assoc name a)) )   )
                   ))
               jsonData)) )
    ;;(displayln listPrice)
    (gp-run listPrice *gp1*)
    )
  )