#lang racket

(require "geneticOperation.rkt")
(require "expression.rkt")
(require "config.rkt")
(require "apiRequest.rkt")

(define (gp-run listPrice gp)
  (let ( (np (gp-np gp))
         (operators (gp-operators gp))
         (depth (gp-depth gp))
         (pc (gp-pc gp))
         (pm (gp-pm gp))
         (endSimul (gp-endSimul gp))
         (nRepeat (gp-nRepeat gp)) )
    (let ( (popBegin
            (append (for/list ( (i (in-range (/ np 2))) )
                      (gen-expression-full operators depth))
                    (for/list ( (i (in-range (/ np 2))) )
                       (gen-expression-grow operators depth)) ))  )
      popBegin)
    )
  )
            