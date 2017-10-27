#lang racket

(provide (all-defined-out))

(define-struct gp
  (np operators depth pc pm endSimul nRepeat))