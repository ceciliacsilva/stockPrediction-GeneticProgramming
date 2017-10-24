#lang racket

(provide (all-defined-out))

(define-struct gp
  (np depth pc pm endSimul nRepeat))