#lang racket

(provide (all-defined-out))

(define-struct gp
  (np operators depth pc pm nTournament k nElite endSimul nRepeat))