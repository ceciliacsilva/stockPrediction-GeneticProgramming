#lang racket

(provide (all-defined-out))

(define-struct gp
  (np nInputs operators depth pc pm nTournament k nElite [endSimul #:mutable] [nRepeat #:mutable] [fitnessFunction #:auto]))