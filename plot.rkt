#lang racket

(provide (all-defined-out))

(require plot)

(require "expression.rkt")

(plot-new-window? #t)

(plot-width 900)
(plot-height 600)

(define (plot-stockPrediction listPrice expr nInputs)
  (plot
   (list
    (points (for/list ( (infos (in-list listPrice))
                        (i (in-naturals)) )
              (list i
                    (car infos)))
            
            #:alpha 1
            #:sym 'fullcircle1
            #:color "blue")
    (points (for/list ( (infos (in-list (reverse (cdr (reverse listPrice)))))
                        (i (in-naturals 1)) )
              ;;(displayln (expression-run expr infos (inputs-create nInputs)))
              (list i
                    (expression-run expr infos nInputs) ))
            
            #:alpha 1
            #:sym 'fullcircle1
            #:color "red")
    )
   )
  )