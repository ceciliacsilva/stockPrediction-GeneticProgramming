#lang racket

(provide (all-defined-out))

(require plot)

(require "expression.rkt")

(plot-new-window? #t)

(plot-width 900)
(plot-height 600)

(define (plot-stockPrediction listPrice expr nInputs outputName)
  (let ( (stockInfos (for/list ( (infos (in-list listPrice))
                                 (i (in-naturals)) )
                       (list i
                             (caar infos))))
         (predictions (for/list ( (infos (in-list (reverse (reverse listPrice))))
                                  (i (in-naturals 1)) )
                        ;;(displayln (expression-run expr infos (inputs-create nInputs)))
                        (list i
                              (expression-run expr infos nInputs) ))) )
    
    (plot-file
     (list
      (tick-grid)
      (points stockInfos
              #:label "Stock Price"
              #:alpha 1
              #:sym 'fullcircle1
              #:color "blue")
      (points predictions
              #:label "Price Prediction"
              #:alpha 1
              #:sym 'fullcircle1
              #:color "red")
      ;(point-label (last stockInfos))
      ;(point-label (last predictions))
      )
     #:x-min -2
     #:y-min (* 0.95 (apply min (map cadr stockInfos)))
     #:x-max (* (length predictions) 1.05)
     #:y-max (* 1.05 (apply max (map cadr stockInfos)))
     outputName
     )
    )
  )