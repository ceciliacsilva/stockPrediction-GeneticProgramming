#lang racket

(require "geneticOperation.rkt")
(require "expression.rkt")
(require "config.rkt")
(require "main.rkt")

(require "apiRequest.rkt")
(require "plot.rkt")

(define (stockPrediction stock num)
  (let* ( (stockInfos (stock-getInfos stock num))
          (nInputs  (length (car stockInfos))) )
    ;;(displayln stockInfos)
    ;;(displayln nInputs)
    (let ( (np 50)
           (depth 4)
           (pc 0.7)
           (pm 0.3)
           (nTournament 3)
           (k 0.7)
           (nElite 10)
           (endSimul 50)
           (nRepeat 50) )
    (let ( (gpConfigs
            (make-gp np nInputs *operators* depth pc pm nTournament k nElite endSimul nRepeat)) )
      (let-values ( ((bestExpr pop)
                     (gp-run stockInfos gpConfigs))  )

        (displayln bestExpr)

        (plot-stockPrediction stockInfos (car bestExpr) (inputs-create nInputs))
        
        ))
      ))
  )

(define (stock-getInfos stock num)
    (let-values ( ((listMarketCap listStockPriceHistory) (stockGetInfos stock)) )
      (let ( (list1yearTreasureRate (1yearTreasureRate)) )
        (for/list ( (marketCap  (in-list (reverse listMarketCap)))
                    (stockPrice (in-list (reverse listStockPriceHistory)))
                    (1yearTreasure (in-list (reverse list1yearTreasureRate)))
                    (i (in-range num)) )
          ;;(displayln marketCap)
          `(
            ,(match stockPrice
               ( (list "d"    _
                       "o"    _
                       "h"    _
                       "l"    _
                       "c"    valueClose
                       "ma50" valueMa50
                       "ma200" _)
                 (/ (string->number valueClose) 1) ))
            
            ,(match marketCap
               ( (list "date" _  "v1" value)
                 (/ (string->number value) 10000) ))

            ,(match 1yearTreasure
               ( (list "date" _  "close" value)
                 (/ (string->number value) 100) ))
            
            )
          )
        ))
  )