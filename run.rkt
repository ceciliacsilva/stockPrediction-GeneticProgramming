#lang racket

(require "geneticOperation.rkt")
(require "expression.rkt")
(require "config.rkt")
(require "main.rkt")

(require "apiRequest.rkt")
(require "plot.rkt")

(define (stockPrediction stock num nameSimul)
  (let* ( (stockInfos (stock-getInfos stock num))
          (nInputs  (length (caar stockInfos)))
          (outputPlot (string-append nameSimul "/" "plot.png"))
          (outputExpr (string-append nameSimul "/" "expr.txt"))
          (outputPop  (string-append nameSimul "/" "pop.txt"))
          (outputStock (string-append nameSimul "/" "stock.txt")) )
    ;;(displayln stockInfos)
    ;;(displayln nInputs)
    (make-directory* nameSimul)
    (let ( (np 70)
           (depth 4)
           (pc 0.7)
           (pm 0.3)
           (nTournament 3)
           (k 0.7)
           (nElite 10)
           (endSimul 100)
           (nRepeat 100) )
    (let ( (gpConfigs
            (make-gp np nInputs *operators* depth pc pm nTournament k nElite endSimul nRepeat)) )
      (let-values ( ((bestExpr pop)
                     (gp-run stockInfos gpConfigs))  )

        (file-bkpInfo bestExpr outputExpr)
        (file-bkpInfo pop outputPop)
        (file-bkpInfo stockInfos outputStock)

        (plot-stockPrediction stockInfos (car bestExpr) (inputs-create nInputs) outputPlot)
        
        ))
      ))
  )

(define (file-bkpInfo info name)
  (define out (open-output-file name #:exists 'replace))
  (write info out)
  (close-output-port out)
  )

(define (stock-getInfos stock num)
  (reverse (stock-getInfosMatch stock num))
  )

(define (stock-getInfosMatch stock num)
    (let-values ( ((listMarketCap listStockPriceHistory) (stockGetInfos stock)) )
      (let ( (list1yearTreasureRate (1yearTreasureRate)) )
        (for/list ( (marketCap  (in-list (reverse listMarketCap)))
                    (stockPrice (in-list (reverse listStockPriceHistory)))
                    (1yearTreasure (in-list (reverse list1yearTreasureRate)))
                    (i (in-range num)) )
          ;;(displayln marketCap)
          `((
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
            .
            ,(match stockPrice
               ( (list "d"    date
                       "o"    _
                       "h"    _
                       "l"    _
                       "c"    _
                       "ma50" _
                       "ma200" _)
                 date ))
            
            )
          )
        ))
  )