#lang racket

(require "geneticOperation.rkt")
(require "expression.rkt")
(require "config.rkt")
(require "main.rkt")

(require "apiRequest.rkt")
(require net/http-client)
(require json)

;;> (define a (run "aapl" 50))
;;> (define *gp1* (make-gp 10 2 *operators* 4 0.7 0.1 3 0.7 10 10 10))
;;> (define c (gp-run a *gp1*))
;;> c

(define (run stock num)
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