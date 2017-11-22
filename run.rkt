#lang racket

(require "geneticOperation.rkt")
(require "expression.rkt")
(require "config.rkt")
(require "main.rkt")

(require "apiRequest.rkt")
(require "plot.rkt")

(define *output* "output/")

(define (run-stockPrediction nameSimul)
  (let* ( (outputPlot (string-append *output* nameSimul "/" "plot.png"))
          (outputExpr (string-append *output* nameSimul "/" "expr.txt"))
          (outputPop  (string-append *output* nameSimul "/" "pop.txt"))
          (outputStock (string-append *output* nameSimul "/" "stock.txt"))
          (outputInfos (string-append *output* nameSimul "/" "infos.rkt"))
          (outputConfigs (string-append *output* nameSimul "/" "config.rkt")) )
    (let ( (infosToSave (read-file outputInfos))
           (pop         (read-file outputPop))
           (gpConfigList (read-file outputConfigs))
           (listPrice    (read-file outputStock)) )
      ;;(displayln gpConfigList)
      (match infosToSave
        ( (list stock date num)
          (let* ( (listPriceApi (stock-getInfosMatch stock num))
                  (listPriceApiNewValues (stock-getInfosDate listPriceApi date))
                  (listPriceNewComplete (append listPrice listPriceApiNewValues)) 
                  (listPriceNew (take-right listPriceNewComplete num)) )
            
            (let* ( (gpConfigs (apply make-gp gpConfigList))
                    (nInputs (gp-nInputs gpConfigs)) )
              (set-gp-endSimul! gpConfigs 30)
              
              (let-values ( ((bestExpr pop)
                             (gp-run listPriceNew gpConfigs pop))  )
                
                (file-bkpInfo bestExpr outputExpr)
                (file-bkpInfo pop outputPop)
                (file-bkpInfo listPriceNew outputStock)
                (file-bkpInfo (list stock (cdr (last listPriceNew)) num) outputInfos)
                (file-bkpInfo gpConfigList outputConfigs)
                
                (plot-stockPrediction listPriceNewComplete (car bestExpr) (inputs-create nInputs) outputPlot)
                
                ))
            ))
        )
      )
    )
  )

(define (read-file name)
  (call-with-input-file name
    (lambda(p)
      (read p)))
  )

(define (train-stockPrediction stock num nameSimul
                               #:np [np 100] #:endSimul [endSimul 150]
                               #:nRepeat [nRepeat 150])
  (let* ( (stockInfos (stock-getInfos stock num))
          (nInputs  (length (caar stockInfos)))
          (outputDirectory (string-append *output* nameSimul))
          (outputPlot (string-append *output* nameSimul "/" "plot.png"))
          (outputExpr (string-append *output* nameSimul "/" "expr.txt"))
          (outputPop  (string-append *output* nameSimul "/" "pop.txt"))
          (outputStock (string-append *output* nameSimul "/" "stock.txt"))
          (outputInfos (string-append *output* nameSimul "/" "infos.rkt"))
          (outputConfigs (string-append *output* nameSimul "/" "config.rkt")) )
    ;;(displayln stockInfos)
    ;;(displayln nInputs)
    (make-directory* outputDirectory)
    (let ( (infosToSave (list stock (cdr (last stockInfos)) num))
           (np 100)
           (depth 4)
           (pc 0.7)
           (pm 0.3)
           (nTournament 3)
           (k 0.7)
           (nElite 10)
           (endSimul 150)
           (nRepeat 150) )
    (let ( (gpConfigs
            (make-gp np nInputs *operators* depth pc pm nTournament k nElite endSimul nRepeat))
           (gpConfigSymbol
            (list np nInputs *operators* depth pc pm nTournament k nElite endSimul nRepeat)) )
      (let-values ( ((bestExpr pop)
                     (gp-run stockInfos gpConfigs))  )

        (file-bkpInfo bestExpr outputExpr)
        (file-bkpInfo pop outputPop)
        (file-bkpInfo stockInfos outputStock)
        (file-bkpInfo infosToSave outputInfos)
        (file-bkpInfo gpConfigSymbol outputConfigs)
        
        (plot-stockPrediction stockInfos (car bestExpr) (inputs-create nInputs) outputPlot)
        
        ))
      ))
  )

(define (file-bkpInfo info name)
  (define out (open-output-file name #:exists 'replace))
  (write info out)
  (close-output-port out)
  )

(define (stock-getInfosDate listPrice date)
  (filter-map (lambda(a) (and (string>? (cdr a)
                                        date)
                              a)) listPrice)
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