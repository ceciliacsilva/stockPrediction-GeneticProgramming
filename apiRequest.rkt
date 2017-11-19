#lang racket

(require "httpClient.rkt")

(provide (all-defined-out))

(define (stockGetInfos stock)
  (define httpClient (http-request-get "www.macrotrends.net"))
  
  (define marketCapResponse (httpClient "/assets/php/market_cap.php" `(("t" ,stock))))
  (define stockPriceHistoryResponse (httpClient "/assets/php/stock_price_history.php" `(("t" ,stock))))

  (define listMarketCap
    (let* ( (splitChartData    (string-split marketCapResponse "var chartData = [{"))
            (splitEndChartData (string-split (cadr splitChartData) "}];"))
            (listChartData     (string-split (car splitEndChartData) "},{")) )
      (map (lambda(a)
             (string-splitList a '("," ":" "\""))) listChartData )
    ))
  
  (define listStockPriceHistory
    (let* ( (splitChartData    (string-split stockPriceHistoryResponse "var dataDaily = [{"))
            (splitEndChartData (string-split (cadr splitChartData) "}];"))
            (listChartData     (string-split (car splitEndChartData) "},{")) )
      (map (lambda(a)
             (string-splitList a '("," ":" "\""))) listChartData )
    ))

  (values listMarketCap listStockPriceHistory)
  )

(define (1yearTreasureRate)
  (define httpClient (http-request-get "www.macrotrends.net"))
  
  (define 1yearTreasureRateResponse (httpClient "/2492/1-year-treasury-rate-yield-chart" '()))
  
  (define list1yearTreasureRate
    (let* ( (splitChartData    (string-split 1yearTreasureRateResponse "var originalData = [{"))
            (splitEndChartData (string-split (cadr splitChartData) "}];"))
            (listChartData     (string-split (car splitEndChartData) "},{")) )
      (map (lambda(a)
             (string-splitList a '("," ":" "\""))) listChartData )
    ))
  
  list1yearTreasureRate
  )

(define (string-splitList phase splits)
  (let loop ( (listPhase (list phase))
              (listSplit splits) )
    (if (null? listSplit) listPhase
        (loop (flatten (map (lambda(a) (string-split a (car listSplit)))
                            listPhase))
              (cdr listSplit)) )
    )
  )




;;example - Read CVS
(require csv-reading)
(define *dataDirectory* "data")
(define *1yearTreasureRate* "1-year-treasury-rate-yield-chart.csv")
(define (read-1yearTreasureRate)
  (let* ( (file (string-append *dataDirectory* "/" *1yearTreasureRate*))
          (fileReader (open-input-file file)) )
    (let ( (listValues (csv->list
                        (make-csv-reader fileReader))) )
      (lambda (size)
        (take (reverse listValues) size))
      ))
  )