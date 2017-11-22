#lang racket

(require web-server/templates)
(require web-server/http)

(define *output* "output/")
(require "config.rkt")
(require "expression.rkt")

(define (html-responseStock nameSimul)
   (let* ( (outputPlot "plot.png");;(string-append *output* nameSimul "/" "plot.png"))
           (outputExpr (string-append *output* nameSimul "/" "expr.txt"))
           (outputPop  (string-append *output* nameSimul "/" "pop.txt"))
           (outputStock (string-append *output* nameSimul "/" "stock.txt"))
           (outputInfos (string-append *output* nameSimul "/" "infos.rkt"))
           (outputConfigs (string-append *output* nameSimul "/" "config.rkt"))
           (outputHtml (string-append *output* nameSimul "/" "out.html")) )
    (let ( (infosToSave (read-file outputInfos))
           (pop         (read-file outputPop))
           (gpConfigList (read-file outputConfigs))
           (listPrice    (read-file outputStock))
           (exprAll (read-file outputExpr)) )
      (match infosToSave
        ( (list stock date num)
          (let* ( (lastInput (last listPrice))
                  (lastInputPrice (caar lastInput))
                  (lastInputDate  (cdr lastInput))
                  (gpConfigs (apply make-gp gpConfigList))
                  (nInputs (gp-nInputs gpConfigs))
                  (listInput (inputs-create nInputs))
                  (expr (car exprAll))
                  (exprString (~a (car exprAll)))
                  (fitness (cdr exprAll))
                  (prediction (expression-run expr lastInput listInput))
                  (tableInfos
                   (for/list ( (diaryInfo (in-list listPrice)) )
                     (let* ( (price (caar diaryInfo))
                             (pricePrediction (expression-run expr diaryInfo listInput))
                             (errorA (abs (- price pricePrediction)))
                             (errorR (/ errorA price)) )
                     (list (cdr diaryInfo)
                           price
                           pricePrediction
                           (* errorA 100)
                           (* errorR 100)))) )   )


            (call-with-output-file outputHtml
               #:exists 'replace
               (lambda(p)
                 (displayln (string-replace (include-template "html/index.html") "\n" " ") p) ))
            
            )
          )
        )
      ))
  )

(define (read-file name)
  (call-with-input-file name
    (lambda(p)
      (read p)))
  )