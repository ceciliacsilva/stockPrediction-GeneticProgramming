#lang racket

(require pict/tree-layout)
(require pict)

(provide (all-defined-out))

(define (expr->pict expr [cutElement '()])
  (naive-layered (expr->pict-recursion expr cutElement))
  )

(define (expr->pict-recursion expr cutElement)
  (match expr
    ( (list operator arg1 arg2)
      (tree-layout #:pict
                   (node-tree operator cutElement)
                   (tree-edge #:edge-width 1 #:edge-color "black"
                              (expr->pict-recursion arg1 cutElement))
                   (tree-edge #:edge-width 1 #:edge-color "black"
                              (expr->pict-recursion arg2 cutElement)))  )
    ( (list operator arg)
      (tree-layout #:pict (node-tree operator cutElement)
                   (tree-edge #:edge-width 1 #:edge-color "black"
                              (expr->pict-recursion arg cutElement))) )
    ( arg
      (tree-layout #:pict (node-tree arg cutElement)) )
    )
  )

(define (node-tree value cutElement [sizeCircle 30] )
  (pin-over (colorize (disk sizeCircle)
                      (if (equal? value cutElement) (list 139 0 0)
                          "black"))
            (/ sizeCircle 4) (/ sizeCircle 3)
            (colorize (text
                       (cond ( (symbol? value) (symbol->string value) )
                             ( (number? value) (number->string value) ) )) "white"))
  )