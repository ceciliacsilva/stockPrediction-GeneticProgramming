#lang racket

(require "geneticOperation.rkt")
(require "expression.rkt")
(require "config.rkt")
(require "apiRequest.rkt")

(require "drawTree.rkt")


(define expr1 '(+ (/ (sqrt x) (sqr 3)) (* (log 2) (- 7 8))))
(define expr2 '(* (+ 4 (/ 5 (- 9 6))) (sqrt (+ 8 (log 3)))))
(define expr3 '(log (+ 5 (* (sqrt 2) 9))))

(define pos1 1)
(define pos2 8)

(define (tree-mark progn pos)
  (let* ( (subTree (get-treePosition progn pos))
          (nodeMark (if (list? subTree) (car subTree)
                        subTree)) )
    (expr->pict progn nodeMark)
    )
  )

(define (tree-crossover progn1 progn2 pos1 pos2)
  (let ( (subTree1 (get-treePosition progn1 pos1))
         (subTree2 (get-treePosition progn2 pos2)) )
    (values (replace-treeElement progn1 subTree1 subTree2)
            (replace-treeElement progn2 subTree2 subTree1)) )
  )

;;Drawing
(expr->pict expr1)
(expr->pict expr2)

(tree-mark expr1 pos1)
(tree-mark expr2 pos2)

(define-values (filho1 filho2)
  (tree-crossover expr1 expr2 pos1 pos2))

(expr->pict filho1)
(expr->pict filho2)

(define expr4
  (let ( (subTree1 (get-treePosition expr1 pos1)) )     
    (replace-treeElement expr1 subTree1 expr3)
    )
  )

(expr->pict expr3)
(expr->pict expr4)
