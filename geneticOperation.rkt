#lang racket

(require "expression.rkt")
(require "config.rkt")

(provide (all-defined-out))

(define *gp* (make-gp 100 *operators* 3 0.7 0.01 10 10))

(define (crossover progn1 progn2 gp)
  (let ( (pc (gp-pc gp)) )
    (let ( (r (random)) )
      (if (< r pc)
          (let-values ( ((f1 f2) (crossover-operation progn1 progn2)) )
            (list f1 f2))
          (list progn1 progn2))
      ))
  )

(define (crossover-operation progn1 progn2)
  (let* ( (progn1Size (length (flatten progn1)))
          (progn2Size (length (flatten progn2)))
          (r1 (random progn1Size))
          (r2 (random progn2Size)) )
    (let ( (subTree1 (get-treePosition progn1 r1))
           (subTree2 (get-treePosition progn2 r2)) )
      ;(displayln subTree1)
      ;(displayln subTree2)
      (values (replace-treeElement progn1 subTree1 subTree2)
              (replace-treeElement progn2 subTree2 subTree1))
      ))
  )

(define (mutation progn operators gp)
  (let ( (depth (gp-depth gp))
         (pm (gp-pm gp)) )
    (let ( (r (random)) )
      (if (< r pm)
          (mutation-operation progn operators depth)
          progn)
      ))
  )

(define (mutation-operation progn operators depth)
  (let ( (newTree (gen-expression-grow operators (random 1 depth))) )
    (let* ( (prognSize (length (flatten progn)))
            (r1 (random prognSize)) )
      (let ( (subTree (get-treePosition progn r1)) )
        ;;(displayln subTree)
        ;;(displayln newTree)
        (replace-treeElement progn subTree newTree))
      ))
  )

(define (get-treePosition progn target)
  (let ( (treeCut '()) )
    
    (define (cut-recursion progn target index)
      (when (= index target) (set! treeCut progn))
      (match progn
        ( (list operador arg1 arg2)
          (let ( (newIndex (cut-recursion arg1 target (+ index 1))) )
            (cut-recursion arg2 target newIndex))
          )
        ( (list operador arg)
          (cut-recursion arg target (+ index 1))
          )
        ( arg
          (+ index 1) )
        )
      )
    
    (cut-recursion progn target 0)
    treeCut
    ))

(define (replace-treeElement progn subTree newTree)
  
  (define (replace-recursion progn subTree newTree)
    (if (equal? progn subTree) newTree
        (match progn
          ( (list operador arg1 arg2)
            `(,operador ,(replace-recursion arg1 subTree newTree)
                        ,(replace-recursion arg2 subTree newTree)) )
          ( (list operador arg)
            `(,operador ,(replace-recursion arg subTree newTree)) )
          ( arg
            arg))
        )
    )

  (replace-recursion progn subTree newTree)
  )