#lang racket

(require "expression.rkt")
(require "config.rkt")

(provide (all-defined-out))

(define *gp* (make-gp 50 3 *operators* 3 0.7 0.1 3 0.7 3 100 100))

(define (crossover progn1 progn2 gp)
  (let ( (pc (gp-pc gp)) )
    (let ( (r (random)) )
      (if (< r pc)
          (crossover-operation (car progn1) (car progn2))
          (values (car progn1) (car progn2)))
      ))
  )

(define (crossover-operation progn1 progn2)
  ;;(when (null? progn1) (set! progn1 '(+ 1 1)))
  ;;(when (null? progn2) (set! progn2 '(+ 1 1)))
  
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

(define (mutation progn operators gp listPrice)
  (let ( (depth (gp-depth gp))
         (pm (gp-pm gp)) )
    (let ( (r (random)) )
      (if (< r pm)
          (fitness-eval (mutation-operation progn operators depth listPrice) listPrice gp)
          progn)
      ))
  )

(define (mutation-operation progn operators depth listPrice)
  ;;(when (null? progn) (set! progn '(+ 1 1)))
  
  (let ( (newTree (gen-expression-grow-create operators (random 1 depth))) )
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