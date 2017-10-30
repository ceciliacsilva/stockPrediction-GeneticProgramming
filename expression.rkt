#lang racket

(provide (all-defined-out))

(define mFator 10)

(define *operators* '((+ 2) (- 2) (* 2) (/ 2) (sqr 1) (sqrt 1) (log 1)) )

(define (expression-run expression input)
  (with-handlers ([real? (lambda(v) v)]
                  [exn:fail? (lambda(v) #f)])
    (apply (eval `(lambda(=X=) ,expression)) (list input)))
  )

(define (fitness-eval expr listPrice)
  (apply *
   (for/list ( (diaryPrice (in-list listPrice)) )
     (match diaryPrice
       ( (list day price)
         (let ( (output (expression-run expr day)) )
           (if (and output (real? output))
               ;;caso nao seja valido ou real, punir o individuo
               (let* ( (rawFitness (abs (- output price)))
                       (ajustedFitness (/ 1.0 (+ 1 rawFitness))) )
                 ajustedFitness)
               0.1))  )
       )
     )
   )
  )

(define (gen-expression-full operators depth listPrice)
  (let ( (expr (gen-expression-full-create operators depth)) )
    `(,expr . ,(fitness-eval expr listPrice))
    )
  )

(define (gen-expression-full-create operators depth)
  (cond ( (= depth 0)
          (let ( (rInput (random)) )
            (if (< rInput 0.5) '=X=
                (* (random 1 mFator) (random))) )  )
        ( else
          (let* ( (op (random-list operators))
                  (operator (car op))
                  (arity (cadr op)) )
            (cond ( (= arity 1)
                    `(,operator ,(gen-expression-full-create operators (- depth 1))) )
                  ( (= arity 2)
                    `(,operator ,(gen-expression-full-create operators (- depth 1))
                      ,(gen-expression-full-create operators (- depth 1))) )  )
             )
          )
        )
  )

(define (gen-expression-grow operators depth listPrice)
  (let ( (expr (gen-expression-grow-create operators depth)) )
    `(,expr . ,(fitness-eval expr listPrice))
    )
  )

(define (gen-expression-grow-create operators depth)
  (cond ( (= depth 0)
          (let ( (rInput (random)) )
            (if (< rInput 0.5) '=X=
                (* (random 1 mFator) (random))) )  )
        ( else
          (let ( (rChoose (random)) )
            (cond ( (< rChoose 0.5)
                    (let* ( (op (random-list operators))
                            (operator (car op))
                            (arity (cadr op)) )
                      (cond ( (= arity 1)
                              `(,operator ,(gen-expression-grow-create operators (- depth 1))) )
                            ( (= arity 2)
                              `(,operator ,(gen-expression-grow-create operators (- depth 1))
                                          ,(gen-expression-grow-create operators (- depth 1))) )  )
                      )  )
                  ( (< rChoose 0.75)
                    (* (random 1 mFator) (random)) )
                  ( else
                    '=X= ) )
            ) )
        )
  )

(define (random-list l)
  (let ( (len (length l)) )
    (list-ref l (random len)) )
  )