#lang racket

(provide (all-defined-out))

(define mFator 10)

(define *operators* '((+ 2) (- 2) (* 2) (/ 2))) ;(sqr 1) (sqrt 1) (log 1)) )

(define inputs '(=X1= =X2=))

(define (expression-run expression input)
  (with-handlers ([real? (lambda(v) v)]
                  [exn:fail? (lambda(v) #f)])
    (apply (eval `(lambda ,inputs ,expression))  input))
  )

(define (fitness-eval expr listPrice)
  (let loop ( (lp listPrice) (result '()))
      (match lp
        ( (list dataT dataT+1 rest ...)
          (let ( (yt (first dataT))
                 (yt+1 (first dataT+1)) )
            (let ( (output (expression-run expr dataT)) )
              (let ( (fitnessValue
                      (if (and output (real? output))
                          ;;caso nao seja valido ou real, punir o individuo
                          (let* ( (rawFitness (abs (- output yt+1)))
                                  (ajustedFitness (/ 1.0 (+ 1 rawFitness))) )
                            ajustedFitness)
                          0.1)) )
                (loop (cons dataT+1 rest) (cons fitnessValue result))
                ))
            )
          )
        (_ (apply * result)))
      )
    )
#|
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
|#
(define (gen-expression-full operators depth listPrice)
  (let ( (expr (gen-expression-full-create operators depth)) )
    `(,expr . ,(fitness-eval expr listPrice))
    )
  )

(define (gen-expression-full-create operators depth)
  (cond ( (= depth 0)
          (let ( (rInput (random)) )
            (if (< rInput 0.5) (random-list inputs)
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
            (if (< rInput 0.5) (random-list inputs)
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
                    (random-list inputs) ) )
            ) )
        )
  )

(define (random-list l)
  (let ( (len (length l)) )
    (list-ref l (random len)) )
  )