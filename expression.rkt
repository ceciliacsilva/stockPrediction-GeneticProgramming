#lang racket

(provide (all-defined-out))
(require "config.rkt")
(require "functions.rkt")

(define mFator 2)

(define *operators* '((+ 2) (- 2) (* 2) (/ 2)) )

(define (sqrtAbs x)
  (sqrt (abs x))
  )

(define (inputs-create-recursion n)
  (if (<= n 1) `(,(string->symbol (string-append "=X1=")))
      (cons (string->symbol (string-append "=X" (number->string n) "=")) (inputs-create (- n 1)))
      )
  )

(define (inputs-create n)
  (reverse (inputs-create-recursion n))
  )

(define inputs (inputs-create 3))

(define (expression-run expression input inputs)
  (with-handlers ([real? (lambda(v) v)]
                  [exn:fail? (lambda(v) #f)])
    (apply (eval `(lambda ,inputs ,expression))  input))
  )

(define (fitness-eval expr listPrice gp)
  (let loop ( (lp listPrice) (result '()))
      (match lp
        ( (list dataT dataT+1 rest ...)
          (let ( (yt (first dataT))
                 (yt+1 (first dataT+1)) )
            (let ( (output (expression-run expr dataT (inputs-create (gp-nInputs gp)) )) )
              (let ( (fitnessValue
                      (if (and output (real? output))
                          ;;caso nao seja valido ou real, punir o individuo
                          (let* ( (rawFitness (abs (- output yt+1)))
                                  (ajustedFitness (/ 1.0 (+ 1 rawFitness))) )
                            ;;ajustedFitness
                            rawFitness
                            ;;output
                            )
                          ;;0.1
                          0
                          )
                      ) )
                (loop (cons dataT+1 rest) (cons fitnessValue result))
                ))
            )
          )
        (_ ;;(apply * result)
         ;;(rmse (map car (cdr listPrice)) result)
         (sqrt (var result))
           ))
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
(define (gen-expression-full operators gp listPrice)
  (let ( (depth (gp-depth gp))
         (inputs (inputs-create (gp-nInputs gp))) )
    (let ( (expr (gen-expression-full-create operators depth inputs)) )
      `(,expr . ,(fitness-eval expr listPrice gp))
      ))    
  )

(define (gen-expression-full-create operators depth inputs)
  (cond ( (= depth 0)
          (let ( (rInput (random)) )
            (if (< rInput 0.5) (random-list inputs)
                (* (random 1 mFator) (random))) )  )
        ( else
          (let* ( (op (random-list operators))
                  (operator (car op))
                  (arity (cadr op)) )
            (cond ( (= arity 1)
                    `(,operator ,(gen-expression-full-create operators (- depth 1) inputs)) )
                  ( (= arity 2)
                    `(,operator ,(gen-expression-full-create operators (- depth 1) inputs)
                      ,(gen-expression-full-create operators (- depth 1) inputs)) )  )
             )
          )
        )
  )

(define (gen-expression-grow operators gp listPrice)
  (let ( (depth (gp-depth gp))
         (inputs (inputs-create (gp-nInputs gp))) )
    (let ( (expr (gen-expression-grow-create operators depth inputs)) )
      `(,expr . ,(fitness-eval expr listPrice gp))
    ))
  )

(define (gen-expression-grow-create operators depth inputs)
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
                              `(,operator ,(gen-expression-grow-create operators (- depth 1) inputs)) )
                            ( (= arity 2)
                              `(,operator ,(gen-expression-grow-create operators (- depth 1) inputs)
                                          ,(gen-expression-grow-create operators (- depth 1) inputs)) )  )
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