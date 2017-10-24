#lang racket

(define mFator 10)

(define *operators* '((+ 2) (- 2) (* 2) (/ 2) (sqr 1) (sqrt 1) (log 1)) )

(define (expression-run expression input)
  (with-handlers ([number? (lambda(v) v)]
                  [exn:fail? (lambda(v) #f)])
    (apply (eval `(lambda(=X=) ,expression)) (list input)))
  )

(define (gen-expression-full operators profundidade)
  (cond ( (= profundidade 0)
          (let ( (rInput (random)) )
            (if (< rInput 0.5) '=X=
                (* (random 1 mFator) (random))) )  )
        ( else
          (let* ( (op (random-list operators))
                  (operator (car op))
                  (arity (cadr op)) )
            (cond ( (= arity 1)
                    `(,operator ,(gen-expression-full operators (- profundidade 1))) )
                  ( (= arity 2)
                    `(,operator ,(gen-expression-full operators (- profundidade 1))
                      ,(gen-expression-full operators (- profundidade 1))) )  )
             )
          )
        )
  )

(define (gen-expression-grow operators profundidade)
  (cond ( (= profundidade 0)
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
                              `(,operator ,(gen-expression-grow operators (- profundidade 1))) )
                            ( (= arity 2)
                              `(,operator ,(gen-expression-grow operators (- profundidade 1))
                                          ,(gen-expression-grow operators (- profundidade 1))) )  )
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