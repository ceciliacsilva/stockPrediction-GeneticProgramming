#lang racket

(define (crossover progn1 progn2)
  (let* ( (progn1Size (length (flatten progn1)))
          (progn2Size (length (flatten progn2)))
          (r1 (random progn1Size))
          (r2 (random progn2Size)) )
    (let ( (subTree1 (get-treePosition progn1 r1))
           (subTree2 (get-treePosition progn2 r2)) )
      (displayln subTree1)
      (displayln subTree2)
      (values (replace-treePosition progn1 subTree1 subTree2)
              (replace-treePosition progn2 subTree2 subTree1))
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

(define (replace-treePosition progn subTree newTree)
  
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