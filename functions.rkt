#lang racket

(define (var X)
  (let ( (XMedia (/ (apply + X) (length X))) )
    (/ (apply + (map (lambda(xi) (sqr (- xi XMedia))) X))
       (length X))
    ))

(define (vaf Y Ŷ)
  (let ( (y-ŷ (map (lambda(a b) (- a b)) Y Ŷ)) )
    (- 1 (/ (var y-ŷ)
            (var Y)))
    ))

(define (rmse Y Ŷ)
  (sqrt (/
         (apply +
                (map (lambda(a b) (sqr (- a b))) Y Ŷ))
         (length Y)))
  )