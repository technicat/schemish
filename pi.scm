#!/usr/local/bin/gosh

(use srfi-27) ; random-real

(define pi 
  (lambda (radius) 
    (piter radius 0 0)))

(define piter
  (lambda (radius inside total)
    (print (* 4.0 (/ inside (+ 0.000000001 total))))
    (if (incircle? (random radius) (random radius) radius)
        (piter radius (+ 1 inside) (+ 1 total))
        (piter radius inside (+ 1 total)))))

(define random
  (lambda (range)
    (- (* (random-real) (+ range range)) range)))

(define incircle? 
  (lambda (x y radius)
    (< (+ (square x) (square y)) (square radius))))

(define square
  (lambda (x) (* x x)))

(use gauche.parseopt) ; command line args

(define (main args)
  (let-args (cdr args)
      ((r "r|radius=i" 100)
       . restargs
      )
    (pi r)))

