#!/usr/local/bin/gosh

(use gauche.parseopt) ; command line args
(use srfi-27) ; random-real

(define (main args)
  (let-args (cdr args)
      ((h "h|help")
        (r "r|radius=i" 100)
       . restargs
      )
      (if h
         (print "pi.scm -r radius")
          (pi r))))

(define pi 
  (lambda (radius) 
    (let f ((inside 0)
            (total 0))
      (print (* 4.0 (/ inside (+ 0.000000001 total))))
      (if (incircle? (random radius) (random radius) radius)
        (f (+ 1 inside) (+ 1 total))
        (f inside (+ 1 total))))))

(define random
  (lambda (range)
    (- (* (random-real) (+ range range)) range)))

(define incircle? 
  (lambda (x y radius)
    (< (+ (square x) (square y)) (square radius))))

(define square
  (lambda (x) (* x x)))

