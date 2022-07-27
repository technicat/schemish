(use srfi-27)

(define (pi radius) ((piter radius 0 0)))

(define (piter radius inside total)
    (print (* 4.0 (/ inside (+ 0.000000001 total))))
    (if (incircle? (random radius) (random radius) radius)
        (piter radius (+ 1 inside) (+ 1 total))
        (piter radius inside (+ 1 total))))

(define (random range)
  (- (* (random-real) (+ range range)) range))

(define (incircle? x y radius)
  (< (+ (square x) (square y)) (square radius)))

(define (square x) (* x x))


