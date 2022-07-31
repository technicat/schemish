#!/usr/local/bin/gosh

(use gauche.parseopt) ; command line args

(define (main args)
  (let-args (cdr args)
      ((f "f|file=s")
       . restargs
      )
    (count-file f)))

(define count-file
    (lambda (file)
        (call-with-input-file file count-input)))

(define count-input
    (lambda (p)
        (let f ((total 0))
             (if (eof-object? (read-line p))
                (print total)
                (f (+ 1 total))))))

