#!/usr/local/bin/gosh

(use gauche.parseopt) ; command line args

(define (main args)
 (let-args (cdr args)
  ((h "h|help")
   (y "y|years=n" 10)
   (s "s|start=n" 176900)
   (e "e|end=n" 200000)
   . restargs
   )
  (if h
   (print "int.scm -s start -e end -y years")
   (print (interest s e y)))))

(define (interest start end years)
 (- (expt (/ end start) (/ 1 years)) 1))
