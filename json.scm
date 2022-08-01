#!/usr/local/bin/gosh

(use gauche.parseopt) ; command line args
(use file.util) ; directory
(use srfi-180)

(define (main args)
  (let-args (cdr args)
      ((h "h|help")
        (f "f|file=s")
       . restargs
      )
    (if h
        (print "json.scm -f file")
        (let ((count 
            (if f
                (call-with-input-file f json-input)
                (json-current-directory))))
            (print count)))))

(define json-current-directory
    (lambda ()
        (json-directory (current-directory))))

(define json-directory
    (lambda (path) 
        (directory-fold path
            (lambda (file result)
                (json-input file))
            0
            :lister
            (lambda (dir seed)
                (values (remove (lambda (file)
                            (and (file-is-regular? file) ; wrong extension
                                (not (equal? (path-extension file) "json"))))
                        (directory-list dir :add-path? #\t :children? #\t))
                    seed)))))

(define json-input
    (lambda (p)
        (json-generator p)))

