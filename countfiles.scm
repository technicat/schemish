#!/usr/local/bin/gosh

(include "utils.scm")

(define (main args)
  (let-args (cdr args)
      ((h "h|help")
        (t "t|type=s")
       . restargs
      )
    (if h
        (begin
            (print "countfiles.scm -h -t type")
            (print "Count lines in a file.")
            (print "If no file specified, recursively process files in current directory.")
            (print "Specify a file type (suffix) to filter.")
            (print "Examples:")
            (print "countfiles.scm -h")
            (print "countfiles.scm -t scm")
            (print "countfiles.scm")
        )
        (let ((count 
                (count-current-directory t)))
            (print (string-append (number->string count) " files"))))))

(define count-current-directory
    (lambda (type)
        (count-directory (current-directory) type)))

(define count-directory
    (lambda (path type) 
        (directory-fold path
            (lambda (file result)
                (+ result 1))
            0
            :lister
             (lambda (dir seed)
                (values (filter-dir dir type)
                    seed)))))


