#!/usr/local/bin/gosh

(use gauche.parseopt) ; command line args
(use file.util) ; directory

(define (main args)
  (let-args (cdr args)
      ((h "h|help")
        (t "t|type=s")
       . restargs
      )
    (if h
        (print "countfiles.scm -t type")
        (let ((count 
                (count-current-directory t)))
            (print count)))))

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
                (values (remove (lambda (file)
                            (and (file-is-regular? file) ; wrong extension
                                (not (equal? (path-extension file) type))))
                        (directory-list dir :add-path? #\t :children? #\t))
                    seed)))))