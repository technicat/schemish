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
                (values (remove
                            (lambda (file)
                                (ignore-file? file type))
                            (directory-list dir :add-path? #\t :children? #\t))
                    seed)))))

(define ignore-file?
    (lambda (file type)
      (let-values (((dir name ext) (decompose-path file)))
        (or (eq? (string-ref name 0) #\.) ; ignore dot files/directories
            (and type ; check extension
                (file-is-regular? file) 
                (not (equal? ext type)))))))

