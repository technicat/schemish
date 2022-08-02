#!/usr/local/bin/gosh

(use gauche.parseopt) ; command line args
(use file.util) ; directory

(define (main args)
  (let-args (cdr args)
      ((h "h|help")
        (f "f|file=s")
        (t "t|type=s")
       . restargs
      )
    (if h
        (print "countlines.scm -f file -t type")
        (let ((count 
            (if f
                (call-with-input-file f count-input)
                (count-current-directory t))))
            (print count)))))

(define count-current-directory
    (lambda (type)
        (count-directory (current-directory) type)))

(define count-directory
    (lambda (path type) 
        (directory-fold path
            (lambda (file result)
                (+ result 
                    (call-with-input-file file count-input)))
            0
            :lister
            (lambda (dir seed)
                (values (remove (lambda (file)
                            (and type
                                (file-is-regular? file) ; wrong extension
                                (not (equal? (path-extension file) type))))
                        (directory-list dir :add-path? #\t :children? #\t))
                    seed)))))

(define count-input
    (lambda (p)
        (let f ((total 0))
            (guard (e (else total))
             (if (eof-object? (read-line p))
                total
                (f (+ 1 total)))))))
