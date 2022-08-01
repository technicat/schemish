#!/usr/local/bin/gosh

(use gauche.parseopt) ; command line args
(use file.util)

(define (main args)
  (let-args (cdr args)
      ((h "h|help")
      (f "f|file=s")
      (t "t|type=s")
       . restargs
      )
    (if h
        (print "count.scm -f file -t type")
        (let ((count 
            (if f
                (count-file f t)
                (count-current-directory t))))
            (print count)))))

(define count-current-directory
    (lambda (type)
        (count-path (current-directory) type)))

(define count-path
    (lambda (path type) 
        (directory-fold path
            (lambda (file result)
                (+ result 
                    (call-with-input-file file count-input)))
            0
            :lister
            (lambda (dir seed)
                (values (remove (lambda (file)
                             (or (eq? (string-ref file 0) #\.) ; ignore dot files
                            (and (file-is-directory? file) ; weird directories
                                (path-extension file))
                            (and (file-is-regular? file) ; wrong extension
                                (not (equal? (path-extension file) type)))))
                        (directory-list dir :add-path? #\t :children? #\t))
                    seed)))))

(define count-input
    (lambda (p)
        (let f ((total 0))
             (if (eof-object? (read-line p))
                total
                (f (+ 1 total))))))

