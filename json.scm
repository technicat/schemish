#!/usr/local/bin/gosh

(use gauche.parseopt) ; command line args
(use file.util) ; directory
;(use srfi-180)
(use rfc.json)

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
                (json-file f)
                (json-current-directory))))
            (print count)))))

(define json-current-directory
    (lambda ()
        (json-directory (current-directory))))

(define json-directory
    (lambda (path) 
        (directory-fold path
            (lambda (file result)
                (+ result (json-file file)))
            0
            :lister
            (lambda (dir seed)
                (values (remove (lambda (file)
                            (and (file-is-regular? file) ; wrong extension
                                (not (equal? (path-extension file) "json"))))
                        (directory-list dir :add-path? #\t :children? #\t))
                    seed)))))

(define json-file
    (lambda (file)
        (guard (e (else (print (string-append "JSON error in " file))
                        0))
            (call-with-input-file file json-input))))

(define json-input
    (lambda (p)
        (let ((result (parse-json p)))
            (if (list? result)
                (length result)
                1))))

