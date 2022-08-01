#!/usr/local/bin/gosh

(use gauche.parseopt) ; command line args
(use file.util)

(define (main args)
  (let-args (cdr args)
      ((f "f|file=s")
       . restargs
      )
    (if f
        (count-file f)
        (count-current-directory))))

(define count-current-directory
    (lambda ()
        (count-directory (current-directory))))

(define count-directory
    (lambda (dir)
        (count-files (directory-list dir))))

(define count-files
    (lambda (files)
        (if (not (null? files))
            (begin 
                (count-file (car files))
                (count-files (cdr files))))))

(define count-file
    (lambda (file)
        (if (not (eq? (string-ref file 0) #\.))
            (begin
                (print file)
                (if (file-is-directory? file)
                    (count-directory file)
                    (call-with-input-file file count-input))))))

(define count-input
    (lambda (p)
        (let f ((total 0))
             (if (eof-object? (read-line p))
                (print total)
                (f (+ 1 total))))))

