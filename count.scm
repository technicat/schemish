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
        (if (null? files)
            0
            (+ (count-file (car files))
                (count-files (cdr files))))))

(define count-file
    (lambda (file)
        (if (eq? (string-ref file 0) #\.)
            0
            (begin
                (print file)
                (let ((count (if (file-is-directory? file)
                                (count-directory file)
                                (call-with-input-file file count-input))))
                     (print count)
                     count)))))

(define count-input
    (lambda (p)
        (let f ((total 0))
             (if (eof-object? (read-line p))
                total
                (f (+ 1 total))))))

