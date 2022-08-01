#!/usr/local/bin/gosh

(use gauche.parseopt) ; command line args
(use file.util)

(define (main args)
  (let-args (cdr args)
      ((h "h|help")
      (f "f|file=s")
       . restargs
      )
    (if h
        (begin (print "count.scm -f file")
                (print "count.scm -h"))
        (if f
            (count-file f)
            (count-current-directory)))))

(define count-current-directory
    (lambda ()
        (count-directory (current-directory))))

(define count-directory
    (lambda (dir)
        (print dir)
        (let ((count (count-files (directory-list dir))))
            (print count)
            count)))

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

