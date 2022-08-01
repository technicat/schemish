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
        (if f
            (count-file f t)
            (count-current-directory t)))))

(define count-current-directory
    (lambda (type)
        (count-directory (current-directory) type)))

(define count-directory
    (lambda (dir type)
        (print dir)
        (let ((count (count-files (directory-list dir :add-path? #\t :children? #\t) type)))
            (print count)
            count)))

(define count-files
    (lambda (files type)
        (if (null? files)
            0
            (+ (count-file (car files) type)
                (count-files (cdr files) type)))))

(define count-file
    (lambda (file type)
        (if (or (eq? (string-ref file 0) #\.)
                (and (file-is-directory? file)
                    (path-extension file))
                (and (file-is-regular? file)
                    (not (equal? (path-extension file) type))))
            0
            (begin
                (print file)
                (let ((count (if (file-is-directory? file)
                                (count-directory file type)
                                (call-with-input-file file count-input))))
                     (print count)
                     count)))))

(define count-input
    (lambda (p)
        (let f ((total 0))
             (if (eof-object? (read-line p))
                total
                (f (+ 1 total))))))

