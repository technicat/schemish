; http://community.schemewiki.org/?scheme-style
; working on a scheme formatter - in progress

(define format-file
    (lambda (file)
        (call-with-input-file file format-input)))

(define format-input
    (lambda (p)
        (let ((x (read p)))
                (if (eof-object? x)
                    '()
                     (begin
                        (format-list x)
                        (newline)
                        (format-input p))))))

(define format-list
    (lambda (exp)
        (cond ((null? exp)
                (begin 
                   (write-char #\))
                    ))
            ((list? exp)
            (begin
                (write-char #\()
                (write (car exp))
                (format-list (cdr exp))))
            (else
                (begin
                (print " ")
                (write exp ))))))
           