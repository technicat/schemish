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
                        (format-exp x 0)
                        (newline)
                        (format-input p))))))

(define format-exp
    (lambda (exp indent)
        (if (list? exp)
                (begin  
                (newline)
                (spaces indent)
                (write-char #\()
                (format-list exp (+ 1 indent)))
            (write exp))))

(define format-list
    (lambda (exp indent)
        (cond ((null? exp)
                (begin 
                   (write-char #\))
                    ))
            (else
            (format-exp (car exp) indent)
            (spaces 1)
            (format-list (cdr exp) indent)))))

(define spaces
    (lambda (num)
        (let f ((x num))
            (if (> x 0)
                (begin
                    (write-char #\ )
                     (f (- x 1)))))))

           