(define format-file
    (lambda (file)
        (call-with-input-file file format-input)))

(define format-input 
    (lambda (p)
        (let f ([x (read p)])
                (cond ((eof-object? x)
                    '())
                    (else (begin
                       ; (newline)
                        (write x))
                        (newline)
                        (f (read p)))))))
           