(define format
    (lambda (file)
        (call-with-input-file file
         (lambda (p)
            (let f ([x (read p)])
                (cond ((eof-object? x)
                    '())
                    (else (begin
                        (write x))
                        (newline)
                        (f (read p)))))))))
           