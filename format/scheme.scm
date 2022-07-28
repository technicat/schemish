(define format-file
    (lambda (file)
        (call-with-input-file file format-input)))

(define format-input 
    (lambda (p)
        (let ((x (read p)))
                (if (eof-object? x)
                    '()
                     (begin
                        (write x)
                        (newline)
                        (format-input p))))))
           