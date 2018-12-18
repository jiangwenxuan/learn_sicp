(define (double f)
  (lambda (x)
    (begin
      (display x)
      (newline)
      (f (f x)))))

;(((double (double double)) inc) 5)