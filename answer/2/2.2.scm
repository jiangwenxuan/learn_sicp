(define (make-segment a b)
  (cons a b))
(define (start-segment seg)
  (car seg))
(define (end-segment seg)
  (cdr seg))
(define (make-point a b)
  (cons a b))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))

(define (mid-point seg)
  (let ((mid-x (average (x-point (start-segment seg))
                        (x-point (end-segment seg))))
        (mid-y (average (y-point (start-segment seg))
                        (y-point (end-segment seg)))))
    (make-point mid-x mid-y)))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ".")
  (display (y-point p))
  (display ")"))