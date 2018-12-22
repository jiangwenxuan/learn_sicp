(define tolerance 0.00001)
(define (fix-point f first-guess)
  (define (enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (enough? guess next)
          next
          (begin
            (newline)
            (display guess)
            (try next)))))
  (try first-guess))

(fix-point (lambda (x) (/ (log 1000) (log x))) 2)
(newline)
(fix-point
 (lambda (x) (/ (+ x (/ (log 1000) (log x))) 2))
 2)