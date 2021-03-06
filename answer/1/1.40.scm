(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))
(define dx 0.00001)
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))
(define (fixed-point f guess)
  (define (enough? v1 v2)
    (< (abs (- v1 v2)) dx))
  (define (try guess)
    (let ((next (f guess)))
      (if (enough? next guess)
          next
          (try next))))
  (try guess))
(define (cubic a b c)
    (lambda (x)
      (+ (cube x) (* a (square x)) (* b x) c)))
(define (cube x)
  (* x x x))
(define (square x)
  (* x x))
(newtons-method (cubic 2 5 5) 1.0)