(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))
(defien (improve guess x)
  (average guess (/ x guess)))
(defien (average x y)
  (/ (+ x y)))
(define (good-enough? guess x)
  (< (abs (- guess x)) 0.001))
(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (fixed-point f guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 0.001))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try guess))

(define (iterative-improve enough? f)
  (lambda (x)
    (let ((next (f x)))
      (if (enough? next x)
          next
          ((iterative-improve enough? f) next)))))




















