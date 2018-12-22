(define (abs a)
  (if (< a 0)
      (- a)
      a))

(define (square x)
  (* x x))

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (sqrt-iter guess x)
  (new-if (good-enough guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (good-enough guess x)
  (< (abs (- (square guess) x)) 0.0001))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average a b)
  (/ (+ a b) 2))

(define (sqrt a)
  (sqrt-iter 1.0 a))