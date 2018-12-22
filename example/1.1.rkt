(define (abs a )
  (if (< a 0)
      (- a)
      a))

(define (square x)
  (* x x))

(define (sqrt-iter guess x)
  (if (good-enough guess x)
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

(define (sqrt x)
  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))
  (sqrt-iter 1.0 x))
























