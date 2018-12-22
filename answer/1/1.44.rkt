(define (average f x)
  (/ (+ (f x) (f (+ x dx)) (f (- x dx))) 3))
(define dx 0.0001)

(define (smooth f)
  (lambda (x)
    (average f x)))

(define (repeated f x)
  (define (iter a result num)
    (if (= a num)
        result
        (iter (inc a)
              (f result)
              num)))
  (lambda (y)
    (iter 0 y x)))

(define (smooth-n-times f n)
  ((repeated smooth n) f))