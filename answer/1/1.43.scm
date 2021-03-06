(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f x)
  (define (iter a result num)
    (if (= a num)
        result
        (iter (inc a)
              (f result)
              num)))
  (lambda (y)
    (iter 0 y x)))
(define (square x)
  (* x x))
((repeated square 2) 5)