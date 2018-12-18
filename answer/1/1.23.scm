(define (smallest-divisor n)
  (find-divisor n 2))
(define (divides? a b)
  (= (remainder a b) 0))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? n test-divisor)
         test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (next a)
  (if (= a 2)
      3
      (+ a 2)))

(define (prime? a)
  (= (smallest-divisor a) a))