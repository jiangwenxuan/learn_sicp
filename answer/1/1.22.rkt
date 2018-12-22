;f-prime
(define (square a)
  (* a a))
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))
(define (fast-prime n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime n (- times 1)))
        (else false)))
(define (f-prime n)
  (fast-prime n 15))
;normal-prime
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (prime? n)
  (= (smallest-divisor n) n))

;my answer
(define (search-for-primes n)
  (newline)
  (start n (runtime)))
(define (start n start-time)
  (if (prime? n)
      (begin
        (display n)
        (display "   ")
        (display (- (runtime) start-time)))
      (start (+ n 2) start-time)))
;(search-for-primes 1001)
;(search-for-primes 10001)
;(search-for-primes 100001)

; others answer
(define (next-odd n)
  (if (odd? n)
      (+ 2 n)
      (+ 1 n)))
(define (continue-primes n count)
  (cond ((= count 0)
         (display "are primes."))
        ((f-prime n)
         (display n)
         (newline)
         (continue-primes (next-odd n) (- count 1)))
        (else
         (continue-primes (next-odd n) count))))
(define (search-for-primes n)
  (let ((start-time (runtime)))
    (continue-primes n 3)
    (- (runtime) start-time)))
(search-for-primes 1000)
(search-for-primes 10000)
(search-for-primes 100000)


















