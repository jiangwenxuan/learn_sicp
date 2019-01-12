#lang sicp

(define (square x)
  (* x x))
(define (smallest-divisor n)
  (find-divisor n 2))
(define (divides? a b)
  (= (remainder a b) 0))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? n test-divisor)
         test-divisor)
        (else
         (find-divisor n (next test-divisor)))))

(define (next a)
  (if (= a 2)
      3
      (+ a 2)))

(define (prime? a)
  (= (smallest-divisor a) a))
;
;(define (sum-primes1 a b)
;  (define (iter count accum)
;    (cond ((> count b) accum)
;          ((prime? count)
;           (iter (+ count 1) (+ count accum)))
;          (else (iter (+ iter 1) accum))))
;  (iter a 0))
;
;(define (sum-primes2 a b)
;  (accumulate + 0 (filter prime?
;                          (ecumerate-interval a b))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc
                    (map stream-cdr argstreams))))))
(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))
(define (display-line x)
  (newline)
  (display x))

(define-syntax cons-stream
  (syntax-rules ()
    ((_ A B) (cons A (delay B)))))
(define (stream-car s) (car s))
(define (stream-cdr s) (force (cdr s)))

;an example
(stream-car (stream-cdr
             (stream-filter prime?
                            (stream-enumerate-interval
                             10000 1000000))))
(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (inc low) high))))
(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream
          (stream-car stream)
          (stream-filter pred
                         (stream-cdr stream))))
        (else
         (stream-filter pred (stream-cdr stream)))))

(define-syntax delay
  (syntax-rules ()
    ((_ x)(memo-proc (lambda () x)))))
(define (force delayed-object)
  (delayed-object))
(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))
(define (divisible? x y)
  (= (remainder x y) 0))

(define no-sevens
  (stream-filter (lambda (x) (not divisible? x 7))
                 integers))
(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))
;(define fibs (fibgen 0 1))

(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x)
             (not (divisible? x (stream-car stream))))
           (stream-cdr stream)))))

(define prime (sieve (integers-starting-from 2)))
;(stream-ref prime 90)

(define ones (cons-stream 1 ones))
(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define another-integers
  (cons-stream 1
               (add-streams ones another-integers)))

(define fibs
  (cons-stream 0
               (cons-stream 1
                            (add-streams (stream-cdr fibs)
                                         fibs))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))
(define double (cons-stream 1 (scale-stream double 2)))
(define primes
  (cons-stream 2 (stream-filter new-prime? (integers-starting-from 3))))
(define (new-prime? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) true)
          ((divisible? n (stream-car ps)) false)
          (else (iter (stream-cdr ps)))))
  (iter primes))

;(define (sqrt-steam x)
;  (define guesses
;    (cons-stream 1.0
;                 (stream-map (lambda (guess)
;                               (sqrt-improve guess x))
;                             guesses)))
;  guesses)

(define (partial-sums s)
  (define result
    (cons-stream 0 (add-streams result s)))
  (stream-cdr result))

(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))
(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))
         
(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))
(define random-numbers
  (cons-stream random-init
               (stream-map rand-update random-numbers)))
(define cesaro-stream
  (map-successive-pairs (lambda (r1 r2) (= (gcd r1 r2) 1))
                        random-numbers))
(define (map-successive-pairs f s)
  (cons-stream
   (f (stream-car s) (stream-car (stream-cdr s)))
   (map-successive-pairs f (stream-cdr (stream-cdr s)))))
(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monto-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))
(define pi
  (stream-map (lambda (p) (sqrt (/ 6 p)))
              (monto-carlo cesaro-stream 0 0)))




























