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
        (else (find-divisor n (next test-divisor)))))

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
;          ((prime? count) (iter (+ count 1) (+ count accum)))
;          (else (iter (+ iter 1) accum))))
;  (iter a 0))
;
;(define (sum-primes2 a b)
;  (accumulate + 0 (filter prime? (ecumerate-interval a b))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))
(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))
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
                            (stream-enumerate-interval 10000 1000000))))
(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (inc low) high))))
(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

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


































