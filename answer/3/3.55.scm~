#lang sicp

(define-syntax cons-stream
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))

(define (stream-car s) (car s))
(define (stream-cdr s) (force (cdr s)))

(define-syntax delay
  (syntax-rules ()
    ((_ x) (memo-proc (lambda () x)))))
(define (force x)
  (x))
(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

(define (stream-map proc . argstreams)
  (if (null? (car argstreams))
      '()
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc
                    (map stream-cdr argstreams))))))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))
(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define ones (cons-stream 1 ones))
(define integers
  (cons-stream 1 (add-streams ones integers)))

(define (partial-sums s)
  (define result
    (cons-stream 0 (add-streams result s)))
  (stream-cdr result))

(define (new-partial-sums s)
  (cons-stream (stream-car s)
               (add-streams (stream-cdr s)
                            (new-partial-sums s))))








