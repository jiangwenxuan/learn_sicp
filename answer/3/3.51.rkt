#lang sicp

(define-syntax cons-stream
  (syntax-rules ()
    ((_ A B) (cons A (delay B)))))
(define (stream-car s)
  (car s))
(define (stream-cdr s)
  (force (cdr s)))
(define (force y)
  (y))
(define-syntax delay
  (syntax-rules ()
    ((_ x)(memo-proc (lambda () x)))))
(define (memo-proc proc)
  (let ((already-run? false)
        (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

(define (show x)
  (display-line x)
  x)
(define (display-line x)
  (newline)
  (display x))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-null? s)
  (null? s))
(define the-empty-stream '())
(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream
       (proc (stream-car s))
       (stream-map proc (stream-cdr s)))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      '()
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

;(define x (stream-map show (stream-enumerate-interval 0 10)))
;(stream-ref x 5)
;(stream-ref x 7)