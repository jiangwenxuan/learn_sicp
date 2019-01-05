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
(define sum 0)
(define (accum x)
  (set! sum (+ x sum))
  sum)
(define (stream-filter pred s)
  (cond ((stream-null? s) the-empty-stream)
        ((pred (stream-car s))
         (cons-stream (stream-car s)
                      (stream-filter pred (stream-cdr s))))
        (else
         (stream-filter pred (stream-cdr s)))))
(define (stream-enumerate-interval low high)
  (if (> low high)
      '()
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream
       (proc (stream-car s))
       (stream-map proc (stream-cdr s)))))

(define (stream-ref stream n)
  (if (= n 0)
      (stream-car stream)
      (stream-ref (stream-cdr stream) (- n 1))))

(define seq (stream-map accum (stream-enumerate-interval 1 20)))
(define y (stream-filter even? seq))
(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))
(stream-ref y 7)
;(display-stream z)