#lang sicp

(define (stream-car s)
  (car s))
(define (stream-cdr s)
  (force (cdr s)))
(define (cons-stream x y)
  (cons x (delay y)))
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
(define (delay x)
  (memo-proc (lambda () x)))

;(define (stream-map proc s)
;  (if (stream-null? s)
;      the-empty-stream
;      (cons-stream (proc (stream-car s))
;                   (stream-map proc (stream-cdr s)))))
(define the-empty-stream nil)
(define stream-null? null?)

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))
















