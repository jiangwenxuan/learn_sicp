#lang sicp

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
(define-syntax cons-stream
  (syntax-rules ()
    ((_ A B) (cons A (delay B)))))
(define (stream-car s) (car s))
(define (stream-cdr s) (force (cdr s)))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc
                    (map stream-cdr argstreams))))))

(define (merge s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge
                                      (stream-cdr s1)
                                      s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge
                                      s1
                                      (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge
                                (stream-cdr s1)
                                (stream-cdr s2)))))))))
(define ones (cons-stream 1 ones))
(define (add-streams s1 s2)
  (stream-map + s1 s2))
(define integers (cons-stream 1 (add-streams ones
                                             integers)))
(define even (scale-stream integers 2))
(define three (scale-stream integers 3))
(define five (scale-stream integers 5))

(define s
  (cons-stream 1
               (merge
                (scale-stream s 2)
                (merge (scale-stream s 3)
                       (scale-stream s 5)))))
                 












