#lang sicp

;(define (cons x y)
;  (let ((new (get-new-pair)))
;    (set-car! new x)
;    (set-cdr! new y)
;    new))

(define x (list 'a 'b))
(define z1 (cons x x))
(define z2 (cons (list 'a 'b) (list 'a 'b)))
(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)


