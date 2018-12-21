#lang sicp
(define (square x)
  (* x x))
(define (square-list-1 items)
  (if (null? items)
      nil
      (cons (square (car items))
            (square-list-1 (cdr items)))))
(define (square-list-2 items)
  (map square items))
(square-list-2 (list 1 2 3 4 5))