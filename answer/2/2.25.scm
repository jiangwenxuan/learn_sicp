#lang sicp

(define (choose1 items)
  (car (cdr (car (cdr (cdr items))))))

(define (choose2 items)
  (car (car items)))

(define (choose3 items)
  (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr items)))))))))))))
(choose1 '(1 3 (5 7) 9))
(choose2 '((7)))
(choose3 '(1 (2 (3 (4 (5 '(6 7)))))))