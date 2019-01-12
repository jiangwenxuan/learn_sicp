#lang sicp
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items nil))
(define (square x)
  (* x x))

(define (new-square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))
(new-square-list '(1 2 3 4 5))
(square-list '(1 2 3 4 5))