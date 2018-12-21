#lang sicp

(define (reverse items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (car things) answer))))
  (iter items '()))
(reverse '(1 2 3 4))

(define x (list (list 1 2) (list 3 4)))

(define (deep-reverse a)
  (if (null? a)
      '()
      (append (deep-reverse (cdr a))
              (if (pair? (car a))
                  (list (reverse (car a)))
                  (list (car a))))))

(define (deep-reverse2 tree)
  (define (deep-reverse-iter items result)
    (if (null? items)
        result
        (let ((head (car items)))
          (deep-reverse-iter (cdr items)
                             (cons (if (pair? head)
                                       (deep-reverse head)
                                       head)
                                   result)))))
  (deep-reverse-iter tree '()))

(define (better-deep tree)
  (if (pair? tree)
      (reverse (map better-deep tree))
      tree))

(deep-reverse x)





















