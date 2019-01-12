#lang sicp

(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq)
          (accumulate op initial (cdr seq)))))

(define (count-leaves tree)
  (accumulate +
              0
              (map (lambda (root)
                     (if (leaf? root)
                         1
                         (count-leaves root)))
                   t)))
