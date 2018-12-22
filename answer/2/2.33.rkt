#lang sicp

(define (filter predicate seq)
  (cond ((null? seq) '())
        ((predicate (car seq))
         (cons (car seq)
               (filter predicate (cdr seq))))
        (else
         (filter predicate (cdr seq)))))

(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq)
          (accumulate op initial (cdr seq)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (inc low) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree))
         (list tree))
        (else
         (append (enumerate-tree (car tree))
                 (enumerate-tree (cdr tree))))))

(define (map p seq)
  (accumulate (lambda (x y)
                (cons (p x)
                      y))
              nil
              seq))

(define (append seq1 seq2)
  (accumulate cons
              seq2
              seq1))

(define (length seq)
  (accumulate (lambda (x y)
                (inc y))
              0
              seq))
              






















