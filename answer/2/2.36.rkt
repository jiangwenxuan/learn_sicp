#lang sicp

(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq)
          (accumulate op initial (cdr seq)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(accumulate-n +
              0
              (list
               (list 1 2 3)
               (list 4 5 6)
               (list 7 8 9)
               (list 10 11 12)))