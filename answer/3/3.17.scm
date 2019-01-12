#lang sicp

(define (count-pairs x)
  (if (not (pairs? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(define (better-count-pairs1 x)
  (length (inner x '())))
(define (inner x memo-list)
  (if (and (pair? x)
           (false (memq x memo-list)))
      (inner (car x)
             (inner (cdr x) (cons x memo-list)))
      memo-list))

