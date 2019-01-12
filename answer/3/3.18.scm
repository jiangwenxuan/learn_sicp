#lang sicp

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define (judge-cycle x)
  (define seen nil)
  (define (judge y)
    (cond ((null? y) false)
          ((memq (car y) seen) true)
          (else (set! seen (cons (car y) seen))
                (judge (cdr y)))))
  (judge x))

(define a (list 'a 'b 'c))

(judge-cycle a)

(make-cycle a)

(judge-cycle a)