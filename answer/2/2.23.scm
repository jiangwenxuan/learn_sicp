#lang sicp
(define (for-each pro items)
  (if (not (null? items))
      (begin
        (pro (car items))
        (for-each pro (cdr items)))))
(for-each (lambda (x) (newline) (display x))
          (list 3 4 5))

(define (for-each proc items)
  (if (null? items)
      '()
      (proc (car items))
      (for-each proc (cdr items))))