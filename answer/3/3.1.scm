#lang sicp

(define (make-accumulator x)
  (lambda (amount)
    (begin
      (set! x (+ x amount))
      x)))