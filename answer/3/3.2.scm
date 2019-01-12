#lang sicp

(define (make-motitored proc)
  (let ((count 0))
    (lambda (x)
      (cond ((eq? x 'how-many-calls?) count)
            ((eq? x 'reset-count) (set! count 0))
            (else (set! count (inc count))
                  (proc x))))))