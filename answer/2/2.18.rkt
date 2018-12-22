(define (reverse items)
  (iter items '()))
(define (iter items result)
  (if (null? items)
      result
      (iter (cdr items) (cons (car items) result))))
(reverse '(1 2 3 4 5))