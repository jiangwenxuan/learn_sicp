(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                 (expext-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))
(define (first-denomination items)
  (car items))
(define (expext-first-denomination items)
  (cdr items))
(define (no-more? items)
  (null? items))
(define (reverse items)
  (iter items '()))
(define (iter items result)
  (if (null? items)
      result
      (iter (cdr items) (cons (car items) result))))
(define us-coins (list 50 25 10 5 1))
(cc 100 us-coins)
(cc 100 (reverse us-coins))