(define (multi a b)
  (if (= b 0)
      0
      (+ a (multi a (- b 1)))))
(define (double a)
  (multi a 2))
(define (halve a)
  (/ a 2))
(define (fast-multi a b)
  (cond ((= b 0) 0)
        ((even? b)
         (fast-multi (double a) (halve b)))
        ((odd? b)
         (+ a (fast-multi a (- b 1))))))