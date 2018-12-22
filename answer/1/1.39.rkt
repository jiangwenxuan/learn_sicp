(define (cont-frac-iter n d k x)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (dec i) (/ (n i x) (- (d i) result)))))
  (iter k 0))

(define (square x)
  (* x x))

(define (tan-cf x k)
  (cont-frac-iter (lambda (i x)
                    (if (= i 1)
                        x
                        (square x)))
                  (lambda (i)
                    (- (* 2 i) 1))
                  10
                  x))

(tan-cf (/ 3.14 4) 10)