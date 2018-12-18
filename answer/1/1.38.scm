(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (dec i) (/ (n i) (+ (d i) result)))))
  (iter k 0))

(define (d i)
  (cond ((= 1 i) 1)
        ((= 2 i) 2)
        (else (let ((current1 (remainder (- i 2) 3))
                   (current2 (quotient (- i 2) 3)))
              (if (= current1 0)
                  (+ 2 (* 2 current1))
                  1)))))

(cont-frac-iter (lambda (i) 1.0)
                d
                11)
