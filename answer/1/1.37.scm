(define (cont-frac n d k)
  (define (frac start)
    (if (= start k)
        (/ (n start) (d start))
        (/ (n start)
           (+ (d start) (frac (inc start))))))
  (frac 1))

(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (dec i) (/ (n i) (+ (d i) result)))))
  (iter k 0))


(cont-frac-iter (lambda (i) 1.0)
                (lambda (i) 1.0)
                11)

