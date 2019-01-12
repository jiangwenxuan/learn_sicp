(define (product-recu term a next b)
  (if (> a b)
      1
      (* (term a)
         (product-recu term (next a) next b))))
(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))
(define (square a)
  (* a a))
(define (cc a)
  (/ (* (- a 1) (+ a 1)) (square a)))
(define (next a)
  (+ a 2))
(define (pi-sum1 a)
  (* 4
     (product-recu cc a next 100)))
(exact->inexact (pi-sum1 3))
(define (pi-sum2 a)
  (* 4
     (product-iter cc a next 100)))
(exact->inexact (pi-sum2 3))
