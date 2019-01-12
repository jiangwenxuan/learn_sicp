(define (f g)
  (g 2))
(define (square b)
  (* b b))
(f square)
(f (lambda (x) (* x (+ 2 x))))
(f f)
;(2 2)
;we need 2 is a procedure