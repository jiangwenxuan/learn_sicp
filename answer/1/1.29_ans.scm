(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (y k)
    (f (+ a (* k h))))
  (define (factor k)
    (cond ((or (= k 0) (= k n))
           1)
          ((odd? k)
           4)
          (else
           2)))
  (define (term k)
    (* (factor k)
       (y k)))
  (if (not (even? n))
      (error "n can't be odd")
      (* (/ h 3)
         (sum term (exact->inexact 0) next n))))
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (new-simpson f a b n)
  (let ((h (/ (- b a) n)))
    (define (term k)
      (let ((y (f (+ a (* k h)))))
        (cond ((or (= k 0)
                   (= k n))
               y)
              ((even? k) (* 2 y))
              (else (* 4 y)))))
    (* (/ h 3)
       (sum term 0 inc n))))
(define (cube x)
  (* x x x))

             
            
















