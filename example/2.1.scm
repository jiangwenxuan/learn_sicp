(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (demon x) (demon y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (demon y))
               (* (numer y) (demon x)))
            (* (demon x) (demon y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (demon x) (demon y))))

(define (div-rat x y)
  (make-rat (* (numer x) (demon y))
            (* (demon x) (numer x))))

(define (equal-rat? x y)
  (= (* (numer x) (demon y))
     (* (numer y) (demon x))))

(define (make-rat x y)
  (cons x y))
(define (numer x)
  (car x))
(define (demon x)
  (cdr x))
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (demon x)))
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(define (cons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else
           error "argument not 0 or 1 -- CONS" m))))
(define (car x)
  (x 0))
(define (cdr x)
  (x 1))

























