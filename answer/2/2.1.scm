(define (better-make-rat n d)
  (cond ((and (< n 0) (< d 0))
         (make-rat (- n) (= d)))
        ((and (> n 0) (< d 0))
         (make-rat (- (abs n)) (abs d)))
        (else
         (let (
               (g (gcd n d))
               )
           (cons (/ n g) (/ d g))))))

(define (another-better-make-rat n d)
  (let ((g (abs (gcd n d)))
        (signed-n (if (negative? d)
                      (- n)
                      n))
        (signed-n (abs d)))
    (cons (/ signed-n g) (/ signed-d g))))