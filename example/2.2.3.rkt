#lang sicp

(define (square x)
  (* x x))

(define (sum-odd-square tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree)
             (square tree)
             0))
        (else
         (+ (sum-odd-square (car tree))
            (sum-odd-square (cdr tree))))))

(define (fib b)
  (fib-iter 1 0 0 1 b))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p p) (* q q))
                   (+ (* 2 p q) (* q q))
                   (/ count 2)))
        (else
         (fib-iter (+ (* b q) (* a q) (* a p))
                   (+ (* b p) (* q a))
                   p
                   q
                   (- count 1)))))
                      
        

(define (even-fibs n)
  (define (next k)
    (if (> k n)
        nil
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))

(define (filter predicate seq)
  (cond ((null? seq) nil)
        ((predicate (car seq))
         (cons (car seq)
               (filter  predicate (cdr seq))))
        (else (filter predicate (cdr seq)))))

(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq)
          (accumulate op initial (cdr seq)))))

(accumulate + 0 (list 1 2 3 4 5))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree))
         (list tree))
        (else
         (append (enumerate-tree (car tree))
                 (enumerate-tree (cdr tree))))))

(define (sum-odd-tree tree)
  (accumulate +
              0
              (map square
                   (filter odd?
                           (enumerate-tree tree)))))

(define (new-even? fibs n)
  (accumulate cons
              nil
              (filter even?
                      (map fib
                           (enumerate-interval 0 n)))))

(define (list-fib-square n)
  (accumulate cons
              nil
              (map square
                   (map fib
                        (enumerate-interval 0 n)))))

(define (product-of-squares-of-odd-elements seq)
  (accumulate *
              1
              (map square
                   (filter odd? seq))))

(accumulate append
            nil
            (map (lambda (i)
                   (map (lambda (j) (list i j))
                        (enumerate-interval 1 (- i 1))))
                 (enumerate-interval 1 6)))



                 













