#lang sicp

; sorted list

;(define (union-set set1 set2)
;  (define (merge a b c)
;    (cond ((and (null? a) (null? b)) c)
;          ((null? a) (append c b))
;          ((null? b) (append c a))
;          (else
;           (let ((x1 (car a)) (x2 (car b)))
;             (cond ((= x1 x2)
;                    (merge (cdr a) (cdr b) (cons c x1)))
;                   ((< x1 x2)
;                    (merge (cdr a) b (cons c x1)))
;                   (else
;                    (merge a (cdr b) (cons c x2))))))))
;  (merge set1 set2 '()))
;
(define (append a b)
  (if (null? a)
      b
      (cons (car a) (append (cdr a) b))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x1 (car set1))
               (x2 (car set2)))
           (cond ((= x1 x2)
                  (cons x1 (union-set (cdr set1)
                                      (cdr set2))))
                 ((< x1 x2)
                  (cons x1 (union-set (cdr set1)
                                      set2)))
                 (else
                  (cons x2 (union-set set1
                                      (cdr set2)))))))))


















