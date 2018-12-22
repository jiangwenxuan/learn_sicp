(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-reg (cdr items) (- n 1))))

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))
(define (length-iter items)
  (define (iter items result)
    (if (null? items)
        result
        (iter (cdr items) (+ result 1))))
  (iter items 0))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (scale-list items factor)
  (if (null? items)
      nil
      (cons (* (car items) factor)
            (scale-list (cdr items) factor))))

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (new-scale-list items factor)
  (map (lambda (x) (* x factor))
       items))








