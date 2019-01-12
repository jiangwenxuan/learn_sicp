#lang sicp

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (car (cdr x)))
(define (weight-leaf x) (car (cdr (cdr x))))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))
(define (append a b)
  (if (null? a)
      b
      (cons (car a) (append (cdr a) b))))

(define (left-branch tree)
  (car tree))
(define (right-branch tree)
  (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define (choose-branch bits branch)
  (cond ((= bits 0) (left-branch branch))
        ((= bits 1) (right-branch branch))
        (else
         (error "bad bit -- CHOOSE-BRANCH" bits))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set)))
         (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (find message items)
  (cond ((null? items) -1)
        ((eq? message (car items)) 1)
        (else
         (find message (cdr items)))))

(define (encode-symbol message tree)
  (cond
    ((leaf? tree) '())
    ((= (find message
                  (symbols (left-branch tree)))
            1)
         (cons 0 (encode-symbol
                  message
                  (left-branch tree))))
        ((= (find message
                  (symbols (right-branch tree)))
            1)
         (cons 1 (encode-symbol
                  message
                  (right-branch tree))))
        (error "wront message")))

(define sample-tree
  (make-code-tree (make-leaf 'a 4)
                  (make-code-tree
                   (make-leaf 'b 2)
                   (make-code-tree (make-leaf 'd 1)
                                   (make-leaf 'c 1)))))

(encode '(a d a b b c a) sample-tree)















