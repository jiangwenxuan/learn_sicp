#lang sicp

(define (square-tree tree)
  (tree-map square tree))

(define (square z)
  (* z z))

(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (not (pair? sub-tree))
             (proc sub-tree)
             (tree-map proc sub-tree)))
       tree))
(define (another-tree-map proc tree)
  (cond ((null? tree) '())
        ((not (pair? tree))
         (proc tree))
        (else
         (cons (another-tree-map proc (car tree))
               (another-tree-map proc (cdr tree))))))