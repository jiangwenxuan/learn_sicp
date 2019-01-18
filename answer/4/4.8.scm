#lang sicp

(define (named-let? exp)
  (and (tagged-list? exp 'let)
       (symbol? (cadr exp))))

(define (let-initials exp) (map cadr (cadr exp)))
(define (let-parameters exp) (map car (cadr exp)))
(define (named-let->combination exp)
  (let ((procedure-name (car exp)))
    (make-begin
     (list
      (list 'define procedure-name
            (make-lambda
             (let-parameters exp)
             (let-body exp)))
      (cons precedure-name (let-initials exp))))))

(define (let-combinition exp)
  (if (named-let? exp)
      (named-let->combination (cdr exp))
      (cons (make-lambda (let-parameters exp)
                         (let-body exp))
            (let-initials exp))))