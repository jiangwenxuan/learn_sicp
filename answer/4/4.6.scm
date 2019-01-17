#lang sicp

(define (eval exp env)
  (cond ((let? exp)
         (eval (let->lambda exp) env))
        (else
         (error "unknown exp" exp))))

(define (let->lambda exp)
  (cons (make-lambda (let-parameters exp)
                     (let-body exp))
        (let-initials exp)))

(define (let-parameters exp)
  (map car (cadr exp)))

(define (let-initials exp)
  (map cadr (cadr exp)))

(define (let-body exp)
  (cddr exp))