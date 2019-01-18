#lang sicp

(define (let*->nested-lets exp)
  (define (make-let params)
    (cond ((last-exp? params)
           (append (list 'let
                         (list (car params)))
                   (let-body exp)))
          (else (list 'let
                      (list (car params))
                      (make-let (cdr params))))))
  (make-let (cadr exp)))

(define (eval-let* exp env)
  (eval (let*->nested-lets exp) env))
                                                  