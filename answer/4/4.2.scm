#lang sicp

; 修改过程和赋值的子句的顺序:
; (define x 3) is application, it will look for define in
; the environment

(define (eval exp env)
  (cond
    ((procedure-call? exp)
     (apply (eval (operator exp) env)
            (list-of-values (operands exp) env)))
;...
    (else
     (error "unknown exp"))))