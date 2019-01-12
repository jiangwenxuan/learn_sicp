#lang sicp

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((left (eval (first-operand exp) env)))
        (let ((right (list-of-values (rest-operands exps) env)))
          (cons left right)))))

(define (new-list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((right (list-of-values (rest-operands exps) env)))
        (let ((left (eval (first-operand exp) env)))
          (cons left right)))))