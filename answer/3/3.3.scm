#lang sicp

(define (make-account secret-password balance)
  (lambda (word m amount)
    (if (eq? word secret-password)

        (cond ((eq? m 'withdraw)
               (if (>= balance amount)
                   (begin
                     (set! balance
                           (- balance amount))
                     balance)
                   "incufficient funds"))
              ((eq? m 'deposit)
               (begin (set! balance
                            (+ balance amount))
                      balance))
              (else (error "unknown request" m)))
        "incorrect password")))
