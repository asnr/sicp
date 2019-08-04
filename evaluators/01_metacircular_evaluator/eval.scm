(define (metacircular-eval exp env)
  (cond ((self-evaluating? exp) exp)))

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))
