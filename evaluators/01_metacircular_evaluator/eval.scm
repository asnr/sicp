(load "syntax.scm")

(define (metacircular-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((definition? exp) (eval-definition exp env))
        (else
         (error "Unknown expression type: EVAL" exp))))

(define (eval-definition exp env)
  (define-variable! (definition-variable exp) (definition-value exp) env)
  'ok)
