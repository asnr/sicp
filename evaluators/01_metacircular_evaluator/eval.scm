(load "syntax.scm")

(define (metacircular-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((definition? exp) (eval-definition exp env))
        ((application? exp)
         (metacircular-apply (metacircular-eval (operator exp) env)
                             (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type: EVAL" exp))))

(define (eval-definition exp env)
  (define-variable! (definition-variable exp) (definition-value exp) env)
  'ok)

(define (metacircular-apply proc argument-list)
  ;; Only handles primitive functions (evaluated in MIT Scheme)
  (apply (cadr proc) argument-list))

(define (list-of-values operand-exps env)
  (map (lambda (operand-exp) (metacircular-eval operand-exp env))
       operand-exps))
