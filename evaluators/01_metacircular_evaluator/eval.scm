(load "syntax.scm")
(load "environments.scm")

(define (metacircular-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env))
        ((application? exp)
         (metacircular-apply (metacircular-eval (operator exp) env)
                             (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type: EVAL" exp))))

(define (eval-definition exp env)
  (if (definition-procedure? exp)
      (define-variable!
        (definition-proc-name exp)
        (make-procedure (definition-parameters exp) (definition-body exp) env)
        env)
      (define-variable!
        (definition-variable exp)
        (metacircular-eval (definition-value exp) env)
        env))
  'ok)

(define (eval-if exp env)
  (if (metacircular-eval (if-predicate exp) env)
      (metacircular-eval (if-consequent exp) env)
      (metacircular-eval (if-alternative exp) env)))

(define (make-procedure parameters body env)
  (list 'compound parameters body env))

(define (proc-parameters proc-object) (cadr proc-object))
(define (proc-body proc-object) (caddr proc-object))
(define (proc-base-env proc-object) (cadddr proc-object))

(define (metacircular-apply proc-object argument-list)
  (if (eq? (car proc-object) 'primitive)
      (apply (cadr proc-object) argument-list)
      (let ((new-env (extend-environment (proc-parameters proc-object)
                                         argument-list
                                         (proc-base-env proc-object))))
        (define (eval-expression-list exps env)
          (let ((result (metacircular-eval (car exps) env)))
            (if (null? (cdr exps))
                result
                (eval-expression-list (cdr exps) env))))
        (eval-expression-list (proc-body proc-object) new-env))))

(define (list-of-values operand-exps env)
  (map (lambda (operand-exp) (metacircular-eval operand-exp env))
       operand-exps))
