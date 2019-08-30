(load "syntax.scm")
(load "environments.scm")

(define (metacircular-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((definition? exp) (eval-definition exp env))
        ((assignment? exp) (eval-assignment exp env))
        ((if? exp) (eval-if exp env))
        ((and? exp) (metacircular-eval (and->if exp) env))
        ((lambda? exp) (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env))
        ((begin? exp) (eval-sequence (begin-actions exp) env))
        ((cond? exp) (metacircular-eval (cond->if exp) env))
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

(define (eval-assignment exp env)
  (set-variable-most-recent-scope! (assignment-variable exp)
                                   (metacircular-eval (assignment-value exp) env)
                                   env))

(define (eval-if exp env)
  (if (metacircular-eval (if-predicate exp) env)
      (metacircular-eval (if-consequent exp) env)
      (if (if-has-alternative? exp)
          (metacircular-eval (if-alternative exp) env)))
  )

(define (and->if exp)
  (define (convert predicates)
    (let ((curr-predicate (car predicates))
          (tail (cdr predicates)))
      (if (null? tail)
          curr-predicate
          (list 'if curr-predicate (convert tail) 'false))))
  (convert (and-predicates exp)))

(define (cond->if exp)
  (define (convert branches)
    (let* ((curr-branch (car branches))
           (branch-predicate (cond-branch-predicate curr-branch))
           (tail-branches (cdr branches)))
      (if (null? tail-branches)
          (list 'if
                (if (eq? branch-predicate 'else)
                    'true
                    branch-predicate)
                (cond-branch-value curr-branch))
          (list 'if
                branch-predicate
                (cond-branch-value curr-branch)
                (convert tail-branches))))
    )
  (convert (cond-branches exp)))

(define (eval-sequence exps env)
  (let ((result (metacircular-eval (car exps) env)))
    (if (null? (cdr exps))
        result
        (eval-sequence (cdr exps) env))))

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
        (eval-sequence (proc-body proc-object) new-env))))

(define (list-of-values operand-exps env)
  (map (lambda (operand-exp) (metacircular-eval operand-exp env))
       operand-exps))
