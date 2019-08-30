(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp)
  (symbol? exp))

(define (quoted? exp)
  (eq? (car exp) 'quote))
(define (text-of-quotation exp)
  (cadr exp))

(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (let ((is-definition (tagged-list? exp 'define)))
    (if (and is-definition
             (definition-value? exp)
             (not (eq? (length exp) 3)))
        (error "DEFINITION SYNTAX: value definition has length" (length exp)))
    is-definition))
(define (definition-value? exp) (symbol? (cadr exp)))
(define (definition-procedure? exp) (pair? (cadr exp)))
(define (definition-proc-name exp) (car (cadr exp)))
(define (definition-parameters exp) (cdr (cadr exp)))
(define (definition-body exp) (cddr exp))
(define (definition-variable exp) (cadr exp))
(define (definition-value exp) (caddr exp))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-has-alternative? exp) (> (length exp) 3))
(define (if-alternative exp) (cadddr exp))

(define (and? exp) (tagged-list? exp 'and))
(define (and-predicates exp) (cdr exp))

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-branches exp) (cdr exp))
(define (cond-branch-predicate branch-exp) (car branch-exp))
(define (cond-branch-value branch-exp) (cadr branch-exp))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (tagged-list? exp tag)
  (and (pair? exp)
       (eq? (car exp) tag)))
