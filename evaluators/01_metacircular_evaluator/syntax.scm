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

(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-procedure? exp) (pair? (cadr exp)))
(define (definition-proc-name exp) (car (cadr exp)))
(define (definition-parameters exp) (cdr (cadr exp)))
(define (definition-body exp) (cddr exp))
(define (definition-variable exp) (cadr exp))
(define (definition-value exp) (caddr exp))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp) (cadddr exp))

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (tagged-list? exp tag)
  (and (pair? exp)
       (eq? (car exp) tag)))
