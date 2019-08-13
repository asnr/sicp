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
;; Doesn't handle defining functions
(define (definition-variable exp) (cadr exp))
(define (definition-value exp) (caddr exp))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp) (cadddr exp))

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (tagged-list? exp tag)
  (and (pair? exp)
       (eq? (car exp) tag)))
