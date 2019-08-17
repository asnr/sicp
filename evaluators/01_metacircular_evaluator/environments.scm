(define the-empty-environment '())

(define (setup-environment)
  (let ((initial-env (extend-environment primitive-procedure-names
                                         primitive-procedure-objects
                                         the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define (extend-environment vars vals base-env)
  (let ((new-frame (cons vars vals)))
    (cons new-frame base-env)))

(define primitive-procedures
  (list (list 'display display)
        (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'list list)
        (list 'null? null?)
        (list '+ +)
        (list 'eq? eq?)
        (list 'equal? equal?)
        (list '< <)
        (list '> >)))
(define primitive-procedure-names
  (map car primitive-procedures))
(define primitive-procedure-objects
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (lookup-variable-value variable-symbol env)

  (define (lookup-value-in-frame names values)
    (if (null? names)
        '()
        (let ((curr-name (car names))
              (curr-value (car values)))
          (if (eq? curr-name variable-symbol)
              (list curr-value)
              (lookup-value-in-frame (cdr names) (cdr values))))
        )
    )

  (let* ((frame (car env))
         (names (car frame))
         (values (cdr frame))
         (lookup-result (lookup-value-in-frame names values)))
    (if (null? lookup-result)
        (let ((parent-env (cdr env)))
          (if (null? parent-env)
              (error "Variable name not found" variable-symbol)
              (lookup-variable-value variable-symbol parent-env)))
        (car lookup-result))
    )
  )

;; Redefining the same variable will shadow the original entry, not modify it
(define (define-variable! var val env)
  (let* ((frame (car env))
         (names (car frame))
         (values (cdr frame))
         (new-names (cons var names))
         (new-values (cons val values)))
    (set-car! frame new-names)
    (set-cdr! frame new-values)))
