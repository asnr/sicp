(define the-empty-environment '())

(define (setup-environment)
  (let ((initial-env (extend-environment '() '() the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define (extend-environment vars vals base-env)
  (let ((new-frame (cons vars vals)))
    (cons new-frame base-env)))

(define (lookup-variable-value variable-symbol env)

  (define (lookup-value-in-frame names values)
    (if (null? names)
        (error "Variable name not found" variable-symbol)
        (let ((curr-name (car names))
              (curr-value (car values)))
          (if (eq? curr-name variable-symbol)
              curr-value
              (lookup-value-in-frame (cdr names) (cdr values))))
        )
    )

  (let* ((frame (car env))
         (names (car frame))
         (values (cdr frame)))
    (lookup-value-in-frame names values))
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
