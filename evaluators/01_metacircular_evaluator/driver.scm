(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let* ((input (read))
         (output (metacircular-eval input the-global-environment)))
    (announce-output output-prompt)
    (user-print output))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (display object))
  ;; (if (compound-procedure? object)
  ;;     (display (list 'compound-procedure
  ;;                     (procedure-parameters object)
  ;;                     (procedure-body object)
  ;;                     '<procedure-env>))
  ;;     (display object)))
