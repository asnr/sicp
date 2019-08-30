(define (test-if-no-alternative)
  (assert-eq '#!unspecific (if false 1)))

(define (test-simple-compound-procedure)
  (define (my-add x y) (+ x y))
  (assert-eq 71 (my-add 68 3)))

(define (test-lexical-scope)
  (define (make-adder x) (lambda (y) (+ x y)))
  (define x 100)
  (assert-eq 3 ((make-adder 1) 2)))

(define (test-cond-simple-take-first-branch)
  (define x "foo")
  (assert-eq 'first (cond ((equal? x "foo") 'first)
                          ((equal? x "bar") 'second)))
  )

(define (test-cond-simple-take-second-branch)
  (define x "bar")
  (assert-eq 'second (cond ((equal? x "foo") 'first)
                           ((equal? x "bar") 'second)))
  )

(define (test-cond-else)
  (define x 30)
  (assert-eq 'else (cond ((eq? x 20) 'first)
                         ((< x 20) 'second)
                         (else 'else)))
  )

(define (test-cond-no-branch-is-true)
  (assert-eq '#!unspecific (cond (false 1))))

(define (test-set!)
  (define x 199)
  (set! x 299)
  (assert-eq 299 x))

(define (test-set!-returns-old-value)
  (define x -1)
  (assert-eq -1 (set! x -2)))

(define (test-set!-expression-as-value)
  (define x 77)
  (set! x (+ 77 77))
  (assert-eq 154 x))

(define (test-set!-parent-scope)
  (define x 'outer-scope-original)
  (define (change-outer-variable) (set! x 'outer-scope-new))
  (change-outer-variable)
  (assert-eq 'outer-scope-new x))

(define (test-and-single-predicate)
  (assert-eq true (and true)))

(define (test-and-two-predicates)
  (assert-eq false (and true false)))

(define (test-and-only-evaluates-necessary-branches)
  (define x 'before-and)
  (and false (set! x 'changed-in-and))
  (assert-eq 'before-and x))

(define (test-or-single-predicate)
  (assert-eq true (or true)))

(define (test-or-two-predicates)
  (assert-eq true (or false true)))

(define (test-or-only-evaluates-necessary-branches)
  (define x 'before-or)
  (or true (set! x 'changed-in-or))
  (assert-eq 'before-or x))

(define (run-tests tests)
  (define failures (collect-results '() tests))
  (if (null? failures)
      'all-tests-succeeded
      (list 'some-tests-failed failures)))

(define (collect-results failures-so-far tests)
  (if (null? tests)
      failures-so-far
      (begin
        (define test-result ((car tests)))
        (if (test-successful? test-result)
            (collect-results failures-so-far (cdr tests))
            (collect-results (cons test-result failures-so-far) (cdr tests))))))

(define (test-successful? test-result)
  (eq? (car test-result) 'success))

(define (assert-eq expected computed)
  (if (eq? expected computed)
      (list 'success)
      (list 'failure "Expected two values to be equal" expected computed)))

(run-tests (list test-if-no-alternative
                 test-simple-compound-procedure
                 test-lexical-scope
                 test-cond-simple-take-first-branch
                 test-cond-simple-take-second-branch
                 test-cond-else
                 test-cond-no-branch-is-true
                 test-set!
                 test-set!-returns-old-value
                 test-set!-expression-as-value
                 test-set!-parent-scope
                 test-and-single-predicate
                 test-and-two-predicates
                 test-and-only-evaluates-necessary-branches
                 test-or-single-predicate
                 test-or-two-predicates
                 test-or-only-evaluates-necessary-branches
                 ))
