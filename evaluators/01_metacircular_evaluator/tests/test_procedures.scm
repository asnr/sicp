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
                 ))
