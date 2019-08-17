(define (test-if-no-alternative)
  (assert-eq '#!unspecific (if false 1)))

(define (test-simple-compound-procedure)
  (define (my-add x y) (+ x y))
  (assert-eq 71 (my-add 68 3)))

(define (test-lexical-scope)
  (define (make-adder x) (lambda (y) (+ x y)))
  (define x 100)
  (assert-eq 3 ((make-adder 1) 2)))

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
                 ))
