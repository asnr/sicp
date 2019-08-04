(define (make-accumulator val)
  (lambda (summand)
    (set! val (+ val summand))
    val))
