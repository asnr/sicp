(define (add-streams xs ys)
  (cons-stream (+ (stream-car xs) (stream-car ys))
               (add-streams (stream-cdr xs) (stream-cdr ys))))

(define fibs
  (cons-stream
   0
   (cons-stream 1 (add-streams (stream-cdr fibs) fibs))))

;; How many additions are performed to calculate (stream-ref fibs n)? Show that
;; the number of additions would be exponentially greater without a memoization
;; optimisation.

;; With memoization: number of additions is n - 1. Every reference to fibs is a
;; reference to the same nested list of function pointers that have memoized
;; their result.

;; Without memoization: number of additions it Fib(n - 1). Base case: immediate.
;; For case n + 1, ugh not sure the best way of organising the proof, I'm just
;; going to leave it.

;; (stream-ref fibs 0) -> no additions
(stream-car (cons-stream 0 ...))
;; (stream-ref fibs 1) -> no additions
(stream-car (stream-cdr (cons-stream 0 (cons-stream 1 ...))))
;; (stream-ref fibs 2) -> 1 addition
(stream-car (stream-cdr (stream-cdr
  (cons-stream
   0
   (cons-stream
    1
    (cons-stream
     (+ (stream-car (stream-cdr (cons-stream 0 (cons-stream 1 ...))))
        (stream-car (cons-stream 0 ...)))
     ...))))))
;; (stream-ref fibs 3) ->
(stream-car (stream-cdr (stream-cdr (stream-cdr
  (cons-stream
  0
  (cons-stream
    1
    (cons-stream
     (+ (stream-car (stream-cdr (cons-stream 0 (cons-stream 1 ...))))
        (stream-car (cons-stream 0 ...)))
     (cons-stream
      (+ (stream-car (stream-cdr (stream-cdr
                                  (cons-stream 0 (cons-stream 1 (cons-stream (+ ...) ...))))))
         (stream-car (stream-cdr (cons-stream 0 (cons-stream 1 ...)))))
      ...))))))))

;; With memoization

