(define (count-pairs l)
  ; Count the unique number of pairs in a structure `l'.

  (define (contained-in? l elem)
    (if (not (pair? l))
        false
        (or (eq? (car l) elem) (contained-in? (cdr l) elem))))

  (define (unique-pairs pairs-so-far structure)
    (cond ((not (pair? structure)) pairs-so-far)
          ((contained-in? pairs-so-far structure) pairs-so-far)
          (else
           (let* ((pairs-with-this (cons structure pairs-so-far))
                  (pairs-with-car (unique-pairs pairs-with-this
                                                (car structure))))
             (unique-pairs pairs-with-car (cdr structure))))))

  (length (unique-pairs '() l)))

(count-pairs '(a b c))
;; -> 3, correct

(count-pairs
 (let ((terminal-pair (cons 'x 'y))
       (mid-pair (cons '() '())))
   (set-car! mid-pair terminal-pair)
   (set-cdr! mid-pair terminal-pair)
   (cons mid-pair mid-pair)))
;; -> 3, correct


(count-pairs
 (let ((pair-2 (cons '() '()))
       (pair-3 (cons '() '())))
   (set-car! pair-2 pair-3)
   (set-car! pair-3 pair-2)
   (cons pair-2 pair-3)))
;; -> 3, correct
