;; Attempts to count unique pairs in structure `l'
(define (count-pairs l)
  (if (not (pair? l))
      0
      (+ 1 (count-pairs (car l)) (count-pairs (cdr l)))))

(count-pairs '(a b c))
;; -> 3, correct

(count-pairs
 (let ((terminal-pair (cons 'x 'y))
       (mid-pair (cons '() '())))
   (set-car! mid-pair terminal-pair)
   (set-cdr! mid-pair terminal-pair)
   (cons mid-pair mid-pair)))
;; -> 7, incorrect


(count-pairs
 (let ((pair-2 (cons '() '()))
       (pair-3 (cons '() '())))
   (set-car! pair-2 pair-3)
   (set-car! pair-3 pair-2)
   (cons pair-2 pair-3)))
;; Infinite loop. "Aborting!: maximum recursion depth exceeded"
