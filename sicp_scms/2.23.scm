(define (for-each1 f iterms)
  (if (not (null? iterms))
      (begin (f (car iterms))
           (for-each f (cdr iterms)))))


(for-each1 (lambda (x) (newline) (display x)) (list 1 2 3))
