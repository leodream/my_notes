(define (last-pair list)
  (cond ((null? list) (error "empty list input."))
        ((null? (cdr list)) list)
        (else (last-pair (cdr list)))))

(last-pair (list 23 72 149 34))
