(define (unlist e)   ; return the element if the list only has one element
  (if (null? (cdr e))
      (car e)
      e))

(define (sum? e)         ; e is a mul expression as long as it has *
  (and (pair? e)
       (pair? (memq '+ e))))

(define (addend e)
  (unlist (drop-right e (length (memq '+ e)))))

(define (augend e)
  (unlist (cdr (memq '+ e))))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (append (if (pair? a1)
                          a1
                          (list a1))
                      (list '+)
                      (if (pair? a2)
                          a2
                          (list a2))))))

(define (product? e) ; e is a mul expr if it only has *
  (and (pair? e)
       (pair? (memq '* e))
       (not (memq '+ e))))

(define (multiplier e)
  (car e))
(define (multiplicand e)
  (unlist (cddr e)))

(define (make-product m1 m2)
  (cond ((or (=number? m2 0) (=number? m1 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (let ((handle (lambda (x)
                              (if (and (pair? x)
                                       (product? x))
                                  x
                                  (list x)))))
                (append (handle m1)
                        (list '*)
                        (handle m2))))))

; still no need to change deriv
(deriv '(x + 3 * (x + y + 2)) 'x)
