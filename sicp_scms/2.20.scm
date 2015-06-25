(define (same-parity x . lit)
  (define (iter result remain)
    (if (null? remain)
        result
        (iter (if (odd? (- x (car remain)))
                  result
                  (cons (car remain) result))
              (cdr remain))))
  (reverse (iter (list x) lit)))


(same-parity 2 2 3 4 5 6 7)
