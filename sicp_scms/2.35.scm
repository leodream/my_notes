(define (count-leaves tree)
  (accumulate (lambda (x y) (+ y 1))
              0
              (enumerate-tree tree)))

(define (enumerate-tree tree)
  (cond ((null? tree) ())
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))


(count-leaves (list 1 (list 2 3 (list 4)) (list 5 6)))


(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) (if (pair? x)
                                       (count-leaves x)
                                       1)) t)))
