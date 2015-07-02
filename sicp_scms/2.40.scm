(define (unique-pairs n)
  (flatmap (lambda (j)
             (map (lambda (i) (list i j))
                  (enumerate-interval 1 (- j 1))))
           (enumerate-interval 1 n)))

(unique-pairs 4)
