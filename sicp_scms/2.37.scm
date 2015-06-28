(define (dot-product v w)
  (accumulate + 0 (map * v w)))


(define (matrix-*-vector m v)
  (map (lambda (col) (dot-product col v)) m))


(define (matrix-*-matrix m n)
  (map (lambda (m-col)
         (map (lambda (n-row)
                (dot-product m-col n-row))
              (transpose n)))
       m))


(define m (list (list 1 2 3 4)
                (list 4 5 6 6)
                (list 6 7 8 9)))
(define n (list (list 1 4 6)
                (list 2 5 7)
                (list 3 6 8)
                (list 4 6 9)))

(matrix-*-matrix m n)
