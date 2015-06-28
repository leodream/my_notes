(define (tree-map f tree)
  (if (null? tree)
      ()
      (map (lambda (x)
             (if (pair? x)
                 (tree-map f x)
                 (f x)))
           tree)))

(define (square-tree tree) (tree-map square tree))

(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
