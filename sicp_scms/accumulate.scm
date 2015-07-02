(define (accumulate f init items)
  (if (null? items)
      init
      (f (car items)
         (accumulate f init (cdr items)))))

(define (filter predicate squence)
  (cond ((null? squence) ())
        ((predicate (car squence))
         (cons (car squence)
               (filter predicate (cdr squence))))
        (else (filter predicate (cdr squence)))))

(define (enumerate-tree tree)
  (cond ((null? tree) ())
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(define (enumerate-interval a b)
  (if (> a b)
      ()
      (cons a (enumerate-interval (+ a 1) b))))
