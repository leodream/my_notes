(define (map1 f items)
  (accumulate (lambda (x y)
                (cons (f x) y))
              ()
              items))

(map1 square (list 2 3 4))


(define (append1 l1 l2)
  (accumulate cons l2 l1))

(append (list (list 1 2) 3) (list 4 (list 5)))


(define (length items)
  (accumulate (lambda (x y)
                (+ x 1))
              0
              items))

(length (list 1 2 (list 3 4) 5))
