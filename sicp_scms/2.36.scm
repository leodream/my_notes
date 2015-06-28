(define (accumulate-n f init l-items)
  (map (lambda (x)
         (accumulate f init x))
       (transpose l-items)))

(define (transpose items)
  (if (null? (car items))
      ()
      (cons (map car items)
            (transpose (map cdr items)))))


(accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

(define (accumulate-n f init items)
  (if (null? (car items))
      ()
      (cons (accumulate f init (map car items))
            (accumulate-n f init (map cdr items)))))
