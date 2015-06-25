(define (reverse1 lit)
  (iter () lit))


(define (iter new old)
  (if (null? old)
      new
      (iter (cons (car old) new) (cdr old))))

(reverse1 (list 1 2 3 4 5))
