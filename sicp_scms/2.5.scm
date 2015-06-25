(define (cons2 x y)
  (* (expt 2 x)
     (expt 3 y)))

(define (car2 p)
  (define (iter k re)
    (if (= 0 (remainder re 2))
        (iter (+ k 1) (/ re 2))
        k))
  (iter 0 p))


(define (cdr2 p)
  (define (iter k re)
    (if (= 0 (remainder re 3))
        (iter (+ k 1) (/ re 3))
        k))
  (iter 0 p))


(cdr2 (cons2 3 -5))
