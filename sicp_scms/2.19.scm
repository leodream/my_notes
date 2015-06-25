(define us-cons (list 100 50 20 10 5 2 1 0.5))

(define (count-change list amount)
  (cond ((= 0 amount) 1)
        ((< amount 0) 0)
        ((null? list) 0)
        (else (+ (count-change (cdr list) amount)
                 (count-change list (- amount (car list)))))))


(count-change us-cons 100)
