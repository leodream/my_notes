(define (equal? l1 l2)
  (cond ((not (= (length l1) (length l2))) #f)
        ((null? l1) #t)
        (else (and (eq? (car l1) (car l2))
                   (equal? (cdr l1) (cdr l2))))))


;; 网络答案，注意考虑子元素为表的情况
(define (equal? l1 l2)
  (if (and (pair? l1) (pair? l2))
      (and (equal? (car l1) (car l2))
           (equal? (cdr l1) (cdr l2)))
      (eq? l1 l2)))

(equal? '(a '() b c) '(a '() b c))
