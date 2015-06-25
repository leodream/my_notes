(define x (list (list 1 (list 2)) 3 (list (list (list 4) 5)) 6))


;; map 版本
(define (fringe tree)
  (if (pair? tree)
      (upper
       (map fringe tree))
      tree)
  )

;; 把(( 1 2) 3 ((4))) ==> ( 1 2 3 (4))
(define (upper tree)
  (define (iter new remain)
    (if (null? remain)
        new
        (iter (if (pair? (car remain))
                  (append (reverse (car remain)) new)
                  (cons (car remain)
                        new))
              (cdr remain))))
  (reverse (iter () tree)))

(fringe x)


;; 正常版本
(define (fringe2 tree)
  (define (iter result remain) ;; 正序遍历该树的子结点
    (if (null? remain)
        result
        (iter (if (pair? (car remain))
                  (iter result (car remain))
                  (cons (car remain) result))
              (cdr remain))))
  (reverse (iter () tree)))

(fringe2 x)



;; 网络答案
(define (fringe3 items)
  (define (iter items result)
    (cond ((null? items) result)
          ((not (pair? items)) (append result (list items)))
          (else (iter (cdr items) (iter (car items) result)))))
  (iter items '()))

(fringe3 x)




;; 二叉版本
(define (fringe-bin tree)
  (cond ((null? tree) ())
        ((not (pair? tree)) (list tree))
        (else (append (fringe-bin (car tree))
                      (fringe-bin (cdr tree))))))

(fringe-bin (list (list 1 (list 2 3)) (list (list 4 5) 6)))



;; 注意下面的例子中， (cons x result) 并不会改变result的值，
;; 故输出的是(1)(2)(3)
(define (test result remain)
  (for-each (lambda (x) (display (cons x result)))
            remain)
  result)

(test () (list 1 2 3))
