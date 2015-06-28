;; 把任取的一个元素加入其剩余元素形成的子集+剩余元素子集=该集合子集
(define (subsets s)
  (if (null? s)
      (list ()) ; 注意这里是(()), 改为返回()的话,遇到后面的map,则不会执行
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x)
                            (cons (car s) x))
                          rest)))))

(subsets (list 1 2 3))
