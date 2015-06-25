;; leo 通用版本
(define (reverse1 tree)
  (iter () tree))
(define (iter new remain)
  (if (null? remain)
      new
      (iter (cons (if (pair? (car remain))
                      (reverse1 (car remain))
                      (car remain))
                  new)
            (cdr remain))))


(reverse1 (list 1 2 (list 3 4) 5))


;; 简化版
(define (deep-reverse1 L)
  (if (pair? L)
      (reverse (map deep-reverse1 L))
      L))

(deep-reverse1 (list 1 2 (list 3 4) 5))

;; 二叉树版本
(define (deep-reverse2 tree)
    (cond ((null? tree)         ; 空树
            '())
          ((not (pair? tree))   ; 叶子
            tree)
          (else
            (reverse (list (deep-reverse2 (car tree))            ; 递归地逆序左右子树
                           (deep-reverse2 (cadr tree)))))))
