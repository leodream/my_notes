(define (fold-right f init items)
  (if (null? items)
      init
      (f (car items)
         (fold-right f init (cdr items)))))

(define (fold-left f init items)
  (define (iter result remain)
    (if (null? remain)
        result
        (iter (f  result (car remain))
              (cdr remain))))
  (iter init items))


(define (fold-left f init items)
  (if (null? items)
      init
      (fold-left f (f (car items) init) (cdr items))))

(define (reverse1 items)
  (fold-right (lambda (a b)
               (append b (list a))) () items))

(reverse1 (list 1 2 3 4 5))

(define (reverse2 items)
  (fold-left cons () items))

(reverse2 (list 1 2 3 4 5))

;; 注意这两个reverse的时间,空间复杂度是不一样的,
; left 时间O(n) 空间O(n)
; right 时间O(n^2) 空间O(n)
