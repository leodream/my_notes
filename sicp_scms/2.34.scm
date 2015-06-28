(define (horner-eval x items)
  (accumulate (lambda (a b)
                (+ a
                   (* x b)))
              0
              items))


(horner-eval 2 (list 1 3 0 5 0 1))



;; 注意, 这两个累积过程不一样,第一个过程中,init会被f第一个处理,但实际上是最后的
(define (accumulate f init items)
  (define (iter result remain)
    (if (null? remain)
        result
        (iter (f result (car remain))
              (cdr remain))))
  (iter init items))


(define (accumulate f init items)
  (if (null? items)
      init
      (f (car items)
         (accumulate f init (cdr items)))))
