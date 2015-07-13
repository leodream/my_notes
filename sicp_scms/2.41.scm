(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-board ())

(define (adjoin-position new-row k rest-of-queens)
  (append (list (list new-row k)) rest-of-queens))

(define (safe? k positions)
  (or (< (length positions) 2)
      (and (safe? k (cdr positions))
           (accumulate (lambda (x y) (and x y))
                       #t
                       (map (lambda (pos)
                              (let ((x1 (caar positions))
                                    (y1 (cadar positions))
                                    (x2 (car pos))
                                    (y2 (cadr pos)))
                                (not (or (= x1 x2)
                                         (= y1 y2)
                                         (= (abs (- x1 x2))
                                            (abs (- y1 y2)))))))
                            (cdr positions))))))


(safe? 3 (list (list 1 2) (list 3 3)))

(queens 3)


;; 网上答案. 注意所使用的数据结构不一样，但queens是共用的

(define (adjoin-position new-row k rest-of-queens) ;; 此处忽略了k，因为元素本身在列表中的位置就是k
    (cons new-row rest-of-queens))

(define (safe? k position)
    (iter-check (car position)
                (cdr position)
                 1))

(define (iter-check row-of-new-queen rest-of-queens i)
    (if (null? rest-of-queens)  ; 下方所有皇后检查完毕，新皇后安全
        #t
        (let ((row-of-current-queen (car rest-of-queens)))
            (if (or (= row-of-new-queen row-of-current-queen)           ; 行碰撞
                    (= row-of-new-queen (+ i row-of-current-queen))     ; 右下方碰撞
                    (= row-of-new-queen (- row-of-current-queen i)))    ; 左下方碰撞
                #f
                (iter-check row-of-new-queen
                            (cdr rest-of-queens)    ; 继续检查剩余的皇后
                            (+ i 1))))))            ; 更新步进值
