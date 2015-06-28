(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (branch-length branch) (car branch))
(define (branch-structure branch) (cadr branch))

(define (total-weight tree)
  (define (cal-branch branch)
    (let ((struct (branch-structure branch)))
      (if (pair? struct)
          (total-weight struct)
          struct)))
  (if (null? tree)
      0
      (+ (cal-branch (left-branch tree))
         (cal-branch (right-branch tree)))))


(define mobile (make-mobile (make-branch 10 20)
                            (make-branch 4 50)))

(define another-mobile (make-mobile (make-branch 1 mobile)
                                    (make-branch 10 7)))

(total-weight another-mobile)

(define (torque branch)
  (if (not (pair? branch)) ; 如果branch是单独的重量则直接返回
      branch
      (* (if (pair? (branch-structure branch))
             (total-weight (branch-structure branch))
             (branch-structure branch))
         (branch-length branch))))


(define (balance? tree)
  (or (not (pair? tree))  ;非mobile也算作平衡
      (let ((left (left-branch tree))
            (right (right-branch tree)))
        (and   (= (torque left)
                  (torque right))
               (balance? (branch-structure left))
               (balance? (branch-structure right))))))

another-mobile
(torque (branch-structure (left-branch mobile)))

(balance? another-mobile)


;; 如果改用以下实现，
(define (make-mobile left right) (cons left right))
(define (make-branch length tree) (cons length tree))

;; 则只需要改下面的过程
(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cdr mobile))
(define (branch-length branch) (car branch))
(define (branch-structure branch) (cdr branch))
