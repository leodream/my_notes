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
    (+ (let ((struct (branch-structure branch)))
         (if (pair? struct)
             (total-weight struct)
             struct))))
  (if (null? tree)
      0
      (+ (cal-branch (left-branch tree))
         (cal-branch (right-branch tree)))))


(define mobile (make-mobile (make-branch 10 20)       ; 活动体的总重量为 20 + 25 = 45
                            (make-branch 10 25)))

(define another-mobile (make-mobile (make-branch 10 mobile)
                                    (make-branch 10 20)))

(total-weight another-mobile)

(define (torque branch)
  (* (if (pair? (branch-structure branch))
         (total-weight (branch-structure branch))
          (branch-structure branch))
     (branch-length branch)))

(define (balance? tree)
  (or (pair? tree)
      (and   (= (torque (left-branch tree))
                (torque (right-branch tree)))
             (balance? (left-branch tree))
             (balance? (right-branch tree)))))

another-mobile
(torque (branch-structure (right-branch another-mobile)))
