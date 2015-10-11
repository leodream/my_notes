(load "type-tag.scm")

;; dense term list operations
(define (install-dense-terms-package)
  (define (adjoin-term term term-list)
    (let ((exponent (order term))
          (len (length term-list)))
      (define (iter-adjoin times terms)
        (cond ((=zero? (coeff term))
               terms))
        ((= exponent times)
         (cons (coeff term) terms))
        (else (iter-adjoin (+ times 1)
                           (cons 0 terms))))
      (iter-adjoin len term-list)))

  (define (first-term term-list)
    (cons (- (length term-list) 1)
          (car term-list)))

  (define (coeff-list term-list) term-list) ; dense-list的所有项的系数就是它本身

  (put 'adjoin-term '(term dense-list)
       (lambda (x y)
         (attach-tag 'dense-list (adjoin-term x y))))
  (put 'make 'dense-list
       (lambda (l)
         (attach-tag 'dense-list l)))

  (put 'first-term '(term dense-list) first-term)
  (put 'coeff-list '(dense-list) coeff-list)
  )

;; sparse term list operations
(define (install-sparse-terms-package)
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term)) ; 2-80 =zero?
        term-list
        (cons term term-list)))

  (define (first-term term-list) (car term-list))

  (define (coeff-term term-list) (map cdr term-list))

  (put 'adjoin-term '(term parse-list)
       (lambda (x y)
         (attach-tag 'parse-list (adjoin-term x y))))
  (put 'make 'dense-list
       (lambda (l)
         (attach-tag 'dense-list l)))

  (put 'first-term '(term parse-list) first-term)
  (put 'coeff-term '(parse-list) coeff-term)
  )


;; common term-list operations
(define (the-empty-termlist) '())
(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list) (null? term-list))


;; term operations
(define (install-term-package)
  (put 'order '(term)
       (lambda (x) (car x)))
  (put 'coeff '(term)
       (lambda (x) (cdr x)))
  (put 'make 'term
       (lambda (order coeff)
         (attach-tag 'term (list order coeff))))
  )


;; 关联构造函数
(define (make-term order coeff)
  ((get 'make 'term) order coeff))
(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))
(define (make-dense-terms . l)
  ((get 'make 'dense-list) l))
(define (make-parse-terms . l)
  ((get 'make 'parse-list) l))


;; polynomial operations
(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1)
         (variable? v2)
         (eq? v1 v2)))
  (define (=number? exp num)
    (and (number? exp)
         (= exp num)))

  (put '=zero? 'polynomial
       (lambda (p)
         ;;  map a empty-term-list will return '(), every a '() will still return #t
         (every =zero? (coeff-list (term-list p)))))

  ;; representation of terms and term lists
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- MUL-POLY"
               (list p1 p2))))

  (define (add-terms l1 l2)
    (cond ((empty-termlist? l1) l2)
          ((empty-termlist? l2) l1)
          (else
           (let ((t1 (first-term l1)) (t2 (first-term l2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-term (rest-terms l1) l2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-term l1 (rest-terms l2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms l1)
                                (rest-terms l2)))))))))

  (define (mul-terms l1 l2)
    (if (empty-termlist? l1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term l1) l2)
                   (mul-terms (rest-terms l1) l2))))

  (define (mul-term-by-all-terms t1 l)
    (if (empty-termlist? l)
        (the-empty-termlist)
        (let ((t2 (first-term l)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-term l))))))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)
