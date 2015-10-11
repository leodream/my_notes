;; 安装常规数，每个操作都有两个参数，所以用表(scheme-number scheme-number)
(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  (put 'raise '(scheme-number)
       (lambda (x) (make-rational x 1)))

  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'scheme-number-done)


;; 安装有理数算术包
(define (install-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))

  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'equ? '(rational rational)
       (lambda (x y) (and (= (numer x) (numer y))
                          (= (denom x) (denom y)))))
  (put 'raise '(rational)
       (lambda (x) (make-complex-from-real-imag (/ (numer x) (denom x)) 0)))
  (put 'project '(rational)
       (lambda (x) (make-scheme-number (round (/ (numer x) (denom x))))))

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'rational-done)

(load "complex-system.scm")
(install-rational-package)
(install-scheme-number-package)

(define coercion-table (make-table))
(define get-coercion (coercion-table 'lookup-proc))
(define put-coercion (coercion-table 'insert-proc!))

;; 如果类型不同试图强制到任一类型
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else
                         (error "No method for these tyeps"
                                (list 'convert type-tags))))))
              (error "No method for these types"
                     (list op type-tags)))))))

(define (make-rational n d)
  ((get 'make 'rational) n d))
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (raise x) (apply-generic 'raise x))

(put-coercion 'scheme-number 'complex (lambda (x)
                                        (make-complex-from-real-imag (contents x) 0)))
(make-rational 3 4)
(add (make-scheme-number 3)
     (make-complex-from-real-imag 3 4))
(equ? (make-rational 2 4) (make-rational 1 2))

(equ? (raise (raise (make-scheme-number 4))) (make-complex-from-mag-ang 4 0))

;; 试图根据类型树提升对象
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (apply apply-generic
                 (append (list op)
                         (if (all-the-same? type-tags)
                             (map raise args)
                             (raise-to-same args))))))))

(define (all-the-same? items)
  (every (lambda (x) (eq? x (car items)))
         items))

(define (find-common-type args)
  (list-ref (reverse (type-inheritance (car args)))
            (- (apply min
                      (map (lambda (x)
                             (length (type-inheritance x)))
                           args))
               1)))

(define (raise-to-same args)
  (let ((type (find-common-type args)))
    (map (lambda (x)
           (raise-into x type))
         args)))

(raise-to-same (list (make-complex-from-mag-ang 4 0)
                     (make-scheme-number 4)))

(define (raise-into obj type)
  (cond ((eq? type (type-tag obj)) obj)
        ((get 'raise (list (type-tag obj)))
         (raise-into (raise obj) type))
        (else (error "cannot raise" (list obj 'into type)))))

(define (is-super? super sub)
  (memq (type-tag super) (type-inheritance sub)))

(define (type-inheritance a)
  (let ((type (type-tag a)))
    (if (get 'raise (list type))
        (cons (type-tag a)
              (type-inheritance (raise a)))
        (list type))))


(equ? (make-complex-from-mag-ang 4 0)
      (make-scheme-number 4))
(real-part (make-scheme-number 4))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (let ((result (apply proc (map contents args))))
            (if (memq op '(raise)) ; 如果是有意提升，不要drop
                result
                (drop result)))
          (apply apply-generic
                 (append (list op)
                         (if (all-the-same? type-tags)
                             (map raise args)
                             (raise-to-same args))))))))
(define (project x) (apply-generic 'project x))
(define (drop x)
  (if (and (pair? x) ; 仅当是特殊数据的时候才考虑drop
           (get 'project (list (type-tag x)))
           (equ? (raise (project x)) x))
      (drop (project x))
      x))

(add (make-complex-from-real-imag 4 0)
     (make-rational 2 1))
