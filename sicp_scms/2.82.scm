(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (not (all-the-same? type-tags))
              (let ((converters (find-converters type-tags)))
                (if converters
                    (apply apply-generic
                           (append (list op)
                                   (map (lambda (x y) (x y))
                                        converts
                                        args)))
                    (error "No convert for these tyeps"
                           type-tags)))
              (error "No method for these types"
                     (list op type-tags)))))))

;; iterate the list and find a list of converter that all have define
(define (find-converters items)
  (define (iter n)
    (if (= n (length items))
        '()
        (let ((cur (list-ref items n)))
          (let ((converts
                 (map (lambda (x)
                        (if (eq? x cur)
                            (lambda (x) x)   ; No need to convert if they are the same type
                            (get-coercion x cur)))
                      items)))
            (if (find converts '())
                (iter (+ n 1))
                converts)))))
  (iter 0))


(define (all-the-same? items)
  (every (lambda (x) (eq? x (car items)))
         items))
