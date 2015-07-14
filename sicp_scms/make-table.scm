(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup k1 k2)
      (let ((subtable (assoc k1 (cdr local-table))))
        (if subtable
            (let ((record (assoc k2 (cdr subtable))))
              (if record
                  (cdr record)
                  #f))
            #f)))
    (define (insert k1 k2 value)
      (let ((subtable (assoc k1 (cdr local-table))))
        (if subtable
            (let ((record (assoc k2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons k2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list k1
                                  (cons k2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc) insert)
            (else (error "Unknow operation --TABLE" m))))
    dispatch))

;; (define operation-table (make-table))
;; (define get (operation-table 'lookup-proc))
;; (define put (operation-table 'insert-proc))
;; (put 'a 'a 1)
;; (get 'a 'a)
