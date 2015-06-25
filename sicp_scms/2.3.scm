(define (length l)
  (let ((start (start-segment l))
        (end (end-segment l)))
    (sqrt (+ (square (- (x-point start)
                        (x-point end)))
             (square (- (y-point start)
                        (y-point end)))))))


(define (perimeter rect)
  (* 2 (+ (length (long-seg rect))
          (length (short-seg rect)))))

(define (area rect)
  (* (length (long-seg rect))
     (length (short-seg rect))))


(define (rectangle1 long short)
  (cons long short))

(define (long-seg rect) (car rect))
(define (short-seg rect) (cdr rect))

(define (rectangle2 p1 p2)
  (cons p1 p2))

(define (long-seg2 rect)
  (let ((lb (car rect))
        (ru (cdr rect)))
    (make-segment lb (make-point (x-point lb)
                                   (y-point ru)))))

(define (short-seg2 rect)
  (let ((lb (car rect))
        (ru (cdr rect)))
    (make-segment lb (make-point (x-point ru)
                                   (y-point lb)))))


(define (print-result rect)
  (display (perimeter rect))
  (newline)
  (display (area rect)))

(define lb (make-point -2 -1))
(define ru (make-point 4 2))
(define l1 (make-segment lb (make-point -2 2)))
(define l2 (make-segment lb (make-point 4 -1)))


(let ((rect (rectangle1 l1 l2)))
  (print-result rect))

(let ((rect (rectangle2 lb ru)))
  (print-result rect))
