(define (make-point x y) (cons x y))

(define (x-point p) (car p))

(define (y-point p) (cdr p))

(define (make-segment p1 p2) (cons p1 p2))

(define (start-segment l) (car l))

(define (end-segment l) (cdr l))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (midpoint-segment l)
  (let ((start (start-segment l))
        (end (end-segment l))
        (average (lambda (x y) (/ (+ x y)))))
    (make-point    (average (x-point start)
                            (x-point end))
                   (average (y-point start)
                            (y-point end)))))

(define start (make-point 1 3))
(define end (make-point 4 3))
(define seg (make-segment start end))
(define mid (midpoint-segment seg))
(print-point mid)
