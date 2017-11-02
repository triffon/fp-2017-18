(define (list? l)
  (or (null? l)
      (and
         (pair? l)
         (list? (cdr l)))))

(define l (list 1 2 3 4 5))
(define l2 (list 3 4 5))

(define (1+ x) (+ x 1))

(define (length l)
  (if (null? l) 0
      (1+ (length (cdr l)))))

(define (append l1 l2)
  (if (null? l1) l2
      (cons (car l1) (append (cdr l1) l2))))

; (list x1 x2) --> (x1 x2)
; (append l1 l2) --> (... l1 ... l2 ...)
; (cons x l)   --> (x ...l)

(define (reverse l)
  (if (null? l) l
      (append (reverse (cdr l)) (list (car l)))))

(define (list-tail l n)
  (if (= n 0) l
      (list-tail (cdr l) (- n 1))))

(define (repeated f n)
  (lambda (x)
    (if (= n 0) x
        (f ((repeated f (- n 1)) x)))))

(define (list-tail l n)
  ((repeated cdr n) l))

(define (list-ref l n)
  (car (list-tail l n)))
