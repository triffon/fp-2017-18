(define (fixed-point? f x) (= (f x) x))
;(define (fixed-point? number function) (= (number function) function))

(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a) (accumulate op nv (next a) b term next))))

(define (1+ x) (+ x 1))

(define (id x) x)

(define (p n x)
  (define (term i) (* (- (1+ n) i) (expt x i)))
  (accumulate + 0 0 n term 1+))

(define (p1 n x)
  (define (op a b) (+ (* a x) b))
  (accumulate op 0 1 (1+ n ) id 1+))

(define (p2 n x)
  (define (op a b) (+ (* b x) a))
  (accumulate op 0 1 (1+ n ) id 1+))


(define (accumulate-i op nv a b term next)
  (if (> a b) nv
      (accumulate-i op (op nv (term a)) (next a) b term next)))

(define (p3 n x)
  (define (op a b) (+ (* a x) b))
  (accumulate-i op 0 1 (1+ n) id 1+))

(define (fact n)
  (accumulate * 1 1 n id 1+))

(define (pow x n)
  (accumulate * 1 1 n (lambda (i) x) 1+))

(define (my-exp x n)
  (accumulate + 0 0 n (lambda (i) (/ (pow x i) (fact i))) 1+))

;(define (my-exp x n)
;   (accumulate (lambda (a b) (+ a (/ (* b x) i))) 0 1 n (lambda (i) (/ 1 i))
;              (lambda (i) (+ i 1))))




(define (my-exp x n)
  ;(accumulate (lambda (u v) (+ 1 (* x u v))) 0 1 n (lambda (i) (/ 1 i)) 1+))
  (accumulate (lambda (u v) (+ 1 (* u v))) 0 1 n (lambda (i) (/ x i)) 1+))

(define (exists a b p?)
  (accumulate (lambda (u v) (or u v)) #f a b p? 1+))

(define (prime? n)
  (define (divisor? k) (= (remainder n k) 0))
  (and (> n 1)
       (not (exists 2 (sqrt n) divisor?))))

