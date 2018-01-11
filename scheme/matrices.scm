(define m '((1 2 3) (4 5 6)))

(define (foldr op nv l)
  (if (null? l) nv
      (op (car l) (foldr op nv (cdr l)))))

(define (1+ n) (+ n 1))

(define (from-to a b)
  (if (> a b) '()
      (cons a (from-to (+ a 1) b))))

(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a) (accumulate op nv (next a) b term next))))

(define (all p? l) (foldr (lambda (u v) (and u v)) #t (map p? l)))

(define (matrix? m)
  (and (list? m)
       (all list? m)
       (not (null? m))
       (not (null? (car m)))
       (all (lambda (row) (= (length (car m)) (length row))) m)))

(define (get-rows m) (length m))
(define get-rows length)
(define (get-columns m) (length (car m)))

(define (get-first-row m) (car m))
(define get-first-row car)

(define (get-first-column m) (map car m))

(define del-first-row cdr)
(define (del-first-column m) (map cdr m))

(define (get-row i m) (list-ref m i))
(define (get-column i m) (map (lambda (row) (list-ref row i)) m))
(define (get-element i j m) (list-ref (get-row i m) j))

(define (make-matrix-from-rows lr) lr)
;(define (make-matrix-from-columns lc) ...)

(define (transpose m)
  (make-matrix-from-rows 
   (map (lambda (i) (get-column i m)) (from-to 0 (- (get-columns m) 1)))))

(define (transpose m)
  (accumulate cons '() 0 (- (get-columns m) 1)  (lambda (i) (get-column i m)) 1+))

(define (transpose m)
  (apply map list m))

(define (sum-vectors v1 v2) (map + v1 v2))
(define (sum-matrices m1 m2) (map sum-vectors m1 m2))

(define map-rows map)
(define (map-columns f m) (map-rows f (transpose m)))

(define (mult-vectors v1 v2) (apply + (map * v1 v2)))
(define (mult-matrices m1 m2)
  (let ((m2t (transpose m2)))
    (map (lambda (row1)
           (map (lambda (column2) (mult-vectors row1 column2)) m2t))
         m1)))

(define (mult-matrices m1 m2)
  (map-rows (lambda (row1)
           (map-columns (lambda (column2) (mult-vectors row1 column2)) m2))
         m1))
