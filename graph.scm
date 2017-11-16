(define (filter p? l)
  (cond ((null? l) l)
        ((p? (car l)) (cons (car l) (filter p? (cdr l))))
        (else (filter p? (cdr l)))))

(define (exists? p? l)
  ;; неконструктивно съществуване
  (not (null? (filter p? l))))

(define (safe f) (lambda (x) (and x (f x))))

(define (car#f l) (and l (car l)))

(define (search p? l)
  ;; конструктивно съществуване
  (car#f (filter p? l)))

(define (search p l)
  (and (not (null? l))
       (or (p (car l))
           (search p (cdr l)))))

(define (all? p l)
  (not (search (lambda (x) (not (p x))) l)))

(define g '((1 2 3) (2 3 6) (3 4 6) (4 1 5) (5 3) (6 5)))

(define (vertices g) (map car g))

(define (children v g)
  (cdr (assq v g)))

(define (edge? u v g)
  (memq v (children u g)))

(define (map-children v f g)
  (map f (children v g)))

(define (search-children v f g)
  (search f (children v g)))