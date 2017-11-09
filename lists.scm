(define (foldr op nv l)
  (if (null? l) nv
      (op (car l) (foldr op nv (cdr l)))))

(define (foldl op nv l)
  (if (null? l) nv
      (foldl op (op nv (car l)) (cdr l))))

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

(define (length l)
  (foldr (lambda (u v) (+ v 1)) 0 l))

(define (sum l)
  (foldr + 0 l))

(define (length l)
  (sum (map (lambda (x) 1) l)))
  
(define (append l1 l2)
  (if (null? l1) l2
      (cons (car l1) (append (cdr l1) l2))))

(define (append l1 l2)
  (foldr cons l2 l1))

; (list x1 x2) --> (x1 x2)
; (append l1 l2) --> (... l1 ... l2 ...)
; (cons x l)   --> (x ...l)

(define (rcons x l)
  (append l (list x)))

;;(define (foldr op nv l)
;;  (if (null? l) nv
;;      (op (car l) (foldr op nv (cdr l)))))


(define (reverse l)
  (if (null? l) l
      (rcons (car l) (reverse (cdr l)))))


(define (last l)
  (if (null? (cdr l)) (car l)
      (last (cdr l))))

;;(define (reverse l)  (if (null? l)
 ;;     (cons (last l) (reverse .....))))

;;(define (reverse l)
;;  (if (null? l) l
;;      (cons (car l) (reverse (cdr l)))))

(define (reverse l)
  (define (iter r l)
    (if (null? l) r
        (iter (cons (car l) r) (cdr l))))
  (iter '() l))

(define (reverse l)
  (foldr rcons '() l))

(define (snoc x y) (cons y x))

(define (reverse l)
  (foldl snoc '() l))

(define (from-to a b)
  (if (> a b) '()
      (cons a (from-to (+ a 1) b))))

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

(define (member-all = x l)
  (cond ((null? l) #f)
        ((= x (car l)) l)
        (else (member-all = x (cdr l)))))

(define (memq x l) (member-all eq? x l))
(define (memqv x l) (member-all eqv? x l))
(define (member x l) (member-all equal? x l))

(define (member-all? = x l)
  (foldr (lambda (u v) (or (= u x) v)) #f l))

(define (collect a b next)
  (if (> a b) '()
      (cons a (collect (next a) b next))))

(define (from-to a b) (collect a b 1+))

(define (map f l)
  (if (null? l) l
      (cons (f (car l)) (map f (cdr l)))))

(define (map f l)
  (foldr (lambda (u v) (cons (f u) v)) '() l))

;;(define (filter p? l)
;;  (cond ((null? l) l)
 ;;       ((p? (car l)) (cons (car l) (filter p? (cdr l))))
  ;;      (else (filter p? (cdr l)))))

(define (filter p? l)
  (if (null? l) l
      (let ((fl (filter p? (cdr l))))
        (if (p? (car l)) (cons (car l) fl)
            fl))))

(define (filter p? l)
  (foldr (lambda (u v) (if (p? u) (cons u v) v)) '() l))

(define (accumulate op nv a b term next)
  (foldr op nv (map term (collect a b next))))

(define dl '((1 (2)) (((3) 4) (5 (6)) () (7)) 8))

(define (atom? x) (and (not (pair? x)) (not (null? x))))

(define (count-atoms l)
  (cond ((null? l) 0)
        ((atom? l) 1)
        (else (+ (count-atoms (car l)) (count-atoms (cdr l))))))

(define (flatten l)
  (cond ((null? l) '())
        ((atom? l) (list l))
        (else (append (flatten (car l)) (flatten (cdr l))))))

;;(define (reverse l)
;;  (if (null? l) l
;;      (rcons (car l) (reverse (cdr l)))))


(define (deep-reverse l)
  (cond ((null? l) '())
        ((atom? l) l)
        (else (rcons (deep-reverse (car l)) (deep-reverse (cdr l)) ))))

(define (deep-fold nv term l)
  (cond ((null? l) nv)
        ((atom? l) (term l))
        (else (op (deep-fold nv term (car l))
                  (deep-fold nv term (cdr l))))))

(define (branch p? f g) (lambda (x) (if (p? x) (f x) (g x))))

(define (deep-fold nv term op l)
  (foldr op nv (map (branch atom? term (lambda (l) (deep-fold nv term op l))) l)))

(define (append . l)
  (if (null? l) '()
      (let ((l1 (car l)))
        (if (null? l1) (apply append (cdr l))
            (cons (car l1)
                  (apply append (cons (cdr l1) (cdr l))))))))

(define (evali x) (eval x (interaction-environment)))