(define (filter p? l)
  (cond ((null? l) l)
        ((p? (car l)) (cons (car l) (filter p? (cdr l))))
        (else (filter p? (cdr l)))))

(define (exists? p? l)
  ;; неконструктивно съществуване
  (not (null? (filter p? l))))

(define (safe f) (lambda (x) (and x (f x))))

(define (cons#f x y) (if y (cons x y) #f))
(define (cons#f x y) (and y (cons x y)))
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

(define (search-child v f g)
  (search f (children v g)))

(define (all-children? v f g)
  (all? f (children v g)))

(define (childless g)
  (filter (lambda (v) (null? (children v g))) (vertices g)))

(define g2 '((1 2 3) (2 3 6) (3 4 6) (4 1 5) (5 3) (6)))

(define g3 '((1 2 4) (2 1 3) (3 2 5 6) (4 1) (5 3) (6 3)))


(define (parents v g)
  (filter (lambda (u) (edge? u v g)) (vertices g)))

(define (symmetric? g)
  (all? (lambda (u)
          (all? (lambda (v) (edge? v u g)) (children u g)))
        (vertices g)))

(define (symmetric? g)
  (all? (lambda (u)
          (all-children? u (lambda (v) (edge? v u g)) g))
        (vertices g)))

(define (symmetric? g)
  (all? (lambda (u)
          (not (search-child u (lambda (v) (not (edge? v u g))) g)))
        (vertices g)))

(define g-weights-edges '((a . ((b . 3) (c . 5)))))
(define g-weights-vertices '(((a . 3) . (b c)) ((b . 7) . (a))))
(define (vertices-weights g) (map caar g))
(define (children-weights-edges u g) (map car (cdr (assq u g))))

(define g-weights-vertices-and-edges '(((a . 3) . ((b . 10) (c . 11)))))

(define (dfs-path? u v g)
  (or (eqv? u v)
      (search-child u (lambda (w) (dfs-path? w v g)) g)))

;; #f ако няма път
;; пътя, ако има
(define (dfs-path u v g)
  (or (and (eqv? u v) (list u))
      (search-child u (lambda (w) (cons#f u (dfs-path w v g))) g)))

;; с проверка за зацикляне
(define (dfs-path u v g)
  ;; търсим итеративно път, натрупваме го в path наобратно
  (define (dfs-search path)
    ;; последният добавен в пътя връх е last
    (let ((last (car path)))
      (cond
        ;; ако съвпада с v: успех
        ((eqv? v last) (reverse path))
        ;; ако вече го е имало, отказваме се
        ((memv last (cdr path)) #f)
        ;; иначе, пробваме всички деца на last
        (else (search-child last (lambda (w) (dfs-search (cons w path))) g)))))
  (dfs-search (list u)))


;; с проверка за зацикляне
(define (dfs-path u v g)
  ;; търсим итеративно път, натрупваме го в path наобратно
  (define (dfs-search path)
    ;; последният добавен в пътя връх е last
    (let ((last (car path)))
      (or (and (eqv? v last) (reverse path))
          (and (not (memv last (cdr path)))
               (search-child last (lambda (w) (dfs-search (cons w path))) g)))))
  (dfs-search (list u)))

(define (dfs-all-paths u v g)
  ;; търсим итеративно път, натрупваме го в path наобратно
  (define (dfs-search path)
    ;; последният добавен в пътя връх е last
    (let ((last (car path)))
      (cond
        ;; ако съвпада с v: успех
        ((eqv? v last) (list (reverse path)))
        ;; ако вече го е имало, отказваме се
        ((memv last (cdr path)) '())
        ;; иначе, пробваме всички деца на last
        (else (apply append
                     (map-children last (lambda (w) (dfs-search (cons w path))) g))))))
  (dfs-search (list u)))

;; проверка за път с търсене в ширина
(define (bfs-path? u v g)
  (define (bfs-level l)
    (and (not (null? l))
         (or (and (memq v l) #t)
             (bfs-level (apply append (map (lambda (w) (children w g)) l))))))
  (bfs-level (list u)))


(define (bfs-path u v g)
  (define (extend path)
    (map-children (car path) (lambda (v) (cons v path)) g))
  (define (acyclic? p)
    (not (memv (car p) (cdr p))))
  (define (extend-acyclic path)
    (filter acyclic? (extend path)))
  (define (extend-level l)
    (apply append (map extend-acyclic l)))
    
  (define (target-path? path)
    (eqv? (car path) v))

  (define (bfs-level l)
    (cond
      ;; ако нивото е празно - няма път
      ((null? l) #f)
      ;; ако в нивото сме стигнали до целевия връх: връщаме първия път
      ((exists? target-path? l)
       (car (filter target-path? l)))
      ;; иначе, разширяваме нивото до следващото ниво
      (else
       (bfs-level (extend-level l)))))

  (bfs-level (list (list u))))

(define (bfs-path u v g)
  (define (extend path)
    (map-children (car path) (lambda (v) (cons v path)) g))
  (define (acyclic? p)
    (not (memv (car p) (cdr p))))
  (define (extend-acyclic path)
    (filter acyclic? (extend path)))
  (define (extend-level l)
    (apply append (map extend-acyclic l)))
    
  (define (target-path path)
    (and (eqv? (car path) v) path))

  (define (bfs-level l)
    (and (not (null? l))
         (or (search target-path l)
             (bfs-level (extend-level l)))))

  (bfs-level (list (list u))))


(define (bfs-all-paths u v g)
  (define (extend path)
    (map-children (car path) (lambda (v) (cons v path)) g))
  (define (acyclic? p)
    (not (memv (car p) (cdr p))))
  (define (extend-acyclic path)
    (filter acyclic? (extend path)))
  (define (extend-level l)
    (apply append (map extend-acyclic l)))
    
  (define (target-path? path)
    (eqv? (car path) v))

  (define (bfs-level l)
    (if
     ;; ако нивото е празно - няма пътища
     (null? l) '()
     ;; взимаме всички целеви пътища в текущото ниво + пътищата от следващото ниво
     (append (filter target-path? l)
             (bfs-level (extend-level l)))))

  (bfs-level (list (list u))))
