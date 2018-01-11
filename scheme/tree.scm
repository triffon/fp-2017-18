(define t '(1 (2 () ())
              (3 (4 () ())
                 (5 () ()))))

;; базови операции

(define (tree? t)
  (or (null? t)
      (and (list? t)
           (= (length t) 3)
           (tree? (cadr t))
           (tree? (caddr t)))))
(define (make-tree root left right) (list root left right))
(define make-tree list)
(define empty-tree '())
(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)
(define empty-tree? null?)

;; операции над дървета

(define (depth-tree t)
  (if (empty-tree? t) 0
      (+ 1 (max (depth-tree (left-tree t))
                (depth-tree (right-tree t))))))


(define (member-tree x t)
  (cond ((empty-tree? t) #f)
        ((equal? (root-tree t) x) t)
        (else (or (member-tree x (left-tree t))
                  (member-tree x (right-tree t))))))

(define (cons#f x y) (and y (cons x y)))

(define (path-tree x t)
  (cond ((empty-tree? t) #f)
        ((equal? (root-tree t) x) (list x))
        (else (cons#f (root-tree t)
                      (or (path-tree x (left-tree t))
                          (path-tree x (right-tree t)))))))

