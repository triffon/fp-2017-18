(define al '((1 . 3) (3 . 5) (5 . 7)))

(define (filter p? l)
  (cond ((null? l) l)
        ((p? (car l)) (cons (car l) (filter p? (cdr l))))
        (else (filter p? (cdr l)))))

(define (assoc-all = key al)
  (cond ((null? al) #f)
        ((= (caar al) key) (car al))
        (else (assoc-all = key (cdr al)))))

(define (assoc key al) (assoc-all equal? key al))

(define (del-assoc key al)
  (filter (lambda (kv) (not (equal? (car kv) key))) al))

(define (add-key-value key value al)
  (cons (cons key value) (del-assoc key al)))