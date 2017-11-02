(define (repeated f n)
  (lambda (x)
    (if (= n 0) x
        (f ((repeated f (- n 1)) x)))))

(define (fact n) ((gamma fact) n))
(define gamma
  (lambda (f)
    (lambda (n)
      (if (= n 0) 1 (* n (f (- n 1)))))))

(define fact
  ((repeated gamma 100) 'empty))

;  (lambda (x)
;        (f ((repeated f) x)))))

(define (gamma-inf me)
  (lambda (n)
   ((gamma (me me)) n)))

(define fact (gamma-inf gamma-inf))

(define (Y gamma)
  (define (gamma-inf me)
    (lambda (n)
      ((gamma (me me)) n)))
  (gamma-inf gamma-inf))

(define gamma-fib
  (lambda (f)
    (lambda (n)
      (if (= n 0) 0
         (if (= n 1) 1
             (+ (f (- n 1)) (f (- n 2))))))))