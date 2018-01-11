(define (omega1 n) (omega1 (+ n 1)))
(define (omega2 n) (+ 1 (omega2 (- n 1))))