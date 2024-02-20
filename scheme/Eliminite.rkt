(define (eliminate limit l)
  (cond
    ((null? l) '())
    ((<= (car l) limit) (cons (car l) (eliminate limit (cdr l))))
    (else (eliminate limit (cdr l)))
  )
)        

(eliminate '1.0 '(2.95 0.95 1.0 5))