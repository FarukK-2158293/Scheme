(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x))
    )
  )
)

(define Count*
  (lambda (l)
    (cond
      ((null? l) 0)
      ((atom? (car l)) (+ 1 (Count* (cdr l))))
      (else
       (+ (Count* (car l)) (Count* (cdr l)))
       )
    )
    )
  )

(count* '((a b) c ((c a)))) 