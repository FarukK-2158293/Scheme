(define (atom? x)
  (and (not (pair? x)) (not (null? x))
  )
)

(define (InsertLR* left pattern right l)
  (cond
    ((null? l) '())
    ((pair? (car l))
     (cons (InsertLR* left pattern right (car l)) (InsertLR* left pattern right (cdr l)))
    )
    ((equal? (car l) pattern)
     (cons left (cons pattern (cons right (InsertLR* left pattern right (cdr l)))))
    )
    (else
     (cons (car l) (InsertLR* left pattern right (cdr l)))
    )
  )
)


(InsertLR* 'a 'b 'c '((b) b (e (b))))