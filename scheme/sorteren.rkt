(define (insert item lst)
  (cond
    ((null? lst) (list item))
    ((null? (cdr lst)) (cons (car lst) (list item)))
    ((< item (car lst)) (cons item lst))
    (else (cons (car lst) (insert item (cdr lst))))
    )
  )

(define (sorting n container)
  (cond
   ((null? n) container)
   (else (sorting (cdr n) (insert (car n) container)))
   )
  )

(define (sort n)
  (sorting n '())
  )

(sort '(5 31 2 4 1))