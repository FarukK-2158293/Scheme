(define (my-append lst1 lst2)
  (if (null? lst1) lst2
      (cons (car lst1) (my-append (cdr lst1) lst2))
  )
)

(define (carprod-helper pair l2)
  (cond
    ((null? l2) '())
    (else (cons (my-append pair (car l2))
                (carprod-helper pair (cdr l2)))
    )
  )
)

(define (carprod l1 l2)
  (cond
    ((null? l1) '())
    (else (my-append (carprod-helper (car l1) l2)
                 (carprod (cdr l1) l2))
    )
  )
)

(carprod '((a b) (c d)) '((e f) (g h)))
