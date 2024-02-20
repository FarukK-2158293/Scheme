(define (prependl lst1 lst2)
  (cond
    ((null? lst1) lst2)
    (else (cons (car lst1) (prependl (cdr lst1) lst2)))
  )
)

(define list1 '(a b c))
(define list2 '(d e f))

(define result (prependl list1 list2))

(display result)
