(define (prependl lst1 lst2)
  (cond
    ((null? lst1) lst2)
    (else (cons (car lst1) (prependl (cdr lst1) lst2)))
  )
)

(define revl
  (lambda (lst)
    (if (null? lst)
        '() ;
        (prependl (revl (cdr lst)) (list (car lst)))
    )
  )
)





(define list1 '(a (b) c d (e f)))
(define list2 '(a (b) c))

(display(Revl list1))
(newline)
(display(Revl list2))


