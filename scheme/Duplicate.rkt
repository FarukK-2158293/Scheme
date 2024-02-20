(define Duplicate
  (lambda (nd)
    (cond
      ((null? nd) '())
      (else (cons (car nd) (cons (car nd) (Duplicate (cdr nd))))
      )
    )
  )
)

(define nondup '(a b c))
(display (Duplicate nondup))
(newline)
