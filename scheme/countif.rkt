
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x))
    )
  )
)

(define (countif* pat lst)
  (cond
    ((null? lst) 0)
    ((and (atom? lst) (eq? lst pat)) 1)
    ((atom? lst) 0)
    (else (+ (countif* pat (car lst)) (countif* pat (cdr lst))))
    )
  )

(countif* 'a '((a b) c ((c a))))