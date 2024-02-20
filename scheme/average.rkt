(define (suml lst collector)
  (cond
    ((null? lst) collector)
    (else (suml (cdr lst) (+ collector (car lst))))
  )
)

(define (average lst)
  (/ (suml lst 0) (length lst))
)

(display (average '(5 31 2 4 1)))  ; Output: 8.6
(newline)
