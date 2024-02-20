(define (suml lst collector)
  (cond
    ((null? lst) collector)
    (else (suml (cdr lst) (+ collector (car lst))))
  )
)

(define (average lst)
  (/ (suml lst 0) (length lst))
)

(define (somatieding lst avg container)
  (cond
    ((null? lst) container)
    (else (somatieding (cdr lst) avg (+ container (expt (- (car lst) avg) 2))))
  )
)
  
(define (variance lst)
  (/ (somatieding lst (average lst) 0) (length lst))
  )

(display (variance '(5 31 2 4 1))) 
(newline)
