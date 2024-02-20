(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x))
    )
  )
)

(define mystery
  ( lambda ( l )
     ( cond (( atom? l ) l )
            (( null? l ) '() )
            ( else ( cons ( mystery ( car l ) ) ( mystery ( cdr l ) ) ) )
            )
     )
  )


(define result1 (mystery '(Faruk WAS   404    a    happy man)))
(define result2 (mystery '(hi(this(is(fucked))))))

(display "Result 1: ")
(display result1)
(newline)

(display "Result 2: ")
(display result2)
(newline)