(define prependl
  (lambda (l1 l2)
    (cond
      ((null? l1) l2)
      (else (cons (car l1) (prependl (cdr l1) l2)))
    )
  )
)

(define nesting
  (lambda (a b n)
   (cond
     ( (= n 0) '() )     
     ( else 
        (prependl (nesting a b (- n 1))
                  (hulp a b n)) 
     )
   )
  )
)

(define hulp
  (lambda (a b n)
   (cond
     ( (= n 0) (cons b '()) )
     ( else 
        (cons a
            (cons 
                  (hulp a b (- n 1))
                  '()
            )
        )
     )
   )
  )
)

(nesting 'a 'b 0) ;geeft als resultaat ’().
(nesting 'a 'b 1) ;geeft als resultaat ’( a (b) ).
(nesting 'a 'b 2) ;geeft als resultaat ’( a (b) a (a (b)) ).
(nesting 'a 'b 3) ;geeft als resultaat ’( a (b) a (a (b)) a (a (a (b))) ).