(define firstOfThree
  (lambda (lst1 lst2 lst3)
  (cond
    ((or (null? lst1) (null? lst2) (null? lst3)) #f)
    (else (list (car lst1) (car lst2) (car lst3)))
  ))
)


(display (firstOfThree '(a b c) '(d) '(e f g h)))
(newline)
(display (firstOfThree '(a b c) '(d) '()))        
