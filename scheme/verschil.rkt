(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a) 
                (member? a (cdr lat))
            )
      )
    )
  )
)

(define (verschil lijst1 lijst2)
  (cond
    ((null? lijst1) '())
    ((member? (car lijst1) lijst2)
     (verschil (cdr lijst1) lijst2))
    (else (cons (car lijst1) (verschil (cdr lijst1) lijst2)))
  )
)

(define set1 '(1 8 9))
(define set2 '(2 8 9 (9)))

(display (verschil set1 set2))
