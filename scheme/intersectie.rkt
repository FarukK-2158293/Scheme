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

(define (intersectie lijst1 lijst2)
  (cond
    ((or (null? lijst1) (null? lijst2)) '())
    ((member? (car lijst1) lijst2)
     (cons (car lijst1) (intersectie (cdr lijst1) lijst2)))
    (else (intersectie (cdr lijst1) lijst2))
  )
)


(define set1 '(1 8 9))
(define set2 '(2 8 9 (9)))

(display (intersectie set1 set2))
