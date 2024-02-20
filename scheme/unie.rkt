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

(define (unie lijst1 lijst2)
  (cond
    ((null? lijst1) lijst2)
    ((null? lijst2) lijst1)
    ((member? (car lijst1) lijst2) (unie (cdr lijst1) lijst2))
    (else (cons (car lijst1) (unie (cdr lijst1) lijst2)))
  )
)

(define set1 '(1 8 9))
(define set2 '(2 8 9 (9)))

(display (unie set1 set2))
