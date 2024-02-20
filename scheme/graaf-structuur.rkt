

(define (setuptemplate k)
  (cond
    ((null? k) '())
    (else (cons (list (car k) 0) (setuptemplate (cdr k)))
    )
    )
  )

(define (updatefreq freq val)
  (cond
    ((null? freq) freq)
    ((eq? (caar freq) val) (cons (cons val (list (+ 1 (cadr (car freq))))) (cdr freq)))
    (else (cons (car freq) (updatefreq (cdr freq) val)))

   )
  )

(define (calcfrequency edge freq)
  (cond
    ((null? edge) freq)
    (else (calcfrequency (cdr edge) (updatefreq freq (caar edge)))) 
    )
  )

(define (out-degree ke)
  (calcfrequency (cadr ke) (setuptemplate (car ke)))
  )


(out-degree '((1 2 3 4) ((1 2) (2 3) (4 2) (4 3))))
(out-degree '((1 2) ()))
(out-degree '(() ()))


