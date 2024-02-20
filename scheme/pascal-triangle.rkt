(define (calculaterow row)
  (cond
    ((null? (cdr row)) row)
    (else (cons (+ (car row) (cadr row)) (calculaterow (cdr row))))
    
    )
  )

(define (next row)
  (cons '1 (calculaterow row))
  )

(define (customappend lst newlst)
  (cond
    ((null? newlst) lst)
    (else (cons (car newlst) (customappend lst (cdr newlst))))
  )
  )

(define (helper n col)
  (cond
    ((= n 0) (col '(1) '((1))))
    (else (helper (- n 1) (lambda (row lst) (col (next row) (customappend (list (next row)) lst)))))
    
    )
  )


(define (pascal-triangle n)
  (helper n (lambda (row lst) lst))
  )



(next '(1 5 10 10 5 1))
(pascal-triangle 0)
(pascal-triangle 4)
