(define positionsHulp
  (lambda (pat lat col)
    (cond
      ((null? lat) ; base case: end of the list
       (col '()))  ; return the collected positions
      ((eq? (car lat) pat)
       (positionsHulp pat (cdr lat) 
                      (lambda (result) ; recursive call with updated collector
                        (col (cons (length lat) result)))))
      (else
       (positionsHulp pat (cdr lat) col)))))

(define collector
  (lambda (x)
    x
    )
  )

(define positions
  (lambda (pat lat)
    (positionsHulp pat lat collector)))

(display (positions 'a '(e c a b a c d e a f a)))


;(define positions_help
;  (lambda (pattern list col)
;    (cond
;      ((null? list) '() 1)
;      ((eq? (car list) pattern)
;       positions_help pattern (cdr list)
;       (lambda (pos counter)
;         col (cons counter pos) (+ 1 counter)
;         ))
;      (else
;       positions_help pattern (cdr list)
;       (lambda (pos counter)
;         col pos (+ 1 counter)
;         ))
;      )
;    )
;  )

;(define positions
;  (lambda (pattern l)
;    positions_help pattern l (lambda (pos counter) pos)
;    )
;  )

;(positions 'a '(e c a b a c d e a f a))