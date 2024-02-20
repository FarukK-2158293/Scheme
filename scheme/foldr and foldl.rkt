(define (reduce-right op lst)
  (cond
    ((null? (cdr lst)) (car lst))
    (else (op (car lst) (reduce-right op (cdr lst))))))

(define (foldr op lst end)
  (op (reduce-right op lst) end)
  )


(define (foldl op lst end)
  (reduce-help-left op (cdr lst) (op end (car lst))))

(define (reduce-help-left op lst res)
  (cond
    ((null? lst) res)
    (else
     (reduce-help-left op (cdr lst) (op res (car lst))))))

(foldl - '(1 2 3 4 5) 10)
(foldr - '(1 2 3 4 5) 10)

