(define (filter pred lst) 
  (cond
    ((null? lst) lst)
    ((pred (car lst)) (cons (car lst) (filter pred (cdr lst))))
    (else (filter pred (cdr lst)))))

(define (reduce-right op lst)
  (cond
    ((null? (cdr lst)) (car lst))
    (else (op (car lst) (reduce-right op (cdr lst))))))

(define (add-numbers lst)
  (reduce-right + (filter number? lst))
  )

(add-numbers '(a 1 b c d 2)) ; -> 3
(add-numbers '(1 2 3)) ; -> 6
(add-numbers '(1 a b)) ; -> 1