(define add1
  (lambda (n)
    (+ n 1)))

(define (zip z lst1 lst2)
  (cond
    ((null? lst1) '())
    (else
     (cons (z (car lst1) (car lst2))
           (zip z (cdr lst1) (cdr lst2))))))

(define (check1 list)
  (cond
    ((null? list) #t)
    ((eq? (car list) #f) #f)
    (else (check1 (cdr list)))))

(define (plus-one? lst1 lst2)
  (check1 (zip (lambda (a b) (eq? (add1 a) b)) lst1 lst2)))

(display (plus-one? '(2 3 6) '(3 4 7))) ; -> #t
(newline)
(display (plus-one? '(1 2 3) '(1 3 4))) ; -> #f
(newline)
