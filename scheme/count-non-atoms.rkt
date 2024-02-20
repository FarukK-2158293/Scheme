(define (count-non-atoms lst res)
  (cond
    ((null? lst) res)
    ((list? (car lst)) 
     (count-non-atoms (cdr lst) (+ res 1)))
    (else
     (count-non-atoms (cdr lst) res))))

(display (count-non-atoms '(a () b) 0)) ; -> 1
(display (count-non-atoms '(1 (2 (3)) 4) 0)) ; -> 1
(display (count-non-atoms '((a) (b c)) 0)) ; -> 2
