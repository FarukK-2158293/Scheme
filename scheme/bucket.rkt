(define (filter pred lst) 
  (cond
    ((null? lst) lst)
    ((pred (car lst)) (cons (car lst) (filter pred (cdr lst))))
    (else (filter pred (cdr lst)))))

(define (remove-dub lst)
  (cond
    ((null? lst) lst)
    (else (cons (car lst) (remove-dub (filter (lambda (x) (not (= x (car lst)))) (cdr lst)))))))

(define (getbuckets search lst)
  (if (null? search)
      '()
      (cons (filter (lambda (x) (= x (car search))) lst)
            (getbuckets (cdr search) lst))))

(define (bucket lst)
  (getbuckets (remove-dub lst) lst)
  )

(display (bucket '(1))) ; -> '((1))
(newline)
(display (bucket '(1 1 2))) ; -> '((1 1) (2))
(newline)
(display (bucket '(1 1 2 3 3 3 4 5))) ; -> '((1 1) (2) (3 3 3) (4) (5))

(display (remove-dub '(1 1 2 3 3 3 4 5))) ; -> '((1 1) (2) (3 3 3) (4) (5))

