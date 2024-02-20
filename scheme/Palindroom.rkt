(define omkeren
  (lambda (lst)
    (define (omkeren-hulp lst acc)
      (if (null? lst)
          acc
          (omkeren-hulp (cdr lst) (cons (car lst) acc))))
    (omkeren-hulp lst '())))

(define (palindroom lst)
  (define omgekeerde-lijst (omkeren lst))
  (define (lijst-samenvoegen lst1 lst2)
    (if (null? lst1) lst2
        (cons (car lst1) (lijst-samenvoegen (cdr lst1) lst2))))
  (lijst-samenvoegen lst omgekeerde-lijst))

(display (palindroom '(a (b c) d))) ; Outputs '(a (b c) d d (b c) a)
