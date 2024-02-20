(define multirember&co
  (lambda (a lat col)
    (cond
      ((null? lat) (col '() '()))
      (( < (car lat) a)
       (multirember&co a (cdr lat) 
                     (lambda (newlat seen) 
                       (col newlat (cons (car lat) seen)))))
      (else 
       (multirember&co a (cdr lat) 
                     (lambda (newlat seen) 
                       (col (cons (car lat) newlat) seen)))))))



(define lat '(3 1 4 1 5 9 2 6))
(define a 5)
(define initial-collector (lambda (x y)
(cons x (cons y '()))))

(multirember&co a lat initial-collector)
