(define globalvar '())


(define my-null
  (lambda (pointer mem)
    (cond
      ((null? mem) #t)
      (else #f))))

(define my-remove
  (lambda (pointer mem)
    (cond
      ((my-null pointer mem) '())
      ((eq? mem pointer) (car (cdr mem)))
      (else (list (car mem) (my-remove pointer (car (cdr mem))))))))

(define my-car
  (lambda (pointer)
    (cond
      ((list? pointer) (car pointer))
      (else '()))))

(define my-cdr
  (lambda (pointer)
    (cond
      ((list? pointer) (car (cdr pointer)))
      (else '()))))

(define my-cons
  (lambda (var pointer)
    (cond
      ((my-null var (cdr pointer)) (list (cons var pointer)))
      ((null? (car pointer)) (list (cons var pointer)))
      (else (cons (car pointer) (my-cons var (cdr pointer)))))))

(define my-list-to-list
  (lambda (ls)
    (cond
      ((null? ls) (list '()))
      (else (cons (car ls) (my-list-to-list (car (cdr ls))))))))

(define pointer (list 5 '()))
(set! globalvar (list 7 (list 3 pointer)))

(my-remove pointer globalvar)

(define result1 (my-car pointer)) 
(define result2 (my-cdr pointer))

(display result1)
(newline)
(display result2)
(newline)
(my-cons '4 globalvar)
(my-list-to-list globalvar)

