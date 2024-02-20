(define (freqhelper lst val)
  (cond
    ((null? lst) (list (list 1 val)))
    ((eq? (car (cdr (car lst))) val) (cons (cons (+ (car (car lst)) 1) (list val)) (cdr lst)))
    (else (cons (car lst) (freqhelper (cdr lst) val)))
    )
  )

(define (frequency l col)
  (cond
    ((null? l) (col '()))
    (else
     (frequency (cdr l) (lambda (lst) (col (freqhelper lst (car l)))))
     )
   )
  )

(define (maxminhelper lst val comparison)
  (cond
    ((null? lst) (list val))
    ((comparison (car (car lst)) (car val)) (maxminhelper (cdr lst) (car lst) comparison))
    (else (maxminhelper (cdr lst) val comparison))
    )
 )

(define (max-finder lst)
  (cons lst (maxminhelper lst (car lst) >))
  )

(define (min-finder lst)
    (cons lst (maxminhelper lst (car lst) <))
  )

(freqhelper '((1 c) (3 b) (1 d) (3 a)) 'a)

(frequency '(a b c a b d a b) max-finder)
(frequency '(a b c a b a) min-finder)
