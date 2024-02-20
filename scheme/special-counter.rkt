(define (specialprod val1 val2)
    (cond
      ((eq? val1 0) val2)
      (else (* val1 val2))
      )
  )

(define (special-counter lst val col)
      (cond
        ((null? lst) (col '0 '0))
        ((< (car lst) val) (special-counter (cdr lst) val (lambda (som prod) (col (+ som (car lst)) (specialprod prod (car lst))))))
        (else (special-counter (cdr lst) val (lambda (som prod) (col (+ som (car lst)) prod))))
      )
)

(define (colhelper som prod)
  (cons som (list prod))
  )

(special-counter '(1 2 3 4 5 6) 4 colhelper)
