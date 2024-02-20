(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define minus
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (sub1 (minus n (sub1 m)))))))

(define Div3
  (lambda (L)
    (cond
      ((< L 0) #f)
      ((zero? L) #t)
      (else (Div3 (minus L 3)))))
  )

(display(Div3 30))
