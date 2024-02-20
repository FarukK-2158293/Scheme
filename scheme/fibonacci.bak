(define (calcfib n)
  (cond
    ((= n 0) 1)
    ((= n 1) 1)
    (else (+ (calcfib (- n 1)) (calcfib (- n 2))))
  )
  )

(define (Fibonacci k)
  (cond
    ((= k 0) '(1))
    (else (cons (calcfib k) (Fibonacci (- k 1))))
  )
)

(Fibonacci 10)
