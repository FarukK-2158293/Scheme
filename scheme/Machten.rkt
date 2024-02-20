(define Macht
  (lambda (m n)
    (cond
      ((zero? n) 1)
      (else (* m (Macht m (- n 1)))))))

(define Machten
  (lambda ( m n)
    (cond
      ((zero? n) '(1))
      (else (cons (Macht m n) (Machten m (- n 1)))))))

(display (Machten 3 3))
