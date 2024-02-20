(define Convert
  (lambda (list)
    (cond((null? list) 0)
         (else(+ (* (car list) (expt 10 (- (length list) 1))) (Convert(cdr list))))
      )
  )
)
  
(convert '(3 2 1))