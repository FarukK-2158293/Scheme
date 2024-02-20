(define newton
  (lambda (x tolerance)
    
    (define (improve guess)      ;guess = 1 / 2 * ( guess + x / guess )
      (/ (+ guess (/ x guess)) 2))

    (define (good-enough? guess) ;abs( x - guess * guess ) > tolerance
      (< (abs (- (* guess guess) x)) tolerance))

    (define (sqrt-iter guess)    ;while
      (cond ((good-enough? guess) guess)
            (else (sqrt-iter (improve guess)))))

    (sqrt-iter (/ x 2))))        ;guess = x / 2

(display (newton 5 0.5))
