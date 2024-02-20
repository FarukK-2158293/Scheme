(define (my-map f lst)
  (cond
    ((null? lst) '())
    (else
     (cons
      (f (car lst))
      (my-map f (cdr lst))))))

(define (check1 list)
  (cond
    ((null? list) #t)
    ((eq? (car list) #f) #f)
    (else (check1 (cdr list)))
    )
  )

(define (check2 list)
  (cond
    ((null? list) #f)
    ((eq? (car list) #t) #t)
    (else (check2 (cdr list)))
    )
  )

(define (for-all lst p)
   (check1 (my-map p lst))
)

(define (there-exists lst p)
 (check2 (my-map p lst))
)

(display (for-all '(2 4 6) even?)) ; Output: #t
(newline)
(display (for-all '(1 2 3) even?)) ; Output: #f
(newline)
(display (for-all '() even?))      ; Output: #t
(newline)

(display (there-exists '(2 4 6) even?)) ; Output: #t
(newline)
(display (there-exists '(1 2 3) even?)) ; Output: #t
(newline)
(display (there-exists '(1 3) even?))   ; Output: #f
(newline)
(display (there-exists '() even?))      ; Output: #f
(newline)
