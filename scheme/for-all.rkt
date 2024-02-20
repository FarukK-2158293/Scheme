(define (for-all lst p)
  (cond
    ((null? lst) #t)       
    ((p (car lst))         
     (for-all (cdr lst) p)) 
    (else #f)              
  )
)

(define (there-exists lst p)
  (cond
    ((null? lst) #f)      
    ((p (car lst)) #t)     
    (else (there-exists (cdr lst) p)) 
  )
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
