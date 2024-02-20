(define (foldl op lst end)
  (cond
    ((null? lst) lst)
    (else (reduce-help-left op (cdr lst) (list (car lst)) end)) 
    )
  )

(define (reduce-help-left op lst res end)
  (cond
    ((null? lst) (op res end))
    (else
     (reduce-help-left op (cdr lst) (op res (car lst)) end))))

(define (reversethis lst)
  (foldl (lambda (acc el) (append (list el) acc)) lst '()))

(display (reversethis '())) ; -> '()
(newline)
(display (reversethis '(1 2 3))) ; -> '(3 2 1)
(newline)
(display (reversethis '(1 (2 3)))) ; -> '((2 3) 1)

;----------------------------------

(define (foldl op lst z)
  (cond
    ((null? lst) z)
    (else (foldl-help op lst z))
    )
  )

(define (foldl-help op lst res)
  (cond
    ((null? lst) res)
    (else
     (foldl-help op (cdr lst) (op (car lst) res))))
    )


(define (reverser lst)
  (foldl cons lst '())
  )

(reverser '()) ; -> '()
(reverser '(1 2 3)) ; -> '(3 2 1)
(reverser '(1 (2 3))) ; -> '((2 3) 1)


;--------------------------------

(define foldl
  (lambda (op lst z)
    (cond
    ((null? lst) z)
    (else
     (foldl op (cdr lst) (op (car lst) z))))))


(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x))
    )
  )
)

(define (filter pred lst) 
  (cond
    ((null? lst) lst)
    ((pred (car lst)) (cons (car lst) (filter pred (cdr lst))))
    (else (filter pred (cdr lst)))))


(define (reduce-left op lst)
  (reduce-help-left op (cdr lst) (car lst)))

(define (reduce-help-left op lst res)
  (cond
    ((null? lst) res)
    (else
     (reduce-help-left op (cdr lst) (op (car lst) res)))))


(define rv1
  (lambda (lst)
    (foldl cons lst '())))


;Question:
#|It is possible, however, you'll need support of a helper function which makes an improper list, proper. In our case, this is the removedots function. It has a special purpose.|#


(define removedots
  (lambda (lst)
    (cond
      ((atom? lst) (cons lst '() ))
    (else
     (cons (car lst) (removedots (cdr lst)))))))
                                           



(define rv2
  (lambda (lst)
    (cond
    ((null? lst) '())
    (else
    (removedots (reduce-left cons lst))))))

;foldl
(rv1 '()) ; -> '()
(rv1 '(1 2 3)) ; -> '(3 2 . 1)
(rv1 '(1 (2 3))) ; -> '((2 3) . 1)

;reduce left
(rv2 '()) ; -> '()
(rv2 '(1 2 3)) ; -> '(3 2 1)
(rv2 '(1 (2 3))) ; -> '((2 3) 1)