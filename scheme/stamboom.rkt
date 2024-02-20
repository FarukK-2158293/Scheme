(define children-of
  (lambda (tree)
    (car(cdr(cdr(cdr(cdr tree)))))))

(define partner-of
  (lambda (tree)
    (car(cdr(cdr(cdr tree))))))

(define fTree
  (lambda (tree)
    (fTree-help tree 0)
    )
  )

(define fTree-help
  (lambda (tree counter)
    (cond
      ((null? tree) 0)
      (else
    (+(+(+ 1 counter) (*partner tree)) (*children (children-of tree) counter))))))

(define *partner
  (lambda (tree)
    (cond
      ((eq? (partner-of tree) 'unknown) 0)
    (else 1))))

(define *children
  (lambda (tree counter)
    (cond
      ((null? tree) 0)
    (else
      (+ (fTree-help (car tree) counter)  (*children (cdr tree) counter))))))

;-------------------------------------------------------------------------------------------------------;
(define Age-of-person
  (lambda (year tree)
    (- year (car (cdr tree)))))

(define Avg
  (lambda (year tree)
    (/ (Avg-help year tree ) (fTree2 tree)))) 

(define Avg-help
  (lambda (year tree)
    (cond
      ((null? tree) 0)
      (else
       (+ (Age-of-person year tree) (*childrens-age year (children-of tree)))))))

(define *childrens-age
  (lambda (year tree)
    (cond
      ((null? tree) 0)
    (else
     (+ (Avg-help year (car tree)) (*childrens-age year (cdr tree)))))))

(define fTree2
  (lambda (tree)
    (fTree-help2 tree 0)
    )
  )


(define fTree-help2
  (lambda (tree counter)
    (cond
      ((null? tree) 0)
      (else
    (+(+ 1 counter) (*children2 (children-of tree) counter))))))

(define *children2
  (lambda (tree counter)
    (cond
      ((null? tree) 0)
    (else
      (+ (fTree-help2 (car tree) counter)  (*children2 (cdr tree) counter))))))


(define p '(Dave 1920 brown Kelly ((Anna 1955 blue James ((Bob 1980 blue unknown ())(Lara 1982 green unknown ())))(Carl 1957 brown Elisabeth ()))))
(define example-tree'(Alice 1980 brown Bob((Charlie 2005 blue unknown ())(David 2008 green unknown ())(Eva 1985 brown Frank((Grace 2010 blue unknown ())(Henry 2012 green unknown ())(Isabel 2016 brown unknown ()))))))

(fTree p) ;8
(fTree example-tree) ;9

(Age-of-person 2023 p)
(Age-of-person 2023 example-tree)
