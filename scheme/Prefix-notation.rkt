(define infix->prefix
  (lambda (lst)
    (cond
      ((list? lst)
       (cond
         ((= (length lst) 3)
          (list (infix->prefix (car (cdr lst)))
                (car lst)
                (infix->prefix (car (cdr (cdr lst)))))
          )
         (else
          (list infix->prefix lst)
          )
         )
        )
      (else
       lst
       )
      )
    )
  )

(display (infix->prefix '(3 + 4)))
(newline)
(display (infix->prefix '(3 + (4 * 5))))