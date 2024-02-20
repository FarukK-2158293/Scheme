(define Suffixen
  (lambda (suf)
    (cond
      ((null? suf) '(()))
      (else (cons suf (Suffixen (cdr suf))))
    )
  )
)

(define suf '(a b c d))

(display (Suffixen suf))
(newline)
