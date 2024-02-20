
(define whatIsIt
  (lambda (waarde)
    (cond
    ((number? waarde) 'number)
    ((symbol? waarde) 'symbol)
    (else 'list)
    )
  )
)


(whatIsIt 'a);
(whatIsIt 4);
(whatIsIt '(a b c));
(whatIsIt '());