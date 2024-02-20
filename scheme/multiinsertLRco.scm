;-----------------------------------
; Hulpfuncties

(define add1
  (lambda (n)
    (+ n 1)))


;-----------------------------------

; Deze collector wordt als laatste aangeroepen, en moet
; je dus manueel meegeven aan de allereerste oproep van 
; multiinsertLR&co:
(define col0
    (lambda (x y z)
        (cons y (cons z x))
    )
)

; MultiinsertLR&co
;
; Voorbeeld-oproep:
;   (multiinsertLR&co 'salty 'fish 'chips '(chips and fish or fish and chips) col0)

(define multiinsertLR&co
    (lambda (new oldL oldR lat col)
        (cond
            ; Basisgeval
            ( (null? lat)
              (col '() 0 0)
            )
            
            ; Merk op dat de recursieve oproepen van multiinsertLR&co hieronder
            ; telkens dezelfde waarden gebruiken voor de eerste vier parameters.
            ; Het is enkel de collector die verschilt.
            
            ; Ontdekking van oldL
            ( (eq? (car lat) oldL)
              (multiinsertLR&co new oldL oldR (cdr lat) 
                                              ; Collector
                                              (lambda (newlat L R)
                                                (col (cons new (cons oldL newlat)) ; cons 'new' langs links
                                                     (add1 L)
                                                     R
                                                )
                                              )
              )
            )
            
            ; Ontdekking van oldR (op pag 141 wordt aangenomen dat oldR verschillend is van oldL)
            ( (eq? (car lat) oldR)
              (multiinsertLR&co new oldL oldR (cdr lat)
                                              ; Collector
                                              (lambda (newlat L R)
                                                (col (cons oldR (cons new newlat)) ; cons 'new' lang rechts
                                                     L
                                                     (add1 R)
                                                )
                                              )
              )
            )
            
            ; We hebben op dit punt niet oldL en niet oldR ontdekt.
            ( else
              (multiinsertLR&co new oldL oldR (cdr lat)
                                              ; Collector
                                              (lambda (newlat L R)
                                                (col (cons (car lat) newlat) ; sowieso de (car lat) behouden
                                                     L
                                                     R
                                                )
                                              )
              )
            )
        )
    )
)
