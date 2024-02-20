; substodd*

(define substodd*
    (lambda (old new l)
        ; Uitleg over de collector-functie volgt hieronder.
        (hulp old new l 0 (lambda (x e) e))
    )
)
                            
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x))
         )
    )
  )
  
(define add1
  (lambda (x)
    (+ x 1)))

; In vergelijking met de oplossing voor hoofdstuk 7, 
; neemt de functie 'hulp' nu ook een collector als parameter.
; Deze collector heeft de parameters (x e), waarbij x het aantal
; tegengekomen voorkomens van 'old' is, en e is het resultaat
; dat substodd* moet teruggeven. De waarde x wordt enkel gebruikt
; bij recursie op (car l).
(define hulp
    (lambda (old new l aantal col)
        (cond
            
            ; - - - - - - - - - - - - - - - - - - - - - -
            ( (null? l) (col aantal '()) )
            
            ; - - - - - - - - - - - - - - - - - - - - - -
            ( (atom? (car l))
                (cond 
                    ( (eq? (car l) old)
                        ; Dan is (car l) het atom 'old'. We moeten nu sowieso het aantal
                        ; tegengekomen voorkomens van 'old' verhogen.
                        (cond 
                            ( (odd? (add1 aantal))
                                ; We moeten substitueren: dit gebeurt in de collector.
                                (hulp old new (cdr l) (add1 aantal) (lambda (x e)
                                                                        (col x (cons new e))))
                            )
                            ( else 
                                ; We moeten niet substitueren, maar wel 'old' behouden. Dit gebeurt
                                ; opnieuw in de collector.
                                (hulp old new (cdr l) (add1 aantal) (lambda (x e)
                                                                        (col x (cons old e))))
                            )
                        )
                    )
                    ; Dan is (car l) dus niet het atom 'old'. We behouden (car l) in de collector.
                    ( else (hulp old new (cdr l) aantal (lambda (x e)
                                                            (col x (cons (car l) e))))
                    )
                )
            )
            
            ; - - - - - - - - - - - - - - - - - - - - - -
            ; Dus (car l) is zelf een list. Dan hebben we recursie op (car l) en (cdr l).
            ( else 
                ( hulp old new (car l) aantal (lambda (x e)
                                                ; De parameter x is het aantal voorkomens van 'old' tot zover,
                                                ; waarin ook de voorkomens van 'old' zitten van (car l). Dit
                                                ; geven we mee aan de recursie op (cdr l).
                                                (hulp old new (cdr l) x (lambda (y f)
                                                                            ; f is de gesubstitueerde versie van (cdr l).
                                                                            ; Daarop "consen" we het resultaat van de recursie op
                                                                            ; (car l), waardoor de nesting behouden blijft.
                                                                            (col y (cons e f))))))
            )
        )
    )
)


 (substodd* 'a 'b '(a (c (a) a) c d (a)))
 (substodd* 'a '15 '(a (a) b b l (a (a))))