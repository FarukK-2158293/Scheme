;------------------------------------------------
; Hulpfuncties

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x))
    )
  )
)

;------------------------------------------------

; Deze collector wordt als laatste aangeroepen, en moet
; je dus manueel meegeven aan de allereerste oproep van 
; evens-only*&co:
(define the-last-friend
    (lambda (newl product sum)
        (cons sum
            (cons product newl)
        )
    )
)                

; Evens-only*&co
;
; Voorbeeld-oproep:
;    (evens-only*&co '((9 1 2 8) 3 10 ((9 9) 7 6) 2) the-last-friend)

(define evens-only*&co
    (lambda (l col)
        (cond

            ; Basisgeval.
            ( (null? l)
              (col '() 1 0)
              ; Voor de tweede parameter van de collector geven we 1 mee ipv 0 
              ; omdat het product anders altijd 0 is.
            )
            
            ; Vaststelling dat (car l) een atom is.
            ; De aanname is dat alle atoms getallen voorstellen.
            ( (atom? (car l))
              (cond              
                ; Vaststelling dat (car l) een even getal is.
                ( (even? (car l))
                  (evens-only*&co (cdr l)
                                  ; Collector
                                  (lambda (newl p s)
                                    (col (cons (car l) newl) ; (car l) is even -> we bewaren (car l)
                                         (* (car l) p)       ; Neem product met bestaand product.
                                         s                   ; De bestaande som blijft onveranderd.
                                    )
                                  )
                  )
                )
                
                ; Vaststelling dat (car l) een oneven getal is.
                ( else
                  (evens-only*&co (cdr l)
                                  ; Collector
                                  (lambda (newl p s)
                                    (col newl                ; (car l) is oneven -> we bewaren (car l) niet.
                                         p                   ; Het bestaand product blijft onveranderd.
                                         (+ (car l) s)       ; De som groeit.
                                    )
                                  )
                  )
                )
              )
            )
            
            ; (car l) is geen atom -> dan moeten we de technieken van
            ; hoofdstuk 5 toepassen om recursief verder te gaan op (car l).
            ( else
              (evens-only*&co (car l)
                              ; Collector voor (car l) -> deze is verantwoordelijk
                              ; voor het verder gaan op (cdr l), met opnieuw
                              ; een gepaste collector.
                              (lambda (al ap as)
                                (evens-only*&co (cdr l)
                                    ; Collector voor (cdr l)
                                    (lambda (dl dp ds)
                                        (col (cons al dl)       ; Aangezien 'al' zelf een lijst is, zorgt deze cons voor 
                                                                ; een geneste lijst in het resultaat.
                                             (* ap dp)          ; De producten vermenigvuldigen.
                                             (+ as ds)          ; De sommen optellen
                                        )
                                    )
                                )
                              )
              )
            )
        )
    )
)
