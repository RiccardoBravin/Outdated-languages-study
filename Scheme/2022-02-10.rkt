; Consider the following code:
; (define (r x y . s)
;  (set! s (if (cons? s) (car s) 1))
;  (lambda ()
;     (if (< x y)
;       (let ((z x))
;          (set! x (+ s x))
;       z)
;  y)))
; 1. What can we use r for? Describe how it works and give some useful examples of its usage.
; 2. It makes sense to create a version of r without the y parameter? If the answer is yes, implement
; such version; if no, explain why.

#lang racket


(define (r x y . s)
 (set! s (if (cons? s) (car s) 1)) ;if s is a cons we set s to the first element, otherwhise we set it to 1
 (lambda () ;return a function 
    (if (< x y) ;if x >= y ritorna y altrimenti incrementa x e ritorna il vecchio valore
      (let ((z x))
        (set! x (+ s x))
        z
      )
      y
    )
 )
)

; r can be used to describe a linear function with a limit since it returns a function without parameters that each time 
; it gets called returns the value x if it is smaller than y and increments x with step s if given, or 1 otherwhise. 
; Once x becomes >= of y it continues to return y. It could thus be used in making cycles with a step different than 1

;examples:
(define counter (r 0 10 2))

(counter);0
(counter);2
(counter);4
(counter);6
(counter);8
(counter);10
(counter);10
(counter);10...

(define counter2 (r 0 4))
(counter2);0
(counter2);1
(counter2);2
(counter2);3
(counter2);4
(counter2);4...


; It would make sense to make a function without an upper limit that just returns values going up to infinity
(define (r x . s)
  (set! s (if (cons? s) (car s) 1))
  (lambda () 
    (let ((z x))
      (set! x (+ s x))
      z
    )
  )
)