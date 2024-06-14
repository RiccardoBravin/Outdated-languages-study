;Ex 1
;SCHEME: 
;Write a function 'depth-encode' that takes in input a list possibly containing
;other lists at multiple nesting levels, and returns it as a flat list where
;each element is paired with its nesting level in the original list.
;
;E.g. (depth-encode '(1 (2 3) 4 (((5) 6 (7)) 8) 9 (((10))))) 
;returns
;((0 . 1) (1 . 2) (1 . 3) (0 . 4) (3 . 5) (2 . 6) (3 . 7) (1 . 8) (0 . 9) (3 . 10))

#lang racket

(define (depth-encode list)
  (define (depth-enc in depth)
    (cond
      [(null? in) in]
      [(list? (car in))
         (append (depth-enc (car in) (+ 1 depth)) (depth-enc (cdr in) depth))
      ]
      [else
         (cons (cons depth (car in)) (depth-enc (cdr in) depth))
      ] 
    )
  )
  (depth-enc list 0)
)



(define (depth-encode-prof ls)
  (define (enc-aux l)
    (cond
      [(null? l) l]
      [(list? (car l))
        (append
         (map (λ (nx) (cons (+ (car nx) 1) (cdr nx))) (enc-aux (car l)))
         (enc-aux (cdr l))
        )
      ]
      [else (cons (cons 0 (car l)) (enc-aux (cdr l)))]
    )
  )
  (enc-aux ls)
)


(depth-encode-prof '(1 (2 3) 4 (((5) 6 (7)) 8) 9 (((10))))) 
(depth-encode '(1 (2 3) 4 (((5) 6 (7)) 8) 9 (((10))))) 
