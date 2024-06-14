#lang racket

;implementing merge sort


(define (merge a b) 
  (cond
    [(null? a) b]
    [(null? b) a]
    [else (let(
                (h1 (car a))
                (h2 (car b))
                (t1 (cdr a))
                (t2 (cdr b))
              )
              (if (<= h1 h2)
                  (cons h1 (merge t1 b))
                  (cons h2 (merge t2 a))
              )    
           )
    ]
  )
)

(merge '(1 2 3 4) '(1 1 3 4))

(define (halve l)
  (let (
        (pivot (quotient (length l) 2))
       )
       (split-at l pivot)
  )
)


(halve '(1 2 3 4 5))


(define (merge-sort l)
  (cond
    [(null? l) l]
    [(null? (cdr l)) l]
    [else (let-values (
                       [(dx sx) (halve l)]
                      )
            (merge (merge-sort dx) (merge-sort sx))
          )
    ]  
  )
)

(merge-sort '(5 4 3 2 1))
 