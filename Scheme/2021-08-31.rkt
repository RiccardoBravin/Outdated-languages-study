; 1) Define a procedure which takes a natural number n and a default value, and creates a n by n matrix
; filled with the default value, implemented through vectors (i.e. a vector of vectors).
; 2) Let S = {0, 1, ..., n-1} x {0, 1, ..., n-1} for a natural number n. Consider a n by n matrix M, stored in a
; vector of vectors, containing pairs (x,y) ∈ S, as a function from S to S (e.g. f(2,3) = (1,0) is represented
; by M[2][3] = (1,0)). Define a procedure to check if M defines a bijection (i.e. a function that is both
; injective and surjective).

#lang racket


(define (make-matrix n v_def)
   (define matrix (make-vector n #f))
    (let loop ((i 0))
              (unless (= i n)
                (begin
                  (vector-set! matrix i (make-vector n v_def))
                  (loop (+ 1 i))
                )
              )
              matrix
    )
)

(define mat (make-matrix 2 '(0 . 0)))
(displayln mat)


(vector-set! (vector-ref mat 0) 0 '(0 . 0))
(vector-set! (vector-ref mat 0) 1 '(0 . 1))
(vector-set! (vector-ref mat 1) 0 '(1 . 0))
(vector-set! (vector-ref mat 1) 1 '(1 . 1))



(displayln mat)

;function that returns the value in position x y from a matrix
(define (matrix-at matrix x y)
  (vector-ref (vector-ref matrix x) y)
)

;function that checks that a pair contains values inside the range [0, size)
(define (mat-bounds size pair)
  (if (and (and (> (car pair) -1) (< (car pair) size)) (and (> (cdr pair) -1) (< (cdr pair) size)))
      #true
      #false
  )
)


;function that fills a hash map of all touples in the matrix. If the touple contains indices not in the matrix we skip. If in the end we have the size of the map = n*n we have a bijection
(define (bijective? matrix)
  (define size (vector-length matrix))
  (define hmap (make-hash))
  ;cycle on row
  (let loop1 ((i 0))
    (when (< i size)
        (begin
          ;cycle on column
          (let loop2 ((j 0))
             (when (and (< j size) (mat-bounds size (matrix-at matrix i j))) ;check that j < size and that the value inside the couple is a valid matrix position
                 (begin 
                   (hash-set! hmap (matrix-at matrix i j) 0) ;insert in map the current couple (we don't care about the value
                   (loop2 (+ 1 j)) ;go to next column
                 )
             )
          )
          (loop1 (+ 1 i)) ;go to next row
        )
    ) 
  )

  (= (* size size) (hash-count hmap))
)

(bijective? mat)

