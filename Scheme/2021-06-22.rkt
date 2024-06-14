;Define a function mix which takes a variable number of arguments x0 x1 x2 ... xn, the first one is a function,
;then return the list (x1 (x2 ... (x0(x1) x0(x2) ... x0(xn)) xn) xn-1) ... x1).
#lang racket

(define (func g . xs)
  (foldr (lambda (x y) (list x y x)) (map g xs) xs) ;;the starting element for the foldr is the (x0(x1) x0(x2) ... x0(xn)) list then we add to the left and right
                                                    ;;the elements of the xs list starting from the end with foldr
)



(define (f g . L)
  (foldr
     (lambda (x y) (list x y x))
     (map g L)
     L
  )
)


(foldr (lambda (x y)  (cons x y)) '() '(1,2,3,4,5)) ;;the starting element for the foldr is the (x0(x1) x0(x2) ... x0(xn)) list then we add to the left and right

(f add1 1 2 3 4 5)
(func add1 1 2 3 4 5)