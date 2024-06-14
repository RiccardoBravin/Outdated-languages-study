;;; Define a list-to-compose pure function, which takes a list containing functions of one argument and
;;; returns their composition.
;;; E.g. (list-to-compose (list f g h)) is the function f(g(h(x)).
#lang racket

(define (list-to-compose l) 
  (lambda (x)
    (foldr (lambda (f y) (f y)) x l)
  )
)

((list-to-compose (list (lambda (x) (+ 1 x)) (lambda (x) (* 2 x)))) 2)