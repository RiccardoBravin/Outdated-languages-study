;;; We want to implement a version of call/cc, called store-cc, where the continuation is called only once and
;;; it is implicit, i.e. we do not need to pass a variable to the construct to store it. Instead, to run the
;;; continuation, we can use the associated construct run-cc (which may take parameters). The composition
;;; of store-cc must be managed using in the standard last-in-first-out approach.
#lang racket

(define (ccstack) '())

(define (pushcc x) (set! ccstack (cons x ccstack)) )

(define (run-cc . vars) (
    if (cons? ccstack)
        (let [(x (car ccstack))]
            (set! ccstack (cdr ccstack))
            (apply x vars)
        )
        ;;else
        (error "No cc to run")
))



(define-syntax store-cc
    (syntax-rules ()
        [
            (_ body ...)
            (call/cc (lambda (context) (pushcc context) body ...))
        ]
    )
)



;;; E.g. if we run:

(define (test)
 (define x 0)
 (store-cc
    (displayln "here")
    (set! x (+ 1 x))
 )
 (displayln x)
 (set! x (+ 1 x))
 x
)

(test)
(run-cc)



;;; we will get:
;;; here
;;; 1
;;; 2
;;; and if we call (run-cc)
;;; we get:
;;; 2
;;; 3
;;; and the continuation is discarded.
    