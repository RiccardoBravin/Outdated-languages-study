;;; Consider the technique “closures as objects” as seen in class, where a closure assumes the role of a class.
;;; In this technique, the called procedure (which works like a class in OOP) returns a closure which is
;;; essentially the dispatcher of the object.
;;; Define the define-dispatcher macro for generating the dispatcher in an automatic way, as illustrated by
;;; the following example:
;;; (define (make-man)
;;;  (let ((p (make-entity))
;;;  (name "man"))
;;;  (define prefix+name
;;;  (lambda (prefix)
;;;  (string-append prefix name)))
;;;  (define change-name
;;;  (lambda (new-name)
;;;  (set! name new-name)))
;;;  (define-dispatcher methods: (prefix+name change-name) parent: p)))
;;; where p is the parent of the current instance of class man, and make-entity is its constructor.
;;; If there is no inheritance (or it is a base class), define-dispatcher can be used without the parent: p part.
;;; Then, an instance of class man can be created and its methods can be called as follows:
;;; > (define carlo (make-man))
;;; > (carlo 'change-name "Carlo")
;;; > (carlo 'prefix+name "Mr. ")
;;; "Mr. Carlo"



#lang racket

(define-syntax define-dispatcher
  (define-rules (methods: parent:)
    [(_ methods: (met ...) parent: par)
      (lambda (msg . args)
        (case msg
          [(met) (apply met args)] ; if the obj has that method we apply it to the arguments in list form
          ...
          [else (apply p (cons msg args))] ; if the obj doesn't have that method we send the msg and arg to the parent
        )
      )
    ]
    [(_ methods: (met ...))
      (lambda (msg . args)
        (case msg
          [(met) (apply met args)] ; if the obj has that method we apply it to the arguments in list form
          ...
          [else (error "Unknown method" msg)] ; if the obj doesn't have that method we report an error 
        )
      )
    ]
  )
)
