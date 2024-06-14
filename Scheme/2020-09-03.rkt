#lang racket


;;; Define a construct variant of call/cc, called call/cc-store, with syntax:

;;; (call/cc-store (k in v) e1 ...)

;;; where k is the current continuation, v is a visible variable in the current
;;; scope, and e1 ... is the body of the construct. The semantics is the same of the
;;; usual call/cc (with a simplified syntax, not requiring a lambda), but the
;;; current continuation must also be stored in v, before executing the body.

(define-syntax call/cc-store 
  (syntax-rules (in)
    [(_ (k in v) body ...)
      (call/cc (lambda (k) 
        (set! v k)
        body ...
      ))
    ]
  )
)


;;; Define a pure, tail-recursive function, with O(n) complexity, that, given a list
;;; (e1 e2 ... en), n > 0, returns (en ... e2 e1 e1 e2 ... en). You cannot use
;;; folds, named lets, and reverse.

(define (mirror x . xs)
  (if (null? xs)
    (list x x)
    (append (cons x (mirror xs)) (list x)) 
  )
)

