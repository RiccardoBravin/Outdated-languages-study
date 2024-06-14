;;; 1. Design a construct to define multiple functions with the same number of arguments at the same time. The
;;; proposed syntax is the following:
;;; (multifun <list of function names> <list of parameters> <list of bodies>).
;;; E.g. (multifun (f g) (x)
;;;  ((+ x x x)
;;;  (* x x)))
;;; defines the two functions f with body (+ x x x) and g with body (* x x), respectively.
;;; 2. Would be possible to define something similar, but using a procedure and lambda functions instead of a
;;; macro? If yes, do it; if no, explain why

(define-syntax  multifun
    (syntax-rules ()
        [(multifun (f . fs) (param ...) (body . bodies))
            (define (f param ...) (body))
            (multifun (fs) (param ...) (bodies))
        ]
    )
)

;;example??
(multifun (f g) (x)
 ((+ x x x)
 (* x x)))


 ; something similar could be a function which returns a list of lambdas each corresponding to the required implementation...
 ; but we cannot bind an undefined number of function names thus this being impossible to achieve

 