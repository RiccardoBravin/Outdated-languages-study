;Define a defun construct like in Common Lisp, where (defun f (x1 x2 ...) body) is used for defining a
;function f with parameters x1 x2 ....
;Every function defined in this way should also be able to return a value x by calling (ret x).
#lang racket

(define ret_stack '())

(define (ret x)
  ((car ret_stack) x)
)

(define-syntax defun
    (syntax-rules  ()
    [(_ func (param ...) body ...) ; syntax to be substituted
      (define (func param ...) ;;after name and parameters we construct the call/cc to exit if ret is called
        (let (
               (exit (call/cc (lambda (context)
                                   (set! ret_stack (cons context ret_stack)) ;;add to the stack the current 
                                    body ...) ;;execute function code
               ))
             )
              (set! ret_stack (cdr ret_stack)) ;;remove latest context from stack
               exit ;; return variable to caller (saved either when call/cc terminates or when we remove and call the ret x
      )
    )
  ]
  )
)


(defun sas (x y)
  (begin
    (when (zero? (* x y)) (ret 1))
    (+ x y)
   )
)

(sas 5 0)