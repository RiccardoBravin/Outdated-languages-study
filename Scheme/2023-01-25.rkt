;;; We want to implement a for-each/cc procedure which takes a condition, a list and a body and performs a for-each.
;;; The main difference is that, when the condition holds for the current value, the continuation of the body is stored in
;;; a global queue of continuations. We also need an auxiliary procedure, called use-cc, which extracts and call the
;;; oldest stored continuation in the global queue, discarding it.
#lang racket

(define queuecc '())

(define (use-cc)
  (when (cons? queuecc)
    (let  [(c (car queuecc))]
      (set! queuecc (cdr queuecc))
      (c)
    )
  )
)


(define (for-each/cc cond lst body)(
    when (cons? lst)
        (let [(x (car lst))]
            (call/cc (lambda (c) 
                (when (cond x)
                    (set! queuecc (append queuecc (list c)))
                 )
                 (body x)
                
            ))
            (for-each/cc cond (cdr lst) body)
        )   
))


;;; E.g. if we run:
(println "Main execution")
(for-each/cc odd?
    '(1 2 3 4)
    (lambda (x) (displayln x))
)
(println "first use-cc")
(use-cc)
(println "second use-cc")
(use-cc)

;;; two continuations corresponding to the values 1 and 3
;;; will be stored in the global queue.
;;; Then, if we run: (use-scc), we will get on screen:
;;; 2
;;; 3
;;; 4
