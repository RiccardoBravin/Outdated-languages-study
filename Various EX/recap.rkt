#lang racket

;procedure -----------------------------------------------------------
(define (greet name) ;(define (<fun_name> <params>) <body>)
  (if (string? name) ;condition
      (displayln (string-append "Hello Mr/Ms. " name "!")) ;true case
      (displayln "I cannot greet something that is not a string") ; false case
  )
)

#(greet "Pippo")
#(greet 2)

;tail-recursion
(define (factorial n acc)
  (if (zero? n)
      acc
      (factorial (- n 1) (* n acc));tail recursive because factorial is the last thing to be called
  )
)
(factorial 10 1)

;recursion with hidden parameter 
(define (fact n)
  (define (fact-tail n acc)
    (if (zero? n)
      acc
      (fact-tail (- n 1) (* n acc))
    )
  )
  (fact-tail n 1)
)
(fact 10)

;list ---------------------------------------
(define nums '(1 2 3 4 5 6 7 8 9 10)) ;alternatively (define nums (range 1 100))

;map f lst ---------------------------------------------------
(map (lambda (x) (+ x 10)) nums);add 10 to list

;foldl f acc lst <- this is the tail recursive one ----------------------------------------
(foldl cons '() nums);reverse list

;foldr f acc lst
(foldr cons '() nums)
(foldr * 1 nums);bad use of foldr, better to use foldl when possible

;cond [(c1) (ex1)] [(c2) (ex2)] ... [else (exn)] ------------------------------------
(define (fizzbuzz n)
  (cond
    [(zero? (modulo n 15)) "FizzBuzz"]
    [(zero? (modulo n 5))  "Buzz"]
    [(zero? (modulo n 3))  "Fizz"]
    [else n]
  )
)

#(map fizzbuzz (range 1 100))

;structs -----------------------------------------------------
(struct person
  (name ;immutable by default
  [age #:mutable]
  )
)

(struct node
  ([value #:mutable])
)

;struct nome <superclss> [(field <:#attribute>) (...)...]
(struct b-node node ;this is a struct that contains left, right and value (inherited and first element to define in declarations)
  (left right)
)


(define (leaf? n)
  (and (node? n) (not (b-node? n)))
)


(define (print-tree t)
  (displayln (node-value t))
  (unless (leaf? t)
    (print-tree (b-node-left t))
    (print-tree (b-node-right t))
  )
)
    

(define tree-inst (b-node 3 (b-node 2 (node 0) (node 1)) (b-node 5 (node 4) (node 6))))

#(print-tree tree-inst)

(define (tree-apply f t)
  (set-node-value! t (f (node-value t)))
  (when (not (leaf? t))
    (begin ;perfrm actions sequentially
      (tree-apply f (b-node-left t))
      (tree-apply f (b-node-right t))
    )
  )
  t ;we need to return the tree we just modified otherwhise it doesn't work
)

#(print-tree (tree-apply (lambda (x) (- x 10)) tree-inst))


;closure -----------------------------------------------------
(define (make-counter)
  (let ((count 0)) ;;count is a starting internal value that cannot be changed from outside and the lambda contains this value in it's own space  
    (lambda () (set! count (+ 1 count)) count) ;;lambda to increase count and return it
  )
)

(define cnt1 (make-counter))
(define cnt2 (make-counter))

#(println (cnt1))
#(println (cnt1))
#(println (cnt2))
#(println (cnt1))
#(println (cnt1))
#(println (cnt2))

;macros ----------------------------------------------------
(define-syntax while
  (syntax-rules ()
    [(_ stmnt body ...)
       (let loop ()
          (when stmnt
              (begin
                body
                ...
                (loop)
              )
          )
       )
    ]
  )
)

(while (< (cnt1) 5)
       (displayln (cnt2))
)

;multiple syntax rules
(define-syntax say
  (syntax-rules (hello goodbye)
    [(_ hello) (begin (displayln "hello")(say goodbye))];rule 1 (recursive)
    [(_ goodbye) (displayln "byee")];rule 2
    [(_ ...) (displayln "Nothing to say")];rule 3
  )
)

(say hello)
(say sosss)

