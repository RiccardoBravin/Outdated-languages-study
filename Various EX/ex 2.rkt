#lang racket

;fancy way to make a for loop
#`(let loop ([i 0]) ;#' è un modo per commentare tutto il blocco
  (when (<= i 10)
    (displayln (string-append "Loop " (~a i) " times"))
    (loop (+ i 1))
  )
)

;good explanation of argument list
(define (split-sum x . xs)
  (displayln x); => 1
  (displayln xs); => '(2 3 4 5 6)
  (+ x (apply + xs))
)
(split-sum 1 2 3 4 5 6) ; => 21


;;CLASSES
(struct person(
   name [age #:mutable]
  )
)

(define p1 (person "Ada" 24))
(define p2 "Bob")

(person? p1)
(person? p2)

(set-person-age! p1 28)
(person-age p1)


;Section in which we define a tree structure
(struct node(
   [value #:mutable]
))

(struct binary-node
  node ;;parent variables
  (left ;left variable
   right);right variable
)

(define (leaf? n)
  (and (node? n) (not (binary-node? n)))
)


(define tree_inst (binary-node 4 (binary-node 3 (node 4) (node 2))(node 1)))

;; ====== DISPLAY TREE NICELY ======
; feel free to ignore this code
(require pict)
(require pict/tree-layout)
(define (tree-show n)
  (define (tree-show-helper n)
    (cond [(leaf? n) (tree-layout #:pict (text (~a (node-value n))))]
          [(binary-node? n) (tree-layout #:pict (text (~a (node-value n)))
                                         (tree-show-helper (binary-node-left n))
                                         (tree-show-helper (binary-node-right n)))]))
  (naive-layered (tree-show-helper n)))
;; =================================

(tree-show tree_inst)

;exec function on tree nodes recursively
(define (exec-on-tree f tree)
  (f (node-value tree))
  (unless (leaf? tree)
    (exec-on-tree f (binary-node-left tree))
    (exec-on-tree f (binary-node-right tree))
  )
)

(exec-on-tree displayln tree_inst)


;apply function to tree nodes recursively
(define (apply-to-tree f tree)
  (set-node-value! tree (f (node-value tree)))
  (unless (leaf? tree)
    (apply-to-tree f (binary-node-left tree))
    (apply-to-tree f (binary-node-right tree))
  )
)

(apply-to-tree add1 tree_inst)
(tree-show tree_inst)

;;CLOSURES
(define (make-counter)
  (let ((count 0))
    (lambda()
      (set! count (+ 1 count))
      count
    )
  )
)

(define i (make-counter))

(displayln (i))
(define j (make-counter))

(displayln (i))
(displayln (j))
(displayln (i))

;;MACROS
;sytax that says hello to every person in list
(define-syntax hello
  (syntax-rules();additional words needed to me matched
    ([_ name ...] ;matches hello and a series of variables
     (displayln (string-join (list "hello" name ...))) ;concatenates "hello" with the sequence of names
    )
  )
)

(hello "pippo" "pluto" "paperino")

;;while loop definition
(define-syntax while
  (syntax-rules()
    ([while cond body ...]
      (let loop()
        (when cond
          (begin
            body ...
            (loop)
          )
        )
      )
    )
  )
)


(define k 0)
(while (< k 5) (displayln k) (set! k (+ k 1)))
