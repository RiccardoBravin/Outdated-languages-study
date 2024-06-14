#lang racket

(struct node
  ([value #:mutable])
)

(struct b-node node ;this is a struct that contains left, right and value (inherited)
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



; Make a (tree-map f n) procedure that
; does not operate 'in place'

(define (tree-map f t)
  (cond
    [(leaf? t) (node (f (node-value t)))] ;if it is a leaf apply f and return the node
    [ else (let [
                 (v (f (node-value t)))
                 (l (tree-map f (b-node-left t)))
                 (r (tree-map f (b-node-right t)))
                ]
                (b-node v l r)
           )
    ]
  )
)


(define tree-inst (b-node 3(b-node 2 (node 0) (node 1)) (b-node 5 (node 4) (node 6))))

(print-tree (tree-map (lambda (x) (+ x 1)) tree-inst))
