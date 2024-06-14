;;; Define a let** construct that behaves like the standard let*, but gives to variables provided without a binding the
;;; value of the last defined variable. It also contains a default value, stated by a special keyword def:, to be used if the
;;; first variable is given without binding.
;;; For example:
;;; (let** def: #f
;;;  (a (b 1) (c (+ b 1)) d (e (+ d 1)) f)
;;;  (list a b c d e f))
;;; should return '(#f 1 2 2 3 3), because a assumes the default value #f, while d = c and f = e.
#lang racket

(define-syntax let**
    (syntax-rules (def:)
        [(_ def: d_val ((v1 val)) body ...)
            (let [(v1 val)]
                body ...
            )
        ]
        [(_ def: d_val (v1) body ...)
            (let [(v1 d_val)]
                 body ...
            )
        ]
        [(_ def: d_val ((v1 val) . rest) body ...)
            (let [(v1 val)]
                (let** def: val rest body ...)
            )
        ]
        [(_ def: d_val (v1 . rest) body ...)
            (let [(v1 d_val)]
                (let** def: d_val rest body ...)
            )
        ]
    )
)

(let** def: #f
 (a (b 1) (c (+ b 1)) d (e (+ d 1)) f)
 (list a b c d e f))

