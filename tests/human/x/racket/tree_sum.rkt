#lang racket
(struct leaf ())
(struct node (left value right))

(define (sum-tree t)
  (match t
    [(leaf) 0]
    [(node l v r) (+ (sum-tree l) v (sum-tree r))]))

(define t (node (leaf) 1 (node (leaf) 2 (leaf))))
(displayln (sum-tree t))
