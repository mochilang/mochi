#lang racket
(struct Leaf ())
(struct Node (left value right))

(define (sum-tree t)
  (match t
    [(Leaf) 0]
    [(Node l v r) (+ (sum-tree l) v (sum-tree r))]))

(define t (Node (Leaf) 1 (Node (Leaf) 2 (Leaf))))
(displayln (sum-tree t))
