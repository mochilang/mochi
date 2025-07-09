#lang racket
(struct Leaf () #:transparent #:mutable)
(struct Node (left value right) #:transparent #:mutable)
(define (sum_tree t)
  (let/ec return
(return (match t [(Leaf) 0] [(Node left value right) (+ (+ (sum_tree left) value) (sum_tree right))]))
  ))
(define t (Node Leaf 1 (Node Leaf 2 Leaf)))
(displayln (sum_tree t))
