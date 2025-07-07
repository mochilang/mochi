(define Leaf 'Leaf)
(define (Node left value right)
  (list 'Node left value right))

(define (sum-tree t)
  (cond
    ((eq? t Leaf) 0)
    ((eq? (car t) 'Node)
     (+ (sum-tree (cadr t))
        (caddr t)
        (sum-tree (cadddr t))))))

(define t
  (Node Leaf 1
        (Node Leaf 2 Leaf)))

(begin (display (sum-tree t)) (newline))
