(define Leaf 'Leaf)
(define (Node left value right)
  (list 'Node left value right)
)

(define (sum_tree t)
  (call/cc (lambda (return)
    (return (let ((_t t)) (cond ((equal? _t Leaf) 0) ((and (pair? _t) (eq? (car _t) 'Node)) (let ((left (list-ref _t 1)) (value (list-ref _t 2)) (right (list-ref _t 3))) (+ (+ (sum_tree left) value) (sum_tree right)))) (else '()))))
  ))
)

(define t (Node Leaf 1 (Node Leaf 2 Leaf)))
(begin (display (sum_tree t)) (newline))
