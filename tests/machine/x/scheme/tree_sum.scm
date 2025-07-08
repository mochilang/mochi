
(define (sum_tree t)
  (call/cc (lambda (return)
    (return (let ((_t t)) (cond ((equal? _t Leaf) 0) ((equal? _t (Node left value right)) (+ (+ (sum_tree left) value) (sum_tree right))) (else '()))))
  ))
)

(define t (list (cons 'left Leaf) (cons 'value 1) (cons 'right (list (cons 'left Leaf) (cons 'value 2) (cons 'right Leaf)))))
(begin (display (sum_tree t)) (newline))
