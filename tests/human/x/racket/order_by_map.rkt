#lang racket
(define data
  (list (hash 'a 1 'b 2)
        (hash 'a 1 'b 1)
        (hash 'a 0 'b 5)))

(define sorted
  (sort data
        (lambda (x y)
          (let ([ax (hash-ref x 'a)] [ay (hash-ref y 'a)])
            (if (= ax ay)
                (< (hash-ref x 'b) (hash-ref y 'b))
                (< ax ay))))) )
(displayln sorted)
