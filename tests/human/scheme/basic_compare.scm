(define (displayln x) (display x) (newline))
(define (bool->string b) (if b "true" "false"))

(let* ((a (- 10 3))
       (b (+ 2 2)))
  (displayln a)
  (displayln (bool->string (= a 7)))
  (displayln (bool->string (< b 5))))
