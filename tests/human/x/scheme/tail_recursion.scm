(define (sum-rec n acc)
  (if (= n 0)
      acc
      (sum-rec (- n 1) (+ acc n))))
(display (sum-rec 10 0))
(newline)
