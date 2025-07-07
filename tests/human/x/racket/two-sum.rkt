#lang racket
(define (two-sum nums target)
  (or (for*/first ([i (in-range (length nums))]
                   [j (in-range (+ i 1) (length nums))]
                   #:when (= (+ (list-ref nums i) (list-ref nums j)) target))
        (list i j))
      (list -1 -1)))
(define result (two-sum '(2 7 11 15) 9))
(displayln (list-ref result 0))
(displayln (list-ref result 1))
