#lang racket
(define (twoSum nums target)
  (let/ec return
(define n (if (string? nums) (string-length nums) (length nums)))
(for ([i (in-range 0 n)])
(for ([j (in-range (+ i 1) n)])
(if (equal? (+ (if (string? nums) (string-ref nums i) (list-ref nums i)) (if (string? nums) (string-ref nums j) (list-ref nums j))) target)
  (begin
(return '(i j))
  )
  (void)
)
)
)
(return '((- 1) (- 1)))
  ))
(define result (twoSum '(2 7 11 15) 9))
(displayln (if (string? result) (string-ref result 0) (list-ref result 0)))
(displayln (if (string? result) (string-ref result 1) (list-ref result 1)))
