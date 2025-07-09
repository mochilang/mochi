#lang racket
(define (twoSum nums target)
  (let/ec return
(define n (cond [(string? nums) (string-length nums)] [(hash? nums) (hash-count nums)] [else (length nums)]))
(for ([i (in-range 0 n)])
(for ([j (in-range (+ i 1) n)])
(if (equal? (+ (if (string? nums) (string-ref nums i) (list-ref nums i)) (if (string? nums) (string-ref nums j) (list-ref nums j))) target)
  (begin
(return (list i j))
  )
  (void)
)
)
)
(return (list (- 1) (- 1)))
  ))
(define result (twoSum (list 2 7 11 15) 9))
(displayln (if (string? result) (string-ref result 0) (list-ref result 0)))
(displayln (if (string? result) (string-ref result 1) (list-ref result 1)))
