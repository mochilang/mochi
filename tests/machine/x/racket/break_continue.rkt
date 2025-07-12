#lang racket
(define numbers '(1 2 3 4 5 6 7 8 9))
(let/ec break
  (for ([n (if (hash? numbers) (hash-keys numbers) numbers)])
    (let/ec continue
(if (equal? (remainder n 2) 0)
  (begin
(continue)
  )
  (void)
)
(if (cond [(string? n) (string>? n 7)] [(string? 7) (string>? n 7)] [else (> n 7)])
  (begin
(break)
  )
  (void)
)
(displayln (string-join (map ~a (list "odd number:" n)) " "))
    )
  )
)
