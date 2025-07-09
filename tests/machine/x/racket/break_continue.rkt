#lang racket
(define numbers '(1 2 3 4 5 6 7 8 9))
(let/ec break
  (for ([n numbers])
    (let/ec continue
(if (equal? (remainder n 2) 0)
  (begin
(continue)
  )
  (void)
)
(if (> n 7)
  (begin
(break)
  )
  (void)
)
(displayln (string-join (map ~a (list "odd number:" n)) " "))
    )
  )
)
