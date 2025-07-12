#lang racket
(define nums '(1 2))
(define letters '("A" "B"))
(define bools '(#t #f))
(define combos (for*/list ([n nums] [l letters] [b bools]) (hash 'n n 'l l 'b b)))
(displayln "--- Cross Join of three lists ---")
(for ([c (if (hash? combos) (hash-keys combos) combos)])
(displayln (string-join (map ~a (list (hash-ref c 'n) (hash-ref c 'l) (hash-ref c 'b))) " "))
)
