(define x 8)
(define msg (if (> x 10)
                "big"
                (if (> x 5) "medium" "small")))
(display msg)
(newline)
