#lang racket
(displayln (cond [(string? "mochi") (string-length "mochi")] [(hash? "mochi") (hash-count "mochi")] [else (length "mochi")]))
