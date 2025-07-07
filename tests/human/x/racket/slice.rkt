#lang racket
(require racket/list)
(displayln (sublist '(1 2 3) 1 3))
(displayln (sublist '(1 2 3) 0 2))
(displayln (substring "hello" 1 4))
