;; Generated by Mochi 0.10.33 on 2025-07-21 18:06 +0700
#lang racket
(require racket/list racket/string)
(struct group (key items))

(define matrix (list (list 1 2) (list 3 4)))
(set! matrix 5)
(displayln (list-ref (list-ref matrix 1) 0))
