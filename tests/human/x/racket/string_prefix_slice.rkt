#lang racket
(define prefix "fore")
(define s1 "forest")
(displayln (string=? (substring s1 0 (string-length prefix)) prefix))
(define s2 "desert")
(displayln (string=? (substring s2 0 (string-length prefix)) prefix))
