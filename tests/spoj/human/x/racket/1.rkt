#lang racket
;; Solution for SPOJ TEST - Life, the Universe, and Everything
;; https://www.spoj.com/problems/TEST

(let loop ()
  (define v (read))
  (unless (eof-object? v)
    (unless (= v 42)
      (displayln v)
      (loop))))
