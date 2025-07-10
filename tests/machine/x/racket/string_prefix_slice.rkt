#lang racket
(require racket/list)
(define prefix "fore")
(define s1 "forest")
(displayln (equal? (cond [(string? s1) (substring s1 0 (cond [(string? prefix) (string-length prefix)] [(hash? prefix) (hash-count prefix)] [else (length prefix)]))] [(hash? s1) (hash-ref s1 0)] [else (take (drop s1 0) (- (cond [(string? prefix) (string-length prefix)] [(hash? prefix) (hash-count prefix)] [else (length prefix)]) 0))]) prefix))
(define s2 "desert")
(displayln (equal? (cond [(string? s2) (substring s2 0 (cond [(string? prefix) (string-length prefix)] [(hash? prefix) (hash-count prefix)] [else (length prefix)]))] [(hash? s2) (hash-ref s2 0)] [else (take (drop s2 0) (- (cond [(string? prefix) (string-length prefix)] [(hash? prefix) (hash-count prefix)] [else (length prefix)]) 0))]) prefix))
