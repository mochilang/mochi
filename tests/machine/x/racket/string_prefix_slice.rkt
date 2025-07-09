#lang racket
(define prefix "fore")
(define s1 "forest")
(displayln (equal? (if (string? s1) (substring s1 0 (cond [(string? prefix) (string-length prefix)] [(hash? prefix) (hash-count prefix)] [else (length prefix)])) (take (drop s1 0) (- (cond [(string? prefix) (string-length prefix)] [(hash? prefix) (hash-count prefix)] [else (length prefix)]) 0))) prefix))
(define s2 "desert")
(displayln (equal? (if (string? s2) (substring s2 0 (cond [(string? prefix) (string-length prefix)] [(hash? prefix) (hash-count prefix)] [else (length prefix)])) (take (drop s2 0) (- (cond [(string? prefix) (string-length prefix)] [(hash? prefix) (hash-count prefix)] [else (length prefix)]) 0))) prefix))
