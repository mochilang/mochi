#lang racket
(displayln (cond [(string? "a") (string<? "a" "b")] [(string? "b") (string<? "a" "b")] [else (< "a" "b")]))
(displayln (cond [(string? "a") (string<=? "a" "a")] [(string? "a") (string<=? "a" "a")] [else (<= "a" "a")]))
(displayln (cond [(string? "b") (string>? "b" "a")] [(string? "a") (string>? "b" "a")] [else (> "b" "a")]))
(displayln (cond [(string? "b") (string>=? "b" "b")] [(string? "b") (string>=? "b" "b")] [else (>= "b" "b")]))
