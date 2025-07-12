#lang racket
(define (boom )
  (let/ec return
(displayln "boom")
(return #t)
  ))
(displayln (and (and (cond [(string? 1) (string<? 1 2)] [(string? 2) (string<? 1 2)] [else (< 1 2)]) (cond [(string? 2) (string<? 2 3)] [(string? 3) (string<? 2 3)] [else (< 2 3)])) (cond [(string? 3) (string<? 3 4)] [(string? 4) (string<? 3 4)] [else (< 3 4)])))
(displayln (and (and (cond [(string? 1) (string<? 1 2)] [(string? 2) (string<? 1 2)] [else (< 1 2)]) (cond [(string? 2) (string>? 2 3)] [(string? 3) (string>? 2 3)] [else (> 2 3)])) (boom )))
(displayln (and (and (and (cond [(string? 1) (string<? 1 2)] [(string? 2) (string<? 1 2)] [else (< 1 2)]) (cond [(string? 2) (string<? 2 3)] [(string? 3) (string<? 2 3)] [else (< 2 3)])) (cond [(string? 3) (string>? 3 4)] [(string? 4) (string>? 3 4)] [else (> 3 4)])) (boom )))
