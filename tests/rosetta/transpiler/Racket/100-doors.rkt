;; Generated by Mochi 0.10.40 on 2025-07-25 19:02 +0700
#lang racket/base
(require racket/list racket/string racket/math racket/match json)
(define nowSeed (let ([s (getenv "MOCHI_NOW_SEED")]) (and s (string->number s))))
(define (now)
  (if nowSeed
      (begin (set! nowSeed (modulo (+ (* nowSeed 1664525) 1013904223) 2147483647)) nowSeed)
      (inexact->exact (floor (* (current-inexact-milliseconds) 1000)))))
(define (int x)
  (cond
    [(integer? x) x]
    [(number? x) (inexact->exact (truncate x))]
    [(string? x) (let ([n (string->number x)]) (if n (inexact->exact (truncate n)) 0))]
    [else 0]))
(define (float x)
  (cond
    [(number? x) (exact->inexact x)]
    [(string? x) (let ([n (string->number x)]) (if n (exact->inexact n) 0.0))]
    [else 0.0]))
(define (input) (read-line))
(define (upper s) (string-upcase s))
(define (lower s) (string-downcase s))
(define (sublist lst start end)
  (if (string? lst)
      (substring lst start end)
      (take (drop lst start) (- end start))))

(define (pad-start s width ch)
  (if (< (string-length s) width)
      (string-append (make-string (- width (string-length s)) (string-ref ch 0)) s)
      s))

(let* ([_start_mem (current-memory-use)] [_start (now)])
(define doors (list))
(let/ec _break (let ([i 0])
  (let loop ()
    (when (< i 100)
(set! doors (append doors (list #f)))
      (set! i (+ i 1))
      (loop)))
))
(let/ec _break (let ([pass 1])
  (let loop ()
    (when (< pass 101)
(define idx (- pass 1))
(let/ec _break (let loop ()
  (if (< idx 100) (let ()
    (set! doors (list-set doors idx (not (list-ref doors idx))))
    (set! idx (let ([__l idx] [__r pass]) (if (and (string? __l) (string? __r)) (string-append __l __r) (+ __l __r))))
    (loop)) (void))))
      (set! pass (+ pass 1))
      (loop)))
))
(let/ec _break (let ([row 0])
  (let loop ()
    (when (< row 10)
(define line "")
(let/ec _break (let ([col 0])
  (let loop ()
    (when (< col 10)
(define idx (let ([__l (* row 10)] [__r col]) (if (and (string? __l) (string? __r)) (string-append __l __r) (+ __l __r))))
(if (list-ref doors idx) (let ()
(set! line (string-append line "1"))
) (let ()
(set! line (string-append line "0"))
))
(if (< col 9) (let ()
(set! line (string-append line " "))
) (void))
      (set! col (+ col 1))
      (loop)))
))
(displayln line)
      (set! row (+ row 1))
      (loop)))
))
  (let* ([_end (now)] [_end_mem (current-memory-use)]
         [_dur (- _end _start)]
         [_dur_us _dur]
         [_mem (max 0 (- _end_mem _start_mem))])
    (displayln "{")
    (displayln (format "  \"duration_us\": ~a," _dur_us))
    (displayln (format "  \"memory_bytes\": ~a," _mem))
    (displayln "  \"name\": \"main\"")
    (displayln "}")
  )
)
