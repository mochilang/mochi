;; Generated by Mochi 0.10.50 on 2025-07-31 00:17 +0700
#lang racket/base
(require racket/list racket/string racket/math racket/match json openssl/sha1)
(define nowSeed (let ([s (getenv "MOCHI_NOW_SEED")]) (and s (string->number s))))
(define (now)
  (if nowSeed
      (begin (set! nowSeed (modulo (+ (* nowSeed 1664525) 1013904223) 2147483647)) nowSeed)
      (inexact->exact (floor (* (current-inexact-milliseconds) 1000)))))
(define (int x)
  (cond
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

(define (slice seq start end)
  (define len (if (string? seq) (string-length seq) (length seq)))
  (define s (int start))
  (define e (int end))
  (when (< s 0) (set! s (+ len s)))
  (when (< e 0) (set! e (+ len e)))
  (set! s (max 0 (min len s)))
  (set! e (max 0 (min len e)))
  (when (< e s) (set! e s))
  (if (string? seq) (substring seq s e) (sublist seq s e)))
(define (pad-start s width ch)
  (let ([s (format "~a" s)])
    (if (< (string-length s) width)
        (string-append (make-string (- width (string-length s)) (string-ref ch 0)) s)
        s)))
(define (index-of s ch)
  (let loop ([i 0])
    (cond [(>= i (string-length s)) -1]
          [(string=? (substring s i (add1 i)) ch) i]
          [else (loop (add1 i))])))
(define (_repeat s n)
  (cond
    [(string? s) (apply string-append (make-list (int n) s))]
    [(list? s) (apply append (make-list (int n) s))]
    [else '()]))
(define (_parse-int-str s base) (int (string->number s base)))
(define (_sha256 bs) (bytes->list (sha256-bytes (list->bytes bs))))
(define (num r) (numerator r))
(define (denom r) (denominator r))
(define (panic msg) (error msg))

(let* ([_start_mem (current-memory-use)] [_start (now)])
(define apple 0)
(define banana (let ([__l apple] [__r 1]) (if (and (string? __l) (string? __r)) (string-append __l __r) (+ __l __r))))
(define cherry (let ([__l banana] [__r 1]) (if (and (string? __l) (string? __r)) (string-append __l __r) (+ __l __r))))
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
