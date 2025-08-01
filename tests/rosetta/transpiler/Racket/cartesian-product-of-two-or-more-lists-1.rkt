;; Generated by Mochi 0.10.42 on 2025-07-27 23:50 +0700
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

(define (pad-start s width ch)
  (let ([s (format "~a" s)])
    (if (< (string-length s) width)
        (string-append (make-string (- width (string-length s)) (string-ref ch 0)) s)
        s)))
(define (_repeat s n)
  (cond
    [(string? s) (apply string-append (make-list (int n) s))]
    [(list? s) (apply append (make-list (int n) s))]
    [else '()]))
(define (_parse-int-str s base) (int (string->number s base)))
(define (_sha256 bs) (bytes->list (sha256-bytes (list->bytes bs))))
(define (num r) (numerator r))
(define (denom r) (denominator r))

(let* ([_start_mem (current-memory-use)] [_start (now)])
(define (cart2 a b)
  (let/ec _return (begin
(define p (list))
(let/ec _break (for ([x a])
  (let/ec _cont
(let/ec _break (for ([y b])
  (let/ec _cont
(set! p (append p (list (list x y))))
  )))
  )))
(_return p)
))
)
(define (llStr lst)
  (let/ec _return (begin
(define s "[")
(define i 0)
(let/ec _break (let loop ()
  (if (< i (cond [(string? lst) (string-length lst)] [(hash? lst) (hash-count lst)] [else (length lst)])) (let ()
    (define row (if lst (list-ref lst (int i)) #f))
    (set! s (string-append s "["))
    (define j 0)
    (let/ec _break (let loop ()
  (if (< j (cond [(string? row) (string-length row)] [(hash? row) (hash-count row)] [else (length row)])) (let ()
    (set! s (string-append s (format "~a" (if row (list-ref row (int j)) #f))))
    (if (< j (- (cond [(string? row) (string-length row)] [(hash? row) (hash-count row)] [else (length row)]) 1)) (let ()
(set! s (string-append s " "))
) (void))
    (set! j (let ([__l j] [__r 1]) (if (and (string? __l) (string? __r)) (string-append __l __r) (+ __l __r))))
    (loop)) (void))))
    (set! s (string-append s "]"))
    (if (< i (- (cond [(string? lst) (string-length lst)] [(hash? lst) (hash-count lst)] [else (length lst)]) 1)) (let ()
(set! s (string-append s " "))
) (void))
    (set! i (let ([__l i] [__r 1]) (if (and (string? __l) (string? __r)) (string-append __l __r) (+ __l __r))))
    (loop)) (void))))
(set! s (string-append s "]"))
(_return s)
))
)
(define (main)
  (let/ec _return (begin
(displayln (llStr (cart2 (list 1 2) (list 3 4))))
(displayln (llStr (cart2 (list 3 4) (list 1 2))))
(displayln (llStr (cart2 (list 1 2) (list))))
(displayln (llStr (cart2 (list) (list 1 2))))
))
)
(main)
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
