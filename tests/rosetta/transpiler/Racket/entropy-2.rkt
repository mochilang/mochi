;; Generated by Mochi 0.10.42 on 2025-07-28 10:03 +0700
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
(define (panic msg) (error msg))

(let* ([_start_mem (current-memory-use)] [_start (now)])
(define (log2 x)
  (let/ec _return (begin
(define k 0.0)
(define v x)
(let/ec _break (let loop ()
  (if (>= v 2.0) (let ()
    (set! v (/ v 2.0))
    (set! k (let ([__l k] [__r 1.0]) (if (and (string? __l) (string? __r)) (string-append __l __r) (+ __l __r))))
    (loop)) (void))))
(let/ec _break (let loop ()
  (if (< v 1.0) (let ()
    (set! v (* v 2.0))
    (set! k (- k 1.0))
    (loop)) (void))))
(define z (/ (- v 1.0) (let ([__l v] [__r 1.0]) (if (and (string? __l) (string? __r)) (string-append __l __r) (+ __l __r)))))
(define zpow z)
(define sum z)
(define i 3)
(let/ec _break (let loop ()
  (if (<= i 9) (let ()
    (set! zpow (* (* zpow z) z))
    (set! sum (let ([__l sum] [__r (/ zpow (exact->inexact i))]) (if (and (string? __l) (string? __r)) (string-append __l __r) (+ __l __r))))
    (set! i (let ([__l i] [__r 2]) (if (and (string? __l) (string? __r)) (string-append __l __r) (+ __l __r))))
    (loop)) (void))))
(define ln2 0.6931471805599453)
(_return (let ([__l k] [__r (/ (* 2.0 sum) ln2)]) (if (and (string? __l) (string? __r)) (string-append __l __r) (+ __l __r))))
))
)
(define (main)
  (let/ec _return (begin
(define s "1223334444")
(define counts (hash))
(define l 0.0)
(define i 0)
(let/ec _break (let loop ()
  (if (< i (cond [(string? s) (string-length s)] [(hash? s) (hash-count s)] [else (length s)])) (let ()
    (define ch (substring s i (let ([__l i] [__r 1]) (if (and (string? __l) (string? __r)) (string-append __l __r) (+ __l __r)))))
    (if (hash-has-key? counts ch) (let ()
(set! counts (hash-set (or counts (hash)) ch (let ([__l (if counts (hash-ref counts ch #f) #f)] [__r 1]) (if (and (string? __l) (string? __r)) (string-append __l __r) (+ __l __r)))))
) (let ()
(set! counts (hash-set (or counts (hash)) ch 1))
))
    (set! l (let ([__l l] [__r 1.0]) (if (and (string? __l) (string? __r)) (string-append __l __r) (+ __l __r))))
    (set! i (let ([__l i] [__r 1]) (if (and (string? __l) (string? __r)) (string-append __l __r) (+ __l __r))))
    (loop)) (void))))
(define hm 0.0)
(let/ec _break (for ([ch (in-hash-keys counts)])
  (let/ec _cont
(define c (exact->inexact (if counts (hash-ref counts ch #f) #f)))
(set! hm (let ([__l hm] [__r (* c (log2 c))]) (if (and (string? __l) (string? __r)) (string-append __l __r) (+ __l __r))))
  )))
(displayln (format "~a" (- (log2 l) (/ hm l))))
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
