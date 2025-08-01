;; Generated by Mochi 0.10.47 on 2025-07-28 11:49 +0700
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
(define (egcd a b)
  (let/ec _return (begin
(if (equal? a 0) (let ()
(_return (list b 0 1))
) (void))
(define res (egcd (modulo b a) a))
(define g (if res (list-ref res (int 0)) #f))
(define x1 (if res (list-ref res (int 1)) #f))
(define y1 (if res (list-ref res (int 2)) #f))
(_return (list g (- y1 (* (quotient b a) x1)) x1))
))
)
(define (modInv a m)
  (let/ec _return (begin
(define r (egcd a m))
(if (not (equal? (if r (list-ref r (int 0)) #f) 1)) (let ()
(_return 0)
) (void))
(define x (if r (list-ref r (int 1)) #f))
(if (< x 0) (let ()
(_return (let ([__l x] [__r m]) (if (and (string? __l) (string? __r)) (string-append __l __r) (+ __l __r))))
) (void))
(_return x)
))
)
(define (crt a n)
  (let/ec _return (begin
(define prod 1)
(define i 0)
(let/ec _break (let loop ()
  (if (< i (cond [(string? n) (string-length n)] [(hash? n) (hash-count n)] [else (length n)])) (let ()
    (set! prod (* prod (if n (list-ref n (int i)) #f)))
    (set! i (let ([__l i] [__r 1]) (if (and (string? __l) (string? __r)) (string-append __l __r) (+ __l __r))))
    (loop)) (void))))
(define x 0)
(set! i 0)
(let/ec _break (let loop ()
  (if (< i (cond [(string? n) (string-length n)] [(hash? n) (hash-count n)] [else (length n)])) (let ()
    (define ni (if n (list-ref n (int i)) #f))
    (define ai (if a (list-ref a (int i)) #f))
    (define p (quotient prod ni))
    (define inv (modInv (modulo p ni) ni))
    (set! x (let ([__l x] [__r (* (* ai inv) p)]) (if (and (string? __l) (string? __r)) (string-append __l __r) (+ __l __r))))
    (set! i (let ([__l i] [__r 1]) (if (and (string? __l) (string? __r)) (string-append __l __r) (+ __l __r))))
    (loop)) (void))))
(_return (modulo x prod))
))
)
(define n (list 3 5 7))
(define a (list 2 3 2))
(define res (crt a n))
(displayln (string-append (format "~a" res) " <nil>"))
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
