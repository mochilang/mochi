;; Generated by Mochi 0.10.50 on 2025-07-30 21:05 +0700
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
(define (bernoulli n)
  (let/ec _return (begin
(define a (list))
(define m 0)
(let/ec _break (let loop ()
  (if (<= m n) (let ()
    (set! a (append a (list (/ 1 (let ([__l m] [__r 1]) (if (and (string? __l) (string? __r)) (string-append __l __r) (+ __l __r)))))))
    (define j m)
    (let/ec _break (let loop ()
  (if (>= j 1) (let ()
    (set! a (list-set a (int (- j 1)) (* j (- (if a (list-ref a (int (- j 1))) #f) (if a (list-ref a (int j)) #f)))))
    (set! j (- j 1))
    (loop)) (void))))
    (set! m (let ([__l m] [__r 1]) (if (and (string? __l) (string? __r)) (string-append __l __r) (+ __l __r))))
    (loop)) (void))))
(if (not (equal? n 1)) (let ()
(_return (if a (list-ref a (int 0)) #f))
) (void))
(_return (- (if a (list-ref a (int 0)) #f)))
))
)
(define (binom n k)
  (let/ec _return (begin
(if (or (< k 0) (> k n)) (let ()
(_return 0)
) (void))
(define kk k)
(if (> kk (- n kk)) (let ()
(set! kk (- n kk))
) (void))
(define res 1)
(define i 0)
(let/ec _break (let loop ()
  (if (< i kk) (let ()
    (set! res (* res (- n i)))
    (set! i (let ([__l i] [__r 1]) (if (and (string? __l) (string? __r)) (string-append __l __r) (+ __l __r))))
    (set! res (quotient res i))
    (loop)) (void))))
(_return res)
))
)
(define (faulhaberRow p)
  (let/ec _return (begin
(define coeffs (list))
(define i 0)
(let/ec _break (let loop ()
  (if (<= i p) (let ()
    (set! coeffs (append coeffs (list 0)))
    (set! i (let ([__l i] [__r 1]) (if (and (string? __l) (string? __r)) (string-append __l __r) (+ __l __r))))
    (loop)) (void))))
(define j 0)
(define sign (- 1))
(let/ec _break (let loop ()
  (if (<= j p) (let ()
    (set! sign (- sign))
    (define c (/ 1 (let ([__l p] [__r 1]) (if (and (string? __l) (string? __r)) (string-append __l __r) (+ __l __r)))))
    (if (< sign 0) (let ()
(set! c (- c))
) (void))
    (set! c (* c (binom (let ([__l p] [__r 1]) (if (and (string? __l) (string? __r)) (string-append __l __r) (+ __l __r))) j)))
    (set! c (* c (bernoulli j)))
    (set! coeffs (list-set coeffs (int (- p j)) c))
    (set! j (let ([__l j] [__r 1]) (if (and (string? __l) (string? __r)) (string-append __l __r) (+ __l __r))))
    (loop)) (void))))
(_return coeffs)
))
)
(define (ratStr r)
  (let/ec _return (begin
(define s (format "~a" r))
(if (endsWith s "/1") (let ()
(_return (substring s 0 (- (cond [(string? s) (string-length s)] [(hash? s) (hash-count s)] [else (length s)]) 2)))
) (void))
(_return s)
))
)
(define (endsWith s suf)
  (let/ec _return (begin
(if (< (cond [(string? s) (string-length s)] [(hash? s) (hash-count s)] [else (length s)]) (cond [(string? suf) (string-length suf)] [(hash? suf) (hash-count suf)] [else (length suf)])) (let ()
(_return #f)
) (void))
(_return (string=? (slice s (- (cond [(string? s) (string-length s)] [(hash? s) (hash-count s)] [else (length s)]) (cond [(string? suf) (string-length suf)] [(hash? suf) (hash-count suf)] [else (length suf)])) (cond [(string? s) (string-length s)] [(hash? s) (hash-count s)] [else (length s)])) suf))
))
)
(define (main)
  (let/ec _return (begin
(define p 0)
(let/ec _break (let loop ()
  (if (< p 10) (let ()
    (define row (faulhaberRow p))
    (define line "")
    (define idx 0)
    (let/ec _break (let loop ()
  (if (< idx (cond [(string? row) (string-length row)] [(hash? row) (hash-count row)] [else (length row)])) (let ()
    (set! line (string-append line (pad-start (ratStr (if row (list-ref row (int idx)) #f)) 5 " ")))
    (if (< idx (- (cond [(string? row) (string-length row)] [(hash? row) (hash-count row)] [else (length row)]) 1)) (let ()
(set! line (string-append line "  "))
) (void))
    (set! idx (let ([__l idx] [__r 1]) (if (and (string? __l) (string? __r)) (string-append __l __r) (+ __l __r))))
    (loop)) (void))))
    (displayln line)
    (set! p (let ([__l p] [__r 1]) (if (and (string? __l) (string? __r)) (string-append __l __r) (+ __l __r))))
    (loop)) (void))))
(displayln "")
(define k 17)
(define coeffs (faulhaberRow k))
(define nn 1000)
(define np 1)
(define sum 0)
(define i 0)
(let/ec _break (let loop ()
  (if (< i (cond [(string? coeffs) (string-length coeffs)] [(hash? coeffs) (hash-count coeffs)] [else (length coeffs)])) (let ()
    (set! np (* np nn))
    (set! sum (let ([__l sum] [__r (* (if coeffs (list-ref coeffs (int i)) #f) np)]) (if (and (string? __l) (string? __r)) (string-append __l __r) (+ __l __r))))
    (set! i (let ([__l i] [__r 1]) (if (and (string? __l) (string? __r)) (string-append __l __r) (+ __l __r))))
    (loop)) (void))))
(displayln (ratStr sum))
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
