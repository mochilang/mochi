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
(define (sqrtApprox x)
  (let/ec _return (begin
(define guess x)
(define i 0)
(let/ec _break (let loop ()
  (if (< i 20) (let ()
    (set! guess (/ (let ([__l guess] [__r (/ x guess)]) (if (and (string? __l) (string? __r)) (string-append __l __r) (+ __l __r))) 2.0))
    (set! i (let ([__l i] [__r 1]) (if (and (string? __l) (string? __r)) (string-append __l __r) (+ __l __r))))
    (loop)) (void))))
(_return guess)
))
)
(define (cholesky a)
  (let/ec _return (begin
(define n (cond [(string? a) (string-length a)] [(hash? a) (hash-count a)] [else (length a)]))
(define l (list))
(define i 0)
(let/ec _break (let loop ()
  (if (< i n) (let ()
    (define row (list))
    (define j 0)
    (let/ec _break (let loop ()
  (if (< j n) (let ()
    (set! row (append row (list 0.0)))
    (set! j (let ([__l j] [__r 1]) (if (and (string? __l) (string? __r)) (string-append __l __r) (+ __l __r))))
    (loop)) (void))))
    (set! l (append l (list row)))
    (set! i (let ([__l i] [__r 1]) (if (and (string? __l) (string? __r)) (string-append __l __r) (+ __l __r))))
    (loop)) (void))))
(set! i 0)
(let/ec _break (let loop ()
  (if (< i n) (let ()
    (define j 0)
    (let/ec _break (let loop ()
  (if (<= j i) (let ()
    (define sum (if (if a (list-ref a (int i)) #f) (list-ref (if a (list-ref a (int i)) #f) (int j)) #f))
    (define k 0)
    (let/ec _break (let loop ()
  (if (< k j) (let ()
    (set! sum (- sum (* (if (if l (list-ref l (int i)) #f) (list-ref (if l (list-ref l (int i)) #f) (int k)) #f) (if (if l (list-ref l (int j)) #f) (list-ref (if l (list-ref l (int j)) #f) (int k)) #f))))
    (set! k (let ([__l k] [__r 1]) (if (and (string? __l) (string? __r)) (string-append __l __r) (+ __l __r))))
    (loop)) (void))))
    (if (equal? i j) (let ()
(set! l (list-set l i (list-set (list-ref l i) j (sqrtApprox sum))))
) (let ()
(set! l (list-set l i (list-set (list-ref l i) j (/ sum (if (if l (list-ref l (int j)) #f) (list-ref (if l (list-ref l (int j)) #f) (int j)) #f)))))
))
    (set! j (let ([__l j] [__r 1]) (if (and (string? __l) (string? __r)) (string-append __l __r) (+ __l __r))))
    (loop)) (void))))
    (set! i (let ([__l i] [__r 1]) (if (and (string? __l) (string? __r)) (string-append __l __r) (+ __l __r))))
    (loop)) (void))))
(_return l)
))
)
(define (printMat m)
  (let/ec _return (begin
(define i 0)
(let/ec _break (let loop ()
  (if (< i (cond [(string? m) (string-length m)] [(hash? m) (hash-count m)] [else (length m)])) (let ()
    (define line "")
    (define j 0)
    (let/ec _break (let loop ()
  (if (< j (cond [(string? (if m (list-ref m (int i)) #f)) (string-length (if m (list-ref m (int i)) #f))] [(hash? (if m (list-ref m (int i)) #f)) (hash-count (if m (list-ref m (int i)) #f))] [else (length (if m (list-ref m (int i)) #f))])) (let ()
    (set! line (string-append line (format "~a" (if (if m (list-ref m (int i)) #f) (list-ref (if m (list-ref m (int i)) #f) (int j)) #f))))
    (if (< j (- (cond [(string? (if m (list-ref m (int i)) #f)) (string-length (if m (list-ref m (int i)) #f))] [(hash? (if m (list-ref m (int i)) #f)) (hash-count (if m (list-ref m (int i)) #f))] [else (length (if m (list-ref m (int i)) #f))]) 1)) (let ()
(set! line (string-append line " "))
) (void))
    (set! j (let ([__l j] [__r 1]) (if (and (string? __l) (string? __r)) (string-append __l __r) (+ __l __r))))
    (loop)) (void))))
    (displayln line)
    (set! i (let ([__l i] [__r 1]) (if (and (string? __l) (string? __r)) (string-append __l __r) (+ __l __r))))
    (loop)) (void))))
))
)
(define (demo a)
  (let/ec _return (begin
(displayln "A:")
(printMat a)
(define l (cholesky a))
(displayln "L:")
(printMat l)
))
)
(demo (list (list 25.0 15.0 (- 5.0)) (list 15.0 18.0 0.0) (list (- 5.0) 0.0 11.0)))
(demo (list (list 18.0 22.0 54.0 42.0) (list 22.0 70.0 86.0 62.0) (list 54.0 86.0 174.0 134.0) (list 42.0 62.0 134.0 106.0)))
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
