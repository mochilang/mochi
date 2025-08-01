;; Generated on 2025-07-26 21:56 +0700
(import (only (scheme base) call/cc when list-ref list-set!))
(import (rename (scheme base) (list _list)))
(import (scheme time))
(import (chibi string))
(import (only (scheme char) string-upcase string-downcase))
(import (chibi time) (srfi 98))
(define _now_seeded #f)
(define _now_seed 0)
(define (now)
  (when (not _now_seeded)
    (let ((s (get-environment-variable "MOCHI_NOW_SEED")))
      (when (and s (string->number s))
        (set! _now_seed (string->number s))
        (set! _now_seeded #t))))
  (if _now_seeded
      (begin
        (set! _now_seed (modulo (+ (* _now_seed 1664525) 1013904223) 2147483647))
        _now_seed)
      (inexact->exact (* (current-second) 1000000000))))(import (chibi time))
(define (_mem) (* 1024 (resource-usage-max-rss (get-resource-usage resource-usage/self))))
(import (chibi json))
(define (to-str x)
  (cond ((pair? x)
         (string-append "[" (string-join (map to-str x) ", ") "]"))
        ((hash-table? x)
         (let* ((ks (hash-table-keys x))
                (pairs (map (lambda (k)
                              (string-append (to-str k) ":" (to-str (hash-table-ref x k))))
                            ks)))
           (string-append "{" (string-join pairs ", ") "}")))
        ((null? x) "")
        ((string? x) x)
        ((boolean? x) (if x "1" "0"))
        (else (number->string x))))
(define (upper s) (string-upcase s))
(define (lower s) (string-downcase s))
(define (fmod a b) (- a (* (floor (/ a b)) b)))
(define (_gt a b) (cond ((and (number? a) (number? b)) (> a b)) ((and (string? a) (string? b)) (string>? a b)) (else (> a b))))
(define (_lt a b) (cond ((and (number? a) (number? b)) (< a b)) ((and (string? a) (string? b)) (string<? a b)) (else (< a b))))
(define (_ge a b) (cond ((and (number? a) (number? b)) (>= a b)) ((and (string? a) (string? b)) (string>=? a b)) (else (>= a b))))
(define (_le a b) (cond ((and (number? a) (number? b)) (<= a b)) ((and (string? a) (string? b)) (string<=? a b)) (else (<= a b))))
(let ((start29 (now)
)
)
 (begin (define (absf x)
 (call/cc (lambda (ret1)
 (begin (if (< x 0.0)
 (begin (ret1 (- x)
)
)
 (quote ()
)
)
 (ret1 x)
)
)
)
)
 (define (floorf x)
 (call/cc (lambda (ret2)
 (let ((y (let ((v3 x)
)
 (cond ((string? v3)
 (inexact->exact (string->number v3)
)
)
 ((boolean? v3)
 (if v3 1 0)
)
 (else (inexact->exact v3)
)
)
)
)
)
 (begin (ret2 y)
)
)
)
)
)
 (define (indexOf s ch)
 (call/cc (lambda (ret4)
 (let ((i 0)
)
 (begin (call/cc (lambda (break6)
 (letrec ((loop5 (lambda ()
 (if (< i (cond ((string? s)
 (string-length s)
)
 ((hash-table? s)
 (hash-table-size s)
)
 (else (length s)
)
)
)
 (begin (if (string=? (substring s i (+ i 1)
)
 ch)
 (begin (ret4 i)
)
 (quote ()
)
)
 (set! i (+ i 1)
)
 (loop5)
)
 (quote ()
)
)
)
)
)
 (loop5)
)
)
)
 (ret4 (- 1)
)
)
)
)
)
)
 (define (fmtF x)
 (call/cc (lambda (ret7)
 (let ((y (/ (floorf (+ (* x 10000.0)
 0.5)
)
 10000.0)
)
)
 (begin (let ((s (to-str y)
)
)
 (begin (let ((dot (indexOf s ".")
)
)
 (begin (if (equal? dot (- 0 1)
)
 (begin (set! s (string-append s ".0000")
)
)
 (begin (let ((decs (- (- (cond ((string? s)
 (string-length s)
)
 ((hash-table? s)
 (hash-table-size s)
)
 (else (length s)
)
)
 dot)
 1)
)
)
 (begin (if (_gt decs 4)
 (begin (set! s (substring s 0 (+ dot 5)
)
)
)
 (begin (call/cc (lambda (break9)
 (letrec ((loop8 (lambda ()
 (if (_lt decs 4)
 (begin (set! s (string-append s "0")
)
 (set! decs (+ decs 1)
)
 (loop8)
)
 (quote ()
)
)
)
)
)
 (loop8)
)
)
)
)
)
)
)
)
)
 (ret7 s)
)
)
)
)
)
)
)
)
)
 (define (padInt n width)
 (call/cc (lambda (ret10)
 (let ((s (to-str n)
)
)
 (begin (call/cc (lambda (break12)
 (letrec ((loop11 (lambda ()
 (if (< (cond ((string? s)
 (string-length s)
)
 ((hash-table? s)
 (hash-table-size s)
)
 (else (length s)
)
)
 width)
 (begin (set! s (string-append " " s)
)
 (loop11)
)
 (quote ()
)
)
)
)
)
 (loop11)
)
)
)
 (ret10 s)
)
)
)
)
)
 (define (padFloat x width)
 (call/cc (lambda (ret13)
 (let ((s (fmtF x)
)
)
 (begin (call/cc (lambda (break15)
 (letrec ((loop14 (lambda ()
 (if (< (cond ((string? s)
 (string-length s)
)
 ((hash-table? s)
 (hash-table-size s)
)
 (else (length s)
)
)
 width)
 (begin (set! s (string-append " " s)
)
 (loop14)
)
 (quote ()
)
)
)
)
)
 (loop14)
)
)
)
 (ret13 s)
)
)
)
)
)
 (define (avgLen n)
 (call/cc (lambda (ret16)
 (let ((tests 10000)
)
 (begin (let ((sum 0)
)
 (begin (let ((seed 1)
)
 (begin (let ((t 0)
)
 (begin (call/cc (lambda (break18)
 (letrec ((loop17 (lambda ()
 (if (< t tests)
 (begin (let ((visited (_list)
)
)
 (begin (let ((i 0)
)
 (begin (call/cc (lambda (break20)
 (letrec ((loop19 (lambda ()
 (if (< i n)
 (begin (set! visited (append visited (_list #f)
)
)
 (set! i (+ i 1)
)
 (loop19)
)
 (quote ()
)
)
)
)
)
 (loop19)
)
)
)
 (let ((x 0)
)
 (begin (call/cc (lambda (break22)
 (letrec ((loop21 (lambda ()
 (if (not (list-ref visited x)
)
 (begin (list-set! visited x #t)
 (set! sum (+ sum 1)
)
 (set! seed (modulo (+ (* seed 1664525)
 1013904223)
 2147483647)
)
 (set! x (modulo seed n)
)
 (loop21)
)
 (quote ()
)
)
)
)
)
 (loop21)
)
)
)
 (set! t (+ t 1)
)
)
)
)
)
)
)
 (loop17)
)
 (quote ()
)
)
)
)
)
 (loop17)
)
)
)
 (ret16 (/ sum tests)
)
)
)
)
)
)
)
)
)
)
)
)
 (define (ana n)
 (call/cc (lambda (ret23)
 (let ((nn n)
)
 (begin (let ((term 1.0)
)
 (begin (let ((sum 1.0)
)
 (begin (let ((i (- nn 1.0)
)
)
 (begin (call/cc (lambda (break25)
 (letrec ((loop24 (lambda ()
 (if (>= i 1.0)
 (begin (set! term (* term (/ i nn)
)
)
 (set! sum (+ sum term)
)
 (set! i (- i 1.0)
)
 (loop24)
)
 (quote ()
)
)
)
)
)
 (loop24)
)
)
)
 (ret23 sum)
)
)
)
)
)
)
)
)
)
)
)
 (define (main)
 (call/cc (lambda (ret26)
 (let ((nmax 20)
)
 (begin (display (to-str " N    average    analytical    (error)
")
)
 (newline)
 (display (to-str "===  =========  ============  =========")
)
 (newline)
 (let ((n 1)
)
 (begin (call/cc (lambda (break28)
 (letrec ((loop27 (lambda ()
 (if (<= n nmax)
 (begin (let ((a (avgLen n)
)
)
 (begin (let ((b (ana n)
)
)
 (begin (let ((err (* (/ (absf (- a b)
)
 b)
 100.0)
)
)
 (begin (let ((line (string-append (string-append (string-append (string-append (string-append (string-append (string-append (padInt n 3)
 "  ")
 (padFloat a 9)
)
 "  ")
 (padFloat b 12)
)
 "  (")
 (padFloat err 6)
)
 "%)
")
)
)
 (begin (display (to-str line)
)
 (newline)
 (set! n (+ n 1)
)
)
)
)
)
)
)
)
)
 (loop27)
)
 (quote ()
)
)
)
)
)
 (loop27)
)
)
)
)
)
)
)
)
)
)
 (main)
 (let ((end30 (now)
)
)
 (let ((dur31 (quotient (- end30 start29)
 1000)
)
)
 (begin (display (string-append "{\n  \"duration_us\": " (number->string dur31)
 ",\n  \"memory_bytes\": " (number->string (_mem)
)
 ",\n  \"name\": \"main\"\n}")
)
 (newline)
)
)
)
)
)
