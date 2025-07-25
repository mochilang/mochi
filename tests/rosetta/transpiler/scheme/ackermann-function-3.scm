;; Generated on 2025-07-25 20:10 +0700
(import (only (scheme base) call/cc when list-ref list-set! list))
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
(let ((start13 (now)
)
)
 (begin (define (pow_big base exp)
 (call/cc (lambda (ret1)
 (let ((result 1)
)
 (begin (let ((b base)
)
 (begin (let ((e exp)
)
 (begin (call/cc (lambda (break3)
 (letrec ((loop2 (lambda ()
 (if (> e 0)
 (begin (if (equal? (modulo e 2)
 1)
 (begin (set! result (* result b)
)
)
 (quote ()
)
)
 (set! b (* b b)
)
 (set! e (let ((v4 (quotient e 2)
)
)
 (cond ((string? v4)
 (inexact->exact (string->number v4)
)
)
 ((boolean? v4)
 (if v4 1 0)
)
 (else (inexact->exact v4)
)
)
)
)
 (loop2)
)
 (quote ()
)
)
)
)
)
 (loop2)
)
)
)
 (ret1 result)
)
)
)
)
)
)
)
)
)
 (define (bit_len x)
 (call/cc (lambda (ret5)
 (let ((n x)
)
 (begin (let ((c 0)
)
 (begin (call/cc (lambda (break7)
 (letrec ((loop6 (lambda ()
 (if (> n 0)
 (begin (set! n (quotient n 2)
)
 (set! c (+ c 1)
)
 (loop6)
)
 (quote ()
)
)
)
)
)
 (loop6)
)
)
)
 (ret5 c)
)
)
)
)
)
)
)
 (define err "")
 (define (ackermann2 m n)
 (call/cc (lambda (ret8)
 (begin (if (not (string=? err "")
)
 (begin (ret8 0)
)
 (quote ()
)
)
 (if (<= m 3)
 (begin (let ((mi (let ((v9 m)
)
 (cond ((string? v9)
 (inexact->exact (string->number v9)
)
)
 ((boolean? v9)
 (if v9 1 0)
)
 (else (inexact->exact v9)
)
)
)
)
)
 (begin (if (equal? mi 0)
 (begin (ret8 (+ n 1)
)
)
 (quote ()
)
)
 (if (equal? mi 1)
 (begin (ret8 (+ n 2)
)
)
 (quote ()
)
)
 (if (equal? mi 2)
 (begin (ret8 (+ (* 2 n)
 3)
)
)
 (quote ()
)
)
 (if (equal? mi 3)
 (begin (let ((nb (bit_len n)
)
)
 (begin (if (> nb 64)
 (begin (set! err (string-append (string-append "A(m,n)
 had n of " (to-str nb)
)
 " bits; too large")
)
 (ret8 0)
)
 (quote ()
)
)
 (let ((r (pow_big 2 (let ((v10 n)
)
 (cond ((string? v10)
 (inexact->exact (string->number v10)
)
)
 ((boolean? v10)
 (if v10 1 0)
)
 (else (inexact->exact v10)
)
)
)
)
)
)
 (begin (ret8 (- (* 8 r)
 3)
)
)
)
)
)
)
 (quote ()
)
)
)
)
)
 (quote ()
)
)
 (if (equal? (bit_len n)
 0)
 (begin (ret8 (ackermann2 (- m 1)
 1)
)
)
 (quote ()
)
)
 (ret8 (ackermann2 (- m 1)
 (ackermann2 m (- n 1)
)
)
)
)
)
)
)
 (define (show m n)
 (call/cc (lambda (ret11)
 (begin (set! err "")
 (let ((res (ackermann2 m n)
)
)
 (begin (if (not (string=? err "")
)
 (begin (display (to-str (string-append (string-append (string-append (string-append (string-append "A(" (to-str m)
)
 ", ")
 (to-str n)
)
 ")
 = Error: ")
 err)
)
)
 (newline)
 (ret11 (quote ()
)
)
)
 (quote ()
)
)
 (if (<= (bit_len res)
 256)
 (begin (display (to-str (string-append (string-append (string-append (string-append (string-append "A(" (to-str m)
)
 ", ")
 (to-str n)
)
 ")
 = ")
 (to-str res)
)
)
)
 (newline)
)
 (begin (let ((s (to-str res)
)
)
 (begin (let ((pre (substring s 0 20)
)
)
 (begin (let ((suf (substring s (- (cond ((string? s)
 (string-length s)
)
 ((hash-table? s)
 (hash-table-size s)
)
 (else (length s)
)
)
 20)
 (cond ((string? s)
 (string-length s)
)
 ((hash-table? s)
 (hash-table-size s)
)
 (else (length s)
)
)
)
)
)
 (begin (display (to-str (string-append (string-append (string-append (string-append (string-append (string-append (string-append (string-append (string-append "A(" (to-str m)
)
 ", ")
 (to-str n)
)
 ")
 = ")
 (to-str (cond ((string? s)
 (string-length s)
)
 ((hash-table? s)
 (hash-table-size s)
)
 (else (length s)
)
)
)
)
 " digits starting/ending with: ")
 pre)
 "...")
 suf)
)
)
 (newline)
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
)
)
 (define (main)
 (call/cc (lambda (ret12)
 (begin (show 0 0)
 (show 1 2)
 (show 2 4)
 (show 3 100)
 (show 3 1000000)
 (show 4 1)
 (show 4 2)
 (show 4 3)
)
)
)
)
 (main)
 (let ((end14 (now)
)
)
 (let ((dur15 (quotient (- end14 start13)
 1000)
)
)
 (begin (display (string-append "{\n  \"duration_us\": " (number->string dur15)
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
