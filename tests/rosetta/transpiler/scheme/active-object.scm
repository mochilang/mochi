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
(let ((start8 (now)
)
)
 (begin (define PI 3.141592653589793)
 (define (sinApprox x)
 (call/cc (lambda (ret1)
 (let ((term x)
)
 (begin (let ((sum x)
)
 (begin (let ((n 1)
)
 (begin (call/cc (lambda (break3)
 (letrec ((loop2 (lambda ()
 (if (<= n 12)
 (begin (let ((denom (* (* 2 n)
 (+ (* 2 n)
 1)
)
)
)
 (begin (set! term (/ (* (* (- term)
 x)
 x)
 denom)
)
 (set! sum (+ sum term)
)
 (set! n (+ n 1)
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
 (ret1 sum)
)
)
)
)
)
)
)
)
)
 (define dt 0.01)
 (define s 0.0)
 (define t1 0.0)
 (define k1 (sinApprox 0.0)
)
 (define i 1)
 (call/cc (lambda (break5)
 (letrec ((loop4 (lambda ()
 (if (<= i 200)
 (begin (let ((t2 (* i dt)
)
)
 (begin (let ((k2 (sinApprox (* t2 PI)
)
)
)
 (begin (set! s (+ s (* (* (+ k1 k2)
 0.5)
 (- t2 t1)
)
)
)
 (set! t1 t2)
 (set! k1 k2)
 (set! i (+ i 1)
)
)
)
)
)
 (loop4)
)
 (quote ()
)
)
)
)
)
 (loop4)
)
)
)
 (define i2 1)
 (call/cc (lambda (break7)
 (letrec ((loop6 (lambda ()
 (if (<= i2 50)
 (begin (let ((t2 (+ 2.0 (* i2 dt)
)
)
)
 (begin (let ((k2 0.0)
)
 (begin (set! s (+ s (* (* (+ k1 k2)
 0.5)
 (- t2 t1)
)
)
)
 (set! t1 t2)
 (set! k1 k2)
 (set! i2 (+ i2 1)
)
)
)
)
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
 (display (to-str s)
)
 (newline)
 (let ((end9 (now)
)
)
 (let ((dur10 (quotient (- end9 start8)
 1000)
)
)
 (begin (display (string-append "{\n  \"duration_us\": " (number->string dur10)
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
