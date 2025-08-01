;; Generated on 2025-07-26 21:43 +0700
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
(let ((start10 (now)
)
)
 (begin (define (intSqrt x)
 (call/cc (lambda (ret1)
 (begin (if (< x 2)
 (begin (ret1 x)
)
 (quote ()
)
)
 (let ((left 1)
)
 (begin (let ((right (quotient x 2)
)
)
 (begin (let ((ans 0)
)
 (begin (call/cc (lambda (break3)
 (letrec ((loop2 (lambda ()
 (if (<= left right)
 (begin (let ((mid (+ left (quotient (- right left)
 2)
)
)
)
 (begin (let ((sq (* mid mid)
)
)
 (begin (if (equal? sq x)
 (begin (ret1 mid)
)
 (quote ()
)
)
 (if (< sq x)
 (begin (set! left (+ mid 1)
)
 (set! ans mid)
)
 (begin (set! right (- mid 1)
)
)
)
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
 (ret1 ans)
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
 (define (sumRecip n)
 (call/cc (lambda (ret4)
 (let ((s 1)
)
 (begin (let ((limit (intSqrt n)
)
)
 (begin (let ((f 2)
)
 (begin (call/cc (lambda (break6)
 (letrec ((loop5 (lambda ()
 (if (_le f limit)
 (begin (if (equal? (modulo n f)
 0)
 (begin (set! s (+ s (quotient n f)
)
)
 (let ((f2 (quotient n f)
)
)
 (begin (if (not (equal? f2 f)
)
 (begin (set! s (+ s f)
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
 (set! f (+ f 1)
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
 (ret4 s)
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
 (call/cc (lambda (ret7)
 (let ((nums (_list 6 28 120 496 672 8128 30240 32760 523776)
)
)
 (begin (call/cc (lambda (break9)
 (letrec ((loop8 (lambda (xs)
 (if (null? xs)
 (quote ()
)
 (begin (let ((n (car xs)
)
)
 (begin (let ((s (sumRecip n)
)
)
 (begin (if (equal? (modulo s n)
 0)
 (begin (let ((val (quotient s n)
)
)
 (begin (let ((perfect "")
)
 (begin (if (equal? val 1)
 (begin (set! perfect "perfect!")
)
 (quote ()
)
)
 (display (to-str (string-append (string-append (string-append (string-append (string-append "Sum of recipr. factors of " (to-str n)
)
 " = ")
 (to-str val)
)
 " exactly ")
 perfect)
)
)
 (newline)
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
)
 (loop8 (cdr xs)
)
)
)
)
)
)
 (loop8 nums)
)
)
)
)
)
)
)
)
 (main)
 (let ((end11 (now)
)
)
 (let ((dur12 (quotient (- end11 start10)
 1000)
)
)
 (begin (display (string-append "{\n  \"duration_us\": " (number->string dur12)
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
