;; Generated on 2025-07-26 23:50 +0700
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
(let ((start7 (now)
)
)
 (begin (define grid (_list (_list "." "." "." "." ".")
 (_list "." "#" "#" "#" ".")
 (_list "." "#" "." "#" ".")
 (_list "." "#" "#" "#" ".")
 (_list "." "." "." "." ".")
)
)
 (define (flood x y repl)
 (call/cc (lambda (ret1)
 (let ((target (cond ((string? (list-ref grid y)
)
 (substring (list-ref grid y)
 x (+ x 1)
)
)
 ((hash-table? (list-ref grid y)
)
 (hash-table-ref (list-ref grid y)
 x)
)
 (else (list-ref (list-ref grid y)
 x)
)
)
)
)
 (begin (if (string=? target repl)
 (begin (ret1 (quote ()
)
)
)
 (quote ()
)
)
 (define (ff px py)
 (call/cc (lambda (ret2)
 (begin (if (or (or (or (< px 0)
 (< py 0)
)
 (>= py (cond ((string? grid)
 (string-length grid)
)
 ((hash-table? grid)
 (hash-table-size grid)
)
 (else (length grid)
)
)
)
)
 (>= px (cond ((string? (list-ref grid 0)
)
 (string-length (list-ref grid 0)
)
)
 ((hash-table? (list-ref grid 0)
)
 (hash-table-size (list-ref grid 0)
)
)
 (else (length (list-ref grid 0)
)
)
)
)
)
 (begin (ret2 (quote ()
)
)
)
 (quote ()
)
)
 (if (not (string=? (cond ((string? (list-ref grid py)
)
 (substring (list-ref grid py)
 px (+ px 1)
)
)
 ((hash-table? (list-ref grid py)
)
 (hash-table-ref (list-ref grid py)
 px)
)
 (else (list-ref (list-ref grid py)
 px)
)
)
 target)
)
 (begin (ret2 (quote ()
)
)
)
 (quote ()
)
)
 (list-set! (list-ref grid py)
 px repl)
 (ff (- px 1)
 py)
 (ff (+ px 1)
 py)
 (ff px (- py 1)
)
 (ff px (+ py 1)
)
)
)
)
)
 (ff x y)
)
)
)
)
)
 (flood 2 2 "o")
 (call/cc (lambda (break4)
 (letrec ((loop3 (lambda (xs)
 (if (null? xs)
 (quote ()
)
 (begin (let ((row (car xs)
)
)
 (begin (let ((line "")
)
 (begin (call/cc (lambda (break6)
 (letrec ((loop5 (lambda (xs)
 (if (null? xs)
 (quote ()
)
 (begin (let ((ch (car xs)
)
)
 (begin (set! line (string-append line ch)
)
)
)
 (loop5 (cdr xs)
)
)
)
)
)
)
 (loop5 row)
)
)
)
 (display (to-str line)
)
 (newline)
)
)
)
)
 (loop3 (cdr xs)
)
)
)
)
)
)
 (loop3 grid)
)
)
)
 (let ((end8 (now)
)
)
 (let ((dur9 (quotient (- end8 start7)
 1000)
)
)
 (begin (display (string-append "{\n  \"duration_us\": " (number->string dur9)
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
