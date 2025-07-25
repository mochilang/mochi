;; Generated on 2025-07-26 21:53 +0700
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
(let ((start6 (now)
)
)
 (begin (define (listStr xs)
 (call/cc (lambda (ret1)
 (let ((s "[")
)
 (begin (let ((i 0)
)
 (begin (call/cc (lambda (break3)
 (letrec ((loop2 (lambda ()
 (if (< i (cond ((string? xs)
 (string-length xs)
)
 ((hash-table? xs)
 (hash-table-size xs)
)
 (else (length xs)
)
)
)
 (begin (set! s (string-append s (to-str (list-ref xs i)
)
)
)
 (if (_lt (+ i 1)
 (cond ((string? xs)
 (string-length xs)
)
 ((hash-table? xs)
 (hash-table-size xs)
)
 (else (length xs)
)
)
)
 (begin (set! s (string-append s " ")
)
)
 (quote ()
)
)
 (set! i (+ i 1)
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
 (set! s (string-append s "]")
)
 (ret1 s)
)
)
)
)
)
)
)
 (define a (_list 0 0 0 0 0)
)
 (display (to-str (string-append "len(a)
 = " (to-str (cond ((string? a)
 (string-length a)
)
 ((hash-table? a)
 (hash-table-size a)
)
 (else (length a)
)
)
)
)
)
)
 (newline)
 (display (to-str (string-append "a = " (listStr a)
)
)
)
 (newline)
 (list-set! a 0 3)
 (display (to-str (string-append "a = " (listStr a)
)
)
)
 (newline)
 (display (to-str (string-append "a[0] = " (to-str (list-ref a 0)
)
)
)
)
 (newline)
 (define s (take (drop a 0)
 (- 4 0)
)
)
 (define cap_s 5)
 (display (to-str (string-append "s = " (listStr s)
)
)
)
 (newline)
 (display (to-str (string-append (string-append (string-append "len(s)
 = " (to-str (cond ((string? s)
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
 "  cap(s)
 = ")
 (to-str cap_s)
)
)
)
 (newline)
 (set! s (take (drop a 0)
 (- 5 0)
)
)
 (display (to-str (string-append "s = " (listStr s)
)
)
)
 (newline)
 (list-set! a 0 22)
 (list-set! s 0 22)
 (display (to-str (string-append "a = " (listStr a)
)
)
)
 (newline)
 (display (to-str (string-append "s = " (listStr s)
)
)
)
 (newline)
 (set! s (append s (_list 4)
)
)
 (set! s (append s (_list 5)
)
)
 (set! s (append s (_list 6)
)
)
 (set! cap_s 10)
 (display (to-str (string-append "s = " (listStr s)
)
)
)
 (newline)
 (display (to-str (string-append (string-append (string-append "len(s)
 = " (to-str (cond ((string? s)
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
 "  cap(s)
 = ")
 (to-str cap_s)
)
)
)
 (newline)
 (list-set! a 4 (- 1)
)
 (display (to-str (string-append "a = " (listStr a)
)
)
)
 (newline)
 (display (to-str (string-append "s = " (listStr s)
)
)
)
 (newline)
 (set! s (_list)
)
 (call/cc (lambda (break5)
 (letrec ((loop4 (lambda (i)
 (if (< i 8)
 (begin (begin (set! s (append s (_list 0)
)
)
)
 (loop4 (+ i 1)
)
)
 (quote ()
)
)
)
)
)
 (loop4 0)
)
)
)
 (set! cap_s 8)
 (display (to-str (string-append "s = " (listStr s)
)
)
)
 (newline)
 (display (to-str (string-append (string-append (string-append "len(s)
 = " (to-str (cond ((string? s)
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
 "  cap(s)
 = ")
 (to-str cap_s)
)
)
)
 (newline)
 (let ((end7 (now)
)
)
 (let ((dur8 (quotient (- end7 start6)
 1000)
)
)
 (begin (display (string-append "{\n  \"duration_us\": " (number->string dur8)
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
