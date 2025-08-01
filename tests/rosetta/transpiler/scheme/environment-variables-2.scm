;; Generated on 2025-07-28 10:35 +0700
(import (only (scheme base) call/cc when list-ref list-set!))
(import (rename (scheme base) (list _list)))
(import (scheme time))
(import (chibi string))
(import (only (scheme char) string-upcase string-downcase))
(import (scheme write))
(import (srfi 69))
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
      (exact (floor (* (current-second) 1000000000))))
)
(define (_mem) (* 1024 (resource-usage-max-rss (get-resource-usage resource-usage/self))))
(import (srfi 98))
(define (_environ)
  (map (lambda (p) (string-append (car p) "=" (cdr p)))
       (get-environment-variables)))
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
(define (_add a b)
  (cond ((and (number? a) (number? b)) (+ a b))
        ((string? a) (string-append a (to-str b)))
        ((string? b) (string-append (to-str a) b))
        ((and (list? a) (list? b)) (append a b))
        (else (+ a b))))
(define (indexOf s sub) (let ((cur (string-contains s sub)))   (if cur (string-cursor->index s cur) -1)))
(define (_display . args) (apply display args))
(define (padStart s width pad)
  (let loop ((out s))
    (if (< (string-length out) width)
        (loop (string-append pad out))
        out)))
(define (_repeat s n)
  (let loop ((i 0) (out ""))
    (if (< i n)
        (loop (+ i 1) (string-append out s))
        out)))
(define (_parseIntStr s base)
  (let* ((b (if (number? base) base 10))
         (n (string->number (if (list? s) (list->string s) s) b)))
    (if n (inexact->exact (truncate n)) 0)))
(let ((start4 (now)
)
)
 (begin (define (hasPrefix s p)
 (call/cc (lambda (ret1)
 (begin (if (> (cond ((string? p)
 (string-length p)
)
 ((hash-table? p)
 (hash-table-size p)
)
 (else (length p)
)
)
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
 (begin (ret1 #f)
)
 (quote ()
)
)
 (ret1 (string=? (substring s 0 (cond ((string? p)
 (string-length p)
)
 ((hash-table? p)
 (hash-table-size p)
)
 (else (length p)
)
)
)
 p)
)
)
)
)
)
 (define name "SHELL")
 (define prefix (string-append name "=")
)
 (call/cc (lambda (break3)
 (letrec ((loop2 (lambda (xs)
 (if (null? xs)
 (quote ()
)
 (begin (let ((v (car xs)
)
)
 (begin (if (hasPrefix v prefix)
 (begin (_display (to-str (string-append (string-append name " has value ")
 (substring v (cond ((string? prefix)
 (string-length prefix)
)
 ((hash-table? prefix)
 (hash-table-size prefix)
)
 (else (length prefix)
)
)
 (cond ((string? v)
 (string-length v)
)
 ((hash-table? v)
 (hash-table-size v)
)
 (else (length v)
)
)
)
)
)
)
 (newline)
 (quote ()
)
)
 (quote ()
)
)
)
)
 (loop2 (cdr xs)
)
)
)
)
)
)
 (loop2 (_environ)
)
)
)
)
 (_display (to-str (string-append name " not found")
)
)
 (newline)
 (let ((end5 (now)
)
)
 (let ((dur6 (quotient (- end5 start4)
 1000)
)
)
 (begin (_display (string-append "{\n  \"duration_us\": " (number->string dur6)
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
