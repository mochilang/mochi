;; Generated on 2025-07-28 10:03 +0700
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
(let ((start11 (now)
)
)
 (begin (define (pow2 exp)
 (call/cc (lambda (ret1)
 (let ((r 1)
)
 (begin (let ((i 0)
)
 (begin (call/cc (lambda (break3)
 (letrec ((loop2 (lambda ()
 (if (< i exp)
 (begin (set! r (* r 2)
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
 (ret1 r)
)
)
)
)
)
)
)
 (define (bin n digits)
 (call/cc (lambda (ret4)
 (let ((s "")
)
 (begin (let ((i (- digits 1)
)
)
 (begin (call/cc (lambda (break6)
 (letrec ((loop5 (lambda ()
 (if (>= i 0)
 (begin (let ((p (pow2 i)
)
)
 (begin (if (_ge n p)
 (begin (set! s (string-append s "x")
)
 (set! n (- n p)
)
)
 (begin (set! s (string-append s " ")
)
)
)
 (if (> i 0)
 (begin (set! s (string-append s "|")
)
)
 (quote ()
)
)
 (set! i (- i 1)
)
)
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
 (define t (quotient (now)
 1000000000)
)
 (define sec (modulo t 60)
)
 (define mins (quotient t 60)
)
 (define min (modulo mins 60)
)
 (define hour (modulo (quotient mins 60)
 24)
)
 (_display (to-str (bin hour 8)
)
)
 (newline)
 (_display (to-str "")
)
 (newline)
 (_display (to-str (bin min 8)
)
)
 (newline)
 (_display (to-str "")
)
 (newline)
 (define xs "")
 (define i 0)
 (call/cc (lambda (break8)
 (letrec ((loop7 (lambda ()
 (if (< i sec)
 (begin (set! xs (string-append xs "x")
)
 (set! i (+ i 1)
)
 (loop7)
)
 (quote ()
)
)
)
)
)
 (loop7)
)
)
)
 (define out "")
 (define j 0)
 (call/cc (lambda (break10)
 (letrec ((loop9 (lambda ()
 (if (< j (cond ((string? xs)
 (string-length xs)
)
 ((hash-table? xs)
 (hash-table-size xs)
)
 (else (length xs)
)
)
)
 (begin (set! out (string-append out (substring xs j (+ j 1)
)
)
)
 (if (and (equal? (modulo (+ j 1)
 5)
 0)
 (_lt (+ j 1)
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
)
 (begin (set! out (string-append out "|")
)
)
 (quote ()
)
)
 (set! j (+ j 1)
)
 (loop9)
)
 (quote ()
)
)
)
)
)
 (loop9)
)
)
)
 (_display (to-str out)
)
 (newline)
 (let ((end12 (now)
)
)
 (let ((dur13 (quotient (- end12 start11)
 1000)
)
)
 (begin (_display (string-append "{\n  \"duration_us\": " (number->string dur13)
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
