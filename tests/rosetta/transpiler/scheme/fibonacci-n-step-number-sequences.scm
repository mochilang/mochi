;; Generated on 2025-07-30 21:05 +0700
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
(define (_split s sep)
  (let* ((str (if (string? s) s (list->string s)))
         (del (cond ((char? sep) sep)
                     ((string? sep) (if (= (string-length sep) 1)
                                       (string-ref sep 0)
                                       sep))
                     (else sep))))
    (cond
     ((and (string? del) (string=? del ""))
      (map string (string->list str)))
     ((char? del)
      (string-split str del))
     (else
        (let loop ((r str) (acc '()))
          (let ((idx (string-contains r del)))
            (if idx
                (loop (substring r (+ idx (string-length del)))
                      (cons (substring r 0 idx) acc))
                (reverse (cons r acc)))))))))
(let ((start10 (now)
)
)
 (begin (define (show xs)
 (call/cc (lambda (ret1)
 (let ((s "")
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
 (if (_lt i (- (cond ((string? xs)
 (string-length xs)
)
 ((hash-table? xs)
 (hash-table-size xs)
)
 (else (length xs)
)
)
 1)
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
 (ret1 s)
)
)
)
)
)
)
)
 (define (gen init n)
 (call/cc (lambda (ret4)
 (let ((b init)
)
 (begin (let ((res (_list)
)
)
 (begin (let ((sum 0)
)
 (begin (call/cc (lambda (break6)
 (letrec ((loop5 (lambda (xs)
 (if (null? xs)
 (quote ()
)
 (begin (let ((x (car xs)
)
)
 (begin (set! res (append res (_list x)
)
)
 (set! sum (_add sum x)
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
 (loop5 b)
)
)
)
 (call/cc (lambda (break8)
 (letrec ((loop7 (lambda ()
 (if (< (cond ((string? res)
 (string-length res)
)
 ((hash-table? res)
 (hash-table-size res)
)
 (else (length res)
)
)
 n)
 (begin (let ((next sum)
)
 (begin (set! res (append res (_list next)
)
)
 (set! sum (- (+ sum next)
 (list-ref b 0)
)
)
 (set! b (append (slice b 1 (cond ((string? b)
 (string-length b)
)
 ((hash-table? b)
 (hash-table-size b)
)
 (else (length b)
)
)
)
 (_list next)
)
)
)
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
 (ret4 res)
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
 (call/cc (lambda (ret9)
 (let ((n 10)
)
 (begin (_display (to-str (string-append " Fibonacci: " (show (gen (_list 1 1)
 n)
)
)
)
)
 (newline)
 (_display (to-str (string-append "Tribonacci: " (show (gen (_list 1 1 2)
 n)
)
)
)
)
 (newline)
 (_display (to-str (string-append "Tetranacci: " (show (gen (_list 1 1 2 4)
 n)
)
)
)
)
 (newline)
 (_display (to-str (string-append "     Lucas: " (show (gen (_list 2 1)
 n)
)
)
)
)
 (newline)
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
 (begin (_display (string-append "{\n  \"duration_us\": " (number->string dur12)
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
