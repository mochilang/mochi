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
(let ((start16 (now)
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
 (define (ordered xs)
 (call/cc (lambda (ret4)
 (begin (if (equal? (cond ((string? xs)
 (string-length xs)
)
 ((hash-table? xs)
 (hash-table-size xs)
)
 (else (length xs)
)
)
 0)
 (begin (ret4 #t)
)
 (quote ()
)
)
 (let ((prev (list-ref xs 0)
)
)
 (begin (let ((i 1)
)
 (begin (call/cc (lambda (break6)
 (letrec ((loop5 (lambda ()
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
 (begin (if (< (list-ref xs i)
 prev)
 (begin (ret4 #f)
)
 (quote ()
)
)
 (set! prev (list-ref xs i)
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
 (ret4 #t)
)
)
)
)
)
)
)
)
 (define (outOfOrder n)
 (call/cc (lambda (ret7)
 (begin (if (< n 2)
 (begin (ret7 (_list)
)
)
 (quote ()
)
)
 (let ((r (_list)
)
)
 (begin (call/cc (lambda (break9)
 (letrec ((loop8 (lambda ()
 (if #t (begin (set! r (_list)
)
 (let ((i 0)
)
 (begin (call/cc (lambda (break11)
 (letrec ((loop10 (lambda ()
 (if (< i n)
 (begin (set! r (append r (_list (modulo (now)
 3)
)
)
)
 (set! i (+ i 1)
)
 (loop10)
)
 (quote ()
)
)
)
)
)
 (loop10)
)
)
)
 (if (not (ordered r)
)
 (begin (break9 (quote ()
)
)
)
 (quote ()
)
)
)
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
 (ret7 r)
)
)
)
)
)
)
 (define (sort3 a)
 (call/cc (lambda (ret12)
 (let ((lo 0)
)
 (begin (let ((mid 0)
)
 (begin (let ((hi (- (cond ((string? a)
 (string-length a)
)
 ((hash-table? a)
 (hash-table-size a)
)
 (else (length a)
)
)
 1)
)
)
 (begin (call/cc (lambda (break14)
 (letrec ((loop13 (lambda ()
 (if (<= mid hi)
 (begin (let ((v (list-ref a mid)
)
)
 (begin (if (equal? v 0)
 (begin (let ((tmp (list-ref a lo)
)
)
 (begin (list-set! a lo (list-ref a mid)
)
 (list-set! a mid tmp)
 (set! lo (+ lo 1)
)
 (set! mid (+ mid 1)
)
)
)
)
 (if (equal? v 1)
 (begin (set! mid (+ mid 1)
)
)
 (begin (let ((tmp (list-ref a mid)
)
)
 (begin (list-set! a mid (list-ref a hi)
)
 (list-set! a hi tmp)
 (set! hi (- hi 1)
)
)
)
)
)
)
)
)
 (loop13)
)
 (quote ()
)
)
)
)
)
 (loop13)
)
)
)
 (ret12 a)
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
 (call/cc (lambda (ret15)
 (let ((f (outOfOrder 12)
)
)
 (begin (_display (to-str (listStr f)
)
)
 (newline)
 (set! f (sort3 f)
)
 (_display (to-str (listStr f)
)
)
 (newline)
)
)
)
)
)
 (main)
 (let ((end17 (now)
)
)
 (let ((dur18 (quotient (- end17 start16)
 1000)
)
)
 (begin (_display (string-append "{\n  \"duration_us\": " (number->string dur18)
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
