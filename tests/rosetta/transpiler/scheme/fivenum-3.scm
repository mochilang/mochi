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
(let ((start12 (now)
)
)
 (begin (define (sortFloat xs)
 (call/cc (lambda (ret1)
 (let ((arr xs)
)
 (begin (let ((n (cond ((string? arr)
 (string-length arr)
)
 ((hash-table? arr)
 (hash-table-size arr)
)
 (else (length arr)
)
)
)
)
 (begin (let ((i 0)
)
 (begin (call/cc (lambda (break3)
 (letrec ((loop2 (lambda ()
 (if (< i n)
 (begin (let ((j 0)
)
 (begin (call/cc (lambda (break5)
 (letrec ((loop4 (lambda ()
 (if (_lt j (- n 1)
)
 (begin (if (> (list-ref arr j)
 (list-ref arr (+ j 1)
)
)
 (begin (let ((t (list-ref arr j)
)
)
 (begin (list-set! arr j (list-ref arr (+ j 1)
)
)
 (list-set! arr (+ j 1)
 t)
)
)
)
 (quote ()
)
)
 (set! j (+ j 1)
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
 (set! i (+ i 1)
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
 (ret1 arr)
)
)
)
)
)
)
)
)
)
 (define (ceilf x)
 (call/cc (lambda (ret6)
 (let ((i (let ((v7 x)
)
 (cond ((string? v7)
 (exact (floor (string->number v7)
)
)
)
 ((boolean? v7)
 (if v7 1 0)
)
 (else (exact (floor v7)
)
)
)
)
)
)
 (begin (if (> x (+ 0.0 i)
)
 (begin (ret6 (+ i 1)
)
)
 (quote ()
)
)
 (ret6 i)
)
)
)
)
)
 (define (fivenum a)
 (call/cc (lambda (ret8)
 (let ((arr (sortFloat a)
)
)
 (begin (let ((n (cond ((string? arr)
 (string-length arr)
)
 ((hash-table? arr)
 (hash-table-size arr)
)
 (else (length arr)
)
)
)
)
 (begin (let ((half (- (+ n 3)
 (modulo (+ n 3)
 2)
)
)
)
 (begin (let ((n4 (/ (+ 0.0 (quotient half 2)
)
 2.0)
)
)
 (begin (let ((nf (+ 0.0 n)
)
)
 (begin (let ((d (_list 1.0 n4 (/ (+ nf 1.0)
 2.0)
 (- (+ nf 1.0)
 n4)
 nf)
)
)
 (begin (let ((result (_list)
)
)
 (begin (let ((idx 0)
)
 (begin (call/cc (lambda (break10)
 (letrec ((loop9 (lambda ()
 (if (< idx (cond ((string? d)
 (string-length d)
)
 ((hash-table? d)
 (hash-table-size d)
)
 (else (length d)
)
)
)
 (begin (let ((de (list-ref d idx)
)
)
 (begin (let ((fl (let ((v11 (- de 1.0)
)
)
 (cond ((string? v11)
 (exact (floor (string->number v11)
)
)
)
 ((boolean? v11)
 (if v11 1 0)
)
 (else (exact (floor v11)
)
)
)
)
)
)
 (begin (let ((cl (ceilf (- de 1.0)
)
)
)
 (begin (set! result (append result (_list (* 0.5 (_add (cond ((string? arr)
 (substring arr fl (+ fl 1)
)
)
 ((hash-table? arr)
 (hash-table-ref arr fl)
)
 (else (list-ref arr fl)
)
)
 (cond ((string? arr)
 (substring arr cl (+ cl 1)
)
)
 ((hash-table? arr)
 (hash-table-ref arr cl)
)
 (else (list-ref arr cl)
)
)
)
)
)
)
)
 (set! idx (+ idx 1)
)
)
)
)
)
)
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
 (ret8 result)
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
)
)
)
)
)
 (define x1 (_list 36.0 40.0 7.0 39.0 41.0 15.0)
)
 (define x2 (_list 15.0 6.0 42.0 41.0 7.0 36.0 49.0 40.0 39.0 47.0 43.0)
)
 (define x3 (_list 0.14082834 0.0974879 1.73131507 0.87636009 (- 1.95059594)
 0.73438555 (- 0.03035726)
 1.4667597 (- 0.74621349)
 (- 0.72588772)
 0.6390516 0.61501527 (- 0.9898378)
 (- 1.00447874)
 (- 0.62759469)
 0.66206163 1.04312009 (- 0.10305385)
 0.75775634 0.32566578)
)
 (_display (to-str (to-str (fivenum x1)
)
)
)
 (newline)
 (_display (to-str (to-str (fivenum x2)
)
)
)
 (newline)
 (_display (to-str (to-str (fivenum x3)
)
)
)
 (newline)
 (let ((end13 (now)
)
)
 (let ((dur14 (quotient (- end13 start12)
 1000)
)
)
 (begin (_display (string-append "{\n  \"duration_us\": " (number->string dur14)
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
