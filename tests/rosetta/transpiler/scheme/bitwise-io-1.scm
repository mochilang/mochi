;; Generated on 2025-07-27 01:10 +0700
(import (only (scheme base) call/cc when list-ref list-set!))
(import (rename (scheme base) (list _list)))
(import (scheme time))
(import (chibi string))
(import (only (scheme char) string-upcase string-downcase))
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
(define (padStart s width pad)
  (let loop ((out s))
    (if (< (string-length out) width)
        (loop (string-append pad out))
        out)))
(let ((start22 (now)
)
)
 (begin (define (pow2 n)
 (call/cc (lambda (ret1)
 (let ((v 1)
)
 (begin (let ((i 0)
)
 (begin (call/cc (lambda (break3)
 (letrec ((loop2 (lambda ()
 (if (< i n)
 (begin (set! v (* v 2)
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
 (ret1 v)
)
)
)
)
)
)
)
 (define (lshift x n)
 (call/cc (lambda (ret4)
 (ret4 (* x (pow2 n)
)
)
)
)
)
 (define (rshift x n)
 (call/cc (lambda (ret5)
 (ret5 (/ x (pow2 n)
)
)
)
)
)
 (define (NewWriter order)
 (call/cc (lambda (ret6)
 (ret6 (alist->hash-table (_list (cons "order" order)
 (cons "bits" 0)
 (cons "nbits" 0)
 (cons "data" (_list)
)
)
)
)
)
)
)
 (define (writeBitsLSB w c width)
 (call/cc (lambda (ret7)
 (begin (hash-table-set! w "bits" (+ (hash-table-ref w "bits")
 (lshift c (hash-table-ref w "nbits")
)
)
)
 (hash-table-set! w "nbits" (+ (hash-table-ref w "nbits")
 width)
)
 (call/cc (lambda (break9)
 (letrec ((loop8 (lambda ()
 (if (>= (hash-table-ref w "nbits")
 8)
 (begin (let ((b (fmod (hash-table-ref w "bits")
 256)
)
)
 (begin (hash-table-set! w "data" (append (hash-table-ref w "data")
 (_list b)
)
)
 (hash-table-set! w "bits" (rshift (hash-table-ref w "bits")
 8)
)
 (hash-table-set! w "nbits" (- (hash-table-ref w "nbits")
 8)
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
 (ret7 w)
)
)
)
)
 (define (writeBitsMSB w c width)
 (call/cc (lambda (ret10)
 (begin (hash-table-set! w "bits" (+ (hash-table-ref w "bits")
 (lshift c (- (- 32 width)
 (hash-table-ref w "nbits")
)
)
)
)
 (hash-table-set! w "nbits" (+ (hash-table-ref w "nbits")
 width)
)
 (call/cc (lambda (break12)
 (letrec ((loop11 (lambda ()
 (if (>= (hash-table-ref w "nbits")
 8)
 (begin (let ((b (fmod (rshift (hash-table-ref w "bits")
 24)
 256)
)
)
 (begin (hash-table-set! w "data" (append (hash-table-ref w "data")
 (_list b)
)
)
 (hash-table-set! w "bits" (* (fmod (hash-table-ref w "bits")
 (pow2 24)
)
 256)
)
 (hash-table-set! w "nbits" (- (hash-table-ref w "nbits")
 8)
)
)
)
 (loop11)
)
 (quote ()
)
)
)
)
)
 (loop11)
)
)
)
 (ret10 w)
)
)
)
)
 (define (WriteBits w c width)
 (call/cc (lambda (ret13)
 (begin (if (string=? (hash-table-ref w "order")
 "LSB")
 (begin (ret13 (writeBitsLSB w c width)
)
)
 (quote ()
)
)
 (ret13 (writeBitsMSB w c width)
)
)
)
)
)
 (define (CloseWriter w)
 (call/cc (lambda (ret14)
 (begin (if (> (hash-table-ref w "nbits")
 0)
 (begin (if (string=? (hash-table-ref w "order")
 "MSB")
 (begin (hash-table-set! w "bits" (rshift (hash-table-ref w "bits")
 24)
)
)
 (quote ()
)
)
 (hash-table-set! w "data" (append (hash-table-ref w "data")
 (_list (fmod (hash-table-ref w "bits")
 256)
)
)
)
)
 (quote ()
)
)
 (hash-table-set! w "bits" 0)
 (hash-table-set! w "nbits" 0)
 (ret14 w)
)
)
)
)
 (define (toBinary n bits)
 (call/cc (lambda (ret15)
 (let ((b "")
)
 (begin (let ((val n)
)
 (begin (let ((i 0)
)
 (begin (call/cc (lambda (break17)
 (letrec ((loop16 (lambda ()
 (if (< i bits)
 (begin (set! b (string-append (to-str (fmod val 2)
)
 b)
)
 (set! val (/ val 2)
)
 (set! i (+ i 1)
)
 (loop16)
)
 (quote ()
)
)
)
)
)
 (loop16)
)
)
)
 (ret15 b)
)
)
)
)
)
)
)
)
)
 (define (bytesToBits bs)
 (call/cc (lambda (ret18)
 (let ((out "[")
)
 (begin (let ((i 0)
)
 (begin (call/cc (lambda (break20)
 (letrec ((loop19 (lambda ()
 (if (< i (cond ((string? bs)
 (string-length bs)
)
 ((hash-table? bs)
 (hash-table-size bs)
)
 (else (length bs)
)
)
)
 (begin (set! out (string-append out (toBinary (list-ref bs i)
 8)
)
)
 (if (_lt (+ i 1)
 (cond ((string? bs)
 (string-length bs)
)
 ((hash-table? bs)
 (hash-table-size bs)
)
 (else (length bs)
)
)
)
 (begin (set! out (string-append out " ")
)
)
 (quote ()
)
)
 (set! i (+ i 1)
)
 (loop19)
)
 (quote ()
)
)
)
)
)
 (loop19)
)
)
)
 (set! out (string-append out "]")
)
 (ret18 out)
)
)
)
)
)
)
)
 (define (ExampleWriter_WriteBits)
 (call/cc (lambda (ret21)
 (let ((bw (NewWriter "MSB")
)
)
 (begin (set! bw (WriteBits bw 15 4)
)
 (set! bw (WriteBits bw 0 1)
)
 (set! bw (WriteBits bw 19 5)
)
 (set! bw (CloseWriter bw)
)
 (display (to-str (bytesToBits (hash-table-ref bw "data")
)
)
)
 (newline)
)
)
)
)
)
 (ExampleWriter_WriteBits)
 (let ((end23 (now)
)
)
 (let ((dur24 (quotient (- end23 start22)
 1000)
)
)
 (begin (display (string-append "{\n  \"duration_us\": " (number->string dur24)
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
