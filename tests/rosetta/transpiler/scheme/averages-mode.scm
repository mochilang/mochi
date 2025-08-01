;; Generated on 2025-07-26 23:50 +0700
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
(let ((start13 (now)
)
)
 (begin (define arr1 (_list 2 7 1 8 2)
)
 (define counts1 (alist->hash-table (_list)
)
)
 (define keys1 (_list)
)
 (define i 0)
 (call/cc (lambda (break2)
 (letrec ((loop1 (lambda ()
 (if (< i (cond ((string? arr1)
 (string-length arr1)
)
 ((hash-table? arr1)
 (hash-table-size arr1)
)
 (else (length arr1)
)
)
)
 (begin (let ((v (list-ref arr1 i)
)
)
 (begin (if (cond ((string? counts1)
 (if (string-contains counts1 v)
 #t #f)
)
 ((hash-table? counts1)
 (if (hash-table-exists? counts1 v)
 #t #f)
)
 (else (if (member v counts1)
 #t #f)
)
)
 (begin (hash-table-set! counts1 v (+ (hash-table-ref/default counts1 v (quote ()
)
)
 1)
)
)
 (begin (hash-table-set! counts1 v 1)
 (set! keys1 (append keys1 (_list v)
)
)
)
)
 (set! i (+ i 1)
)
)
)
 (loop1)
)
 (quote ()
)
)
)
)
)
 (loop1)
)
)
)
 (define max1 0)
 (set! i 0)
 (call/cc (lambda (break4)
 (letrec ((loop3 (lambda ()
 (if (< i (cond ((string? keys1)
 (string-length keys1)
)
 ((hash-table? keys1)
 (hash-table-size keys1)
)
 (else (length keys1)
)
)
)
 (begin (let ((k (list-ref keys1 i)
)
)
 (begin (let ((c (hash-table-ref/default counts1 k (quote ()
)
)
)
)
 (begin (if (> c max1)
 (begin (set! max1 c)
)
 (quote ()
)
)
 (set! i (+ i 1)
)
)
)
)
)
 (loop3)
)
 (quote ()
)
)
)
)
)
 (loop3)
)
)
)
 (define modes1 (_list)
)
 (set! i 0)
 (call/cc (lambda (break6)
 (letrec ((loop5 (lambda ()
 (if (< i (cond ((string? keys1)
 (string-length keys1)
)
 ((hash-table? keys1)
 (hash-table-size keys1)
)
 (else (length keys1)
)
)
)
 (begin (let ((k (list-ref keys1 i)
)
)
 (begin (if (equal? (hash-table-ref/default counts1 k (quote ()
)
)
 max1)
 (begin (set! modes1 (append modes1 (_list k)
)
)
)
 (quote ()
)
)
 (set! i (+ i 1)
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
 (display (to-str (to-str modes1)
)
)
 (newline)
 (define arr2 (_list 2 7 1 8 2 8)
)
 (define counts2 (alist->hash-table (_list)
)
)
 (define keys2 (_list)
)
 (set! i 0)
 (call/cc (lambda (break8)
 (letrec ((loop7 (lambda ()
 (if (< i (cond ((string? arr2)
 (string-length arr2)
)
 ((hash-table? arr2)
 (hash-table-size arr2)
)
 (else (length arr2)
)
)
)
 (begin (let ((v (list-ref arr2 i)
)
)
 (begin (if (cond ((string? counts2)
 (if (string-contains counts2 v)
 #t #f)
)
 ((hash-table? counts2)
 (if (hash-table-exists? counts2 v)
 #t #f)
)
 (else (if (member v counts2)
 #t #f)
)
)
 (begin (hash-table-set! counts2 v (+ (hash-table-ref/default counts2 v (quote ()
)
)
 1)
)
)
 (begin (hash-table-set! counts2 v 1)
 (set! keys2 (append keys2 (_list v)
)
)
)
)
 (set! i (+ i 1)
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
 (define max2 0)
 (set! i 0)
 (call/cc (lambda (break10)
 (letrec ((loop9 (lambda ()
 (if (< i (cond ((string? keys2)
 (string-length keys2)
)
 ((hash-table? keys2)
 (hash-table-size keys2)
)
 (else (length keys2)
)
)
)
 (begin (let ((k (list-ref keys2 i)
)
)
 (begin (let ((c (hash-table-ref/default counts2 k (quote ()
)
)
)
)
 (begin (if (> c max2)
 (begin (set! max2 c)
)
 (quote ()
)
)
 (set! i (+ i 1)
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
 (define modes2 (_list)
)
 (set! i 0)
 (call/cc (lambda (break12)
 (letrec ((loop11 (lambda ()
 (if (< i (cond ((string? keys2)
 (string-length keys2)
)
 ((hash-table? keys2)
 (hash-table-size keys2)
)
 (else (length keys2)
)
)
)
 (begin (let ((k (list-ref keys2 i)
)
)
 (begin (if (equal? (hash-table-ref/default counts2 k (quote ()
)
)
 max2)
 (begin (set! modes2 (append modes2 (_list k)
)
)
)
 (quote ()
)
)
 (set! i (+ i 1)
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
 (display (to-str (to-str modes2)
)
)
 (newline)
 (let ((end14 (now)
)
)
 (let ((dur15 (quotient (- end14 start13)
 1000)
)
)
 (begin (display (string-append "{\n  \"duration_us\": " (number->string dur15)
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
