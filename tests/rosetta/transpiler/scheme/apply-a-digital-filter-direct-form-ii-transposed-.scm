;; Generated on 2025-07-26 21:34 +0700
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
 (begin (define (applyFilter input a b)
 (call/cc (lambda (ret1)
 (let ((out (_list)
)
)
 (begin (let ((scale (/ 1.0 (list-ref a 0)
)
)
)
 (begin (let ((i 0)
)
 (begin (call/cc (lambda (break3)
 (letrec ((loop2 (lambda ()
 (if (< i (cond ((string? input)
 (string-length input)
)
 ((hash-table? input)
 (hash-table-size input)
)
 (else (length input)
)
)
)
 (begin (let ((tmp 0.0)
)
 (begin (let ((j 0)
)
 (begin (call/cc (lambda (break5)
 (letrec ((loop4 (lambda ()
 (if (and (<= j i)
 (< j (cond ((string? b)
 (string-length b)
)
 ((hash-table? b)
 (hash-table-size b)
)
 (else (length b)
)
)
)
)
 (begin (set! tmp (+ tmp (* (list-ref b j)
 (list-ref input (- i j)
)
)
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
 (set! j 0)
 (call/cc (lambda (break7)
 (letrec ((loop6 (lambda ()
 (if (and (< j i)
 (_lt (+ j 1)
 (cond ((string? a)
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
 (begin (set! tmp (- tmp (* (list-ref a (+ j 1)
)
 (list-ref out (- (- i j)
 1)
)
)
)
)
 (set! j (+ j 1)
)
 (loop6)
)
 (quote ()
)
)
)
)
)
 (loop6)
)
)
)
 (set! out (append out (_list (* tmp scale)
)
)
)
 (set! i (+ i 1)
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
 (ret1 out)
)
)
)
)
)
)
)
)
)
 (define a (_list 1.0 (- 2.7756e-16)
 0.33333333 (- 1.85e-17)
)
)
 (define b (_list 0.16666667 0.5 0.5 0.16666667)
)
 (define sig (_list (- 0.917843918645)
 0.141984778794 1.20536903482 0.190286794412 (- 0.662370894973)
 (- 1.00700480494)
 (- 0.404707073677)
 0.800482325044 0.743500089861 1.01090520172 0.741527555207 0.277841675195 0.400833448236 (- 0.2085993586)
 (- 0.172842103641)
 (- 0.134316096293)
 0.0259303398477 0.490105989562 0.549391221511 0.9047198589)
)
 (define res (applyFilter sig a b)
)
 (define k 0)
 (call/cc (lambda (break9)
 (letrec ((loop8 (lambda ()
 (if (< k (cond ((string? res)
 (string-length res)
)
 ((hash-table? res)
 (hash-table-size res)
)
 (else (length res)
)
)
)
 (begin (display (to-str (cond ((string? res)
 (substring res k (+ k 1)
)
)
 ((hash-table? res)
 (hash-table-ref res k)
)
 (else (list-ref res k)
)
)
)
)
 (newline)
 (set! k (+ k 1)
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
