;; Generated on 2025-07-25 21:06 +0700
(import (only (scheme base) call/cc when list-ref list-set! list))
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
(let ((start8 (now)
)
)
 (begin (define (sel list k)
 (call/cc (lambda (ret1)
 (let ((i 0)
)
 (begin (call/cc (lambda (break3)
 (letrec ((loop2 (lambda ()
 (if (<= i k)
 (begin (let ((minIndex i)
)
 (begin (let ((j (+ i 1)
)
)
 (begin (call/cc (lambda (break5)
 (letrec ((loop4 (lambda ()
 (if (< j (cond ((string? list)
 (string-length list)
)
 ((hash-table? list)
 (hash-table-size list)
)
 (else (length list)
)
)
)
 (begin (if (< (list-ref list j)
 (list-ref list minIndex)
)
 (begin (set! minIndex j)
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
 (let ((tmp (list-ref list i)
)
)
 (begin (list-set! list i (list-ref list minIndex)
)
 (list-set! list minIndex tmp)
 (set! i (+ i 1)
)
)
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
 (ret1 (list-ref list k)
)
)
)
)
)
)
 (define (median a)
 (call/cc (lambda (ret6)
 (let ((arr a)
)
 (begin (let ((half (let ((v7 (quotient (cond ((string? arr)
 (string-length arr)
)
 ((hash-table? arr)
 (hash-table-size arr)
)
 (else (length arr)
)
)
 2)
)
)
 (cond ((string? v7)
 (inexact->exact (string->number v7)
)
)
 ((boolean? v7)
 (if v7 1 0)
)
 (else (inexact->exact v7)
)
)
)
)
)
 (begin (let ((med (sel arr half)
)
)
 (begin (if (equal? (modulo (cond ((string? arr)
 (string-length arr)
)
 ((hash-table? arr)
 (hash-table-size arr)
)
 (else (length arr)
)
)
 2)
 0)
 (begin (ret6 (/ (+ med (list-ref arr (- half 1)
)
)
 2.0)
)
)
 (quote ()
)
)
 (ret6 med)
)
)
)
)
)
)
)
)
)
 (display (to-str (to-str (median (list 3.0 1.0 4.0 1.0)
)
)
)
)
 (newline)
 (display (to-str (to-str (median (list 3.0 1.0 4.0 1.0 5.0)
)
)
)
)
 (newline)
 (let ((end9 (now)
)
)
 (let ((dur10 (quotient (- end9 start8)
 1000)
)
)
 (begin (display (string-append "{\n  \"duration_us\": " (number->string dur10)
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
