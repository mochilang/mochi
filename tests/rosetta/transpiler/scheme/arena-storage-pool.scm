;; Generated on 2025-07-26 21:39 +0700
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
(let ((start9 (now)
)
)
 (begin (define (poolPut p x)
 (call/cc (lambda (ret1)
 (ret1 (append p (_list x)
)
)
)
)
)
 (define (poolGet p)
 (call/cc (lambda (ret2)
 (begin (if (equal? (cond ((string? p)
 (string-length p)
)
 ((hash-table? p)
 (hash-table-size p)
)
 (else (length p)
)
)
 0)
 (begin (display (to-str "pool empty")
)
 (newline)
 (ret2 (alist->hash-table (_list (cons "pool" p)
 (cons "val" 0)
)
)
)
)
 (quote ()
)
)
 (let ((idx (- (cond ((string? p)
 (string-length p)
)
 ((hash-table? p)
 (hash-table-size p)
)
 (else (length p)
)
)
 1)
)
)
 (begin (let ((v (list-ref p idx)
)
)
 (begin (set! p (take (drop p 0)
 (- idx 0)
)
)
 (ret2 (alist->hash-table (_list (cons "pool" p)
 (cons "val" v)
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
 (define (clearPool p)
 (call/cc (lambda (ret3)
 (ret3 (_list)
)
)
)
)
 (define (main)
 (call/cc (lambda (ret4)
 (let ((pool (_list)
)
)
 (begin (let ((i 1)
)
 (begin (let ((j 2)
)
 (begin (display (to-str (to-str (+ i j)
)
)
)
 (newline)
 (set! pool (poolPut pool i)
)
 (set! pool (poolPut pool j)
)
 (set! i 0)
 (set! j 0)
 (let ((res1 (poolGet pool)
)
)
 (begin (set! pool (cond ((string? res1)
 (substring res1 "pool" (+ "pool" 1)
)
)
 ((hash-table? res1)
 (hash-table-ref res1 "pool")
)
 (else (list-ref res1 "pool")
)
)
)
 (set! i (let ((v5 (cond ((string? res1)
 (substring res1 "val" (+ "val" 1)
)
)
 ((hash-table? res1)
 (hash-table-ref res1 "val")
)
 (else (list-ref res1 "val")
)
)
)
)
 (cond ((string? v5)
 (inexact->exact (string->number v5)
)
)
 ((boolean? v5)
 (if v5 1 0)
)
 (else (inexact->exact v5)
)
)
)
)
 (let ((res2 (poolGet pool)
)
)
 (begin (set! pool (cond ((string? res2)
 (substring res2 "pool" (+ "pool" 1)
)
)
 ((hash-table? res2)
 (hash-table-ref res2 "pool")
)
 (else (list-ref res2 "pool")
)
)
)
 (set! j (let ((v6 (cond ((string? res2)
 (substring res2 "val" (+ "val" 1)
)
)
 ((hash-table? res2)
 (hash-table-ref res2 "val")
)
 (else (list-ref res2 "val")
)
)
)
)
 (cond ((string? v6)
 (inexact->exact (string->number v6)
)
)
 ((boolean? v6)
 (if v6 1 0)
)
 (else (inexact->exact v6)
)
)
)
)
 (set! i 4)
 (set! j 5)
 (display (to-str (to-str (+ i j)
)
)
)
 (newline)
 (set! pool (poolPut pool i)
)
 (set! pool (poolPut pool j)
)
 (set! i 0)
 (set! j 0)
 (set! pool (clearPool pool)
)
 (let ((res3 (poolGet pool)
)
)
 (begin (set! pool (cond ((string? res3)
 (substring res3 "pool" (+ "pool" 1)
)
)
 ((hash-table? res3)
 (hash-table-ref res3 "pool")
)
 (else (list-ref res3 "pool")
)
)
)
 (set! i (let ((v7 (cond ((string? res3)
 (substring res3 "val" (+ "val" 1)
)
)
 ((hash-table? res3)
 (hash-table-ref res3 "val")
)
 (else (list-ref res3 "val")
)
)
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
 (let ((res4 (poolGet pool)
)
)
 (begin (set! pool (cond ((string? res4)
 (substring res4 "pool" (+ "pool" 1)
)
)
 ((hash-table? res4)
 (hash-table-ref res4 "pool")
)
 (else (list-ref res4 "pool")
)
)
)
 (set! j (let ((v8 (cond ((string? res4)
 (substring res4 "val" (+ "val" 1)
)
)
 ((hash-table? res4)
 (hash-table-ref res4 "val")
)
 (else (list-ref res4 "val")
)
)
)
)
 (cond ((string? v8)
 (inexact->exact (string->number v8)
)
)
 ((boolean? v8)
 (if v8 1 0)
)
 (else (inexact->exact v8)
)
)
)
)
 (set! i 7)
 (set! j 8)
 (display (to-str (to-str (+ i j)
)
)
)
 (newline)
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
 (main)
 (let ((end10 (now)
)
)
 (let ((dur11 (quotient (- end10 start9)
 1000)
)
)
 (begin (display (string-append "{\n  \"duration_us\": " (number->string dur11)
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
