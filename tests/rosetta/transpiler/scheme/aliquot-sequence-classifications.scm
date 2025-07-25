;; Generated on 2025-07-26 19:01 +0700
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
(let ((start29 (now)
)
)
 (begin (define THRESHOLD 140737488355328)
 (define (indexOf xs value)
 (call/cc (lambda (ret1)
 (let ((i 0)
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
 (begin (if (equal? (list-ref xs i)
 value)
 (begin (ret1 i)
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
 (ret1 (- 0 1)
)
)
)
)
)
)
 (define (contains xs value)
 (call/cc (lambda (ret4)
 (ret4 (not (equal? (indexOf xs value)
 (- 0 1)
)
)
)
)
)
)
 (define (maxOf a b)
 (call/cc (lambda (ret5)
 (if (> a b)
 (begin (ret5 a)
)
 (begin (ret5 b)
)
)
)
)
)
 (define (intSqrt n)
 (call/cc (lambda (ret6)
 (begin (if (equal? n 0)
 (begin (ret6 0)
)
 (quote ()
)
)
 (let ((x n)
)
 (begin (let ((y (quotient (+ x 1)
 2)
)
)
 (begin (call/cc (lambda (break8)
 (letrec ((loop7 (lambda ()
 (if (< y x)
 (begin (set! x y)
 (set! y (quotient (+ x (quotient n x)
)
 2)
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
 (ret6 x)
)
)
)
)
)
)
)
)
 (define (sumProperDivisors n)
 (call/cc (lambda (ret9)
 (begin (if (< n 2)
 (begin (ret9 0)
)
 (quote ()
)
)
 (let ((sqrt (intSqrt n)
)
)
 (begin (let ((sum 1)
)
 (begin (let ((i 2)
)
 (begin (call/cc (lambda (break11)
 (letrec ((loop10 (lambda ()
 (if (_le i sqrt)
 (begin (if (equal? (modulo n i)
 0)
 (begin (set! sum (+ (+ sum i)
 (quotient n i)
)
)
)
 (quote ()
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
 (if (equal? (* sqrt sqrt)
 n)
 (begin (set! sum (- sum sqrt)
)
)
 (quote ()
)
)
 (ret9 sum)
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
 (define (classifySequence k)
 (call/cc (lambda (ret12)
 (let ((last k)
)
 (begin (let ((seq (_list k)
)
)
 (begin (call/cc (lambda (break14)
 (letrec ((loop13 (lambda ()
 (if #t (begin (set! last (sumProperDivisors last)
)
 (set! seq (append seq (_list last)
)
)
 (let ((n (cond ((string? seq)
 (string-length seq)
)
 ((hash-table? seq)
 (hash-table-size seq)
)
 (else (length seq)
)
)
)
)
 (begin (let ((aliquot "")
)
 (begin (if (equal? last 0)
 (begin (set! aliquot "Terminating")
)
 (if (and (equal? n 2)
 (equal? last k)
)
 (begin (set! aliquot "Perfect")
)
 (if (and (equal? n 3)
 (equal? last k)
)
 (begin (set! aliquot "Amicable")
)
 (if (and (>= n 4)
 (equal? last k)
)
 (begin (set! aliquot (string-append (string-append "Sociable[" (to-str (- n 1)
)
)
 "]")
)
)
 (if (equal? last (list-ref seq (- n 2)
)
)
 (begin (set! aliquot "Aspiring")
)
 (if (contains (take (drop seq 1)
 (- (maxOf 1 (- n 2)
)
 1)
)
 last)
 (begin (let ((idx (indexOf seq last)
)
)
 (begin (set! aliquot (string-append (string-append "Cyclic[" (to-str (- (- n 1)
 idx)
)
)
 "]")
)
)
)
)
 (if (or (equal? n 16)
 (> last THRESHOLD)
)
 (begin (set! aliquot "Non-Terminating")
)
 (quote ()
)
)
)
)
)
)
)
)
 (if (not (string=? aliquot "")
)
 (begin (ret12 (alist->hash-table (_list (cons "seq" seq)
 (cons "aliquot" aliquot)
)
)
)
)
 (quote ()
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
 (ret12 (alist->hash-table (_list (cons "seq" seq)
 (cons "aliquot" "")
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
 (define (padLeft n w)
 (call/cc (lambda (ret15)
 (let ((s (to-str n)
)
)
 (begin (call/cc (lambda (break17)
 (letrec ((loop16 (lambda ()
 (if (< (cond ((string? s)
 (string-length s)
)
 ((hash-table? s)
 (hash-table-size s)
)
 (else (length s)
)
)
 w)
 (begin (set! s (string-append " " s)
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
 (ret15 s)
)
)
)
)
)
 (define (padRight s w)
 (call/cc (lambda (ret18)
 (let ((r s)
)
 (begin (call/cc (lambda (break20)
 (letrec ((loop19 (lambda ()
 (if (< (cond ((string? r)
 (string-length r)
)
 ((hash-table? r)
 (hash-table-size r)
)
 (else (length r)
)
)
 w)
 (begin (set! r (string-append r " ")
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
 (ret18 r)
)
)
)
)
)
 (define (joinWithCommas seq)
 (call/cc (lambda (ret21)
 (let ((s "[")
)
 (begin (let ((i 0)
)
 (begin (call/cc (lambda (break23)
 (letrec ((loop22 (lambda ()
 (if (< i (cond ((string? seq)
 (string-length seq)
)
 ((hash-table? seq)
 (hash-table-size seq)
)
 (else (length seq)
)
)
)
 (begin (set! s (string-append s (to-str (list-ref seq i)
)
)
)
 (if (_lt i (- (cond ((string? seq)
 (string-length seq)
)
 ((hash-table? seq)
 (hash-table-size seq)
)
 (else (length seq)
)
)
 1)
)
 (begin (set! s (string-append s ", ")
)
)
 (quote ()
)
)
 (set! i (+ i 1)
)
 (loop22)
)
 (quote ()
)
)
)
)
)
 (loop22)
)
)
)
 (set! s (string-append s "]")
)
 (ret21 s)
)
)
)
)
)
)
)
 (define (main)
 (call/cc (lambda (ret24)
 (begin (display (to-str "Aliquot classifications - periods for Sociable/Cyclic in square brackets:\n")
)
 (newline)
 (let ((k 1)
)
 (begin (call/cc (lambda (break26)
 (letrec ((loop25 (lambda ()
 (if (<= k 10)
 (begin (let ((res (classifySequence k)
)
)
 (begin (display (to-str (string-append (string-append (string-append (string-append (padLeft k 2)
 ": ")
 (padRight (to-str (cond ((string? res)
 (substring res "aliquot" (+ "aliquot" 1)
)
)
 ((hash-table? res)
 (hash-table-ref res "aliquot")
)
 (else (list-ref res "aliquot")
)
)
)
 15)
)
 " ")
 (joinWithCommas (cond ((string? res)
 (substring res "seq" (+ "seq" 1)
)
)
 ((hash-table? res)
 (hash-table-ref res "seq")
)
 (else (list-ref res "seq")
)
)
)
)
)
)
 (newline)
 (set! k (+ k 1)
)
)
)
 (loop25)
)
 (quote ()
)
)
)
)
)
 (loop25)
)
)
)
 (display (to-str "")
)
 (newline)
 (let ((s (_list 11 12 28 496 220 1184 12496 1264460 790 909 562 1064 1488)
)
)
 (begin (let ((i 0)
)
 (begin (call/cc (lambda (break28)
 (letrec ((loop27 (lambda ()
 (if (< i (cond ((string? s)
 (string-length s)
)
 ((hash-table? s)
 (hash-table-size s)
)
 (else (length s)
)
)
)
 (begin (let ((val (list-ref s i)
)
)
 (begin (let ((res (classifySequence val)
)
)
 (begin (display (to-str (string-append (string-append (string-append (string-append (padLeft val 7)
 ": ")
 (padRight (to-str (cond ((string? res)
 (substring res "aliquot" (+ "aliquot" 1)
)
)
 ((hash-table? res)
 (hash-table-ref res "aliquot")
)
 (else (list-ref res "aliquot")
)
)
)
 15)
)
 " ")
 (joinWithCommas (cond ((string? res)
 (substring res "seq" (+ "seq" 1)
)
)
 ((hash-table? res)
 (hash-table-ref res "seq")
)
 (else (list-ref res "seq")
)
)
)
)
)
)
 (newline)
 (set! i (+ i 1)
)
)
)
)
)
 (loop27)
)
 (quote ()
)
)
)
)
)
 (loop27)
)
)
)
 (display (to-str "")
)
 (newline)
 (let ((big 15355717786080)
)
 (begin (let ((r (classifySequence big)
)
)
 (begin (display (to-str (string-append (string-append (string-append (string-append (to-str big)
 ": ")
 (padRight (to-str (cond ((string? r)
 (substring r "aliquot" (+ "aliquot" 1)
)
)
 ((hash-table? r)
 (hash-table-ref r "aliquot")
)
 (else (list-ref r "aliquot")
)
)
)
 15)
)
 " ")
 (joinWithCommas (cond ((string? r)
 (substring r "seq" (+ "seq" 1)
)
)
 ((hash-table? r)
 (hash-table-ref r "seq")
)
 (else (list-ref r "seq")
)
)
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
 (let ((end30 (now)
)
)
 (let ((dur31 (quotient (- end30 start29)
 1000)
)
)
 (begin (display (string-append "{\n  \"duration_us\": " (number->string dur31)
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
