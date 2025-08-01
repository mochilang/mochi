;; Generated on 2025-07-27 16:25 +0700
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
(define (indexOf s sub) (let ((cur (string-contains s sub)))   (if cur (string-cursor->index s cur) -1)))
(define (_display . args) (apply display args))
(define (padStart s width pad)
  (let loop ((out s))
    (if (< (string-length out) width)
        (loop (string-append pad out))
        out)))
(let ((start18 (now)
)
)
 (begin (define (padLeft s w)
 (call/cc (lambda (ret1)
 (let ((res "")
)
 (begin (let ((n (- w (cond ((string? s)
 (string-length s)
)
 ((hash-table? s)
 (hash-table-size s)
)
 (else (length s)
)
)
)
)
)
 (begin (call/cc (lambda (break3)
 (letrec ((loop2 (lambda ()
 (if (> n 0)
 (begin (set! res (string-append res " ")
)
 (set! n (- n 1)
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
 (ret1 (string-append res s)
)
)
)
)
)
)
)
)
 (define (padRight s w)
 (call/cc (lambda (ret4)
 (let ((out s)
)
 (begin (let ((i (cond ((string? s)
 (string-length s)
)
 ((hash-table? s)
 (hash-table-size s)
)
 (else (length s)
)
)
)
)
 (begin (call/cc (lambda (break6)
 (letrec ((loop5 (lambda ()
 (if (< i w)
 (begin (set! out (string-append out " ")
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
 (ret4 out)
)
)
)
)
)
)
)
 (define (indexOf s ch)
 (call/cc (lambda (ret7)
 (let ((i 0)
)
 (begin (call/cc (lambda (break9)
 (letrec ((loop8 (lambda ()
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
 (begin (if (string=? (substring s i (+ i 1)
)
 ch)
 (begin (ret7 i)
)
 (quote ()
)
)
 (set! i (+ i 1)
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
 (ret7 (- 1)
)
)
)
)
)
)
 (define (format2 f)
 (call/cc (lambda (ret10)
 (let ((s (to-str f)
)
)
 (begin (let ((idx (indexOf s ".")
)
)
 (begin (if (_lt idx 0)
 (begin (set! s (string-append s ".00")
)
)
 (begin (let ((need (+ idx 3)
)
)
 (begin (if (_gt (cond ((string? s)
 (string-length s)
)
 ((hash-table? s)
 (hash-table-size s)
)
 (else (length s)
)
)
 need)
 (begin (set! s (substring s 0 need)
)
)
 (begin (call/cc (lambda (break12)
 (letrec ((loop11 (lambda ()
 (if (_lt (cond ((string? s)
 (string-length s)
)
 ((hash-table? s)
 (hash-table-size s)
)
 (else (length s)
)
)
 need)
 (begin (set! s (string-append s "0")
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
)
)
)
)
)
)
 (ret10 s)
)
)
)
)
)
)
)
 (define (cpx h)
 (call/cc (lambda (ret13)
 (let ((x (let ((v14 (+ (/ h 11.25)
 0.5)
)
)
 (cond ((string? v14)
 (inexact->exact (floor (string->number v14)
)
)
)
 ((boolean? v14)
 (if v14 1 0)
)
 (else (inexact->exact (floor v14)
)
)
)
)
)
)
 (begin (set! x (modulo x 32)
)
 (if (< x 0)
 (begin (set! x (+ x 32)
)
)
 (quote ()
)
)
 (ret13 x)
)
)
)
)
)
 (define compassPoint (_list "North" "North by east" "North-northeast" "Northeast by north" "Northeast" "Northeast by east" "East-northeast" "East by north" "East" "East by south" "East-southeast" "Southeast by east" "Southeast" "Southeast by south" "South-southeast" "South by east" "South" "South by west" "South-southwest" "Southwest by south" "Southwest" "Southwest by west" "West-southwest" "West by south" "West" "West by north" "West-northwest" "Northwest by west" "Northwest" "Northwest by north" "North-northwest" "North by west")
)
 (define (degrees2compasspoint h)
 (call/cc (lambda (ret15)
 (ret15 (list-ref compassPoint (cpx h)
)
)
)
)
)
 (define headings (_list 0.0 16.87 16.88 33.75 50.62 50.63 67.5 84.37 84.38 101.25 118.12 118.13 135.0 151.87 151.88 168.75 185.62 185.63 202.5 219.37 219.38 236.25 253.12 253.13 270.0 286.87 286.88 303.75 320.62 320.63 337.5 354.37 354.38)
)
 (_display (to-str "Index  Compass point         Degree")
)
 (newline)
 (define i 0)
 (call/cc (lambda (break17)
 (letrec ((loop16 (lambda ()
 (if (< i (cond ((string? headings)
 (string-length headings)
)
 ((hash-table? headings)
 (hash-table-size headings)
)
 (else (length headings)
)
)
)
 (begin (let ((h (list-ref headings i)
)
)
 (begin (let ((idx (+ (modulo i 32)
 1)
)
)
 (begin (let ((cp (degrees2compasspoint h)
)
)
 (begin (_display (to-str (string-append (string-append (string-append (string-append (string-append (padLeft (to-str idx)
 4)
 "   ")
 (padRight cp 19)
)
 " ")
 (format2 h)
)
 "°")
)
)
 (newline)
 (set! i (+ i 1)
)
)
)
)
)
)
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
 (let ((end19 (now)
)
)
 (let ((dur20 (quotient (- end19 start18)
 1000)
)
)
 (begin (_display (string-append "{\n  \"duration_us\": " (number->string dur20)
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
