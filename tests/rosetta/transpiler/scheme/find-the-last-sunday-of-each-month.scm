;; Generated on 2025-07-30 21:05 +0700
(import (only (scheme base) call/cc when list-ref list-set!))
(import (rename (scheme base) (list _list)))
(import (scheme time))
(import (chibi string))
(import (only (scheme char) string-upcase string-downcase))
(import (scheme write))
(import (srfi 69))
(import (chibi io))
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
(define (_input)
  (let ((l (read-line)))
    (if (eof-object? l) "" l)))
(let ((start12 (now)
)
)
 (begin (define (leapYear y)
 (call/cc (lambda (ret1)
 (ret1 (or (and (equal? (modulo y 4)
 0)
 (not (equal? (modulo y 100)
 0)
)
)
 (equal? (modulo y 400)
 0)
)
)
)
)
)
 (define (monthDays y m)
 (call/cc (lambda (ret2)
 (let ((days (_list 0 31 28 31 30 31 30 31 31 30 31 30 31)
)
)
 (begin (if (and (equal? m 2)
 (leapYear y)
)
 (begin (ret2 29)
)
 (quote ()
)
)
 (ret2 (list-ref days m)
)
)
)
)
)
)
 (define (zeller y m d)
 (call/cc (lambda (ret3)
 (let ((mm m)
)
 (begin (let ((yy y)
)
 (begin (if (< mm 3)
 (begin (set! mm (+ mm 12)
)
 (set! yy (- yy 1)
)
)
 (quote ()
)
)
 (let ((K (modulo yy 100)
)
)
 (begin (let ((J (quotient yy 100)
)
)
 (begin (let ((h (modulo (_add (_add (_add (_add (_add d (quotient (* 13 (+ mm 1)
)
 5)
)
 K)
 (quotient K 4)
)
 (quotient J 4)
)
 (* 5 J)
)
 7)
)
)
 (begin (ret3 (modulo (+ h 6)
 7)
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
 (define (lastSunday y m)
 (call/cc (lambda (ret4)
 (let ((day (monthDays y m)
)
)
 (begin (call/cc (lambda (break6)
 (letrec ((loop5 (lambda ()
 (if (and (_gt day 0)
 (not (equal? (zeller y m day)
 0)
)
)
 (begin (set! day (- day 1)
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
 (ret4 day)
)
)
)
)
)
 (define (monthName m)
 (call/cc (lambda (ret7)
 (let ((names (_list "" "January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December")
)
)
 (begin (ret7 (list-ref names m)
)
)
)
)
)
)
 (define (main)
 (call/cc (lambda (ret8)
 (let ((year (let ((v9 (_input)
)
)
 (cond ((string? v9)
 (exact (floor (string->number v9)
)
)
)
 ((boolean? v9)
 (if v9 1 0)
)
 (else (exact (floor v9)
)
)
)
)
)
)
 (begin (_display (to-str (string-append "Last Sundays of each month of " (to-str year)
)
)
)
 (newline)
 (_display (to-str "==================================")
)
 (newline)
 (let ((m 1)
)
 (begin (call/cc (lambda (break11)
 (letrec ((loop10 (lambda ()
 (if (<= m 12)
 (begin (let ((day (lastSunday year m)
)
)
 (begin (_display (to-str (string-append (string-append (monthName m)
 ": ")
 (to-str day)
)
)
)
 (newline)
 (set! m (+ m 1)
)
)
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
)
)
)
)
)
)
)
 (main)
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
