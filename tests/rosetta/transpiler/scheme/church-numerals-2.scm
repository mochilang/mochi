;; Generated on 2025-07-28 11:58 +0700
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
(let ((start18 (now)
)
)
 (begin (define (id x)
 (call/cc (lambda (ret1)
 (ret1 x)
)
)
)
 (define (compose f g)
 (call/cc (lambda (ret2)
 (ret2 (lambda (x)
 (call/cc (lambda (ret3)
 (ret3 (f (g x)
)
)
)
)
)
)
)
)
)
 (define (zero)
 (call/cc (lambda (ret4)
 (ret4 (lambda (f)
 (call/cc (lambda (ret5)
 (ret5 id)
)
)
)
)
)
)
)
 (define (one)
 (call/cc (lambda (ret6)
 (ret6 id)
)
)
)
 (define (succ n)
 (call/cc (lambda (ret7)
 (ret7 (lambda (f)
 (call/cc (lambda (ret8)
 (ret8 (compose f (n f)
)
)
)
)
)
)
)
)
)
 (define (plus m n)
 (call/cc (lambda (ret9)
 (ret9 (lambda (f)
 (call/cc (lambda (ret10)
 (ret10 (compose (m f)
 (n f)
)
)
)
)
)
)
)
)
)
 (define (mult m n)
 (call/cc (lambda (ret11)
 (ret11 (compose m n)
)
)
)
)
 (define (exp m n)
 (call/cc (lambda (ret12)
 (ret12 (n m)
)
)
)
)
 (define (toInt x)
 (call/cc (lambda (ret13)
 (let ((counter 0)
)
 (begin (define (fCounter f)
 (call/cc (lambda (ret14)
 (begin (set! counter (+ counter 1)
)
 (ret14 f)
)
)
)
)
 (x fCounter)
 (ret13 counter)
)
)
)
)
)
 (define (toStr x)
 (call/cc (lambda (ret15)
 (let ((s "")
)
 (begin (define (fCounter f)
 (call/cc (lambda (ret16)
 (begin (set! s (string-append s "|")
)
 (ret16 f)
)
)
)
)
 (x fCounter)
 (ret15 s)
)
)
)
)
)
 (define (main)
 (call/cc (lambda (ret17)
 (begin (_display (to-str (string-append "zero = " (to-str (toInt (zero)
)
)
)
)
)
 (newline)
 (let ((onev (one)
)
)
 (begin (_display (to-str (string-append "one = " (to-str (toInt onev)
)
)
)
)
 (newline)
 (let ((two (succ (succ (zero)
)
)
)
)
 (begin (_display (to-str (string-append "two = " (to-str (toInt two)
)
)
)
)
 (newline)
 (let ((three (plus onev two)
)
)
 (begin (_display (to-str (string-append "three = " (to-str (toInt three)
)
)
)
)
 (newline)
 (let ((four (mult two two)
)
)
 (begin (_display (to-str (string-append "four = " (to-str (toInt four)
)
)
)
)
 (newline)
 (let ((eight (exp two three)
)
)
 (begin (_display (to-str (string-append "eight = " (to-str (toInt eight)
)
)
)
)
 (newline)
 (_display (to-str (string-append "toStr(four)
 = " (toStr four)
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
