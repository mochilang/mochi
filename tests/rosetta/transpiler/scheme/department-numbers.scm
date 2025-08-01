;; Generated on 2025-07-28 11:25 +0700
(import (only (scheme base) call/cc when list-ref list-set!))
(import (rename (scheme base) (list _list)))
(import (scheme time))
(import (chibi string))
(import (only (scheme char) string-upcase string-downcase))
(import (scheme write))
(import (srfi 69))
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
(_display (to-str "Police  Sanitation  Fire")
)
(newline)
(_display (to-str "------  ----------  ----")
)
(newline)
(define count 0)
(define i 2)
(call/cc (lambda (break2)
 (letrec ((loop1 (lambda ()
 (if (< i 7)
 (begin (let ((j 1)
)
 (begin (call/cc (lambda (break4)
 (letrec ((loop3 (lambda ()
 (if (< j 8)
 (begin (if (not (equal? j i)
)
 (begin (let ((k 1)
)
 (begin (call/cc (lambda (break6)
 (letrec ((loop5 (lambda ()
 (if (< k 8)
 (begin (if (and (not (equal? k i)
)
 (not (equal? k j)
)
)
 (begin (if (equal? (_add (+ i j)
 k)
 12)
 (begin (_display (to-str (string-append (string-append (string-append (string-append (string-append "  " (to-str i)
)
 "         ")
 (to-str j)
)
 "         ")
 (to-str k)
)
)
)
 (newline)
 (set! count (+ count 1)
)
)
 (quote ()
)
)
)
 (quote ()
)
)
 (set! k (+ k 1)
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
)
)
)
 (quote ()
)
)
 (set! j (+ j 1)
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
 (set! i (+ i 2)
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
(_display (to-str "")
)
(newline)
(_display (to-str (string-append (to-str count)
 " valid combinations")
)
)
(newline)
