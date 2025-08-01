;; Generated on 2025-07-26 21:31 +0700
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
(let ((start26 (now)
)
)
 (begin (define (d2d d)
 (call/cc (lambda (ret1)
 (ret1 (fmod d 360.0)
)
)
)
)
 (define (g2g g)
 (call/cc (lambda (ret2)
 (ret2 (fmod g 400.0)
)
)
)
)
 (define (m2m m)
 (call/cc (lambda (ret3)
 (ret3 (fmod m 6400.0)
)
)
)
)
 (define (r2r r)
 (call/cc (lambda (ret4)
 (ret4 (fmod r (* 2.0 3.141592653589793)
)
)
)
)
)
 (define (d2g d)
 (call/cc (lambda (ret5)
 (ret5 (/ (* (d2d d)
 400.0)
 360.0)
)
)
)
)
 (define (d2m d)
 (call/cc (lambda (ret6)
 (ret6 (/ (* (d2d d)
 6400.0)
 360.0)
)
)
)
)
 (define (d2r d)
 (call/cc (lambda (ret7)
 (ret7 (/ (* (d2d d)
 3.141592653589793)
 180.0)
)
)
)
)
 (define (g2d g)
 (call/cc (lambda (ret8)
 (ret8 (/ (* (g2g g)
 360.0)
 400.0)
)
)
)
)
 (define (g2m g)
 (call/cc (lambda (ret9)
 (ret9 (/ (* (g2g g)
 6400.0)
 400.0)
)
)
)
)
 (define (g2r g)
 (call/cc (lambda (ret10)
 (ret10 (/ (* (g2g g)
 3.141592653589793)
 200.0)
)
)
)
)
 (define (m2d m)
 (call/cc (lambda (ret11)
 (ret11 (/ (* (m2m m)
 360.0)
 6400.0)
)
)
)
)
 (define (m2g m)
 (call/cc (lambda (ret12)
 (ret12 (/ (* (m2m m)
 400.0)
 6400.0)
)
)
)
)
 (define (m2r m)
 (call/cc (lambda (ret13)
 (ret13 (/ (* (m2m m)
 3.141592653589793)
 3200.0)
)
)
)
)
 (define (r2d r)
 (call/cc (lambda (ret14)
 (ret14 (/ (* (r2r r)
 180.0)
 3.141592653589793)
)
)
)
)
 (define (r2g r)
 (call/cc (lambda (ret15)
 (ret15 (/ (* (r2r r)
 200.0)
 3.141592653589793)
)
)
)
)
 (define (r2m r)
 (call/cc (lambda (ret16)
 (ret16 (/ (* (r2r r)
 3200.0)
 3.141592653589793)
)
)
)
)
 (define (main)
 (call/cc (lambda (ret17)
 (let ((angles (_list (- 2.0)
 (- 1.0)
 0.0 1.0 2.0 6.2831853 16.0 57.2957795 359.0 399.0 6399.0 1000000.0)
)
)
 (begin (display (to-str "degrees normalized_degs gradians mils radians")
)
 (newline)
 (call/cc (lambda (break19)
 (letrec ((loop18 (lambda (xs)
 (if (null? xs)
 (quote ()
)
 (begin (let ((a (car xs)
)
)
 (begin (display (to-str (string-append (string-append (string-append (string-append (string-append (string-append (string-append (string-append (to-str a)
 " ")
 (to-str (d2d a)
)
)
 " ")
 (to-str (d2g a)
)
)
 " ")
 (to-str (d2m a)
)
)
 " ")
 (to-str (d2r a)
)
)
)
)
 (newline)
)
)
 (loop18 (cdr xs)
)
)
)
)
)
)
 (loop18 angles)
)
)
)
 (display (to-str "\ngradians normalized_grds degrees mils radians")
)
 (newline)
 (call/cc (lambda (break21)
 (letrec ((loop20 (lambda (xs)
 (if (null? xs)
 (quote ()
)
 (begin (let ((a (car xs)
)
)
 (begin (display (to-str (string-append (string-append (string-append (string-append (string-append (string-append (string-append (string-append (to-str a)
 " ")
 (to-str (g2g a)
)
)
 " ")
 (to-str (g2d a)
)
)
 " ")
 (to-str (g2m a)
)
)
 " ")
 (to-str (g2r a)
)
)
)
)
 (newline)
)
)
 (loop20 (cdr xs)
)
)
)
)
)
)
 (loop20 angles)
)
)
)
 (display (to-str "\nmils normalized_mils degrees gradians radians")
)
 (newline)
 (call/cc (lambda (break23)
 (letrec ((loop22 (lambda (xs)
 (if (null? xs)
 (quote ()
)
 (begin (let ((a (car xs)
)
)
 (begin (display (to-str (string-append (string-append (string-append (string-append (string-append (string-append (string-append (string-append (to-str a)
 " ")
 (to-str (m2m a)
)
)
 " ")
 (to-str (m2d a)
)
)
 " ")
 (to-str (m2g a)
)
)
 " ")
 (to-str (m2r a)
)
)
)
)
 (newline)
)
)
 (loop22 (cdr xs)
)
)
)
)
)
)
 (loop22 angles)
)
)
)
 (display (to-str "\nradians normalized_rads degrees gradians mils")
)
 (newline)
 (call/cc (lambda (break25)
 (letrec ((loop24 (lambda (xs)
 (if (null? xs)
 (quote ()
)
 (begin (let ((a (car xs)
)
)
 (begin (display (to-str (string-append (string-append (string-append (string-append (string-append (string-append (string-append (string-append (to-str a)
 " ")
 (to-str (r2r a)
)
)
 " ")
 (to-str (r2d a)
)
)
 " ")
 (to-str (r2g a)
)
)
 " ")
 (to-str (r2m a)
)
)
)
)
 (newline)
)
)
 (loop24 (cdr xs)
)
)
)
)
)
)
 (loop24 angles)
)
)
)
)
)
)
)
)
 (main)
 (let ((end27 (now)
)
)
 (let ((dur28 (quotient (- end27 start26)
 1000)
)
)
 (begin (display (string-append "{\n  \"duration_us\": " (number->string dur28)
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
