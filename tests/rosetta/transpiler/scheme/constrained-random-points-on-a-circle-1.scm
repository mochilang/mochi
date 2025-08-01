;; Generated on 2025-08-01 19:22 +0700
(import (only (scheme base) call/cc when list-ref list-set!))
(import (rename (scheme base) (list _list)))
(import (scheme time))
(import (chibi string))
(import (only (scheme char) string-upcase string-downcase))
(import (scheme write))
(import (srfi 69))
(import (srfi 1))
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
(define (_substring s start end)
  (let* ((len (string-length s))
         (s0 (max 0 (min len start)))
         (e0 (max s0 (min len end))))
    (substring s s0 e0)))
(define (_repeat s n)
  (let loop ((i 0) (out ""))
    (if (< i n)
        (loop (+ i 1) (string-append out s))
        out)))
(define (slice seq start end)
  (let* ((len (if (string? seq) (string-length seq) (length seq)))
         (s (if (< start 0) (+ len start) start))
         (e (if (< end 0) (+ len end) end)))
    (set! s (max 0 (min len s)))
    (set! e (max 0 (min len e)))
    (when (< e s) (set! e s))
    (if (string? seq)
        (_substring seq s e)
        (take (drop seq s) (- e s)))))
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
          (let ((cur (string-contains r del)))
            (if cur
                (let ((idx (string-cursor->index r cur)))
                  (loop (_substring r (+ idx (string-length del)) (string-length r))
                        (cons (_substring r 0 idx) acc)))
                (reverse (cons r acc)))))))))
(let ((start11 (now))) (begin (define nPts 100) (define rMin 10) (define rMax 15) (define span (_add (+ rMax 1) rMax)) (define rows (_list)) (define r 0) (call/cc (lambda (break2) (letrec ((loop1 (lambda () (if (< r span) (begin (let ((row (_list))) (begin (let ((c 0)) (begin (call/cc (lambda (break4) (letrec ((loop3 (lambda () (if (_lt c (* span 2)) (begin (set! row (append row (_list " "))) (set! c (+ c 1)) (loop3)) (quote ()))))) (loop3)))) (set! rows (append rows (_list row))) (set! r (+ r 1)))))) (loop1)) (quote ()))))) (loop1)))) (define u 0) (define seen (alist->hash-table (_list))) (define min2 (* rMin rMin)) (define max2 (* rMax rMax)) (define n 0) (call/cc (lambda (break6) (letrec ((loop5 (lambda () (if (< n nPts) (begin (let ((x (- (modulo (now) span) rMax))) (begin (let ((y (- (modulo (now) span) rMax))) (begin (let ((rs (_add (* x x) (* y y)))) (begin (if (or (< rs min2) (> rs max2)) (begin (loop5)) (quote ())) (set! n (+ n 1)) (let ((row (+ y rMax))) (begin (let ((col (* (+ x rMax) 2))) (begin (list-set! (list-ref rows row) col "*") (let ((key (string-append (string-append (to-str row) ",") (to-str col)))) (begin (if (not (hash-table-ref/default seen key (quote ()))) (begin (hash-table-set! seen key #t) (set! u (+ u 1))) (quote ())))))))))))))) (loop5)) (quote ()))))) (loop5)))) (define i 0) (call/cc (lambda (break8) (letrec ((loop7 (lambda () (if (< i span) (begin (let ((line "")) (begin (let ((j 0)) (begin (call/cc (lambda (break10) (letrec ((loop9 (lambda () (if (_lt j (* span 2)) (begin (set! line (string-append line (cond ((string? (list-ref rows i)) (_substring (list-ref rows i) j (+ j 1))) ((hash-table? (list-ref rows i)) (hash-table-ref (list-ref rows i) j)) (else (list-ref (list-ref rows i) j))))) (set! j (+ j 1)) (loop9)) (quote ()))))) (loop9)))) (_display (to-str line)) (newline) (set! i (+ i 1)))))) (loop7)) (quote ()))))) (loop7)))) (_display (to-str (string-append (to-str u) " unique points"))) (newline) (let ((end12 (now))) (let ((dur13 (quotient (- end12 start11) 1000))) (begin (_display (string-append "{\n  \"duration_us\": " (number->string dur13) ",\n  \"memory_bytes\": " (number->string (_mem)) ",\n  \"name\": \"main\"\n}")) (newline))))))
