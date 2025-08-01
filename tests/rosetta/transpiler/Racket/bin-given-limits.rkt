;; Generated by Mochi 0.10.41 on 2025-07-27 15:57 +0700
#lang racket/base
(require racket/list racket/string racket/math racket/match json)
(define nowSeed (let ([s (getenv "MOCHI_NOW_SEED")]) (and s (string->number s))))
(define (now)
  (if nowSeed
      (begin (set! nowSeed (modulo (+ (* nowSeed 1664525) 1013904223) 2147483647)) nowSeed)
      (inexact->exact (floor (* (current-inexact-milliseconds) 1000)))))
(define (int x)
  (cond
    [(integer? x) x]
    [(number? x) (inexact->exact (truncate x))]
    [(string? x) (let ([n (string->number x)]) (if n (inexact->exact (truncate n)) 0))]
    [else 0]))
(define (float x)
  (cond
    [(number? x) (exact->inexact x)]
    [(string? x) (let ([n (string->number x)]) (if n (exact->inexact n) 0.0))]
    [else 0.0]))
(define (input) (read-line))
(define (upper s) (string-upcase s))
(define (lower s) (string-downcase s))
(define (sublist lst start end)
  (if (string? lst)
      (substring lst start end)
      (take (drop lst start) (- end start))))

(define (pad-start s width ch)
  (if (< (string-length s) width)
      (string-append (make-string (- width (string-length s)) (string-ref ch 0)) s)
      s))

(let* ([_start_mem (current-memory-use)] [_start (now)])
(define (getBins limits data)
  (let/ec _return (begin
(define n (cond [(string? limits) (string-length limits)] [(hash? limits) (hash-count limits)] [else (length limits)]))
(define bins (list))
(define i 0)
(let/ec _break (let loop ()
  (if (< i (let ([__l n] [__r 1]) (if (and (string? __l) (string? __r)) (string-append __l __r) (+ __l __r)))) (let ()
    (set! bins (append bins (list 0)))
    (set! i (let ([__l i] [__r 1]) (if (and (string? __l) (string? __r)) (string-append __l __r) (+ __l __r))))
    (loop)) (void))))
(define j 0)
(let/ec _break (let loop ()
  (if (< j (cond [(string? data) (string-length data)] [(hash? data) (hash-count data)] [else (length data)])) (let ()
    (define d (list-ref data j))
    (define index 0)
    (let/ec _break (let loop ()
  (if (< index (cond [(string? limits) (string-length limits)] [(hash? limits) (hash-count limits)] [else (length limits)])) (let ()
    (if (< d (list-ref limits index)) (let ()
(_break)
) (void))
    (if (equal? d (list-ref limits index)) (let ()
(set! index (let ([__l index] [__r 1]) (if (and (string? __l) (string? __r)) (string-append __l __r) (+ __l __r))))
(_break)
) (void))
    (set! index (let ([__l index] [__r 1]) (if (and (string? __l) (string? __r)) (string-append __l __r) (+ __l __r))))
    (loop)) (void))))
    (set! bins (list-set bins index (let ([__l (list-ref bins index)] [__r 1]) (if (and (string? __l) (string? __r)) (string-append __l __r) (+ __l __r)))))
    (set! j (let ([__l j] [__r 1]) (if (and (string? __l) (string? __r)) (string-append __l __r) (+ __l __r))))
    (loop)) (void))))
(_return bins)
))
)
(define (padLeft n width)
  (let/ec _return (begin
(define s (format "~a" n))
(define pad (- width (cond [(string? s) (string-length s)] [(hash? s) (hash-count s)] [else (length s)])))
(define out "")
(define i 0)
(let/ec _break (let loop ()
  (if (< i pad) (let ()
    (set! out (string-append out " "))
    (set! i (let ([__l i] [__r 1]) (if (and (string? __l) (string? __r)) (string-append __l __r) (+ __l __r))))
    (loop)) (void))))
(_return (string-append out s))
))
)
(define (printBins limits bins)
  (let/ec _return (begin
(define n (cond [(string? limits) (string-length limits)] [(hash? limits) (hash-count limits)] [else (length limits)]))
(displayln (string-append (string-append (string-append "           < " (padLeft (list-ref limits 0) 3)) " = ") (padLeft (list-ref bins 0) 2)))
(define i 1)
(let/ec _break (let loop ()
  (if (< i n) (let ()
    (displayln (string-append (string-append (string-append (string-append (string-append ">= " (padLeft (list-ref limits (- i 1)) 3)) " and < ") (padLeft (list-ref limits i) 3)) " = ") (padLeft (list-ref bins i) 2)))
    (set! i (let ([__l i] [__r 1]) (if (and (string? __l) (string? __r)) (string-append __l __r) (+ __l __r))))
    (loop)) (void))))
(displayln (string-append (string-append (string-append ">= " (padLeft (list-ref limits (- n 1)) 3)) "           = ") (padLeft (list-ref bins n) 2)))
(displayln "")
))
)
(define (main)
  (let/ec _return (begin
(define limitsList (list (list 23 37 43 53 67 83) (list 14 18 249 312 389 392 513 591 634 720)))
(define dataList (list (list 95 21 94 12 99 4 70 75 83 93 52 80 57 5 53 86 65 17 92 83 71 61 54 58 47 16 8 9 32 84 7 87 46 19 30 37 96 6 98 40 79 97 45 64 60 29 49 36 43 55) (list 445 814 519 697 700 130 255 889 481 122 932 77 323 525 570 219 367 523 442 933 416 589 930 373 202 253 775 47 731 685 293 126 133 450 545 100 741 583 763 306 655 267 248 477 549 238 62 678 98 534 622 907 406 714 184 391 913 42 560 247 346 860 56 138 546 38 985 948 58 213 799 319 390 634 458 945 733 507 916 123 345 110 720 917 313 845 426 9 457 628 410 723 354 895 881 953 677 137 397 97 854 740 83 216 421 94 517 479 292 963 376 981 480 39 257 272 157 5 316 395 787 942 456 242 759 898 576 67 298 425 894 435 831 241 989 614 987 770 384 692 698 765 331 487 251 600 879 342 982 527 736 795 585 40 54 901 408 359 577 237 605 847 353 968 832 205 838 427 876 959 686 646 835 127 621 892 443 198 988 791 466 23 707 467 33 670 921 180 991 396 160 436 717 918 8 374 101 684 727 749)))
(define i 0)
(let/ec _break (let loop ()
  (if (< i (cond [(string? limitsList) (string-length limitsList)] [(hash? limitsList) (hash-count limitsList)] [else (length limitsList)])) (let ()
    (displayln (string-append (string-append "Example " (format "~a" (let ([__l i] [__r 1]) (if (and (string? __l) (string? __r)) (string-append __l __r) (+ __l __r))))) "\n"))
    (define bins (getBins (list-ref limitsList i) (list-ref dataList i)))
    (printBins (list-ref limitsList i) bins)
    (set! i (let ([__l i] [__r 1]) (if (and (string? __l) (string? __r)) (string-append __l __r) (+ __l __r))))
    (loop)) (void))))
))
)
(main)
  (let* ([_end (now)] [_end_mem (current-memory-use)]
         [_dur (- _end _start)]
         [_dur_us _dur]
         [_mem (max 0 (- _end_mem _start_mem))])
    (displayln "{")
    (displayln (format "  \"duration_us\": ~a," _dur_us))
    (displayln (format "  \"memory_bytes\": ~a," _mem))
    (displayln "  \"name\": \"main\"")
    (displayln "}")
  )
)
