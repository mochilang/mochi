;; Generated by Mochi 0.10.39 on 2025-07-24 18:38 +0700
#lang racket/base
(require racket/list racket/string racket/math racket/match json)
(define nowSeed (let ([s (getenv "MOCHI_NOW_SEED")]) (and s (string->number s))))
(define (now)
  (if nowSeed
      (begin (set! nowSeed (modulo (+ (* nowSeed 1664525) 1013904223) 2147483647)) nowSeed)
      (inexact->exact (floor (current-inexact-milliseconds)))))
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

(define (pixelFromRgb c)
  (let/ec _return (begin
(define r (modulo (int (quotient c 65536)) 256))
(define g (modulo (int (quotient c 256)) 256))
(define b (modulo c 256))
(_return (hash "R" r "G" g "B" b))
))
)
(define (rgbFromPixel p)
  (let/ec _return (begin
(_return (let ([__l (let ([__l (* (if p (hash-ref p "R" #f) #f) 65536)] [__r (* (if p (hash-ref p "G" #f) #f) 256)]) (if (and (string? __l) (string? __r)) (string-append __l __r) (+ __l __r)))] [__r (if p (hash-ref p "B" #f) #f)]) (if (and (string? __l) (string? __r)) (string-append __l __r) (+ __l __r))))
))
)
(define (NewBitmap x y)
  (let/ec _return (begin
(define data (list))
(define row 0)
(let/ec _break (let loop ()
  (if (< row y) (let ()
    (define r (list))
    (define col 0)
    (let/ec _break (let loop ()
  (if (< col x) (let ()
    (set! r (append r (list (hash "R" 0 "G" 0 "B" 0))))
    (set! col (let ([__l col] [__r 1]) (if (and (string? __l) (string? __r)) (string-append __l __r) (+ __l __r))))
    (loop)) (void))))
    (set! data (append data (list r)))
    (set! row (let ([__l row] [__r 1]) (if (and (string? __l) (string? __r)) (string-append __l __r) (+ __l __r))))
    (loop)) (void))))
(_return (hash "cols" x "rows" y "px" data))
))
)
(define (Extent b)
  (let/ec _return (begin
(_return (hash "cols" (if b (hash-ref b "cols" #f) #f) "rows" (if b (hash-ref b "rows" #f) #f)))
))
)
(define (Fill b p)
  (let/ec _return (begin
(define y 0)
(let/ec _break (let loop ()
  (if (< y (if b (hash-ref b "rows" #f) #f)) (let ()
    (define x 0)
    (let/ec _break (let loop ()
  (if (< x (if b (hash-ref b "cols" #f) #f)) (let ()
    (define px (if b (hash-ref b "px" #f) #f))
    (define row (list-ref px y))
    (set! row (list-set row x p))
    (set! px (list-set px y row))
    (set! b (hash-set (or b (hash)) "px" px))
    (set! x (let ([__l x] [__r 1]) (if (and (string? __l) (string? __r)) (string-append __l __r) (+ __l __r))))
    (loop)) (void))))
    (set! y (let ([__l y] [__r 1]) (if (and (string? __l) (string? __r)) (string-append __l __r) (+ __l __r))))
    (loop)) (void))))
))
)
(define (FillRgb b c)
  (let/ec _return (begin
(Fill b (pixelFromRgb c))
))
)
(define (SetPx b x y p)
  (let/ec _return (begin
(if (or (or (or (< x 0) (>= x (if b (hash-ref b "cols" #f) #f))) (< y 0)) (>= y (if b (hash-ref b "rows" #f) #f))) (let ()
(_return #f)
) (void))
(define px (if b (hash-ref b "px" #f) #f))
(define row (list-ref px y))
(set! row (list-set row x p))
(set! px (list-set px y row))
(set! b (hash-set (or b (hash)) "px" px))
(_return #t)
))
)
(define (SetPxRgb b x y c)
  (let/ec _return (begin
(_return (SetPx b x y (pixelFromRgb c)))
))
)
(define (GetPx b x y)
  (let/ec _return (begin
(if (or (or (or (< x 0) (>= x (if b (hash-ref b "cols" #f) #f))) (< y 0)) (>= y (if b (hash-ref b "rows" #f) #f))) (let ()
(_return (hash "ok" #f))
) (void))
(define row (list-ref (if b (hash-ref b "px" #f) #f) y))
(_return (hash "ok" #t "pixel" (list-ref row x)))
))
)
(define (GetPxRgb b x y)
  (let/ec _return (begin
(define r (GetPx b x y))
(if (not (if r (hash-ref r "ok" #f) #f)) (let ()
(_return (hash "ok" #f))
) (void))
(_return (hash "ok" #t "rgb" (rgbFromPixel (if r (hash-ref r "pixel" #f) #f))))
))
)
(define (ppmSize b)
  (let/ec _return (begin
(define header (string-append (string-append (string-append (string-append "P6\n# Creator: Rosetta Code http://rosettacode.org/\n" (format "~a" (if b (hash-ref b "cols" #f) #f))) " ") (format "~a" (if b (hash-ref b "rows" #f) #f))) "\n255\n"))
(_return (let ([__l (cond [(string? header) (string-length header)] [(hash? header) (hash-count header)] [else (length header)])] [__r (* (* 3 (if b (hash-ref b "cols" #f) #f)) (if b (hash-ref b "rows" #f) #f))]) (if (and (string? __l) (string? __r)) (string-append __l __r) (+ __l __r))))
))
)
(define (pixelStr p)
  (let/ec _return (begin
(_return (string-append (string-append (string-append (string-append (string-append (string-append "{" (format "~a" (if p (hash-ref p "R" #f) #f))) " ") (format "~a" (if p (hash-ref p "G" #f) #f))) " ") (format "~a" (if p (hash-ref p "B" #f) #f))) "}"))
))
)
(define (main)
  (let/ec _return (begin
(define bm (NewBitmap 300 240))
(FillRgb bm 16711680)
(SetPxRgb bm 10 20 255)
(SetPxRgb bm 20 30 0)
(SetPxRgb bm 30 40 1056816)
(define c1 (GetPx bm 0 0))
(define c2 (GetPx bm 10 20))
(define c3 (GetPx bm 30 40))
(displayln (string-append (string-append (string-append "Image size: " (format "~a" (if bm (hash-ref bm "cols" #f) #f))) " × ") (format "~a" (if bm (hash-ref bm "rows" #f) #f))))
(displayln (string-append (format "~a" (ppmSize bm)) " bytes when encoded as PPM."))
(if (if c1 (hash-ref c1 "ok" #f) #f) (let ()
(displayln (string-append "Pixel at (0,0) is " (pixelStr (if c1 (hash-ref c1 "pixel" #f) #f))))
) (void))
(if (if c2 (hash-ref c2 "ok" #f) #f) (let ()
(displayln (string-append "Pixel at (10,20) is " (pixelStr (if c2 (hash-ref c2 "pixel" #f) #f))))
) (void))
(if (if c3 (hash-ref c3 "ok" #f) #f) (let ()
(define p (if c3 (hash-ref c3 "pixel" #f) #f))
(define r16 (* (if p (hash-ref p "R" #f) #f) 257))
(define g16 (* (if p (hash-ref p "G" #f) #f) 257))
(define b16 (* (if p (hash-ref p "B" #f) #f) 257))
(displayln (string-append (string-append (string-append (string-append (string-append "Pixel at (30,40) has R=" (format "~a" r16)) ", G=") (format "~a" g16)) ", B=") (format "~a" b16)))
) (void))
))
)
(main)
