;; Generated on 2025-08-07 16:45 +0700
(import (scheme base))
(import (scheme time))
(import (chibi string))
(import (only (scheme char) string-upcase string-downcase))
(import (srfi 69))
(import (srfi 1))
(define _list list)
(import (chibi time))
(define (_mem) (* 1024 (resource-usage-max-rss (get-resource-usage resource-usage/self))))
(import (chibi json))
(define (to-str x)
  (cond ((pair? x)
         (string-append "[" (string-join (map to-str x) ", ") "]"))
        ((hash-table? x)
         (let* ((ks (hash-table-keys x))
                (pairs (map (lambda (k)
                              (string-append (to-str k) ": " (to-str (hash-table-ref x k))))
                            ks)))
           (string-append "{" (string-join pairs ", ") "}")))
        ((null? x) "[]")
        ((string? x) (let ((out (open-output-string))) (json-write x out) (get-output-string out)))
        ((boolean? x) (if x "true" "false"))
        ((number? x)
         (if (integer? x)
             (number->string (inexact->exact x))
             (number->string x)))
        (else (number->string x))))
(define (to-str-space x)
  (cond ((pair? x)
         (string-append "[" (string-join (map to-str-space x) " ") "]"))
        ((string? x) x)
        (else (to-str x))))
(define (upper s) (string-upcase s))
(define (lower s) (string-downcase s))
(define (fmod a b) (- a (* (floor (/ a b)) b)))
(define (_mod a b) (if (and (integer? a) (integer? b)) (modulo a b) (fmod a b)))
(define (_div a b) (if (and (integer? a) (integer? b) (exact? a) (exact? b)) (quotient a b) (/ a b)))
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
(define (panic msg) (error msg))
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
(define (_len x)
  (cond ((string? x) (string-length x))
        ((hash-table? x) (hash-table-size x))
        (else (length x))))
(define (list-ref-safe lst idx) (if (and (integer? idx) (>= idx 0) (< idx (length lst))) (list-ref lst idx) '()))
(
  let (
    (
      start6 (
        current-jiffy
      )
    )
     (
      jps9 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        doppler_effect org_freq wave_vel obs_vel src_vel
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            begin (
              if (
                equal? wave_vel src_vel
              )
               (
                begin (
                  panic "division by zero implies vs=v and observer in front of the source"
                )
              )
               '(
                
              )
            )
             (
              let (
                (
                  doppler_freq (
                    _div (
                      * org_freq (
                        + wave_vel obs_vel
                      )
                    )
                     (
                      - wave_vel src_vel
                    )
                  )
                )
              )
               (
                begin (
                  if (
                    <= doppler_freq 0.0
                  )
                   (
                    begin (
                      panic "non-positive frequency implies vs>v or v0>v (in the opposite direction)"
                    )
                  )
                   '(
                    
                  )
                )
                 (
                  ret1 doppler_freq
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        absf x
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            begin (
              if (
                < x 0.0
              )
               (
                begin (
                  ret2 (
                    - x
                  )
                )
              )
               '(
                
              )
            )
             (
              ret2 x
            )
          )
        )
      )
    )
     (
      define (
        almost_equal a b tol
      )
       (
        call/cc (
          lambda (
            ret3
          )
           (
            ret3 (
              _le (
                absf (
                  - a b
                )
              )
               tol
            )
          )
        )
      )
    )
     (
      define (
        test_doppler_effect
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            begin (
              if (
                not (
                  almost_equal (
                    doppler_effect 100.0 330.0 10.0 0.0
                  )
                   103.03030303030303 1e-07
                )
              )
               (
                begin (
                  panic "test 1 failed"
                )
              )
               '(
                
              )
            )
             (
              if (
                not (
                  almost_equal (
                    doppler_effect 100.0 330.0 (
                      - 10.0
                    )
                     0.0
                  )
                   96.96969696969697 1e-07
                )
              )
               (
                begin (
                  panic "test 2 failed"
                )
              )
               '(
                
              )
            )
             (
              if (
                not (
                  almost_equal (
                    doppler_effect 100.0 330.0 0.0 10.0
                  )
                   103.125 1e-07
                )
              )
               (
                begin (
                  panic "test 3 failed"
                )
              )
               '(
                
              )
            )
             (
              if (
                not (
                  almost_equal (
                    doppler_effect 100.0 330.0 0.0 (
                      - 10.0
                    )
                  )
                   97.05882352941177 1e-07
                )
              )
               (
                begin (
                  panic "test 4 failed"
                )
              )
               '(
                
              )
            )
             (
              if (
                not (
                  almost_equal (
                    doppler_effect 100.0 330.0 10.0 10.0
                  )
                   106.25 1e-07
                )
              )
               (
                begin (
                  panic "test 5 failed"
                )
              )
               '(
                
              )
            )
             (
              if (
                not (
                  almost_equal (
                    doppler_effect 100.0 330.0 (
                      - 10.0
                    )
                     (
                      - 10.0
                    )
                  )
                   94.11764705882354 1e-07
                )
              )
               (
                begin (
                  panic "test 6 failed"
                )
              )
               '(
                
              )
            )
          )
        )
      )
    )
     (
      define (
        main
      )
       (
        call/cc (
          lambda (
            ret5
          )
           (
            begin (
              test_doppler_effect
            )
             (
              _display (
                if (
                  string? (
                    doppler_effect 100.0 330.0 10.0 0.0
                  )
                )
                 (
                  doppler_effect 100.0 330.0 10.0 0.0
                )
                 (
                  to-str (
                    doppler_effect 100.0 330.0 10.0 0.0
                  )
                )
              )
            )
             (
              newline
            )
          )
        )
      )
    )
     (
      main
    )
     (
      let (
        (
          end7 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur8 (
              quotient (
                * (
                  - end7 start6
                )
                 1000000
              )
               jps9
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur8
              )
               ",\n  \"memory_bytes\": " (
                number->string (
                  _mem
                )
              )
               ",\n  \"name\": \"main\"\n}"
            )
          )
           (
            newline
          )
        )
      )
    )
  )
)
