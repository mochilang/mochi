;; Generated on 2025-08-06 23:15 +0700
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
(define (_div a b) (if (and (integer? a) (integer? b)) (quotient a b) (/ a b)))
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
(define (_len x)
  (cond ((string? x) (string-length x))
        ((hash-table? x) (hash-table-size x))
        (else (length x))))
(
  let (
    (
      start4 (
        current-jiffy
      )
    )
     (
      jps7 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        round_int x
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            ret1 (
              let (
                (
                  v2 (
                    + x 0.5
                  )
                )
              )
               (
                cond (
                  (
                    string? v2
                  )
                   (
                    inexact->exact (
                      floor (
                        string->number v2
                      )
                    )
                  )
                )
                 (
                  (
                    boolean? v2
                  )
                   (
                    if v2 1 0
                  )
                )
                 (
                  else (
                    inexact->exact (
                      floor v2
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        rgb_to_cmyk r_input g_input b_input
      )
       (
        call/cc (
          lambda (
            ret3
          )
           (
            begin (
              if (
                or (
                  or (
                    or (
                      or (
                        or (
                          < r_input 0
                        )
                         (
                          >= r_input 256
                        )
                      )
                       (
                        < g_input 0
                      )
                    )
                     (
                      >= g_input 256
                    )
                  )
                   (
                    < b_input 0
                  )
                )
                 (
                  >= b_input 256
                )
              )
               (
                begin (
                  panic "Expected int of the range 0..255"
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              let (
                (
                  r (
                    _div (
                      + 0.0 r_input
                    )
                     255.0
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      g (
                        _div (
                          + 0.0 g_input
                        )
                         255.0
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          b (
                            _div (
                              + 0.0 b_input
                            )
                             255.0
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              max_val r
                            )
                          )
                           (
                            begin (
                              if (
                                > g max_val
                              )
                               (
                                begin (
                                  set! max_val g
                                )
                              )
                               (
                                quote (
                                  
                                )
                              )
                            )
                             (
                              if (
                                > b max_val
                              )
                               (
                                begin (
                                  set! max_val b
                                )
                              )
                               (
                                quote (
                                  
                                )
                              )
                            )
                             (
                              let (
                                (
                                  k_float (
                                    - 1.0 max_val
                                  )
                                )
                              )
                               (
                                begin (
                                  if (
                                    equal? k_float 1.0
                                  )
                                   (
                                    begin (
                                      ret3 (
                                        _list 0 0 0 100
                                      )
                                    )
                                  )
                                   (
                                    quote (
                                      
                                    )
                                  )
                                )
                                 (
                                  let (
                                    (
                                      c_float (
                                        _div (
                                          * 100.0 (
                                            - (
                                              - 1.0 r
                                            )
                                             k_float
                                          )
                                        )
                                         (
                                          - 1.0 k_float
                                        )
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      let (
                                        (
                                          m_float (
                                            _div (
                                              * 100.0 (
                                                - (
                                                  - 1.0 g
                                                )
                                                 k_float
                                              )
                                            )
                                             (
                                              - 1.0 k_float
                                            )
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          let (
                                            (
                                              y_float (
                                                _div (
                                                  * 100.0 (
                                                    - (
                                                      - 1.0 b
                                                    )
                                                     k_float
                                                  )
                                                )
                                                 (
                                                  - 1.0 k_float
                                                )
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              let (
                                                (
                                                  k_percent (
                                                    * 100.0 k_float
                                                  )
                                                )
                                              )
                                               (
                                                begin (
                                                  let (
                                                    (
                                                      c (
                                                        round_int c_float
                                                      )
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      let (
                                                        (
                                                          m (
                                                            round_int m_float
                                                          )
                                                        )
                                                      )
                                                       (
                                                        begin (
                                                          let (
                                                            (
                                                              y (
                                                                round_int y_float
                                                              )
                                                            )
                                                          )
                                                           (
                                                            begin (
                                                              let (
                                                                (
                                                                  k (
                                                                    round_int k_percent
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                begin (
                                                                  ret3 (
                                                                    _list c m y k
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
          )
        )
      )
    )
     (
      _display (
        if (
          string? (
            rgb_to_cmyk 255 255 255
          )
        )
         (
          rgb_to_cmyk 255 255 255
        )
         (
          to-str (
            rgb_to_cmyk 255 255 255
          )
        )
      )
    )
     (
      newline
    )
     (
      _display (
        if (
          string? (
            rgb_to_cmyk 128 128 128
          )
        )
         (
          rgb_to_cmyk 128 128 128
        )
         (
          to-str (
            rgb_to_cmyk 128 128 128
          )
        )
      )
    )
     (
      newline
    )
     (
      _display (
        if (
          string? (
            rgb_to_cmyk 0 0 0
          )
        )
         (
          rgb_to_cmyk 0 0 0
        )
         (
          to-str (
            rgb_to_cmyk 0 0 0
          )
        )
      )
    )
     (
      newline
    )
     (
      _display (
        if (
          string? (
            rgb_to_cmyk 255 0 0
          )
        )
         (
          rgb_to_cmyk 255 0 0
        )
         (
          to-str (
            rgb_to_cmyk 255 0 0
          )
        )
      )
    )
     (
      newline
    )
     (
      _display (
        if (
          string? (
            rgb_to_cmyk 0 255 0
          )
        )
         (
          rgb_to_cmyk 0 255 0
        )
         (
          to-str (
            rgb_to_cmyk 0 255 0
          )
        )
      )
    )
     (
      newline
    )
     (
      _display (
        if (
          string? (
            rgb_to_cmyk 0 0 255
          )
        )
         (
          rgb_to_cmyk 0 0 255
        )
         (
          to-str (
            rgb_to_cmyk 0 0 255
          )
        )
      )
    )
     (
      newline
    )
     (
      let (
        (
          end5 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur6 (
              quotient (
                * (
                  - end5 start4
                )
                 1000000
              )
               jps7
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur6
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
