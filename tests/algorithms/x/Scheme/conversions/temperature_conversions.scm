;; Generated on 2025-08-06 23:57 +0700
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
      start23 (
        current-jiffy
      )
    )
     (
      jps26 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        floor x
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                i (
                  let (
                    (
                      v2 x
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
             (
              begin (
                if (
                  > (
                    + 0.0 i
                  )
                   x
                )
                 (
                  begin (
                    set! i (
                      - i 1
                    )
                  )
                )
                 (
                  quote (
                    
                  )
                )
              )
               (
                ret1 (
                  + 0.0 i
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        pow10 n
      )
       (
        call/cc (
          lambda (
            ret3
          )
           (
            let (
              (
                p 1.0
              )
            )
             (
              begin (
                let (
                  (
                    i 0
                  )
                )
                 (
                  begin (
                    call/cc (
                      lambda (
                        break5
                      )
                       (
                        letrec (
                          (
                            loop4 (
                              lambda (
                                
                              )
                               (
                                if (
                                  < i n
                                )
                                 (
                                  begin (
                                    set! p (
                                      * p 10.0
                                    )
                                  )
                                   (
                                    set! i (
                                      + i 1
                                    )
                                  )
                                   (
                                    loop4
                                  )
                                )
                                 (
                                  quote (
                                    
                                  )
                                )
                              )
                            )
                          )
                        )
                         (
                          loop4
                        )
                      )
                    )
                  )
                   (
                    ret3 p
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
        round_to x ndigits
      )
       (
        call/cc (
          lambda (
            ret6
          )
           (
            let (
              (
                m (
                  pow10 ndigits
                )
              )
            )
             (
              begin (
                ret6 (
                  _div (
                    floor (
                      _add (
                        * x m
                      )
                       0.5
                    )
                  )
                   m
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        celsius_to_fahrenheit c ndigits
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            ret7 (
              round_to (
                _add (
                  _div (
                    * c 9.0
                  )
                   5.0
                )
                 32.0
              )
               ndigits
            )
          )
        )
      )
    )
     (
      define (
        celsius_to_kelvin c ndigits
      )
       (
        call/cc (
          lambda (
            ret8
          )
           (
            ret8 (
              round_to (
                + c 273.15
              )
               ndigits
            )
          )
        )
      )
    )
     (
      define (
        celsius_to_rankine c ndigits
      )
       (
        call/cc (
          lambda (
            ret9
          )
           (
            ret9 (
              round_to (
                _add (
                  _div (
                    * c 9.0
                  )
                   5.0
                )
                 491.67
              )
               ndigits
            )
          )
        )
      )
    )
     (
      define (
        fahrenheit_to_celsius f ndigits
      )
       (
        call/cc (
          lambda (
            ret10
          )
           (
            ret10 (
              round_to (
                _div (
                  * (
                    - f 32.0
                  )
                   5.0
                )
                 9.0
              )
               ndigits
            )
          )
        )
      )
    )
     (
      define (
        fahrenheit_to_kelvin f ndigits
      )
       (
        call/cc (
          lambda (
            ret11
          )
           (
            ret11 (
              round_to (
                _add (
                  _div (
                    * (
                      - f 32.0
                    )
                     5.0
                  )
                   9.0
                )
                 273.15
              )
               ndigits
            )
          )
        )
      )
    )
     (
      define (
        fahrenheit_to_rankine f ndigits
      )
       (
        call/cc (
          lambda (
            ret12
          )
           (
            ret12 (
              round_to (
                + f 459.67
              )
               ndigits
            )
          )
        )
      )
    )
     (
      define (
        kelvin_to_celsius k ndigits
      )
       (
        call/cc (
          lambda (
            ret13
          )
           (
            ret13 (
              round_to (
                - k 273.15
              )
               ndigits
            )
          )
        )
      )
    )
     (
      define (
        kelvin_to_fahrenheit k ndigits
      )
       (
        call/cc (
          lambda (
            ret14
          )
           (
            ret14 (
              round_to (
                _add (
                  _div (
                    * (
                      - k 273.15
                    )
                     9.0
                  )
                   5.0
                )
                 32.0
              )
               ndigits
            )
          )
        )
      )
    )
     (
      define (
        kelvin_to_rankine k ndigits
      )
       (
        call/cc (
          lambda (
            ret15
          )
           (
            ret15 (
              round_to (
                _div (
                  * k 9.0
                )
                 5.0
              )
               ndigits
            )
          )
        )
      )
    )
     (
      define (
        rankine_to_celsius r ndigits
      )
       (
        call/cc (
          lambda (
            ret16
          )
           (
            ret16 (
              round_to (
                _div (
                  * (
                    - r 491.67
                  )
                   5.0
                )
                 9.0
              )
               ndigits
            )
          )
        )
      )
    )
     (
      define (
        rankine_to_fahrenheit r ndigits
      )
       (
        call/cc (
          lambda (
            ret17
          )
           (
            ret17 (
              round_to (
                - r 459.67
              )
               ndigits
            )
          )
        )
      )
    )
     (
      define (
        rankine_to_kelvin r ndigits
      )
       (
        call/cc (
          lambda (
            ret18
          )
           (
            ret18 (
              round_to (
                _div (
                  * r 5.0
                )
                 9.0
              )
               ndigits
            )
          )
        )
      )
    )
     (
      define (
        reaumur_to_kelvin r ndigits
      )
       (
        call/cc (
          lambda (
            ret19
          )
           (
            ret19 (
              round_to (
                _add (
                  * r 1.25
                )
                 273.15
              )
               ndigits
            )
          )
        )
      )
    )
     (
      define (
        reaumur_to_fahrenheit r ndigits
      )
       (
        call/cc (
          lambda (
            ret20
          )
           (
            ret20 (
              round_to (
                _add (
                  * r 2.25
                )
                 32.0
              )
               ndigits
            )
          )
        )
      )
    )
     (
      define (
        reaumur_to_celsius r ndigits
      )
       (
        call/cc (
          lambda (
            ret21
          )
           (
            ret21 (
              round_to (
                * r 1.25
              )
               ndigits
            )
          )
        )
      )
    )
     (
      define (
        reaumur_to_rankine r ndigits
      )
       (
        call/cc (
          lambda (
            ret22
          )
           (
            ret22 (
              round_to (
                _add (
                  _add (
                    * r 2.25
                  )
                   32.0
                )
                 459.67
              )
               ndigits
            )
          )
        )
      )
    )
     (
      _display (
        if (
          string? (
            celsius_to_fahrenheit 0.0 2
          )
        )
         (
          celsius_to_fahrenheit 0.0 2
        )
         (
          to-str (
            celsius_to_fahrenheit 0.0 2
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
            celsius_to_kelvin 0.0 2
          )
        )
         (
          celsius_to_kelvin 0.0 2
        )
         (
          to-str (
            celsius_to_kelvin 0.0 2
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
            celsius_to_rankine 0.0 2
          )
        )
         (
          celsius_to_rankine 0.0 2
        )
         (
          to-str (
            celsius_to_rankine 0.0 2
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
            fahrenheit_to_celsius 32.0 2
          )
        )
         (
          fahrenheit_to_celsius 32.0 2
        )
         (
          to-str (
            fahrenheit_to_celsius 32.0 2
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
            fahrenheit_to_kelvin 32.0 2
          )
        )
         (
          fahrenheit_to_kelvin 32.0 2
        )
         (
          to-str (
            fahrenheit_to_kelvin 32.0 2
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
            fahrenheit_to_rankine 32.0 2
          )
        )
         (
          fahrenheit_to_rankine 32.0 2
        )
         (
          to-str (
            fahrenheit_to_rankine 32.0 2
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
            kelvin_to_celsius 273.15 2
          )
        )
         (
          kelvin_to_celsius 273.15 2
        )
         (
          to-str (
            kelvin_to_celsius 273.15 2
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
            kelvin_to_fahrenheit 273.15 2
          )
        )
         (
          kelvin_to_fahrenheit 273.15 2
        )
         (
          to-str (
            kelvin_to_fahrenheit 273.15 2
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
            kelvin_to_rankine 273.15 2
          )
        )
         (
          kelvin_to_rankine 273.15 2
        )
         (
          to-str (
            kelvin_to_rankine 273.15 2
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
            rankine_to_celsius 491.67 2
          )
        )
         (
          rankine_to_celsius 491.67 2
        )
         (
          to-str (
            rankine_to_celsius 491.67 2
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
            rankine_to_fahrenheit 491.67 2
          )
        )
         (
          rankine_to_fahrenheit 491.67 2
        )
         (
          to-str (
            rankine_to_fahrenheit 491.67 2
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
            rankine_to_kelvin 491.67 2
          )
        )
         (
          rankine_to_kelvin 491.67 2
        )
         (
          to-str (
            rankine_to_kelvin 491.67 2
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
            reaumur_to_kelvin 80.0 2
          )
        )
         (
          reaumur_to_kelvin 80.0 2
        )
         (
          to-str (
            reaumur_to_kelvin 80.0 2
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
            reaumur_to_fahrenheit 80.0 2
          )
        )
         (
          reaumur_to_fahrenheit 80.0 2
        )
         (
          to-str (
            reaumur_to_fahrenheit 80.0 2
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
            reaumur_to_celsius 80.0 2
          )
        )
         (
          reaumur_to_celsius 80.0 2
        )
         (
          to-str (
            reaumur_to_celsius 80.0 2
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
            reaumur_to_rankine 80.0 2
          )
        )
         (
          reaumur_to_rankine 80.0 2
        )
         (
          to-str (
            reaumur_to_rankine 80.0 2
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
          end24 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur25 (
              quotient (
                * (
                  - end24 start23
                )
                 1000000
              )
               jps26
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur25
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
