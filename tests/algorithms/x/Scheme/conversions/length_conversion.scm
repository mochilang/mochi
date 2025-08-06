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
      start7 (
        current-jiffy
      )
    )
     (
      jps10 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        rstrip_s s
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            begin (
              if (
                and (
                  > (
                    _len s
                  )
                   0
                )
                 (
                  string=? (
                    _substring s (
                      - (
                        _len s
                      )
                       1
                    )
                     (
                      + (
                        - (
                          _len s
                        )
                         1
                      )
                       1
                    )
                  )
                   "s"
                )
              )
               (
                begin (
                  ret1 (
                    _substring s 0 (
                      - (
                        _len s
                      )
                       1
                    )
                  )
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              ret1 s
            )
          )
        )
      )
    )
     (
      define (
        normalize_alias u
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            begin (
              if (
                string=? u "millimeter"
              )
               (
                begin (
                  ret2 "mm"
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                string=? u "centimeter"
              )
               (
                begin (
                  ret2 "cm"
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                string=? u "meter"
              )
               (
                begin (
                  ret2 "m"
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                string=? u "kilometer"
              )
               (
                begin (
                  ret2 "km"
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                string=? u "inch"
              )
               (
                begin (
                  ret2 "in"
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                string=? u "inche"
              )
               (
                begin (
                  ret2 "in"
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                string=? u "feet"
              )
               (
                begin (
                  ret2 "ft"
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                string=? u "foot"
              )
               (
                begin (
                  ret2 "ft"
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                string=? u "yard"
              )
               (
                begin (
                  ret2 "yd"
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                string=? u "mile"
              )
               (
                begin (
                  ret2 "mi"
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              ret2 u
            )
          )
        )
      )
    )
     (
      define (
        has_unit u
      )
       (
        call/cc (
          lambda (
            ret3
          )
           (
            ret3 (
              or (
                or (
                  or (
                    or (
                      or (
                        or (
                          or (
                            string=? u "mm"
                          )
                           (
                            string=? u "cm"
                          )
                        )
                         (
                          string=? u "m"
                        )
                      )
                       (
                        string=? u "km"
                      )
                    )
                     (
                      string=? u "in"
                    )
                  )
                   (
                    string=? u "ft"
                  )
                )
                 (
                  string=? u "yd"
                )
              )
               (
                string=? u "mi"
              )
            )
          )
        )
      )
    )
     (
      define (
        from_factor u
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            begin (
              if (
                string=? u "mm"
              )
               (
                begin (
                  ret4 0.001
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                string=? u "cm"
              )
               (
                begin (
                  ret4 0.01
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                string=? u "m"
              )
               (
                begin (
                  ret4 1.0
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                string=? u "km"
              )
               (
                begin (
                  ret4 1000.0
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                string=? u "in"
              )
               (
                begin (
                  ret4 0.0254
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                string=? u "ft"
              )
               (
                begin (
                  ret4 0.3048
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                string=? u "yd"
              )
               (
                begin (
                  ret4 0.9144
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                string=? u "mi"
              )
               (
                begin (
                  ret4 1609.34
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              ret4 0.0
            )
          )
        )
      )
    )
     (
      define (
        to_factor u
      )
       (
        call/cc (
          lambda (
            ret5
          )
           (
            begin (
              if (
                string=? u "mm"
              )
               (
                begin (
                  ret5 1000.0
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                string=? u "cm"
              )
               (
                begin (
                  ret5 100.0
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                string=? u "m"
              )
               (
                begin (
                  ret5 1.0
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                string=? u "km"
              )
               (
                begin (
                  ret5 0.001
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                string=? u "in"
              )
               (
                begin (
                  ret5 39.3701
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                string=? u "ft"
              )
               (
                begin (
                  ret5 3.28084
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                string=? u "yd"
              )
               (
                begin (
                  ret5 1.09361
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                string=? u "mi"
              )
               (
                begin (
                  ret5 0.000621371
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              ret5 0.0
            )
          )
        )
      )
    )
     (
      define (
        length_conversion value from_type to_type
      )
       (
        call/cc (
          lambda (
            ret6
          )
           (
            let (
              (
                new_from (
                  normalize_alias (
                    rstrip_s (
                      lower from_type
                    )
                  )
                )
              )
            )
             (
              begin (
                let (
                  (
                    new_to (
                      normalize_alias (
                        rstrip_s (
                          lower to_type
                        )
                      )
                    )
                  )
                )
                 (
                  begin (
                    if (
                      not (
                        has_unit new_from
                      )
                    )
                     (
                      begin (
                        panic (
                          string-append (
                            string-append "Invalid 'from_type' value: '" from_type
                          )
                           "'.\nConversion abbreviations are: mm, cm, m, km, in, ft, yd, mi"
                        )
                      )
                    )
                     (
                      quote (
                        
                      )
                    )
                  )
                   (
                    if (
                      not (
                        has_unit new_to
                      )
                    )
                     (
                      begin (
                        panic (
                          string-append (
                            string-append "Invalid 'to_type' value: '" to_type
                          )
                           "'.\nConversion abbreviations are: mm, cm, m, km, in, ft, yd, mi"
                        )
                      )
                    )
                     (
                      quote (
                        
                      )
                    )
                  )
                   (
                    ret6 (
                      * (
                        * value (
                          from_factor new_from
                        )
                      )
                       (
                        to_factor new_to
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
            length_conversion 4.0 "METER" "FEET"
          )
        )
         (
          length_conversion 4.0 "METER" "FEET"
        )
         (
          to-str (
            length_conversion 4.0 "METER" "FEET"
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
            length_conversion 1.0 "kilometer" "inch"
          )
        )
         (
          length_conversion 1.0 "kilometer" "inch"
        )
         (
          to-str (
            length_conversion 1.0 "kilometer" "inch"
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
            length_conversion 2.0 "feet" "meter"
          )
        )
         (
          length_conversion 2.0 "feet" "meter"
        )
         (
          to-str (
            length_conversion 2.0 "feet" "meter"
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
            length_conversion 2.0 "centimeter" "millimeter"
          )
        )
         (
          length_conversion 2.0 "centimeter" "millimeter"
        )
         (
          to-str (
            length_conversion 2.0 "centimeter" "millimeter"
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
            length_conversion 4.0 "yard" "kilometer"
          )
        )
         (
          length_conversion 4.0 "yard" "kilometer"
        )
         (
          to-str (
            length_conversion 4.0 "yard" "kilometer"
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
            length_conversion 3.0 "foot" "inch"
          )
        )
         (
          length_conversion 3.0 "foot" "inch"
        )
         (
          to-str (
            length_conversion 3.0 "foot" "inch"
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
            length_conversion 3.0 "mm" "in"
          )
        )
         (
          length_conversion 3.0 "mm" "in"
        )
         (
          to-str (
            length_conversion 3.0 "mm" "in"
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
          end8 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur9 (
              quotient (
                * (
                  - end8 start7
                )
                 1000000
              )
               jps10
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur9
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
