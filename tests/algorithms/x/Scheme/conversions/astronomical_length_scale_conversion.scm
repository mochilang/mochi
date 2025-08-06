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
      let (
        (
          UNIT_SYMBOL (
            alist->hash-table (
              _list (
                cons "meter" "m"
              )
               (
                cons "kilometer" "km"
              )
               (
                cons "megametre" "Mm"
              )
               (
                cons "gigametre" "Gm"
              )
               (
                cons "terametre" "Tm"
              )
               (
                cons "petametre" "Pm"
              )
               (
                cons "exametre" "Em"
              )
               (
                cons "zettametre" "Zm"
              )
               (
                cons "yottametre" "Ym"
              )
            )
          )
        )
      )
       (
        begin (
          let (
            (
              METRIC_CONVERSION (
                alist->hash-table (
                  _list (
                    cons "m" 0
                  )
                   (
                    cons "km" 3
                  )
                   (
                    cons "Mm" 6
                  )
                   (
                    cons "Gm" 9
                  )
                   (
                    cons "Tm" 12
                  )
                   (
                    cons "Pm" 15
                  )
                   (
                    cons "Em" 18
                  )
                   (
                    cons "Zm" 21
                  )
                   (
                    cons "Ym" 24
                  )
                )
              )
            )
          )
           (
            begin (
              let (
                (
                  ABBREVIATIONS "m, km, Mm, Gm, Tm, Pm, Em, Zm, Ym"
                )
              )
               (
                begin (
                  define (
                    sanitize unit
                  )
                   (
                    call/cc (
                      lambda (
                        ret1
                      )
                       (
                        let (
                          (
                            res (
                              lower unit
                            )
                          )
                        )
                         (
                          begin (
                            if (
                              > (
                                _len res
                              )
                               0
                            )
                             (
                              begin (
                                let (
                                  (
                                    last (
                                      _substring res (
                                        - (
                                          _len res
                                        )
                                         1
                                      )
                                       (
                                        _len res
                                      )
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      string=? last "s"
                                    )
                                     (
                                      begin (
                                        set! res (
                                          _substring res 0 (
                                            - (
                                              _len res
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
                              cond (
                                (
                                  string? UNIT_SYMBOL
                                )
                                 (
                                  if (
                                    string-contains UNIT_SYMBOL res
                                  )
                                   #t #f
                                )
                              )
                               (
                                (
                                  hash-table? UNIT_SYMBOL
                                )
                                 (
                                  if (
                                    hash-table-exists? UNIT_SYMBOL res
                                  )
                                   #t #f
                                )
                              )
                               (
                                else (
                                  if (
                                    member res UNIT_SYMBOL
                                  )
                                   #t #f
                                )
                              )
                            )
                             (
                              begin (
                                ret1 (
                                  hash-table-ref/default UNIT_SYMBOL res (
                                    quote (
                                      
                                    )
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
                            ret1 res
                          )
                        )
                      )
                    )
                  )
                )
                 (
                  define (
                    pow10 exp
                  )
                   (
                    call/cc (
                      lambda (
                        ret2
                      )
                       (
                        begin (
                          if (
                            equal? exp 0
                          )
                           (
                            begin (
                              ret2 1.0
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
                              e exp
                            )
                          )
                           (
                            begin (
                              let (
                                (
                                  res 1.0
                                )
                              )
                               (
                                begin (
                                  if (
                                    < e 0
                                  )
                                   (
                                    begin (
                                      set! e (
                                        - e
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
                                      i 0
                                    )
                                  )
                                   (
                                    begin (
                                      call/cc (
                                        lambda (
                                          break4
                                        )
                                         (
                                          letrec (
                                            (
                                              loop3 (
                                                lambda (
                                                  
                                                )
                                                 (
                                                  if (
                                                    < i e
                                                  )
                                                   (
                                                    begin (
                                                      set! res (
                                                        * res 10.0
                                                      )
                                                    )
                                                     (
                                                      set! i (
                                                        + i 1
                                                      )
                                                    )
                                                     (
                                                      loop3
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
                                            loop3
                                          )
                                        )
                                      )
                                    )
                                     (
                                      if (
                                        < exp 0
                                      )
                                       (
                                        begin (
                                          ret2 (
                                            _div 1.0 res
                                          )
                                        )
                                      )
                                       (
                                        quote (
                                          
                                        )
                                      )
                                    )
                                     (
                                      ret2 res
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
                  define (
                    length_conversion value from_type to_type
                  )
                   (
                    call/cc (
                      lambda (
                        ret5
                      )
                       (
                        let (
                          (
                            from_sanitized (
                              sanitize from_type
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                to_sanitized (
                                  sanitize to_type
                                )
                              )
                            )
                             (
                              begin (
                                if (
                                  not (
                                    cond (
                                      (
                                        string? METRIC_CONVERSION
                                      )
                                       (
                                        if (
                                          string-contains METRIC_CONVERSION from_sanitized
                                        )
                                         #t #f
                                      )
                                    )
                                     (
                                      (
                                        hash-table? METRIC_CONVERSION
                                      )
                                       (
                                        if (
                                          hash-table-exists? METRIC_CONVERSION from_sanitized
                                        )
                                         #t #f
                                      )
                                    )
                                     (
                                      else (
                                        if (
                                          member from_sanitized METRIC_CONVERSION
                                        )
                                         #t #f
                                      )
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    panic (
                                      string-append (
                                        string-append (
                                          string-append "Invalid 'from_type' value: '" from_type
                                        )
                                         "'.\nConversion abbreviations are: "
                                      )
                                       ABBREVIATIONS
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
                                    cond (
                                      (
                                        string? METRIC_CONVERSION
                                      )
                                       (
                                        if (
                                          string-contains METRIC_CONVERSION to_sanitized
                                        )
                                         #t #f
                                      )
                                    )
                                     (
                                      (
                                        hash-table? METRIC_CONVERSION
                                      )
                                       (
                                        if (
                                          hash-table-exists? METRIC_CONVERSION to_sanitized
                                        )
                                         #t #f
                                      )
                                    )
                                     (
                                      else (
                                        if (
                                          member to_sanitized METRIC_CONVERSION
                                        )
                                         #t #f
                                      )
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    panic (
                                      string-append (
                                        string-append (
                                          string-append "Invalid 'to_type' value: '" to_type
                                        )
                                         "'.\nConversion abbreviations are: "
                                      )
                                       ABBREVIATIONS
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
                                    from_exp (
                                      hash-table-ref/default METRIC_CONVERSION from_sanitized (
                                        quote (
                                          
                                        )
                                      )
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        to_exp (
                                          hash-table-ref/default METRIC_CONVERSION to_sanitized (
                                            quote (
                                              
                                            )
                                          )
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            exponent 0
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              > from_exp to_exp
                                            )
                                             (
                                              begin (
                                                set! exponent (
                                                  - from_exp to_exp
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                set! exponent (
                                                  - (
                                                    - to_exp from_exp
                                                  )
                                                )
                                              )
                                            )
                                          )
                                           (
                                            ret5 (
                                              * value (
                                                pow10 exponent
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
                        to-str-space (
                          length_conversion 1.0 "meter" "kilometer"
                        )
                      )
                    )
                     (
                      to-str-space (
                        length_conversion 1.0 "meter" "kilometer"
                      )
                    )
                     (
                      to-str (
                        to-str-space (
                          length_conversion 1.0 "meter" "kilometer"
                        )
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
                        to-str-space (
                          length_conversion 1.0 "meter" "megametre"
                        )
                      )
                    )
                     (
                      to-str-space (
                        length_conversion 1.0 "meter" "megametre"
                      )
                    )
                     (
                      to-str (
                        to-str-space (
                          length_conversion 1.0 "meter" "megametre"
                        )
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
                        to-str-space (
                          length_conversion 1.0 "gigametre" "meter"
                        )
                      )
                    )
                     (
                      to-str-space (
                        length_conversion 1.0 "gigametre" "meter"
                      )
                    )
                     (
                      to-str (
                        to-str-space (
                          length_conversion 1.0 "gigametre" "meter"
                        )
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
                        to-str-space (
                          length_conversion 1.0 "terametre" "zettametre"
                        )
                      )
                    )
                     (
                      to-str-space (
                        length_conversion 1.0 "terametre" "zettametre"
                      )
                    )
                     (
                      to-str (
                        to-str-space (
                          length_conversion 1.0 "terametre" "zettametre"
                        )
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
                        to-str-space (
                          length_conversion 1.0 "yottametre" "zettametre"
                        )
                      )
                    )
                     (
                      to-str-space (
                        length_conversion 1.0 "yottametre" "zettametre"
                      )
                    )
                     (
                      to-str (
                        to-str-space (
                          length_conversion 1.0 "yottametre" "zettametre"
                        )
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
      )
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
