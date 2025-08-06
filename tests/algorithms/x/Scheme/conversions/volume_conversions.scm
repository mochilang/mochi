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
      start10 (
        current-jiffy
      )
    )
     (
      jps13 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      let (
        (
          units (
            _list "cubic meter" "litre" "kilolitre" "gallon" "cubic yard" "cubic foot" "cup"
          )
        )
      )
       (
        begin (
          let (
            (
              from_factors (
                _list 1.0 0.001 1.0 0.00454 0.76455 0.028 0.000236588
              )
            )
          )
           (
            begin (
              let (
                (
                  to_factors (
                    _list 1.0 1000.0 1.0 264.172 1.30795 35.3147 4226.75
                  )
                )
              )
               (
                begin (
                  define (
                    supported_values
                  )
                   (
                    call/cc (
                      lambda (
                        ret1
                      )
                       (
                        let (
                          (
                            result (
                              list-ref units 0
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                i 1
                              )
                            )
                             (
                              begin (
                                call/cc (
                                  lambda (
                                    break3
                                  )
                                   (
                                    letrec (
                                      (
                                        loop2 (
                                          lambda (
                                            
                                          )
                                           (
                                            if (
                                              < i (
                                                _len units
                                              )
                                            )
                                             (
                                              begin (
                                                set! result (
                                                  string-append (
                                                    string-append result ", "
                                                  )
                                                   (
                                                    list-ref units i
                                                  )
                                                )
                                              )
                                               (
                                                set! i (
                                                  + i 1
                                                )
                                              )
                                               (
                                                loop2
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
                                      loop2
                                    )
                                  )
                                )
                              )
                               (
                                ret1 result
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
                    find_index name
                  )
                   (
                    call/cc (
                      lambda (
                        ret4
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
                                break6
                              )
                               (
                                letrec (
                                  (
                                    loop5 (
                                      lambda (
                                        
                                      )
                                       (
                                        if (
                                          < i (
                                            _len units
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              string=? (
                                                list-ref units i
                                              )
                                               name
                                            )
                                             (
                                              begin (
                                                ret4 i
                                              )
                                            )
                                             (
                                              quote (
                                                
                                              )
                                            )
                                          )
                                           (
                                            set! i (
                                              + i 1
                                            )
                                          )
                                           (
                                            loop5
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
                                  loop5
                                )
                              )
                            )
                          )
                           (
                            ret4 (
                              - 1
                            )
                          )
                        )
                      )
                    )
                  )
                )
                 (
                  define (
                    get_from_factor name
                  )
                   (
                    call/cc (
                      lambda (
                        ret7
                      )
                       (
                        let (
                          (
                            idx (
                              find_index name
                            )
                          )
                        )
                         (
                          begin (
                            if (
                              _lt idx 0
                            )
                             (
                              begin (
                                panic (
                                  string-append (
                                    string-append (
                                      string-append "Invalid 'from_type' value: '" name
                                    )
                                     "' Supported values are: "
                                  )
                                   (
                                    supported_values
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
                            ret7 (
                              list-ref from_factors idx
                            )
                          )
                        )
                      )
                    )
                  )
                )
                 (
                  define (
                    get_to_factor name
                  )
                   (
                    call/cc (
                      lambda (
                        ret8
                      )
                       (
                        let (
                          (
                            idx (
                              find_index name
                            )
                          )
                        )
                         (
                          begin (
                            if (
                              _lt idx 0
                            )
                             (
                              begin (
                                panic (
                                  string-append (
                                    string-append (
                                      string-append "Invalid 'to_type' value: '" name
                                    )
                                     "' Supported values are: "
                                  )
                                   (
                                    supported_values
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
                            ret8 (
                              list-ref to_factors idx
                            )
                          )
                        )
                      )
                    )
                  )
                )
                 (
                  define (
                    volume_conversion value from_type to_type
                  )
                   (
                    call/cc (
                      lambda (
                        ret9
                      )
                       (
                        let (
                          (
                            from_factor (
                              get_from_factor from_type
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                to_factor (
                                  get_to_factor to_type
                                )
                              )
                            )
                             (
                              begin (
                                ret9 (
                                  * (
                                    * value from_factor
                                  )
                                   to_factor
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
                          volume_conversion 4.0 "cubic meter" "litre"
                        )
                      )
                    )
                     (
                      to-str-space (
                        volume_conversion 4.0 "cubic meter" "litre"
                      )
                    )
                     (
                      to-str (
                        to-str-space (
                          volume_conversion 4.0 "cubic meter" "litre"
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
                          volume_conversion 1.0 "litre" "gallon"
                        )
                      )
                    )
                     (
                      to-str-space (
                        volume_conversion 1.0 "litre" "gallon"
                      )
                    )
                     (
                      to-str (
                        to-str-space (
                          volume_conversion 1.0 "litre" "gallon"
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
                          volume_conversion 1.0 "kilolitre" "cubic meter"
                        )
                      )
                    )
                     (
                      to-str-space (
                        volume_conversion 1.0 "kilolitre" "cubic meter"
                      )
                    )
                     (
                      to-str (
                        to-str-space (
                          volume_conversion 1.0 "kilolitre" "cubic meter"
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
                          volume_conversion 3.0 "gallon" "cubic yard"
                        )
                      )
                    )
                     (
                      to-str-space (
                        volume_conversion 3.0 "gallon" "cubic yard"
                      )
                    )
                     (
                      to-str (
                        to-str-space (
                          volume_conversion 3.0 "gallon" "cubic yard"
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
                          volume_conversion 2.0 "cubic yard" "litre"
                        )
                      )
                    )
                     (
                      to-str-space (
                        volume_conversion 2.0 "cubic yard" "litre"
                      )
                    )
                     (
                      to-str (
                        to-str-space (
                          volume_conversion 2.0 "cubic yard" "litre"
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
                          volume_conversion 4.0 "cubic foot" "cup"
                        )
                      )
                    )
                     (
                      to-str-space (
                        volume_conversion 4.0 "cubic foot" "cup"
                      )
                    )
                     (
                      to-str (
                        to-str-space (
                          volume_conversion 4.0 "cubic foot" "cup"
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
                          volume_conversion 1.0 "cup" "kilolitre"
                        )
                      )
                    )
                     (
                      to-str-space (
                        volume_conversion 1.0 "cup" "kilolitre"
                      )
                    )
                     (
                      to-str (
                        to-str-space (
                          volume_conversion 1.0 "cup" "kilolitre"
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
          end11 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur12 (
              quotient (
                * (
                  - end11 start10
                )
                 1000000
              )
               jps13
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur12
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
