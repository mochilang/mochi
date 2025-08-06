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
            _list "km/h" "m/s" "mph" "knot"
          )
        )
      )
       (
        begin (
          let (
            (
              speed_chart (
                _list 1.0 3.6 1.609344 1.852
              )
            )
          )
           (
            begin (
              let (
                (
                  speed_chart_inverse (
                    _list 1.0 0.277777778 0.621371192 0.539956803
                  )
                )
              )
               (
                begin (
                  define (
                    index_of arr value
                  )
                   (
                    call/cc (
                      lambda (
                        ret1
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
                                            _len arr
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              string=? (
                                                list-ref arr i
                                              )
                                               value
                                            )
                                             (
                                              begin (
                                                ret1 i
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
                            ret1 (
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
                    units_string arr
                  )
                   (
                    call/cc (
                      lambda (
                        ret4
                      )
                       (
                        let (
                          (
                            s ""
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
                                                _len arr
                                              )
                                            )
                                             (
                                              begin (
                                                if (
                                                  > i 0
                                                )
                                                 (
                                                  begin (
                                                    set! s (
                                                      string-append s ", "
                                                    )
                                                  )
                                                )
                                                 (
                                                  quote (
                                                    
                                                  )
                                                )
                                              )
                                               (
                                                set! s (
                                                  string-append s (
                                                    list-ref arr i
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
                                ret4 s
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
                    round3 x
                  )
                   (
                    call/cc (
                      lambda (
                        ret7
                      )
                       (
                        let (
                          (
                            y (
                              _add (
                                * x 1000.0
                              )
                               0.5
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                z (
                                  let (
                                    (
                                      v8 y
                                    )
                                  )
                                   (
                                    cond (
                                      (
                                        string? v8
                                      )
                                       (
                                        inexact->exact (
                                          floor (
                                            string->number v8
                                          )
                                        )
                                      )
                                    )
                                     (
                                      (
                                        boolean? v8
                                      )
                                       (
                                        if v8 1 0
                                      )
                                    )
                                     (
                                      else (
                                        inexact->exact (
                                          floor v8
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    zf (
                                      + 0.0 z
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    ret7 (
                                      _div zf 1000.0
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
                    convert_speed speed unit_from unit_to
                  )
                   (
                    call/cc (
                      lambda (
                        ret9
                      )
                       (
                        let (
                          (
                            from_index (
                              index_of units unit_from
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                to_index (
                                  index_of units unit_to
                                )
                              )
                            )
                             (
                              begin (
                                if (
                                  or (
                                    _lt from_index 0
                                  )
                                   (
                                    _lt to_index 0
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        msg (
                                          string-append (
                                            string-append (
                                              string-append (
                                                string-append (
                                                  string-append "Incorrect 'from_type' or 'to_type' value: " unit_from
                                                )
                                                 ", "
                                              )
                                               unit_to
                                            )
                                             "\nValid values are: "
                                          )
                                           (
                                            units_string units
                                          )
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        panic msg
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
                                let (
                                  (
                                    result (
                                      * (
                                        * speed (
                                          list-ref speed_chart from_index
                                        )
                                      )
                                       (
                                        list-ref speed_chart_inverse to_index
                                      )
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        r (
                                          round3 result
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        ret9 r
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
                          convert_speed 100.0 "km/h" "m/s"
                        )
                      )
                    )
                     (
                      to-str-space (
                        convert_speed 100.0 "km/h" "m/s"
                      )
                    )
                     (
                      to-str (
                        to-str-space (
                          convert_speed 100.0 "km/h" "m/s"
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
                          convert_speed 100.0 "km/h" "mph"
                        )
                      )
                    )
                     (
                      to-str-space (
                        convert_speed 100.0 "km/h" "mph"
                      )
                    )
                     (
                      to-str (
                        to-str-space (
                          convert_speed 100.0 "km/h" "mph"
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
                          convert_speed 100.0 "km/h" "knot"
                        )
                      )
                    )
                     (
                      to-str-space (
                        convert_speed 100.0 "km/h" "knot"
                      )
                    )
                     (
                      to-str (
                        to-str-space (
                          convert_speed 100.0 "km/h" "knot"
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
                          convert_speed 100.0 "m/s" "km/h"
                        )
                      )
                    )
                     (
                      to-str-space (
                        convert_speed 100.0 "m/s" "km/h"
                      )
                    )
                     (
                      to-str (
                        to-str-space (
                          convert_speed 100.0 "m/s" "km/h"
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
                          convert_speed 100.0 "m/s" "mph"
                        )
                      )
                    )
                     (
                      to-str-space (
                        convert_speed 100.0 "m/s" "mph"
                      )
                    )
                     (
                      to-str (
                        to-str-space (
                          convert_speed 100.0 "m/s" "mph"
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
                          convert_speed 100.0 "m/s" "knot"
                        )
                      )
                    )
                     (
                      to-str-space (
                        convert_speed 100.0 "m/s" "knot"
                      )
                    )
                     (
                      to-str (
                        to-str-space (
                          convert_speed 100.0 "m/s" "knot"
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
                          convert_speed 100.0 "mph" "km/h"
                        )
                      )
                    )
                     (
                      to-str-space (
                        convert_speed 100.0 "mph" "km/h"
                      )
                    )
                     (
                      to-str (
                        to-str-space (
                          convert_speed 100.0 "mph" "km/h"
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
                          convert_speed 100.0 "mph" "m/s"
                        )
                      )
                    )
                     (
                      to-str-space (
                        convert_speed 100.0 "mph" "m/s"
                      )
                    )
                     (
                      to-str (
                        to-str-space (
                          convert_speed 100.0 "mph" "m/s"
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
                          convert_speed 100.0 "mph" "knot"
                        )
                      )
                    )
                     (
                      to-str-space (
                        convert_speed 100.0 "mph" "knot"
                      )
                    )
                     (
                      to-str (
                        to-str-space (
                          convert_speed 100.0 "mph" "knot"
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
                          convert_speed 100.0 "knot" "km/h"
                        )
                      )
                    )
                     (
                      to-str-space (
                        convert_speed 100.0 "knot" "km/h"
                      )
                    )
                     (
                      to-str (
                        to-str-space (
                          convert_speed 100.0 "knot" "km/h"
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
                          convert_speed 100.0 "knot" "m/s"
                        )
                      )
                    )
                     (
                      to-str-space (
                        convert_speed 100.0 "knot" "m/s"
                      )
                    )
                     (
                      to-str (
                        to-str-space (
                          convert_speed 100.0 "knot" "m/s"
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
                          convert_speed 100.0 "knot" "mph"
                        )
                      )
                    )
                     (
                      to-str-space (
                        convert_speed 100.0 "knot" "mph"
                      )
                    )
                     (
                      to-str (
                        to-str-space (
                          convert_speed 100.0 "knot" "mph"
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
