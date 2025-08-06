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
          time_chart (
            alist->hash-table (
              _list (
                cons "seconds" 1.0
              )
               (
                cons "minutes" 60.0
              )
               (
                cons "hours" 3600.0
              )
               (
                cons "days" 86400.0
              )
               (
                cons "weeks" 604800.0
              )
               (
                cons "months" 2629800.0
              )
               (
                cons "years" 31557600.0
              )
            )
          )
        )
      )
       (
        begin (
          let (
            (
              time_chart_inverse (
                alist->hash-table (
                  _list (
                    cons "seconds" 1.0
                  )
                   (
                    cons "minutes" (
                      _div 1.0 60.0
                    )
                  )
                   (
                    cons "hours" (
                      _div 1.0 3600.0
                    )
                  )
                   (
                    cons "days" (
                      _div 1.0 86400.0
                    )
                  )
                   (
                    cons "weeks" (
                      _div 1.0 604800.0
                    )
                  )
                   (
                    cons "months" (
                      _div 1.0 2629800.0
                    )
                  )
                   (
                    cons "years" (
                      _div 1.0 31557600.0
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
                  units (
                    _list "seconds" "minutes" "hours" "days" "weeks" "months" "years"
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      units_str "seconds, minutes, hours, days, weeks, months, years"
                    )
                  )
                   (
                    begin (
                      define (
                        contains arr t
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
                                                   t
                                                )
                                                 (
                                                  begin (
                                                    ret1 #t
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
                                ret1 #f
                              )
                            )
                          )
                        )
                      )
                    )
                     (
                      define (
                        convert_time time_value unit_from unit_to
                      )
                       (
                        call/cc (
                          lambda (
                            ret4
                          )
                           (
                            begin (
                              if (
                                < time_value 0.0
                              )
                               (
                                begin (
                                  panic "'time_value' must be a non-negative number."
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
                                  from (
                                    lower unit_from
                                  )
                                )
                              )
                               (
                                begin (
                                  let (
                                    (
                                      to (
                                        lower unit_to
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      if (
                                        or (
                                          not (
                                            contains units from
                                          )
                                        )
                                         (
                                          not (
                                            contains units to
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          let (
                                            (
                                              invalid_unit from
                                            )
                                          )
                                           (
                                            begin (
                                              if (
                                                contains units from
                                              )
                                               (
                                                begin (
                                                  set! invalid_unit to
                                                )
                                              )
                                               (
                                                quote (
                                                  
                                                )
                                              )
                                            )
                                             (
                                              panic (
                                                string-append (
                                                  string-append (
                                                    string-append (
                                                      string-append "Invalid unit " invalid_unit
                                                    )
                                                     " is not in "
                                                  )
                                                   units_str
                                                )
                                                 "."
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
                                      let (
                                        (
                                          seconds (
                                            * time_value (
                                              cond (
                                                (
                                                  string? time_chart
                                                )
                                                 (
                                                  _substring time_chart from (
                                                    + from 1
                                                  )
                                                )
                                              )
                                               (
                                                (
                                                  hash-table? time_chart
                                                )
                                                 (
                                                  hash-table-ref time_chart from
                                                )
                                              )
                                               (
                                                else (
                                                  list-ref time_chart from
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
                                              converted (
                                                * seconds (
                                                  cond (
                                                    (
                                                      string? time_chart_inverse
                                                    )
                                                     (
                                                      _substring time_chart_inverse to (
                                                        + to 1
                                                      )
                                                    )
                                                  )
                                                   (
                                                    (
                                                      hash-table? time_chart_inverse
                                                    )
                                                     (
                                                      hash-table-ref time_chart_inverse to
                                                    )
                                                  )
                                                   (
                                                    else (
                                                      list-ref time_chart_inverse to
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
                                                  scaled (
                                                    * converted 1000.0
                                                  )
                                                )
                                              )
                                               (
                                                begin (
                                                  let (
                                                    (
                                                      int_part (
                                                        let (
                                                          (
                                                            v5 (
                                                              _add scaled 0.5
                                                            )
                                                          )
                                                        )
                                                         (
                                                          cond (
                                                            (
                                                              string? v5
                                                            )
                                                             (
                                                              exact (
                                                                floor (
                                                                  string->number v5
                                                                )
                                                              )
                                                            )
                                                          )
                                                           (
                                                            (
                                                              boolean? v5
                                                            )
                                                             (
                                                              if v5 1 0
                                                            )
                                                          )
                                                           (
                                                            else (
                                                              exact (
                                                                floor v5
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      ret4 (
                                                        _div (
                                                          + int_part 0.0
                                                        )
                                                         1000.0
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
                            convert_time 3600.0 "seconds" "hours"
                          )
                        )
                         (
                          convert_time 3600.0 "seconds" "hours"
                        )
                         (
                          to-str (
                            convert_time 3600.0 "seconds" "hours"
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
                            convert_time 360.0 "days" "months"
                          )
                        )
                         (
                          convert_time 360.0 "days" "months"
                        )
                         (
                          to-str (
                            convert_time 360.0 "days" "months"
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
                            convert_time 360.0 "months" "years"
                          )
                        )
                         (
                          convert_time 360.0 "months" "years"
                        )
                         (
                          to-str (
                            convert_time 360.0 "months" "years"
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
                            convert_time 1.0 "years" "seconds"
                          )
                        )
                         (
                          convert_time 1.0 "years" "seconds"
                        )
                         (
                          to-str (
                            convert_time 1.0 "years" "seconds"
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
