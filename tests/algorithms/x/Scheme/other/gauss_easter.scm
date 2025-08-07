;; Generated on 2025-08-07 16:11 +0700
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
        gauss_easter year
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                metonic_cycle (
                  _mod year 19
                )
              )
            )
             (
              begin (
                let (
                  (
                    julian_leap_year (
                      _mod year 4
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        non_leap_year (
                          _mod year 7
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            leap_day_inhibits (
                              _div year 100
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                lunar_orbit_correction (
                                  _div (
                                    + 13 (
                                      * 8 leap_day_inhibits
                                    )
                                  )
                                   25
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    leap_day_reinstall_number (
                                      _div (
                                        + 0.0 leap_day_inhibits
                                      )
                                       4.0
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        secular_moon_shift (
                                          _mod (
                                            - (
                                              _add (
                                                - 15.0 (
                                                  + 0.0 lunar_orbit_correction
                                                )
                                              )
                                               (
                                                + 0.0 leap_day_inhibits
                                              )
                                            )
                                             leap_day_reinstall_number
                                          )
                                           30.0
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            century_starting_point (
                                              _mod (
                                                - (
                                                  + 4.0 (
                                                    + 0.0 leap_day_inhibits
                                                  )
                                                )
                                                 leap_day_reinstall_number
                                              )
                                               7.0
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                days_to_add (
                                                  _mod (
                                                    _add (
                                                      * 19.0 (
                                                        + 0.0 metonic_cycle
                                                      )
                                                    )
                                                     secular_moon_shift
                                                  )
                                                   30.0
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    days_from_phm_to_sunday (
                                                      _mod (
                                                        _add (
                                                          _add (
                                                            _add (
                                                              * 2.0 (
                                                                + 0.0 julian_leap_year
                                                              )
                                                            )
                                                             (
                                                              * 4.0 (
                                                                + 0.0 non_leap_year
                                                              )
                                                            )
                                                          )
                                                           (
                                                            * 6.0 days_to_add
                                                          )
                                                        )
                                                         century_starting_point
                                                      )
                                                       7.0
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    if (
                                                      and (
                                                        equal? days_to_add 29.0
                                                      )
                                                       (
                                                        equal? days_from_phm_to_sunday 6.0
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        ret1 (
                                                          alist->hash-table (
                                                            _list (
                                                              cons "month" 4
                                                            )
                                                             (
                                                              cons "day" 19
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                     '(
                                                      
                                                    )
                                                  )
                                                   (
                                                    if (
                                                      and (
                                                        equal? days_to_add 28.0
                                                      )
                                                       (
                                                        equal? days_from_phm_to_sunday 6.0
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        ret1 (
                                                          alist->hash-table (
                                                            _list (
                                                              cons "month" 4
                                                            )
                                                             (
                                                              cons "day" 18
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                     '(
                                                      
                                                    )
                                                  )
                                                   (
                                                    let (
                                                      (
                                                        offset (
                                                          let (
                                                            (
                                                              v2 (
                                                                + days_to_add days_from_phm_to_sunday
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
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            total (
                                                              + 22 offset
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            if (
                                                              > total 31
                                                            )
                                                             (
                                                              begin (
                                                                ret1 (
                                                                  alist->hash-table (
                                                                    _list (
                                                                      cons "month" 4
                                                                    )
                                                                     (
                                                                      cons "day" (
                                                                        - total 31
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             '(
                                                              
                                                            )
                                                          )
                                                           (
                                                            ret1 (
                                                              alist->hash-table (
                                                                _list (
                                                                  cons "month" 3
                                                                )
                                                                 (
                                                                  cons "day" total
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
      define (
        format_date year d
      )
       (
        call/cc (
          lambda (
            ret3
          )
           (
            let (
              (
                month (
                  if (
                    < (
                      hash-table-ref d "month"
                    )
                     10
                  )
                   (
                    string-append "0" (
                      to-str-space (
                        hash-table-ref d "month"
                      )
                    )
                  )
                   (
                    to-str-space (
                      hash-table-ref d "month"
                    )
                  )
                )
              )
            )
             (
              begin (
                let (
                  (
                    day (
                      if (
                        < (
                          hash-table-ref d "day"
                        )
                         10
                      )
                       (
                        string-append "0" (
                          to-str-space (
                            hash-table-ref d "day"
                          )
                        )
                      )
                       (
                        to-str-space (
                          hash-table-ref d "day"
                        )
                      )
                    )
                  )
                )
                 (
                  begin (
                    ret3 (
                      string-append (
                        string-append (
                          string-append (
                            string-append (
                              to-str-space year
                            )
                             "-"
                          )
                           month
                        )
                         "-"
                      )
                       day
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
          years (
            _list 1994 2000 2010 2021 2023 2032 2100
          )
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
                            < i (
                              _len years
                            )
                          )
                           (
                            begin (
                              let (
                                (
                                  y (
                                    list-ref-safe years i
                                  )
                                )
                              )
                               (
                                begin (
                                  let (
                                    (
                                      e (
                                        gauss_easter y
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      _display (
                                        if (
                                          string? (
                                            string-append (
                                              string-append (
                                                string-append "Easter in " (
                                                  to-str-space y
                                                )
                                              )
                                               " is "
                                            )
                                             (
                                              format_date y e
                                            )
                                          )
                                        )
                                         (
                                          string-append (
                                            string-append (
                                              string-append "Easter in " (
                                                to-str-space y
                                              )
                                            )
                                             " is "
                                          )
                                           (
                                            format_date y e
                                          )
                                        )
                                         (
                                          to-str (
                                            string-append (
                                              string-append (
                                                string-append "Easter in " (
                                                  to-str-space y
                                                )
                                              )
                                               " is "
                                            )
                                             (
                                              format_date y e
                                            )
                                          )
                                        )
                                      )
                                    )
                                     (
                                      newline
                                    )
                                     (
                                      set! i (
                                        + i 1
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
                           '(
                            
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
