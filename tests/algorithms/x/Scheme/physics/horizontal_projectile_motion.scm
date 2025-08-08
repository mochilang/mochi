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
      start15 (
        current-jiffy
      )
    )
     (
      jps18 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      let (
        (
          PI 3.141592653589793
        )
      )
       (
        begin (
          let (
            (
              TWO_PI 6.283185307179586
            )
          )
           (
            begin (
              let (
                (
                  g 9.80665
                )
              )
               (
                begin (
                  define (
                    _mod x m
                  )
                   (
                    call/cc (
                      lambda (
                        ret1
                      )
                       (
                        ret1 (
                          - x (
                            * (
                              + 0.0 (
                                let (
                                  (
                                    v2 (
                                      _div x m
                                    )
                                  )
                                )
                                 (
                                  cond (
                                    (
                                      string? v2
                                    )
                                     (
                                      exact (
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
                                      exact (
                                        floor v2
                                      )
                                    )
                                  )
                                )
                              )
                            )
                             m
                          )
                        )
                      )
                    )
                  )
                )
                 (
                  define (
                    sin x
                  )
                   (
                    call/cc (
                      lambda (
                        ret3
                      )
                       (
                        let (
                          (
                            y (
                              - (
                                _mod (
                                  + x PI
                                )
                                 TWO_PI
                              )
                               PI
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                y2 (
                                  * y y
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    y3 (
                                      * y2 y
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        y5 (
                                          * y3 y2
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            y7 (
                                              * y5 y2
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            ret3 (
                                              - (
                                                _add (
                                                  - y (
                                                    _div y3 6.0
                                                  )
                                                )
                                                 (
                                                  _div y5 120.0
                                                )
                                              )
                                               (
                                                _div y7 5040.0
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
                    deg_to_rad deg
                  )
                   (
                    call/cc (
                      lambda (
                        ret4
                      )
                       (
                        ret4 (
                          _div (
                            * deg PI
                          )
                           180.0
                        )
                      )
                    )
                  )
                )
                 (
                  define (
                    floor x
                  )
                   (
                    call/cc (
                      lambda (
                        ret5
                      )
                       (
                        let (
                          (
                            i (
                              let (
                                (
                                  v6 x
                                )
                              )
                               (
                                cond (
                                  (
                                    string? v6
                                  )
                                   (
                                    inexact->exact (
                                      floor (
                                        string->number v6
                                      )
                                    )
                                  )
                                )
                                 (
                                  (
                                    boolean? v6
                                  )
                                   (
                                    if v6 1 0
                                  )
                                )
                                 (
                                  else (
                                    inexact->exact (
                                      floor v6
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
                             '(
                              
                            )
                          )
                           (
                            ret5 (
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
                        ret7
                      )
                       (
                        let (
                          (
                            result 1.0
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
                                    break9
                                  )
                                   (
                                    letrec (
                                      (
                                        loop8 (
                                          lambda (
                                            
                                          )
                                           (
                                            if (
                                              < i n
                                            )
                                             (
                                              begin (
                                                set! result (
                                                  * result 10.0
                                                )
                                              )
                                               (
                                                set! i (
                                                  + i 1
                                                )
                                              )
                                               (
                                                loop8
                                              )
                                            )
                                             '(
                                              
                                            )
                                          )
                                        )
                                      )
                                    )
                                     (
                                      loop8
                                    )
                                  )
                                )
                              )
                               (
                                ret7 result
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
                    round x n
                  )
                   (
                    call/cc (
                      lambda (
                        ret10
                      )
                       (
                        let (
                          (
                            m (
                              pow10 n
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                y (
                                  floor (
                                    _add (
                                      * x m
                                    )
                                     0.5
                                  )
                                )
                              )
                            )
                             (
                              begin (
                                ret10 (
                                  _div y m
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
                    check_args init_velocity angle
                  )
                   (
                    call/cc (
                      lambda (
                        ret11
                      )
                       (
                        begin (
                          if (
                            or (
                              > angle 90.0
                            )
                             (
                              < angle 1.0
                            )
                          )
                           (
                            begin (
                              panic "Invalid angle. Range is 1-90 degrees."
                            )
                          )
                           '(
                            
                          )
                        )
                         (
                          if (
                            < init_velocity 0.0
                          )
                           (
                            begin (
                              panic "Invalid velocity. Should be a positive number."
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
                    horizontal_distance init_velocity angle
                  )
                   (
                    call/cc (
                      lambda (
                        ret12
                      )
                       (
                        begin (
                          check_args init_velocity angle
                        )
                         (
                          let (
                            (
                              radians (
                                deg_to_rad (
                                  * 2.0 angle
                                )
                              )
                            )
                          )
                           (
                            begin (
                              ret12 (
                                round (
                                  _div (
                                    * (
                                      * init_velocity init_velocity
                                    )
                                     (
                                      sin radians
                                    )
                                  )
                                   g
                                )
                                 2
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
                    max_height init_velocity angle
                  )
                   (
                    call/cc (
                      lambda (
                        ret13
                      )
                       (
                        begin (
                          check_args init_velocity angle
                        )
                         (
                          let (
                            (
                              radians (
                                deg_to_rad angle
                              )
                            )
                          )
                           (
                            begin (
                              let (
                                (
                                  s (
                                    sin radians
                                  )
                                )
                              )
                               (
                                begin (
                                  ret13 (
                                    round (
                                      _div (
                                        * (
                                          * (
                                            * init_velocity init_velocity
                                          )
                                           s
                                        )
                                         s
                                      )
                                       (
                                        * 2.0 g
                                      )
                                    )
                                     2
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
                    total_time init_velocity angle
                  )
                   (
                    call/cc (
                      lambda (
                        ret14
                      )
                       (
                        begin (
                          check_args init_velocity angle
                        )
                         (
                          let (
                            (
                              radians (
                                deg_to_rad angle
                              )
                            )
                          )
                           (
                            begin (
                              ret14 (
                                round (
                                  _div (
                                    * (
                                      * 2.0 init_velocity
                                    )
                                     (
                                      sin radians
                                    )
                                  )
                                   g
                                )
                                 2
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
                      v0 25.0
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          angle 20.0
                        )
                      )
                       (
                        begin (
                          _display (
                            if (
                              string? (
                                horizontal_distance v0 angle
                              )
                            )
                             (
                              horizontal_distance v0 angle
                            )
                             (
                              to-str (
                                horizontal_distance v0 angle
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
                                max_height v0 angle
                              )
                            )
                             (
                              max_height v0 angle
                            )
                             (
                              to-str (
                                max_height v0 angle
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
                                total_time v0 angle
                              )
                            )
                             (
                              total_time v0 angle
                            )
                             (
                              to-str (
                                total_time v0 angle
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
      )
    )
     (
      let (
        (
          end16 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur17 (
              quotient (
                * (
                  - end16 start15
                )
                 1000000
              )
               jps18
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur17
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
