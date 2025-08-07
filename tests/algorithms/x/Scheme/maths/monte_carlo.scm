;; Generated on 2025-08-07 10:06 +0700
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
(
  let (
    (
      start18 (
        current-jiffy
      )
    )
     (
      jps21 (
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
              rand_seed 123456789
            )
          )
           (
            begin (
              define (
                rand_float
              )
               (
                call/cc (
                  lambda (
                    ret1
                  )
                   (
                    begin (
                      set! rand_seed (
                        _mod (
                          + (
                            * 1103515245 rand_seed
                          )
                           12345
                        )
                         2147483648
                      )
                    )
                     (
                      ret1 (
                        _div (
                          + 0.0 rand_seed
                        )
                         2147483648.0
                      )
                    )
                  )
                )
              )
            )
             (
              define (
                rand_range min_val max_val
              )
               (
                call/cc (
                  lambda (
                    ret2
                  )
                   (
                    ret2 (
                      _add (
                        * (
                          rand_float
                        )
                         (
                          - max_val min_val
                        )
                      )
                       min_val
                    )
                  )
                )
              )
            )
             (
              define (
                abs_float x
              )
               (
                call/cc (
                  lambda (
                    ret3
                  )
                   (
                    begin (
                      if (
                        < x 0.0
                      )
                       (
                        begin (
                          ret3 (
                            - x
                          )
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      ret3 x
                    )
                  )
                )
              )
            )
             (
              define (
                sqrtApprox x
              )
               (
                call/cc (
                  lambda (
                    ret4
                  )
                   (
                    begin (
                      if (
                        equal? x 0.0
                      )
                       (
                        begin (
                          ret4 0.0
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
                          guess (
                            _div x 2.0
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
                                            < i 20
                                          )
                                           (
                                            begin (
                                              set! guess (
                                                _div (
                                                  _add guess (
                                                    _div x guess
                                                  )
                                                )
                                                 2.0
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
                              ret4 guess
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
                pi_estimator iterations
              )
               (
                call/cc (
                  lambda (
                    ret7
                  )
                   (
                    let (
                      (
                        inside 0.0
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
                                          < i iterations
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                x (
                                                  rand_range (
                                                    - 1.0
                                                  )
                                                   1.0
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    y (
                                                      rand_range (
                                                        - 1.0
                                                      )
                                                       1.0
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    if (
                                                      _le (
                                                        _add (
                                                          * x x
                                                        )
                                                         (
                                                          * y y
                                                        )
                                                      )
                                                       1.0
                                                    )
                                                     (
                                                      begin (
                                                        set! inside (
                                                          + inside 1.0
                                                        )
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
                                                )
                                              )
                                            )
                                          )
                                           (
                                            loop8
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
                                  loop8
                                )
                              )
                            )
                          )
                           (
                            let (
                              (
                                proportion (
                                  _div inside (
                                    + 0.0 iterations
                                  )
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    pi_estimate (
                                      * proportion 4.0
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    _display (
                                      if (
                                        string? "The estimated value of pi is"
                                      )
                                       "The estimated value of pi is" (
                                        to-str "The estimated value of pi is"
                                      )
                                    )
                                  )
                                   (
                                    _display " "
                                  )
                                   (
                                    _display (
                                      if (
                                        string? pi_estimate
                                      )
                                       pi_estimate (
                                        to-str pi_estimate
                                      )
                                    )
                                  )
                                   (
                                    newline
                                  )
                                   (
                                    _display (
                                      if (
                                        string? "The numpy value of pi is"
                                      )
                                       "The numpy value of pi is" (
                                        to-str "The numpy value of pi is"
                                      )
                                    )
                                  )
                                   (
                                    _display " "
                                  )
                                   (
                                    _display (
                                      if (
                                        string? PI
                                      )
                                       PI (
                                        to-str PI
                                      )
                                    )
                                  )
                                   (
                                    newline
                                  )
                                   (
                                    _display (
                                      if (
                                        string? "The total error is"
                                      )
                                       "The total error is" (
                                        to-str "The total error is"
                                      )
                                    )
                                  )
                                   (
                                    _display " "
                                  )
                                   (
                                    _display (
                                      if (
                                        string? (
                                          abs_float (
                                            - PI pi_estimate
                                          )
                                        )
                                      )
                                       (
                                        abs_float (
                                          - PI pi_estimate
                                        )
                                      )
                                       (
                                        to-str (
                                          abs_float (
                                            - PI pi_estimate
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
                    )
                  )
                )
              )
            )
             (
              define (
                area_under_curve_estimator iterations f min_value max_value
              )
               (
                call/cc (
                  lambda (
                    ret10
                  )
                   (
                    let (
                      (
                        sum 0.0
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
                                break12
                              )
                               (
                                letrec (
                                  (
                                    loop11 (
                                      lambda (
                                        
                                      )
                                       (
                                        if (
                                          < i iterations
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                x (
                                                  rand_range min_value max_value
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                set! sum (
                                                  + sum (
                                                    f x
                                                  )
                                                )
                                              )
                                               (
                                                set! i (
                                                  + i 1
                                                )
                                              )
                                            )
                                          )
                                           (
                                            loop11
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
                                  loop11
                                )
                              )
                            )
                          )
                           (
                            let (
                              (
                                expected (
                                  _div sum (
                                    + 0.0 iterations
                                  )
                                )
                              )
                            )
                             (
                              begin (
                                ret10 (
                                  * expected (
                                    - max_value min_value
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
                area_under_line_estimator_check iterations min_value max_value
              )
               (
                call/cc (
                  lambda (
                    ret13
                  )
                   (
                    begin (
                      define (
                        identity_function x
                      )
                       (
                        call/cc (
                          lambda (
                            ret14
                          )
                           (
                            ret14 x
                          )
                        )
                      )
                    )
                     (
                      let (
                        (
                          estimated_value (
                            area_under_curve_estimator iterations identity_function min_value max_value
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              expected_value (
                                _div (
                                  - (
                                    * max_value max_value
                                  )
                                   (
                                    * min_value min_value
                                  )
                                )
                                 2.0
                              )
                            )
                          )
                           (
                            begin (
                              _display (
                                if (
                                  string? "******************"
                                )
                                 "******************" (
                                  to-str "******************"
                                )
                              )
                            )
                             (
                              newline
                            )
                             (
                              _display (
                                if (
                                  string? "Estimating area under y=x where x varies from"
                                )
                                 "Estimating area under y=x where x varies from" (
                                  to-str "Estimating area under y=x where x varies from"
                                )
                              )
                            )
                             (
                              _display " "
                            )
                             (
                              _display (
                                if (
                                  string? min_value
                                )
                                 min_value (
                                  to-str min_value
                                )
                              )
                            )
                             (
                              newline
                            )
                             (
                              _display (
                                if (
                                  string? "Estimated value is"
                                )
                                 "Estimated value is" (
                                  to-str "Estimated value is"
                                )
                              )
                            )
                             (
                              _display " "
                            )
                             (
                              _display (
                                if (
                                  string? estimated_value
                                )
                                 estimated_value (
                                  to-str estimated_value
                                )
                              )
                            )
                             (
                              newline
                            )
                             (
                              _display (
                                if (
                                  string? "Expected value is"
                                )
                                 "Expected value is" (
                                  to-str "Expected value is"
                                )
                              )
                            )
                             (
                              _display " "
                            )
                             (
                              _display (
                                if (
                                  string? expected_value
                                )
                                 expected_value (
                                  to-str expected_value
                                )
                              )
                            )
                             (
                              newline
                            )
                             (
                              _display (
                                if (
                                  string? "Total error is"
                                )
                                 "Total error is" (
                                  to-str "Total error is"
                                )
                              )
                            )
                             (
                              _display " "
                            )
                             (
                              _display (
                                if (
                                  string? (
                                    abs_float (
                                      - estimated_value expected_value
                                    )
                                  )
                                )
                                 (
                                  abs_float (
                                    - estimated_value expected_value
                                  )
                                )
                                 (
                                  to-str (
                                    abs_float (
                                      - estimated_value expected_value
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
                                  string? "******************"
                                )
                                 "******************" (
                                  to-str "******************"
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
              define (
                pi_estimator_using_area_under_curve iterations
              )
               (
                call/cc (
                  lambda (
                    ret15
                  )
                   (
                    begin (
                      define (
                        semi_circle x
                      )
                       (
                        call/cc (
                          lambda (
                            ret16
                          )
                           (
                            let (
                              (
                                y (
                                  - 4.0 (
                                    * x x
                                  )
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    s (
                                      sqrtApprox y
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    ret16 s
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
                          estimated_value (
                            area_under_curve_estimator iterations semi_circle 0.0 2.0
                          )
                        )
                      )
                       (
                        begin (
                          _display (
                            if (
                              string? "******************"
                            )
                             "******************" (
                              to-str "******************"
                            )
                          )
                        )
                         (
                          newline
                        )
                         (
                          _display (
                            if (
                              string? "Estimating pi using area_under_curve_estimator"
                            )
                             "Estimating pi using area_under_curve_estimator" (
                              to-str "Estimating pi using area_under_curve_estimator"
                            )
                          )
                        )
                         (
                          newline
                        )
                         (
                          _display (
                            if (
                              string? "Estimated value is"
                            )
                             "Estimated value is" (
                              to-str "Estimated value is"
                            )
                          )
                        )
                         (
                          _display " "
                        )
                         (
                          _display (
                            if (
                              string? estimated_value
                            )
                             estimated_value (
                              to-str estimated_value
                            )
                          )
                        )
                         (
                          newline
                        )
                         (
                          _display (
                            if (
                              string? "Expected value is"
                            )
                             "Expected value is" (
                              to-str "Expected value is"
                            )
                          )
                        )
                         (
                          _display " "
                        )
                         (
                          _display (
                            if (
                              string? PI
                            )
                             PI (
                              to-str PI
                            )
                          )
                        )
                         (
                          newline
                        )
                         (
                          _display (
                            if (
                              string? "Total error is"
                            )
                             "Total error is" (
                              to-str "Total error is"
                            )
                          )
                        )
                         (
                          _display " "
                        )
                         (
                          _display (
                            if (
                              string? (
                                abs_float (
                                  - estimated_value PI
                                )
                              )
                            )
                             (
                              abs_float (
                                - estimated_value PI
                              )
                            )
                             (
                              to-str (
                                abs_float (
                                  - estimated_value PI
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
                              string? "******************"
                            )
                             "******************" (
                              to-str "******************"
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
              define (
                main
              )
               (
                call/cc (
                  lambda (
                    ret17
                  )
                   (
                    begin (
                      pi_estimator 1000
                    )
                     (
                      area_under_line_estimator_check 1000 0.0 1.0
                    )
                     (
                      pi_estimator_using_area_under_curve 1000
                    )
                  )
                )
              )
            )
             (
              main
            )
          )
        )
      )
    )
     (
      let (
        (
          end19 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur20 (
              quotient (
                * (
                  - end19 start18
                )
                 1000000
              )
               jps21
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur20
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
