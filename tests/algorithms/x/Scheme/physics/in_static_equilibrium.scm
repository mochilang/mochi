;; Generated on 2025-08-08 16:57 +0700
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
(define _floor floor)
(define (fmod a b) (- a (* (_floor (/ a b)) b)))
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
      start3 (
        current-jiffy
      )
    )
     (
      jps6 (
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
              define (
                _mod x m
              )
               (
                - x (
                  * (
                    + 0.0 (
                      let (
                        (
                          v1 (
                            _div x m
                          )
                        )
                      )
                       (
                        cond (
                          (
                            string? v1
                          )
                           (
                            exact (
                              floor (
                                string->number v1
                              )
                            )
                          )
                        )
                         (
                          (
                            boolean? v1
                          )
                           (
                            if v1 1 0
                          )
                        )
                         (
                          else (
                            exact (
                              floor v1
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
             (
              define (
                sin_approx x
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
             (
              define (
                cos_approx x
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
                            y4 (
                              * y2 y2
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                y6 (
                                  * y4 y2
                                )
                              )
                            )
                             (
                              begin (
                                - (
                                  _add (
                                    - 1.0 (
                                      _div y2 2.0
                                    )
                                  )
                                   (
                                    _div y4 24.0
                                  )
                                )
                                 (
                                  _div y6 720.0
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
                polar_force magnitude angle radian_mode
              )
               (
                let (
                  (
                    theta (
                      if radian_mode angle (
                        _div (
                          * angle PI
                        )
                         180.0
                      )
                    )
                  )
                )
                 (
                  begin (
                    _list (
                      * magnitude (
                        cos_approx theta
                      )
                    )
                     (
                      * magnitude (
                        sin_approx theta
                      )
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
                if (
                  < x 0.0
                )
                 (
                  begin (
                    - x
                  )
                )
                 (
                  begin x
                )
              )
            )
             (
              define (
                in_static_equilibrium forces location eps
              )
               (
                let (
                  (
                    sum_moments 0.0
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
                        let (
                          (
                            n (
                              _len forces
                            )
                          )
                        )
                         (
                          begin (
                            letrec (
                              (
                                loop2 (
                                  lambda (
                                    
                                  )
                                   (
                                    if (
                                      < i n
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            r (
                                              list-ref-safe location i
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                f (
                                                  list-ref-safe forces i
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    moment (
                                                      - (
                                                        * (
                                                          list-ref-safe r 0
                                                        )
                                                         (
                                                          list-ref-safe f 1
                                                        )
                                                      )
                                                       (
                                                        * (
                                                          list-ref-safe r 1
                                                        )
                                                         (
                                                          list-ref-safe f 0
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    set! sum_moments (
                                                      + sum_moments moment
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
                                        )
                                      )
                                       (
                                        loop2
                                      )
                                    )
                                     '(
                                      
                                    )
                                  )
                                )
                              )
                            )
                             (
                              loop2
                            )
                          )
                           (
                            _lt (
                              abs_float sum_moments
                            )
                             eps
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
                  forces1 (
                    _list (
                      _list 1.0 1.0
                    )
                     (
                      _list (
                        - 1.0
                      )
                       2.0
                    )
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      location1 (
                        _list (
                          _list 1.0 0.0
                        )
                         (
                          _list 10.0 0.0
                        )
                      )
                    )
                  )
                   (
                    begin (
                      _display (
                        if (
                          string? (
                            to-str-space (
                              in_static_equilibrium forces1 location1 0.1
                            )
                          )
                        )
                         (
                          to-str-space (
                            in_static_equilibrium forces1 location1 0.1
                          )
                        )
                         (
                          to-str (
                            to-str-space (
                              in_static_equilibrium forces1 location1 0.1
                            )
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
                          forces2 (
                            _list (
                              polar_force 718.4 150.0 #f
                            )
                             (
                              polar_force 879.54 45.0 #f
                            )
                             (
                              polar_force 100.0 (
                                - 90.0
                              )
                               #f
                            )
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              location2 (
                                _list (
                                  _list 0.0 0.0
                                )
                                 (
                                  _list 0.0 0.0
                                )
                                 (
                                  _list 0.0 0.0
                                )
                              )
                            )
                          )
                           (
                            begin (
                              _display (
                                if (
                                  string? (
                                    to-str-space (
                                      in_static_equilibrium forces2 location2 0.1
                                    )
                                  )
                                )
                                 (
                                  to-str-space (
                                    in_static_equilibrium forces2 location2 0.1
                                  )
                                )
                                 (
                                  to-str (
                                    to-str-space (
                                      in_static_equilibrium forces2 location2 0.1
                                    )
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
                                  forces3 (
                                    _list (
                                      polar_force (
                                        * 30.0 9.81
                                      )
                                       15.0 #f
                                    )
                                     (
                                      polar_force 215.0 135.0 #f
                                    )
                                     (
                                      polar_force 264.0 60.0 #f
                                    )
                                  )
                                )
                              )
                               (
                                begin (
                                  let (
                                    (
                                      location3 (
                                        _list (
                                          _list 0.0 0.0
                                        )
                                         (
                                          _list 0.0 0.0
                                        )
                                         (
                                          _list 0.0 0.0
                                        )
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      _display (
                                        if (
                                          string? (
                                            to-str-space (
                                              in_static_equilibrium forces3 location3 0.1
                                            )
                                          )
                                        )
                                         (
                                          to-str-space (
                                            in_static_equilibrium forces3 location3 0.1
                                          )
                                        )
                                         (
                                          to-str (
                                            to-str-space (
                                              in_static_equilibrium forces3 location3 0.1
                                            )
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
                                          forces4 (
                                            _list (
                                              _list 0.0 (
                                                - 2000.0
                                              )
                                            )
                                             (
                                              _list 0.0 (
                                                - 1200.0
                                              )
                                            )
                                             (
                                              _list 0.0 15600.0
                                            )
                                             (
                                              _list 0.0 (
                                                - 12400.0
                                              )
                                            )
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          let (
                                            (
                                              location4 (
                                                _list (
                                                  _list 0.0 0.0
                                                )
                                                 (
                                                  _list 6.0 0.0
                                                )
                                                 (
                                                  _list 10.0 0.0
                                                )
                                                 (
                                                  _list 12.0 0.0
                                                )
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              _display (
                                                if (
                                                  string? (
                                                    to-str-space (
                                                      in_static_equilibrium forces4 location4 0.1
                                                    )
                                                  )
                                                )
                                                 (
                                                  to-str-space (
                                                    in_static_equilibrium forces4 location4 0.1
                                                  )
                                                )
                                                 (
                                                  to-str (
                                                    to-str-space (
                                                      in_static_equilibrium forces4 location4 0.1
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
          end4 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur5 (
              quotient (
                * (
                  - end4 start3
                )
                 1000000
              )
               jps6
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur5
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
