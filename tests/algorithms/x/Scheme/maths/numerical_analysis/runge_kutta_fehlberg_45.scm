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
      start8 (
        current-jiffy
      )
    )
     (
      jps11 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        runge_kutta_fehlberg_45 func x_initial y_initial step_size x_final
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            begin (
              if (
                >= x_initial x_final
              )
               (
                begin (
                  panic "The final value of x must be greater than initial value of x."
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                <= step_size 0.0
              )
               (
                begin (
                  panic "Step size must be positive."
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
                  n (
                    let (
                      (
                        v2 (
                          _div (
                            - x_final x_initial
                          )
                           step_size
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
                      ys (
                        _list
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          x x_initial
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              y y_initial
                            )
                          )
                           (
                            begin (
                              set! ys (
                                append ys (
                                  _list y
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
                                                < i n
                                              )
                                               (
                                                begin (
                                                  let (
                                                    (
                                                      k1 (
                                                        * step_size (
                                                          func x y
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      let (
                                                        (
                                                          k2 (
                                                            * step_size (
                                                              func (
                                                                _add x (
                                                                  _div step_size 4.0
                                                                )
                                                              )
                                                               (
                                                                _add y (
                                                                  _div k1 4.0
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
                                                              k3 (
                                                                * step_size (
                                                                  func (
                                                                    _add x (
                                                                      * (
                                                                        _div 3.0 8.0
                                                                      )
                                                                       step_size
                                                                    )
                                                                  )
                                                                   (
                                                                    _add (
                                                                      _add y (
                                                                        * (
                                                                          _div 3.0 32.0
                                                                        )
                                                                         k1
                                                                      )
                                                                    )
                                                                     (
                                                                      * (
                                                                        _div 9.0 32.0
                                                                      )
                                                                       k2
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
                                                                  k4 (
                                                                    * step_size (
                                                                      func (
                                                                        _add x (
                                                                          * (
                                                                            _div 12.0 13.0
                                                                          )
                                                                           step_size
                                                                        )
                                                                      )
                                                                       (
                                                                        _add (
                                                                          - (
                                                                            _add y (
                                                                              * (
                                                                                _div 1932.0 2197.0
                                                                              )
                                                                               k1
                                                                            )
                                                                          )
                                                                           (
                                                                            * (
                                                                              _div 7200.0 2197.0
                                                                            )
                                                                             k2
                                                                          )
                                                                        )
                                                                         (
                                                                          * (
                                                                            _div 7296.0 2197.0
                                                                          )
                                                                           k3
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
                                                                      k5 (
                                                                        * step_size (
                                                                          func (
                                                                            + x step_size
                                                                          )
                                                                           (
                                                                            - (
                                                                              _add (
                                                                                - (
                                                                                  _add y (
                                                                                    * (
                                                                                      _div 439.0 216.0
                                                                                    )
                                                                                     k1
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  * 8.0 k2
                                                                                )
                                                                              )
                                                                               (
                                                                                * (
                                                                                  _div 3680.0 513.0
                                                                                )
                                                                                 k3
                                                                              )
                                                                            )
                                                                             (
                                                                              * (
                                                                                _div 845.0 4104.0
                                                                              )
                                                                               k4
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
                                                                          k6 (
                                                                            * step_size (
                                                                              func (
                                                                                _add x (
                                                                                  _div step_size 2.0
                                                                                )
                                                                              )
                                                                               (
                                                                                - (
                                                                                  _add (
                                                                                    - (
                                                                                      _add (
                                                                                        - y (
                                                                                          * (
                                                                                            _div 8.0 27.0
                                                                                          )
                                                                                           k1
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        * 2.0 k2
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      * (
                                                                                        _div 3544.0 2565.0
                                                                                      )
                                                                                       k3
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    * (
                                                                                      _div 1859.0 4104.0
                                                                                    )
                                                                                     k4
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  * (
                                                                                    _div 11.0 40.0
                                                                                  )
                                                                                   k5
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        begin (
                                                                          set! y (
                                                                            _add (
                                                                              - (
                                                                                _add (
                                                                                  _add (
                                                                                    _add y (
                                                                                      * (
                                                                                        _div 16.0 135.0
                                                                                      )
                                                                                       k1
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    * (
                                                                                      _div 6656.0 12825.0
                                                                                    )
                                                                                     k3
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  * (
                                                                                    _div 28561.0 56430.0
                                                                                  )
                                                                                   k4
                                                                                )
                                                                              )
                                                                               (
                                                                                * (
                                                                                  _div 9.0 50.0
                                                                                )
                                                                                 k5
                                                                              )
                                                                            )
                                                                             (
                                                                              * (
                                                                                _div 2.0 55.0
                                                                              )
                                                                               k6
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          set! x (
                                                                            + x step_size
                                                                          )
                                                                        )
                                                                         (
                                                                          set! ys (
                                                                            append ys (
                                                                              _list y
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
                                                              )
                                                            )
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
                                  ret1 ys
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
        main
      )
       (
        call/cc (
          lambda (
            ret5
          )
           (
            begin (
              define (
                f1 x y
              )
               (
                call/cc (
                  lambda (
                    ret6
                  )
                   (
                    ret6 (
                      _add 1.0 (
                        * y y
                      )
                    )
                  )
                )
              )
            )
             (
              let (
                (
                  y1 (
                    runge_kutta_fehlberg_45 f1 0.0 0.0 0.2 1.0
                  )
                )
              )
               (
                begin (
                  _display (
                    if (
                      string? (
                        cond (
                          (
                            string? y1
                          )
                           (
                            _substring y1 1 (
                              + 1 1
                            )
                          )
                        )
                         (
                          (
                            hash-table? y1
                          )
                           (
                            hash-table-ref y1 1
                          )
                        )
                         (
                          else (
                            list-ref y1 1
                          )
                        )
                      )
                    )
                     (
                      cond (
                        (
                          string? y1
                        )
                         (
                          _substring y1 1 (
                            + 1 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? y1
                        )
                         (
                          hash-table-ref y1 1
                        )
                      )
                       (
                        else (
                          list-ref y1 1
                        )
                      )
                    )
                     (
                      to-str (
                        cond (
                          (
                            string? y1
                          )
                           (
                            _substring y1 1 (
                              + 1 1
                            )
                          )
                        )
                         (
                          (
                            hash-table? y1
                          )
                           (
                            hash-table-ref y1 1
                          )
                        )
                         (
                          else (
                            list-ref y1 1
                          )
                        )
                      )
                    )
                  )
                )
                 (
                  newline
                )
                 (
                  define (
                    f2 x y
                  )
                   (
                    call/cc (
                      lambda (
                        ret7
                      )
                       (
                        ret7 x
                      )
                    )
                  )
                )
                 (
                  let (
                    (
                      y2 (
                        runge_kutta_fehlberg_45 f2 (
                          - 1.0
                        )
                         0.0 0.2 0.0
                      )
                    )
                  )
                   (
                    begin (
                      _display (
                        if (
                          string? (
                            cond (
                              (
                                string? y2
                              )
                               (
                                _substring y2 1 (
                                  + 1 1
                                )
                              )
                            )
                             (
                              (
                                hash-table? y2
                              )
                               (
                                hash-table-ref y2 1
                              )
                            )
                             (
                              else (
                                list-ref y2 1
                              )
                            )
                          )
                        )
                         (
                          cond (
                            (
                              string? y2
                            )
                             (
                              _substring y2 1 (
                                + 1 1
                              )
                            )
                          )
                           (
                            (
                              hash-table? y2
                            )
                             (
                              hash-table-ref y2 1
                            )
                          )
                           (
                            else (
                              list-ref y2 1
                            )
                          )
                        )
                         (
                          to-str (
                            cond (
                              (
                                string? y2
                              )
                               (
                                _substring y2 1 (
                                  + 1 1
                                )
                              )
                            )
                             (
                              (
                                hash-table? y2
                              )
                               (
                                hash-table-ref y2 1
                              )
                            )
                             (
                              else (
                                list-ref y2 1
                              )
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
     (
      main
    )
     (
      let (
        (
          end9 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur10 (
              quotient (
                * (
                  - end9 start8
                )
                 1000000
              )
               jps11
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur10
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
