;; Generated on 2025-08-07 08:56 +0700
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
      start16 (
        current-jiffy
      )
    )
     (
      jps19 (
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
              AXIS_A 6378137.0
            )
          )
           (
            begin (
              let (
                (
                  AXIS_B 6.356752314245e+06
                )
              )
               (
                begin (
                  let (
                    (
                      RADIUS 6378137.0
                    )
                  )
                   (
                    begin (
                      define (
                        to_radians deg
                      )
                       (
                        call/cc (
                          lambda (
                            ret1
                          )
                           (
                            ret1 (
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
                        sin_taylor x
                      )
                       (
                        call/cc (
                          lambda (
                            ret2
                          )
                           (
                            let (
                              (
                                term x
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    sum x
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
                                                      < i 10
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            k1 (
                                                              * 2.0 (
                                                                + 0.0 i
                                                              )
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                k2 (
                                                                  + k1 1.0
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                set! term (
                                                                  _div (
                                                                    * (
                                                                      * (
                                                                        - term
                                                                      )
                                                                       x
                                                                    )
                                                                     x
                                                                  )
                                                                   (
                                                                    * k1 k2
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                set! sum (
                                                                  + sum term
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
                                        ret2 sum
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
                        cos_taylor x
                      )
                       (
                        call/cc (
                          lambda (
                            ret5
                          )
                           (
                            let (
                              (
                                term 1.0
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    sum 1.0
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
                                            break7
                                          )
                                           (
                                            letrec (
                                              (
                                                loop6 (
                                                  lambda (
                                                    
                                                  )
                                                   (
                                                    if (
                                                      < i 10
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            k1 (
                                                              - (
                                                                * 2.0 (
                                                                  + 0.0 i
                                                                )
                                                              )
                                                               1.0
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                k2 (
                                                                  * 2.0 (
                                                                    + 0.0 i
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                set! term (
                                                                  _div (
                                                                    * (
                                                                      * (
                                                                        - term
                                                                      )
                                                                       x
                                                                    )
                                                                     x
                                                                  )
                                                                   (
                                                                    * k1 k2
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                set! sum (
                                                                  + sum term
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
                                                        loop6
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
                                              loop6
                                            )
                                          )
                                        )
                                      )
                                       (
                                        ret5 sum
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
                        tan_approx x
                      )
                       (
                        call/cc (
                          lambda (
                            ret8
                          )
                           (
                            ret8 (
                              _div (
                                sin_taylor x
                              )
                               (
                                cos_taylor x
                              )
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
                            ret9
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
                                        break11
                                      )
                                       (
                                        letrec (
                                          (
                                            loop10 (
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
                                                    loop10
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
                                          loop10
                                        )
                                      )
                                    )
                                  )
                                   (
                                    ret9 guess
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
                        atanApprox x
                      )
                       (
                        call/cc (
                          lambda (
                            ret12
                          )
                           (
                            begin (
                              if (
                                > x 1.0
                              )
                               (
                                begin (
                                  ret12 (
                                    - (
                                      _div PI 2.0
                                    )
                                     (
                                      _div x (
                                        _add (
                                          * x x
                                        )
                                         0.28
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
                                < x (
                                  - 1.0
                                )
                              )
                               (
                                begin (
                                  ret12 (
                                    - (
                                      _div (
                                        - PI
                                      )
                                       2.0
                                    )
                                     (
                                      _div x (
                                        _add (
                                          * x x
                                        )
                                         0.28
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
                              ret12 (
                                _div x (
                                  _add 1.0 (
                                    * (
                                      * 0.28 x
                                    )
                                     x
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
                        atan2Approx y x
                      )
                       (
                        call/cc (
                          lambda (
                            ret13
                          )
                           (
                            begin (
                              if (
                                > x 0.0
                              )
                               (
                                begin (
                                  let (
                                    (
                                      val (
                                        atanApprox (
                                          _div y x
                                        )
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      ret13 val
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
                                < x 0.0
                              )
                               (
                                begin (
                                  if (
                                    >= y 0.0
                                  )
                                   (
                                    begin (
                                      ret13 (
                                        _add (
                                          atanApprox (
                                            _div y x
                                          )
                                        )
                                         PI
                                      )
                                    )
                                  )
                                   (
                                    quote (
                                      
                                    )
                                  )
                                )
                                 (
                                  ret13 (
                                    - (
                                      atanApprox (
                                        _div y x
                                      )
                                    )
                                     PI
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
                                > y 0.0
                              )
                               (
                                begin (
                                  ret13 (
                                    _div PI 2.0
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
                                < y 0.0
                              )
                               (
                                begin (
                                  ret13 (
                                    _div (
                                      - PI
                                    )
                                     2.0
                                  )
                                )
                              )
                               (
                                quote (
                                  
                                )
                              )
                            )
                             (
                              ret13 0.0
                            )
                          )
                        )
                      )
                    )
                     (
                      define (
                        asinApprox x
                      )
                       (
                        call/cc (
                          lambda (
                            ret14
                          )
                           (
                            let (
                              (
                                denom (
                                  sqrtApprox (
                                    - 1.0 (
                                      * x x
                                    )
                                  )
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    res (
                                      atan2Approx x denom
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    ret14 res
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
                        haversine_distance lat1 lon1 lat2 lon2
                      )
                       (
                        call/cc (
                          lambda (
                            ret15
                          )
                           (
                            let (
                              (
                                flattening (
                                  _div (
                                    - AXIS_A AXIS_B
                                  )
                                   AXIS_A
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    phi_1 (
                                      atanApprox (
                                        * (
                                          - 1.0 flattening
                                        )
                                         (
                                          tan_approx (
                                            to_radians lat1
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
                                        phi_2 (
                                          atanApprox (
                                            * (
                                              - 1.0 flattening
                                            )
                                             (
                                              tan_approx (
                                                to_radians lat2
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
                                            lambda_1 (
                                              to_radians lon1
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                lambda_2 (
                                                  to_radians lon2
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    sin_sq_phi (
                                                      sin_taylor (
                                                        _div (
                                                          - phi_2 phi_1
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
                                                        sin_sq_lambda (
                                                          sin_taylor (
                                                            _div (
                                                              - lambda_2 lambda_1
                                                            )
                                                             2.0
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        set! sin_sq_phi (
                                                          * sin_sq_phi sin_sq_phi
                                                        )
                                                      )
                                                       (
                                                        set! sin_sq_lambda (
                                                          * sin_sq_lambda sin_sq_lambda
                                                        )
                                                      )
                                                       (
                                                        let (
                                                          (
                                                            h_value (
                                                              sqrtApprox (
                                                                _add sin_sq_phi (
                                                                  * (
                                                                    * (
                                                                      cos_taylor phi_1
                                                                    )
                                                                     (
                                                                      cos_taylor phi_2
                                                                    )
                                                                  )
                                                                   sin_sq_lambda
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            ret15 (
                                                              * (
                                                                * 2.0 RADIUS
                                                              )
                                                               (
                                                                asinApprox h_value
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
                      let (
                        (
                          SAN_FRANCISCO (
                            _list 37.774856 (
                              - 122.424227
                            )
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              YOSEMITE (
                                _list 37.864742 (
                                  - 119.537521
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
                                      haversine_distance (
                                        list-ref SAN_FRANCISCO 0
                                      )
                                       (
                                        list-ref SAN_FRANCISCO 1
                                      )
                                       (
                                        list-ref YOSEMITE 0
                                      )
                                       (
                                        list-ref YOSEMITE 1
                                      )
                                    )
                                  )
                                )
                                 (
                                  to-str-space (
                                    haversine_distance (
                                      list-ref SAN_FRANCISCO 0
                                    )
                                     (
                                      list-ref SAN_FRANCISCO 1
                                    )
                                     (
                                      list-ref YOSEMITE 0
                                    )
                                     (
                                      list-ref YOSEMITE 1
                                    )
                                  )
                                )
                                 (
                                  to-str (
                                    to-str-space (
                                      haversine_distance (
                                        list-ref SAN_FRANCISCO 0
                                      )
                                       (
                                        list-ref SAN_FRANCISCO 1
                                      )
                                       (
                                        list-ref YOSEMITE 0
                                      )
                                       (
                                        list-ref YOSEMITE 1
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
          )
        )
      )
    )
     (
      let (
        (
          end17 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur18 (
              quotient (
                * (
                  - end17 start16
                )
                 1000000
              )
               jps19
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur18
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
