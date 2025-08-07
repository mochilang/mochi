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
      start12 (
        current-jiffy
      )
    )
     (
      jps15 (
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
          define (
            floor x
          )
           (
            call/cc (
              lambda (
                ret1
              )
               (
                let (
                  (
                    i (
                      let (
                        (
                          v2 x
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
                     (
                      quote (
                        
                      )
                    )
                  )
                   (
                    ret1 (
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
            modf x m
          )
           (
            call/cc (
              lambda (
                ret3
              )
               (
                ret3 (
                  - x (
                    * (
                      floor (
                        _div x m
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
            sin_taylor x
          )
           (
            call/cc (
              lambda (
                ret4
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
                            ret4 sum
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
                ret7
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
                            ret7 sum
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
            convert_to_2d x y z scale distance
          )
           (
            call/cc (
              lambda (
                ret10
              )
               (
                let (
                  (
                    projected_x (
                      * (
                        _div (
                          * x distance
                        )
                         (
                          + z distance
                        )
                      )
                       scale
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        projected_y (
                          * (
                            _div (
                              * y distance
                            )
                             (
                              + z distance
                            )
                          )
                           scale
                        )
                      )
                    )
                     (
                      begin (
                        ret10 (
                          _list projected_x projected_y
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
            rotate x y z axis angle
          )
           (
            call/cc (
              lambda (
                ret11
              )
               (
                let (
                  (
                    angle (
                      _div (
                        * (
                          _div (
                            modf angle 360.0
                          )
                           450.0
                        )
                         180.0
                      )
                       PI
                    )
                  )
                )
                 (
                  begin (
                    set! angle (
                      modf angle (
                        * 2.0 PI
                      )
                    )
                  )
                   (
                    if (
                      _gt angle PI
                    )
                     (
                      begin (
                        set! angle (
                          - angle (
                            * 2.0 PI
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
                      string=? axis "z"
                    )
                     (
                      begin (
                        let (
                          (
                            new_x (
                              - (
                                * x (
                                  cos_taylor angle
                                )
                              )
                               (
                                * y (
                                  sin_taylor angle
                                )
                              )
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                new_y (
                                  _add (
                                    * y (
                                      cos_taylor angle
                                    )
                                  )
                                   (
                                    * x (
                                      sin_taylor angle
                                    )
                                  )
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    new_z z
                                  )
                                )
                                 (
                                  begin (
                                    ret11 (
                                      _list new_x new_y new_z
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
                      quote (
                        
                      )
                    )
                  )
                   (
                    if (
                      string=? axis "x"
                    )
                     (
                      begin (
                        let (
                          (
                            new_y (
                              - (
                                * y (
                                  cos_taylor angle
                                )
                              )
                               (
                                * z (
                                  sin_taylor angle
                                )
                              )
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                new_z (
                                  _add (
                                    * z (
                                      cos_taylor angle
                                    )
                                  )
                                   (
                                    * y (
                                      sin_taylor angle
                                    )
                                  )
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    new_x x
                                  )
                                )
                                 (
                                  begin (
                                    ret11 (
                                      _list new_x new_y new_z
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
                      quote (
                        
                      )
                    )
                  )
                   (
                    if (
                      string=? axis "y"
                    )
                     (
                      begin (
                        let (
                          (
                            new_x (
                              - (
                                * x (
                                  cos_taylor angle
                                )
                              )
                               (
                                * z (
                                  sin_taylor angle
                                )
                              )
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                new_z (
                                  _add (
                                    * z (
                                      cos_taylor angle
                                    )
                                  )
                                   (
                                    * x (
                                      sin_taylor angle
                                    )
                                  )
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    new_y y
                                  )
                                )
                                 (
                                  begin (
                                    ret11 (
                                      _list new_x new_y new_z
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
                      quote (
                        
                      )
                    )
                  )
                   (
                    _display (
                      if (
                        string? "not a valid axis, choose one of 'x', 'y', 'z'"
                      )
                       "not a valid axis, choose one of 'x', 'y', 'z'" (
                        to-str "not a valid axis, choose one of 'x', 'y', 'z'"
                      )
                    )
                  )
                   (
                    newline
                  )
                   (
                    ret11 (
                      _list 0.0 0.0 0.0
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
                  convert_to_2d 1.0 2.0 3.0 10.0 10.0
                )
              )
            )
             (
              to-str-space (
                convert_to_2d 1.0 2.0 3.0 10.0 10.0
              )
            )
             (
              to-str (
                to-str-space (
                  convert_to_2d 1.0 2.0 3.0 10.0 10.0
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
                  rotate 1.0 2.0 3.0 "y" 90.0
                )
              )
            )
             (
              to-str-space (
                rotate 1.0 2.0 3.0 "y" 90.0
              )
            )
             (
              to-str (
                to-str-space (
                  rotate 1.0 2.0 3.0 "y" 90.0
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
     (
      let (
        (
          end13 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur14 (
              quotient (
                * (
                  - end13 start12
                )
                 1000000
              )
               jps15
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur14
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
