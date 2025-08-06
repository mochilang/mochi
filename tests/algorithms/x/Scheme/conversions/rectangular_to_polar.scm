;; Generated on 2025-08-06 23:15 +0700
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
          define (
            sqrtApprox x
          )
           (
            call/cc (
              lambda (
                ret1
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
                        ret1 guess
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
                ret4
              )
               (
                begin (
                  if (
                    > x 1.0
                  )
                   (
                    begin (
                      ret4 (
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
                      ret4 (
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
                  ret4 (
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
                ret5
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
                          r (
                            atanApprox (
                              _div y x
                            )
                          )
                        )
                      )
                       (
                        begin (
                          ret5 r
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
                          ret5 (
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
                      ret5 (
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
                      ret5 (
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
                      ret5 (
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
                  ret5 0.0
                )
              )
            )
          )
        )
         (
          define (
            deg rad
          )
           (
            call/cc (
              lambda (
                ret6
              )
               (
                ret6 (
                  _div (
                    * rad 180.0
                  )
                   PI
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
                ret7
              )
               (
                let (
                  (
                    i (
                      let (
                        (
                          v8 x
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
                    ret7 (
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
                ret9
              )
               (
                let (
                  (
                    p 1.0
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
                                      < i n
                                    )
                                     (
                                      begin (
                                        set! p (
                                          * p 10.0
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
                        ret9 p
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
                ret12
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
                    ret12 (
                      _div (
                        floor (
                          _add (
                            * x m
                          )
                           0.5
                        )
                      )
                       m
                    )
                  )
                )
              )
            )
          )
        )
         (
          define (
            rectangular_to_polar real img
          )
           (
            call/cc (
              lambda (
                ret13
              )
               (
                let (
                  (
                    mod (
                      round (
                        sqrtApprox (
                          _add (
                            * real real
                          )
                           (
                            * img img
                          )
                        )
                      )
                       2
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        ang (
                          round (
                            deg (
                              atan2Approx img real
                            )
                          )
                           2
                        )
                      )
                    )
                     (
                      begin (
                        ret13 (
                          _list mod ang
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
            show real img
          )
           (
            call/cc (
              lambda (
                ret14
              )
               (
                let (
                  (
                    r (
                      rectangular_to_polar real img
                    )
                  )
                )
                 (
                  begin (
                    _display (
                      if (
                        string? (
                          to-str-space r
                        )
                      )
                       (
                        to-str-space r
                      )
                       (
                        to-str (
                          to-str-space r
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
         (
          show 5.0 (
            - 5.0
          )
        )
         (
          show (
            - 1.0
          )
           1.0
        )
         (
          show (
            - 1.0
          )
           (
            - 1.0
          )
        )
         (
          show 1e-10 1e-10
        )
         (
          show (
            - 1e-10
          )
           1e-10
        )
         (
          show 9.75 5.93
        )
         (
          show 10000.0 99999.0
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
