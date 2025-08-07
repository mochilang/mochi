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
      start19 (
        current-jiffy
      )
    )
     (
      jps22 (
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
            absf x
          )
           (
            call/cc (
              lambda (
                ret1
              )
               (
                begin (
                  if (
                    < x 0.0
                  )
                   (
                    begin (
                      ret1 (
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
                  ret1 x
                )
              )
            )
          )
        )
         (
          define (
            sqrt x
          )
           (
            call/cc (
              lambda (
                ret2
              )
               (
                begin (
                  if (
                    < x 0.0
                  )
                   (
                    begin (
                      panic "sqrt domain error"
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
                          ret2 guess
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
            ln x
          )
           (
            call/cc (
              lambda (
                ret5
              )
               (
                begin (
                  if (
                    <= x 0.0
                  )
                   (
                    begin (
                      panic "ln domain error"
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
                      y (
                        _div (
                          - x 1.0
                        )
                         (
                          + x 1.0
                        )
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
                              term y
                            )
                          )
                           (
                            begin (
                              let (
                                (
                                  sum 0.0
                                )
                              )
                               (
                                begin (
                                  let (
                                    (
                                      k 0
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
                                                    < k 10
                                                  )
                                                   (
                                                    begin (
                                                      let (
                                                        (
                                                          denom (
                                                            + 0.0 (
                                                              + (
                                                                * 2 k
                                                              )
                                                               1
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        begin (
                                                          set! sum (
                                                            _add sum (
                                                              _div term denom
                                                            )
                                                          )
                                                        )
                                                         (
                                                          set! term (
                                                            * term y2
                                                          )
                                                        )
                                                         (
                                                          set! k (
                                                            + k 1
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
                                      ret5 (
                                        * 2.0 sum
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
            exp_series x
          )
           (
            call/cc (
              lambda (
                ret8
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
                            n 1
                          )
                        )
                         (
                          begin (
                            call/cc (
                              lambda (
                                break10
                              )
                               (
                                letrec (
                                  (
                                    loop9 (
                                      lambda (
                                        
                                      )
                                       (
                                        if (
                                          < n 20
                                        )
                                         (
                                          begin (
                                            set! term (
                                              _div (
                                                * term x
                                              )
                                               (
                                                + 0.0 n
                                              )
                                            )
                                          )
                                           (
                                            set! sum (
                                              + sum term
                                            )
                                          )
                                           (
                                            set! n (
                                              + n 1
                                            )
                                          )
                                           (
                                            loop9
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
                                  loop9
                                )
                              )
                            )
                          )
                           (
                            ret8 sum
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
            powf base exponent
          )
           (
            call/cc (
              lambda (
                ret11
              )
               (
                begin (
                  if (
                    <= base 0.0
                  )
                   (
                    begin (
                      ret11 0.0
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  ret11 (
                    exp_series (
                      * exponent (
                        ln base
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
            integrand x z
          )
           (
            call/cc (
              lambda (
                ret12
              )
               (
                ret12 (
                  * (
                    powf x (
                      - z 1.0
                    )
                  )
                   (
                    exp_series (
                      - x
                    )
                  )
                )
              )
            )
          )
        )
         (
          define (
            gamma_iterative num
          )
           (
            call/cc (
              lambda (
                ret13
              )
               (
                begin (
                  if (
                    <= num 0.0
                  )
                   (
                    begin (
                      panic "math domain error"
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
                      step 0.001
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          limit 100.0
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              x step
                            )
                          )
                           (
                            begin (
                              let (
                                (
                                  total 0.0
                                )
                              )
                               (
                                begin (
                                  call/cc (
                                    lambda (
                                      break15
                                    )
                                     (
                                      letrec (
                                        (
                                          loop14 (
                                            lambda (
                                              
                                            )
                                             (
                                              if (
                                                < x limit
                                              )
                                               (
                                                begin (
                                                  set! total (
                                                    _add total (
                                                      * (
                                                        integrand x num
                                                      )
                                                       step
                                                    )
                                                  )
                                                )
                                                 (
                                                  set! x (
                                                    + x step
                                                  )
                                                )
                                                 (
                                                  loop14
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
                                        loop14
                                      )
                                    )
                                  )
                                )
                                 (
                                  ret13 total
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
            gamma_recursive num
          )
           (
            call/cc (
              lambda (
                ret16
              )
               (
                begin (
                  if (
                    <= num 0.0
                  )
                   (
                    begin (
                      panic "math domain error"
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  if (
                    > num 171.5
                  )
                   (
                    begin (
                      panic "math range error"
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
                      int_part (
                        let (
                          (
                            v17 num
                          )
                        )
                         (
                          cond (
                            (
                              string? v17
                            )
                             (
                              exact (
                                floor (
                                  string->number v17
                                )
                              )
                            )
                          )
                           (
                            (
                              boolean? v17
                            )
                             (
                              if v17 1 0
                            )
                          )
                           (
                            else (
                              exact (
                                floor v17
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
                          frac (
                            - num (
                              + 0.0 int_part
                            )
                          )
                        )
                      )
                       (
                        begin (
                          if (
                            not (
                              or (
                                _lt (
                                  absf frac
                                )
                                 1e-06
                              )
                               (
                                _lt (
                                  absf (
                                    - frac 0.5
                                  )
                                )
                                 1e-06
                              )
                            )
                          )
                           (
                            begin (
                              panic "num must be an integer or a half-integer"
                            )
                          )
                           (
                            quote (
                              
                            )
                          )
                        )
                         (
                          if (
                            _lt (
                              absf (
                                - num 0.5
                              )
                            )
                             1e-06
                          )
                           (
                            begin (
                              ret16 (
                                sqrt PI
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
                            _lt (
                              absf (
                                - num 1.0
                              )
                            )
                             1e-06
                          )
                           (
                            begin (
                              ret16 1.0
                            )
                          )
                           (
                            quote (
                              
                            )
                          )
                        )
                         (
                          ret16 (
                            * (
                              - num 1.0
                            )
                             (
                              gamma_recursive (
                                - num 1.0
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
                ret18
              )
               (
                begin (
                  _display (
                    if (
                      string? (
                        gamma_iterative 5.0
                      )
                    )
                     (
                      gamma_iterative 5.0
                    )
                     (
                      to-str (
                        gamma_iterative 5.0
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
                        gamma_recursive 5.0
                      )
                    )
                     (
                      gamma_recursive 5.0
                    )
                     (
                      to-str (
                        gamma_recursive 5.0
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
                        gamma_recursive 0.5
                      )
                    )
                     (
                      gamma_recursive 0.5
                    )
                     (
                      to-str (
                        gamma_recursive 0.5
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
         (
          main
        )
      )
    )
     (
      let (
        (
          end20 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur21 (
              quotient (
                * (
                  - end20 start19
                )
                 1000000
              )
               jps22
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur21
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
