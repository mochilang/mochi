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
      define (
        pow10 n
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                p 1.0
              )
            )
             (
              begin (
                if (
                  >= n 0
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
                        )
                      )
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
                                      > i n
                                    )
                                     (
                                      begin (
                                        set! p (
                                          _div p 10.0
                                        )
                                      )
                                       (
                                        set! i (
                                          - i 1
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
                ret1 p
              )
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
            ret6
          )
           (
            let (
              (
                i (
                  let (
                    (
                      v7 x
                    )
                  )
                   (
                    cond (
                      (
                        string? v7
                      )
                       (
                        inexact->exact (
                          floor (
                            string->number v7
                          )
                        )
                      )
                    )
                     (
                      (
                        boolean? v7
                      )
                       (
                        if v7 1 0
                      )
                    )
                     (
                      else (
                        inexact->exact (
                          floor v7
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
                    f (
                      + 0.0 i
                    )
                  )
                )
                 (
                  begin (
                    if (
                      > f x
                    )
                     (
                      begin (
                        ret6 (
                          + 0.0 (
                            - i 1
                          )
                        )
                      )
                    )
                     '(
                      
                    )
                  )
                   (
                    ret6 f
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
        format_scientific_3 x
      )
       (
        call/cc (
          lambda (
            ret8
          )
           (
            begin (
              if (
                equal? x 0.0
              )
               (
                begin (
                  ret8 "0.000e+00"
                )
              )
               '(
                
              )
            )
             (
              let (
                (
                  sign ""
                )
              )
               (
                begin (
                  let (
                    (
                      num x
                    )
                  )
                   (
                    begin (
                      if (
                        < num 0.0
                      )
                       (
                        begin (
                          set! sign "-"
                        )
                         (
                          set! num (
                            - num
                          )
                        )
                      )
                       '(
                        
                      )
                    )
                     (
                      let (
                        (
                          exp 0
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
                                        >= num 10.0
                                      )
                                       (
                                        begin (
                                          set! num (
                                            _div num 10.0
                                          )
                                        )
                                         (
                                          set! exp (
                                            + exp 1
                                          )
                                        )
                                         (
                                          loop9
                                        )
                                      )
                                       '(
                                        
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
                                        < num 1.0
                                      )
                                       (
                                        begin (
                                          set! num (
                                            * num 10.0
                                          )
                                        )
                                         (
                                          set! exp (
                                            - exp 1
                                          )
                                        )
                                         (
                                          loop11
                                        )
                                      )
                                       '(
                                        
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
                              temp (
                                floor (
                                  _add (
                                    * num 1000.0
                                  )
                                   0.5
                                )
                              )
                            )
                          )
                           (
                            begin (
                              let (
                                (
                                  scaled (
                                    let (
                                      (
                                        v13 temp
                                      )
                                    )
                                     (
                                      cond (
                                        (
                                          string? v13
                                        )
                                         (
                                          inexact->exact (
                                            floor (
                                              string->number v13
                                            )
                                          )
                                        )
                                      )
                                       (
                                        (
                                          boolean? v13
                                        )
                                         (
                                          if v13 1 0
                                        )
                                      )
                                       (
                                        else (
                                          inexact->exact (
                                            floor v13
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
                                    equal? scaled 10000
                                  )
                                   (
                                    begin (
                                      set! scaled 1000
                                    )
                                     (
                                      set! exp (
                                        + exp 1
                                      )
                                    )
                                  )
                                   '(
                                    
                                  )
                                )
                                 (
                                  let (
                                    (
                                      int_part (
                                        _div scaled 1000
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      let (
                                        (
                                          frac_part (
                                            _mod scaled 1000
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          let (
                                            (
                                              frac_str (
                                                to-str-space frac_part
                                              )
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
                                                            < (
                                                              _len frac_str
                                                            )
                                                             3
                                                          )
                                                           (
                                                            begin (
                                                              set! frac_str (
                                                                string-append "0" frac_str
                                                              )
                                                            )
                                                             (
                                                              loop14
                                                            )
                                                          )
                                                           '(
                                                            
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
                                              let (
                                                (
                                                  mantissa (
                                                    string-append (
                                                      string-append (
                                                        to-str-space int_part
                                                      )
                                                       "."
                                                    )
                                                     frac_str
                                                  )
                                                )
                                              )
                                               (
                                                begin (
                                                  let (
                                                    (
                                                      exp_sign "+"
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      let (
                                                        (
                                                          exp_abs exp
                                                        )
                                                      )
                                                       (
                                                        begin (
                                                          if (
                                                            < exp 0
                                                          )
                                                           (
                                                            begin (
                                                              set! exp_sign "-"
                                                            )
                                                             (
                                                              set! exp_abs (
                                                                - exp
                                                              )
                                                            )
                                                          )
                                                           '(
                                                            
                                                          )
                                                        )
                                                         (
                                                          let (
                                                            (
                                                              exp_str (
                                                                to-str-space exp_abs
                                                              )
                                                            )
                                                          )
                                                           (
                                                            begin (
                                                              if (
                                                                < exp_abs 10
                                                              )
                                                               (
                                                                begin (
                                                                  set! exp_str (
                                                                    string-append "0" exp_str
                                                                  )
                                                                )
                                                              )
                                                               '(
                                                                
                                                              )
                                                            )
                                                             (
                                                              ret8 (
                                                                string-append (
                                                                  string-append (
                                                                    string-append (
                                                                      string-append sign mantissa
                                                                    )
                                                                     "e"
                                                                  )
                                                                   exp_sign
                                                                )
                                                                 exp_str
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
        orbital_transfer_work mass_central mass_object r_initial r_final
      )
       (
        call/cc (
          lambda (
            ret16
          )
           (
            let (
              (
                G (
                  * 6.6743 (
                    pow10 (
                      - 11
                    )
                  )
                )
              )
            )
             (
              begin (
                if (
                  or (
                    <= r_initial 0.0
                  )
                   (
                    <= r_final 0.0
                  )
                )
                 (
                  begin (
                    panic "Orbital radii must be greater than zero."
                  )
                )
                 '(
                  
                )
              )
               (
                let (
                  (
                    work (
                      * (
                        _div (
                          * (
                            * G mass_central
                          )
                           mass_object
                        )
                         2.0
                      )
                       (
                        - (
                          _div 1.0 r_initial
                        )
                         (
                          _div 1.0 r_final
                        )
                      )
                    )
                  )
                )
                 (
                  begin (
                    ret16 (
                      format_scientific_3 work
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
        test_orbital_transfer_work
      )
       (
        call/cc (
          lambda (
            ret17
          )
           (
            begin (
              if (
                not (
                  string=? (
                    orbital_transfer_work (
                      * 5.972 (
                        pow10 24
                      )
                    )
                     1000.0 (
                      * 6.371 (
                        pow10 6
                      )
                    )
                     (
                      * 7.0 (
                        pow10 6
                      )
                    )
                  )
                   "2.811e+09"
                )
              )
               (
                begin (
                  panic "case1 failed"
                )
              )
               '(
                
              )
            )
             (
              if (
                not (
                  string=? (
                    orbital_transfer_work (
                      * 5.972 (
                        pow10 24
                      )
                    )
                     500.0 (
                      * 7.0 (
                        pow10 6
                      )
                    )
                     (
                      * 6.371 (
                        pow10 6
                      )
                    )
                  )
                   "-1.405e+09"
                )
              )
               (
                begin (
                  panic "case2 failed"
                )
              )
               '(
                
              )
            )
             (
              if (
                not (
                  string=? (
                    orbital_transfer_work (
                      * 1.989 (
                        pow10 30
                      )
                    )
                     1000.0 (
                      * 1.5 (
                        pow10 11
                      )
                    )
                     (
                      * 2.28 (
                        pow10 11
                      )
                    )
                  )
                   "1.514e+11"
                )
              )
               (
                begin (
                  panic "case3 failed"
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
        main
      )
       (
        call/cc (
          lambda (
            ret18
          )
           (
            begin (
              test_orbital_transfer_work
            )
             (
              _display (
                if (
                  string? (
                    orbital_transfer_work (
                      * 5.972 (
                        pow10 24
                      )
                    )
                     1000.0 (
                      * 6.371 (
                        pow10 6
                      )
                    )
                     (
                      * 7.0 (
                        pow10 6
                      )
                    )
                  )
                )
                 (
                  orbital_transfer_work (
                    * 5.972 (
                      pow10 24
                    )
                  )
                   1000.0 (
                    * 6.371 (
                      pow10 6
                    )
                  )
                   (
                    * 7.0 (
                      pow10 6
                    )
                  )
                )
                 (
                  to-str (
                    orbital_transfer_work (
                      * 5.972 (
                        pow10 24
                      )
                    )
                     1000.0 (
                      * 6.371 (
                        pow10 6
                      )
                    )
                     (
                      * 7.0 (
                        pow10 6
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
     (
      main
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
