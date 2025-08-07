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
      start35 (
        current-jiffy
      )
    )
     (
      jps38 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        pow2_int n
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                result 1
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
                                  < i n
                                )
                                 (
                                  begin (
                                    set! result (
                                      * result 2
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
                    ret1 result
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
        pow2_float n
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            let (
              (
                result 1.0
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
                                      < i n
                                    )
                                     (
                                      begin (
                                        set! result (
                                          * result 2.0
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
                        let (
                          (
                            m (
                              - 0 n
                            )
                          )
                        )
                         (
                          begin (
                            call/cc (
                              lambda (
                                break8
                              )
                               (
                                letrec (
                                  (
                                    loop7 (
                                      lambda (
                                        
                                      )
                                       (
                                        if (
                                          < i m
                                        )
                                         (
                                          begin (
                                            set! result (
                                              _div result 2.0
                                            )
                                          )
                                           (
                                            set! i (
                                              + i 1
                                            )
                                          )
                                           (
                                            loop7
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
                                  loop7
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
                ret4 result
              )
            )
          )
        )
      )
    )
     (
      define (
        lshift num k
      )
       (
        call/cc (
          lambda (
            ret9
          )
           (
            let (
              (
                result num
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
                                  < i k
                                )
                                 (
                                  begin (
                                    set! result (
                                      * result 2
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
                    ret9 result
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
        rshift num k
      )
       (
        call/cc (
          lambda (
            ret12
          )
           (
            let (
              (
                result num
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
                        break14
                      )
                       (
                        letrec (
                          (
                            loop13 (
                              lambda (
                                
                              )
                               (
                                if (
                                  < i k
                                )
                                 (
                                  begin (
                                    set! result (
                                      _div (
                                        - result (
                                          _mod result 2
                                        )
                                      )
                                       2
                                    )
                                  )
                                   (
                                    set! i (
                                      + i 1
                                    )
                                  )
                                   (
                                    loop13
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
                          loop13
                        )
                      )
                    )
                  )
                   (
                    ret12 result
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
        log2_floor x
      )
       (
        call/cc (
          lambda (
            ret15
          )
           (
            let (
              (
                n x
              )
            )
             (
              begin (
                let (
                  (
                    e 0
                  )
                )
                 (
                  begin (
                    call/cc (
                      lambda (
                        break17
                      )
                       (
                        letrec (
                          (
                            loop16 (
                              lambda (
                                
                              )
                               (
                                if (
                                  >= n 2.0
                                )
                                 (
                                  begin (
                                    set! n (
                                      _div n 2.0
                                    )
                                  )
                                   (
                                    set! e (
                                      + e 1
                                    )
                                  )
                                   (
                                    loop16
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
                          loop16
                        )
                      )
                    )
                  )
                   (
                    call/cc (
                      lambda (
                        break19
                      )
                       (
                        letrec (
                          (
                            loop18 (
                              lambda (
                                
                              )
                               (
                                if (
                                  < n 1.0
                                )
                                 (
                                  begin (
                                    set! n (
                                      * n 2.0
                                    )
                                  )
                                   (
                                    set! e (
                                      - e 1
                                    )
                                  )
                                   (
                                    loop18
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
                          loop18
                        )
                      )
                    )
                  )
                   (
                    ret15 e
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
        float_to_bits x
      )
       (
        call/cc (
          lambda (
            ret20
          )
           (
            let (
              (
                num x
              )
            )
             (
              begin (
                let (
                  (
                    sign 0
                  )
                )
                 (
                  begin (
                    if (
                      < num 0.0
                    )
                     (
                      begin (
                        set! sign 1
                      )
                       (
                        set! num (
                          - num
                        )
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
                        exp (
                          log2_floor num
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            pow (
                              pow2_float exp
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                normalized (
                                  _div num pow
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    frac (
                                      - normalized 1.0
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        mantissa (
                                          let (
                                            (
                                              v21 (
                                                * frac (
                                                  pow2_float 23
                                                )
                                              )
                                            )
                                          )
                                           (
                                            cond (
                                              (
                                                string? v21
                                              )
                                               (
                                                inexact->exact (
                                                  floor (
                                                    string->number v21
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              (
                                                boolean? v21
                                              )
                                               (
                                                if v21 1 0
                                              )
                                            )
                                             (
                                              else (
                                                inexact->exact (
                                                  floor v21
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
                                            exp_bits (
                                              _add exp 127
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            ret20 (
                                              _add (
                                                _add (
                                                  lshift sign 31
                                                )
                                                 (
                                                  lshift exp_bits 23
                                                )
                                              )
                                               mantissa
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
        bits_to_float bits
      )
       (
        call/cc (
          lambda (
            ret22
          )
           (
            let (
              (
                sign_bit (
                  _mod (
                    rshift bits 31
                  )
                   2
                )
              )
            )
             (
              begin (
                let (
                  (
                    sign 1.0
                  )
                )
                 (
                  begin (
                    if (
                      equal? sign_bit 1
                    )
                     (
                      begin (
                        set! sign (
                          - 1.0
                        )
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
                        exp_bits (
                          _mod (
                            rshift bits 23
                          )
                           256
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            exp (
                              - exp_bits 127
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                mantissa_bits (
                                  _mod bits (
                                    pow2_int 23
                                  )
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    mantissa (
                                      _add 1.0 (
                                        _div (
                                          + 0.0 mantissa_bits
                                        )
                                         (
                                          pow2_float 23
                                        )
                                      )
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    ret22 (
                                      * (
                                        * sign mantissa
                                      )
                                       (
                                        pow2_float exp
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
        absf x
      )
       (
        call/cc (
          lambda (
            ret23
          )
           (
            begin (
              if (
                < x 0.0
              )
               (
                begin (
                  ret23 (
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
              ret23 x
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
            ret24
          )
           (
            begin (
              if (
                <= x 0.0
              )
               (
                begin (
                  ret24 0.0
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
                          break26
                        )
                         (
                          letrec (
                            (
                              loop25 (
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
                                      loop25
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
                            loop25
                          )
                        )
                      )
                    )
                     (
                      ret24 guess
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
        is_close a b rel_tol
      )
       (
        call/cc (
          lambda (
            ret27
          )
           (
            ret27 (
              _le (
                absf (
                  - a b
                )
              )
               (
                * rel_tol (
                  absf b
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        fast_inverse_sqrt number
      )
       (
        call/cc (
          lambda (
            ret28
          )
           (
            begin (
              if (
                <= number 0.0
              )
               (
                begin (
                  panic "Input must be a positive number."
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
                  i (
                    float_to_bits number
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      magic 1597463007
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          y_bits (
                            - magic (
                              rshift i 1
                            )
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              y (
                                bits_to_float y_bits
                              )
                            )
                          )
                           (
                            begin (
                              set! y (
                                * y (
                                  - 1.5 (
                                    * (
                                      * (
                                        * 0.5 number
                                      )
                                       y
                                    )
                                     y
                                  )
                                )
                              )
                            )
                             (
                              ret28 y
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
        test_fast_inverse_sqrt
      )
       (
        call/cc (
          lambda (
            ret29
          )
           (
            begin (
              if (
                _gt (
                  absf (
                    - (
                      fast_inverse_sqrt 10.0
                    )
                     0.3156857923527257
                  )
                )
                 0.0001
              )
               (
                begin (
                  panic "fast_inverse_sqrt(10) failed"
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                _gt (
                  absf (
                    - (
                      fast_inverse_sqrt 4.0
                    )
                     0.49915357479239103
                  )
                )
                 0.0001
              )
               (
                begin (
                  panic "fast_inverse_sqrt(4) failed"
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                _gt (
                  absf (
                    - (
                      fast_inverse_sqrt 4.1
                    )
                     0.4932849504615651
                  )
                )
                 0.0001
              )
               (
                begin (
                  panic "fast_inverse_sqrt(4.1) failed"
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
                  i 50
                )
              )
               (
                begin (
                  call/cc (
                    lambda (
                      break31
                    )
                     (
                      letrec (
                        (
                          loop30 (
                            lambda (
                              
                            )
                             (
                              if (
                                < i 60
                              )
                               (
                                begin (
                                  let (
                                    (
                                      y (
                                        fast_inverse_sqrt (
                                          + 0.0 i
                                        )
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      let (
                                        (
                                          actual (
                                            _div 1.0 (
                                              sqrtApprox (
                                                + 0.0 i
                                              )
                                            )
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          if (
                                            not (
                                              is_close y actual 0.00132
                                            )
                                          )
                                           (
                                            begin (
                                              panic "relative error too high"
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
                                  loop30
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
                        loop30
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
            ret32
          )
           (
            begin (
              test_fast_inverse_sqrt
            )
             (
              let (
                (
                  i 5
                )
              )
               (
                begin (
                  call/cc (
                    lambda (
                      break34
                    )
                     (
                      letrec (
                        (
                          loop33 (
                            lambda (
                              
                            )
                             (
                              if (
                                <= i 100
                              )
                               (
                                begin (
                                  let (
                                    (
                                      diff (
                                        - (
                                          _div 1.0 (
                                            sqrtApprox (
                                              + 0.0 i
                                            )
                                          )
                                        )
                                         (
                                          fast_inverse_sqrt (
                                            + 0.0 i
                                          )
                                        )
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
                                                to-str-space i
                                              )
                                               ": "
                                            )
                                             (
                                              to-str-space diff
                                            )
                                          )
                                        )
                                         (
                                          string-append (
                                            string-append (
                                              to-str-space i
                                            )
                                             ": "
                                          )
                                           (
                                            to-str-space diff
                                          )
                                        )
                                         (
                                          to-str (
                                            string-append (
                                              string-append (
                                                to-str-space i
                                              )
                                               ": "
                                            )
                                             (
                                              to-str-space diff
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
                                        + i 5
                                      )
                                    )
                                  )
                                )
                                 (
                                  loop33
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
                        loop33
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
      main
    )
     (
      let (
        (
          end36 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur37 (
              quotient (
                * (
                  - end36 start35
                )
                 1000000
              )
               jps38
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur37
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
