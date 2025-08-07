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
      start50 (
        current-jiffy
      )
    )
     (
      jps53 (
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
            sinApprox x
          )
           (
            call/cc (
              lambda (
                ret1
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
                            n 1
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
                                          <= n 10
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                denom (
                                                  + 0.0 (
                                                    * (
                                                      * 2 n
                                                    )
                                                     (
                                                      + (
                                                        * 2 n
                                                      )
                                                       1
                                                    )
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
                                                   denom
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
                            ret1 sum
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
            cosApprox x
          )
           (
            call/cc (
              lambda (
                ret4
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
                                          <= n 10
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                denom (
                                                  + 0.0 (
                                                    * (
                                                      - (
                                                        * 2 n
                                                      )
                                                       1
                                                    )
                                                     (
                                                      * 2 n
                                                    )
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
                                                   denom
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
            expApprox x
          )
           (
            call/cc (
              lambda (
                ret7
              )
               (
                let (
                  (
                    sum 1.0
                  )
                )
                 (
                  begin (
                    let (
                      (
                        term 1.0
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
                                          < n 10
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
            ln x
          )
           (
            call/cc (
              lambda (
                ret10
              )
               (
                let (
                  (
                    t (
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
                        term t
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
                                n 1
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
                                              <= n 19
                                            )
                                             (
                                              begin (
                                                set! sum (
                                                  _add sum (
                                                    _div term (
                                                      + 0.0 n
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                set! term (
                                                  * (
                                                    * term t
                                                  )
                                                   t
                                                )
                                              )
                                               (
                                                set! n (
                                                  + n 2
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
                                ret10 (
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
         (
          define (
            log10 x
          )
           (
            call/cc (
              lambda (
                ret13
              )
               (
                ret13 (
                  _div (
                    ln x
                  )
                   (
                    ln 10.0
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
                ret14
              )
               (
                begin (
                  if (
                    <= x 0.0
                  )
                   (
                    begin (
                      ret14 0.0
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
                      guess x
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
                              break16
                            )
                             (
                              letrec (
                                (
                                  loop15 (
                                    lambda (
                                      
                                    )
                                     (
                                      if (
                                        < i 10
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
                                          loop15
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
                                loop15
                              )
                            )
                          )
                        )
                         (
                          ret14 guess
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
                ret17
              )
               (
                begin (
                  if (
                    < x 0.0
                  )
                   (
                    begin (
                      ret17 (
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
                  ret17 x
                )
              )
            )
          )
        )
         (
          define (
            normalize audio
          )
           (
            call/cc (
              lambda (
                ret18
              )
               (
                let (
                  (
                    max_val 0.0
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
                            break20
                          )
                           (
                            letrec (
                              (
                                loop19 (
                                  lambda (
                                    
                                  )
                                   (
                                    if (
                                      < i (
                                        _len audio
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            v (
                                              absf (
                                                list-ref audio i
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              _gt v max_val
                                            )
                                             (
                                              begin (
                                                set! max_val v
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
                                       (
                                        loop19
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
                              loop19
                            )
                          )
                        )
                      )
                       (
                        let (
                          (
                            res (
                              _list
                            )
                          )
                        )
                         (
                          begin (
                            set! i 0
                          )
                           (
                            call/cc (
                              lambda (
                                break22
                              )
                               (
                                letrec (
                                  (
                                    loop21 (
                                      lambda (
                                        
                                      )
                                       (
                                        if (
                                          < i (
                                            _len audio
                                          )
                                        )
                                         (
                                          begin (
                                            set! res (
                                              append res (
                                                _list (
                                                  _div (
                                                    list-ref audio i
                                                  )
                                                   max_val
                                                )
                                              )
                                            )
                                          )
                                           (
                                            set! i (
                                              + i 1
                                            )
                                          )
                                           (
                                            loop21
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
                                  loop21
                                )
                              )
                            )
                          )
                           (
                            ret18 res
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
            dft frame bins
          )
           (
            call/cc (
              lambda (
                ret23
              )
               (
                let (
                  (
                    N (
                      _len frame
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        spec (
                          _list
                        )
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
                                break25
                              )
                               (
                                letrec (
                                  (
                                    loop24 (
                                      lambda (
                                        
                                      )
                                       (
                                        if (
                                          < k bins
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                real 0.0
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    imag 0.0
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        n 0
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        call/cc (
                                                          lambda (
                                                            break27
                                                          )
                                                           (
                                                            letrec (
                                                              (
                                                                loop26 (
                                                                  lambda (
                                                                    
                                                                  )
                                                                   (
                                                                    if (
                                                                      < n N
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            angle (
                                                                              _div (
                                                                                * (
                                                                                  * (
                                                                                    * (
                                                                                      - 2.0
                                                                                    )
                                                                                     PI
                                                                                  )
                                                                                   (
                                                                                    + 0.0 k
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  + 0.0 n
                                                                                )
                                                                              )
                                                                               (
                                                                                + 0.0 N
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            set! real (
                                                                              _add real (
                                                                                * (
                                                                                  list-ref frame n
                                                                                )
                                                                                 (
                                                                                  cosApprox angle
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            set! imag (
                                                                              _add imag (
                                                                                * (
                                                                                  list-ref frame n
                                                                                )
                                                                                 (
                                                                                  sinApprox angle
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            set! n (
                                                                              + n 1
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        loop26
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
                                                              loop26
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        set! spec (
                                                          append spec (
                                                            _list (
                                                              _add (
                                                                * real real
                                                              )
                                                               (
                                                                * imag imag
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        set! k (
                                                          + k 1
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                           (
                                            loop24
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
                                  loop24
                                )
                              )
                            )
                          )
                           (
                            ret23 spec
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
            triangular_filters bins spectrum_size
          )
           (
            call/cc (
              lambda (
                ret28
              )
               (
                let (
                  (
                    filters (
                      _list
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        b 0
                      )
                    )
                     (
                      begin (
                        call/cc (
                          lambda (
                            break30
                          )
                           (
                            letrec (
                              (
                                loop29 (
                                  lambda (
                                    
                                  )
                                   (
                                    if (
                                      < b bins
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            center (
                                              _div (
                                                * (
                                                  + b 1
                                                )
                                                 spectrum_size
                                              )
                                               (
                                                + bins 1
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                filt (
                                                  _list
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
                                                        break32
                                                      )
                                                       (
                                                        letrec (
                                                          (
                                                            loop31 (
                                                              lambda (
                                                                
                                                              )
                                                               (
                                                                if (
                                                                  < i spectrum_size
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        v 0.0
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        if (
                                                                          <= i center
                                                                        )
                                                                         (
                                                                          begin (
                                                                            set! v (
                                                                              _div (
                                                                                + 0.0 i
                                                                              )
                                                                               (
                                                                                + 0.0 center
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            set! v (
                                                                              _div (
                                                                                + 0.0 (
                                                                                  - spectrum_size i
                                                                                )
                                                                              )
                                                                               (
                                                                                + 0.0 (
                                                                                  - spectrum_size center
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        set! filt (
                                                                          append filt (
                                                                            _list v
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
                                                                    loop31
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
                                                          loop31
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    set! filters (
                                                      append filters (
                                                        _list filt
                                                      )
                                                    )
                                                  )
                                                   (
                                                    set! b (
                                                      + b 1
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                       (
                                        loop29
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
                              loop29
                            )
                          )
                        )
                      )
                       (
                        ret28 filters
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
            dot mat vec
          )
           (
            call/cc (
              lambda (
                ret33
              )
               (
                let (
                  (
                    res (
                      _list
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
                            break35
                          )
                           (
                            letrec (
                              (
                                loop34 (
                                  lambda (
                                    
                                  )
                                   (
                                    if (
                                      < i (
                                        _len mat
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
                                                j 0
                                              )
                                            )
                                             (
                                              begin (
                                                call/cc (
                                                  lambda (
                                                    break37
                                                  )
                                                   (
                                                    letrec (
                                                      (
                                                        loop36 (
                                                          lambda (
                                                            
                                                          )
                                                           (
                                                            if (
                                                              < j (
                                                                _len vec
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                set! sum (
                                                                  _add sum (
                                                                    * (
                                                                      cond (
                                                                        (
                                                                          string? (
                                                                            list-ref mat i
                                                                          )
                                                                        )
                                                                         (
                                                                          _substring (
                                                                            list-ref mat i
                                                                          )
                                                                           j (
                                                                            + j 1
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        (
                                                                          hash-table? (
                                                                            list-ref mat i
                                                                          )
                                                                        )
                                                                         (
                                                                          hash-table-ref (
                                                                            list-ref mat i
                                                                          )
                                                                           j
                                                                        )
                                                                      )
                                                                       (
                                                                        else (
                                                                          list-ref (
                                                                            list-ref mat i
                                                                          )
                                                                           j
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      list-ref vec j
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                set! j (
                                                                  + j 1
                                                                )
                                                              )
                                                               (
                                                                loop36
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
                                                      loop36
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                set! res (
                                                  append res (
                                                    _list sum
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
                                        loop34
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
                              loop34
                            )
                          )
                        )
                      )
                       (
                        ret33 res
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
            discrete_cosine_transform dct_filter_num filter_num
          )
           (
            call/cc (
              lambda (
                ret38
              )
               (
                let (
                  (
                    basis (
                      _list
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
                            break40
                          )
                           (
                            letrec (
                              (
                                loop39 (
                                  lambda (
                                    
                                  )
                                   (
                                    if (
                                      < i dct_filter_num
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            row (
                                              _list
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                j 0
                                              )
                                            )
                                             (
                                              begin (
                                                call/cc (
                                                  lambda (
                                                    break42
                                                  )
                                                   (
                                                    letrec (
                                                      (
                                                        loop41 (
                                                          lambda (
                                                            
                                                          )
                                                           (
                                                            if (
                                                              < j filter_num
                                                            )
                                                             (
                                                              begin (
                                                                if (
                                                                  equal? i 0
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! row (
                                                                      append row (
                                                                        _list (
                                                                          _div 1.0 (
                                                                            sqrtApprox (
                                                                              + 0.0 filter_num
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
                                                                        angle (
                                                                          _div (
                                                                            * (
                                                                              * (
                                                                                + 0.0 (
                                                                                  + (
                                                                                    * 2 j
                                                                                  )
                                                                                   1
                                                                                )
                                                                              )
                                                                               (
                                                                                + 0.0 i
                                                                              )
                                                                            )
                                                                             PI
                                                                          )
                                                                           (
                                                                            * 2.0 (
                                                                              + 0.0 filter_num
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        set! row (
                                                                          append row (
                                                                            _list (
                                                                              * (
                                                                                cosApprox angle
                                                                              )
                                                                               (
                                                                                sqrtApprox (
                                                                                  _div 2.0 (
                                                                                    + 0.0 filter_num
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
                                                                set! j (
                                                                  + j 1
                                                                )
                                                              )
                                                               (
                                                                loop41
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
                                                      loop41
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                set! basis (
                                                  append basis (
                                                    _list row
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
                                        loop39
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
                              loop39
                            )
                          )
                        )
                      )
                       (
                        ret38 basis
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
            mfcc audio bins dct_num
          )
           (
            call/cc (
              lambda (
                ret43
              )
               (
                let (
                  (
                    norm (
                      normalize audio
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        spec (
                          dft norm (
                            + bins 2
                          )
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            filters (
                              triangular_filters bins (
                                _len spec
                              )
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                energies (
                                  dot filters spec
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    logfb (
                                      _list
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
                                            break45
                                          )
                                           (
                                            letrec (
                                              (
                                                loop44 (
                                                  lambda (
                                                    
                                                  )
                                                   (
                                                    if (
                                                      < i (
                                                        _len energies
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        set! logfb (
                                                          append logfb (
                                                            _list (
                                                              * 10.0 (
                                                                log10 (
                                                                  _add (
                                                                    cond (
                                                                      (
                                                                        string? energies
                                                                      )
                                                                       (
                                                                        _substring energies i (
                                                                          + i 1
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      (
                                                                        hash-table? energies
                                                                      )
                                                                       (
                                                                        hash-table-ref energies i
                                                                      )
                                                                    )
                                                                     (
                                                                      else (
                                                                        list-ref energies i
                                                                      )
                                                                    )
                                                                  )
                                                                   1e-10
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        set! i (
                                                          + i 1
                                                        )
                                                      )
                                                       (
                                                        loop44
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
                                              loop44
                                            )
                                          )
                                        )
                                      )
                                       (
                                        let (
                                          (
                                            dct_basis (
                                              discrete_cosine_transform dct_num bins
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                res (
                                                  dot dct_basis logfb
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                if (
                                                  equal? (
                                                    _len res
                                                  )
                                                   0
                                                )
                                                 (
                                                  begin (
                                                    set! res (
                                                      _list 0.0 0.0 0.0
                                                    )
                                                  )
                                                )
                                                 (
                                                  quote (
                                                    
                                                  )
                                                )
                                              )
                                               (
                                                ret43 res
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
              sample_rate 8000
            )
          )
           (
            begin (
              let (
                (
                  size 16
                )
              )
               (
                begin (
                  let (
                    (
                      audio (
                        _list
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          n 0
                        )
                      )
                       (
                        begin (
                          call/cc (
                            lambda (
                              break47
                            )
                             (
                              letrec (
                                (
                                  loop46 (
                                    lambda (
                                      
                                    )
                                     (
                                      if (
                                        < n size
                                      )
                                       (
                                        begin (
                                          let (
                                            (
                                              t (
                                                _div (
                                                  + 0.0 n
                                                )
                                                 (
                                                  + 0.0 sample_rate
                                                )
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              set! audio (
                                                append audio (
                                                  _list (
                                                    sinApprox (
                                                      * (
                                                        * (
                                                          * 2.0 PI
                                                        )
                                                         440.0
                                                      )
                                                       t
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              set! n (
                                                + n 1
                                              )
                                            )
                                          )
                                        )
                                         (
                                          loop46
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
                                loop46
                              )
                            )
                          )
                        )
                         (
                          let (
                            (
                              coeffs (
                                mfcc audio 5 3
                              )
                            )
                          )
                           (
                            begin (
                              call/cc (
                                lambda (
                                  break49
                                )
                                 (
                                  letrec (
                                    (
                                      loop48 (
                                        lambda (
                                          xs
                                        )
                                         (
                                          if (
                                            null? xs
                                          )
                                           (
                                            quote (
                                              
                                            )
                                          )
                                           (
                                            begin (
                                              let (
                                                (
                                                  c (
                                                    car xs
                                                  )
                                                )
                                              )
                                               (
                                                begin (
                                                  _display (
                                                    if (
                                                      string? c
                                                    )
                                                     c (
                                                      to-str c
                                                    )
                                                  )
                                                )
                                                 (
                                                  newline
                                                )
                                              )
                                            )
                                             (
                                              loop48 (
                                                cdr xs
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                   (
                                    loop48 coeffs
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
          end51 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur52 (
              quotient (
                * (
                  - end51 start50
                )
                 1000000
              )
               jps53
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur52
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
