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
      start32 (
        current-jiffy
      )
    )
     (
      jps35 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        complex_add a b
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            ret1 (
              alist->hash-table (
                _list (
                  cons "re" (
                    + (
                      hash-table-ref a "re"
                    )
                     (
                      hash-table-ref b "re"
                    )
                  )
                )
                 (
                  cons "im" (
                    + (
                      hash-table-ref a "im"
                    )
                     (
                      hash-table-ref b "im"
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
        complex_mul a b
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            let (
              (
                real (
                  - (
                    * (
                      hash-table-ref a "re"
                    )
                     (
                      hash-table-ref b "re"
                    )
                  )
                   (
                    * (
                      hash-table-ref a "im"
                    )
                     (
                      hash-table-ref b "im"
                    )
                  )
                )
              )
            )
             (
              begin (
                let (
                  (
                    imag (
                      _add (
                        * (
                          hash-table-ref a "re"
                        )
                         (
                          hash-table-ref b "im"
                        )
                      )
                       (
                        * (
                          hash-table-ref a "im"
                        )
                         (
                          hash-table-ref b "re"
                        )
                      )
                    )
                  )
                )
                 (
                  begin (
                    ret2 (
                      alist->hash-table (
                        _list (
                          cons "re" real
                        )
                         (
                          cons "im" imag
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
        sqrtApprox x
      )
       (
        call/cc (
          lambda (
            ret3
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
                                    loop4
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
                          loop4
                        )
                      )
                    )
                  )
                   (
                    ret3 guess
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
        complex_abs a
      )
       (
        call/cc (
          lambda (
            ret6
          )
           (
            ret6 (
              sqrtApprox (
                _add (
                  * (
                    hash-table-ref a "re"
                  )
                   (
                    hash-table-ref a "re"
                  )
                )
                 (
                  * (
                    hash-table-ref a "im"
                  )
                   (
                    hash-table-ref a "im"
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
        sin_taylor x
      )
       (
        call/cc (
          lambda (
            ret7
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
                                                  _add (
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
        cos_taylor x
      )
       (
        call/cc (
          lambda (
            ret10
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
                        ret10 sum
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
        exp_taylor x
      )
       (
        call/cc (
          lambda (
            ret13
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
                        i 1.0
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
                                      < i 20.0
                                    )
                                     (
                                      begin (
                                        set! term (
                                          _div (
                                            * term x
                                          )
                                           i
                                        )
                                      )
                                       (
                                        set! sum (
                                          + sum term
                                        )
                                      )
                                       (
                                        set! i (
                                          + i 1.0
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
                        ret13 sum
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
        complex_exp z
      )
       (
        call/cc (
          lambda (
            ret16
          )
           (
            let (
              (
                e (
                  exp_taylor (
                    hash-table-ref z "re"
                  )
                )
              )
            )
             (
              begin (
                ret16 (
                  alist->hash-table (
                    _list (
                      cons "re" (
                        * e (
                          cos_taylor (
                            hash-table-ref z "im"
                          )
                        )
                      )
                    )
                     (
                      cons "im" (
                        * e (
                          sin_taylor (
                            hash-table-ref z "im"
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
        eval_quadratic c z
      )
       (
        call/cc (
          lambda (
            ret17
          )
           (
            ret17 (
              complex_add (
                complex_mul z z
              )
               c
            )
          )
        )
      )
    )
     (
      define (
        eval_exponential c z
      )
       (
        call/cc (
          lambda (
            ret18
          )
           (
            ret18 (
              complex_add (
                complex_exp z
              )
               c
            )
          )
        )
      )
    )
     (
      define (
        iterate_function eval_function c nb_iterations z0 infinity
      )
       (
        call/cc (
          lambda (
            ret19
          )
           (
            let (
              (
                z_n z0
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
                        break21
                      )
                       (
                        letrec (
                          (
                            loop20 (
                              lambda (
                                
                              )
                               (
                                if (
                                  < i nb_iterations
                                )
                                 (
                                  begin (
                                    set! z_n (
                                      eval_function c z_n
                                    )
                                  )
                                   (
                                    if (
                                      _gt (
                                        complex_abs z_n
                                      )
                                       infinity
                                    )
                                     (
                                      begin (
                                        ret19 z_n
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
                                   (
                                    loop20
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
                          loop20
                        )
                      )
                    )
                  )
                   (
                    ret19 z_n
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
        prepare_grid window_size nb_pixels
      )
       (
        call/cc (
          lambda (
            ret22
          )
           (
            let (
              (
                grid (
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
                        break24
                      )
                       (
                        letrec (
                          (
                            loop23 (
                              lambda (
                                
                              )
                               (
                                if (
                                  < i nb_pixels
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
                                                          < j nb_pixels
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                real (
                                                                  _add (
                                                                    - window_size
                                                                  )
                                                                   (
                                                                    _div (
                                                                      * (
                                                                        * 2.0 window_size
                                                                      )
                                                                       (
                                                                        + 0.0 i
                                                                      )
                                                                    )
                                                                     (
                                                                      + 0.0 (
                                                                        - nb_pixels 1
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
                                                                    imag (
                                                                      _add (
                                                                        - window_size
                                                                      )
                                                                       (
                                                                        _div (
                                                                          * (
                                                                            * 2.0 window_size
                                                                          )
                                                                           (
                                                                            + 0.0 j
                                                                          )
                                                                        )
                                                                         (
                                                                          + 0.0 (
                                                                            - nb_pixels 1
                                                                          )
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
                                                                          alist->hash-table (
                                                                            _list (
                                                                              cons "re" real
                                                                            )
                                                                             (
                                                                              cons "im" imag
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
                                                                )
                                                              )
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
                                            set! grid (
                                              append grid (
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
                                    loop23
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
                          loop23
                        )
                      )
                    )
                  )
                   (
                    ret22 grid
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
        julia_demo
      )
       (
        call/cc (
          lambda (
            ret27
          )
           (
            let (
              (
                grid (
                  prepare_grid 1.0 5
                )
              )
            )
             (
              begin (
                let (
                  (
                    c_poly (
                      alist->hash-table (
                        _list (
                          cons "re" (
                            - 0.4
                          )
                        )
                         (
                          cons "im" 0.6
                        )
                      )
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        c_exp (
                          alist->hash-table (
                            _list (
                              cons "re" (
                                - 2.0
                              )
                            )
                             (
                              cons "im" 0.0
                            )
                          )
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            poly_result (
                              _list
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                exp_result (
                                  _list
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    y 0
                                  )
                                )
                                 (
                                  begin (
                                    call/cc (
                                      lambda (
                                        break29
                                      )
                                       (
                                        letrec (
                                          (
                                            loop28 (
                                              lambda (
                                                
                                              )
                                               (
                                                if (
                                                  < y (
                                                    _len grid
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        row_poly (
                                                          _list
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            row_exp (
                                                              _list
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                x 0
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
                                                                              < x (
                                                                                _len (
                                                                                  cond (
                                                                                    (
                                                                                      string? grid
                                                                                    )
                                                                                     (
                                                                                      _substring grid y (
                                                                                        + y 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? grid
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref grid y
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref grid y
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                let (
                                                                                  (
                                                                                    z0 (
                                                                                      cond (
                                                                                        (
                                                                                          string? (
                                                                                            cond (
                                                                                              (
                                                                                                string? grid
                                                                                              )
                                                                                               (
                                                                                                _substring grid y (
                                                                                                  + y 1
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              (
                                                                                                hash-table? grid
                                                                                              )
                                                                                               (
                                                                                                hash-table-ref grid y
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              else (
                                                                                                list-ref grid y
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          _substring (
                                                                                            cond (
                                                                                              (
                                                                                                string? grid
                                                                                              )
                                                                                               (
                                                                                                _substring grid y (
                                                                                                  + y 1
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              (
                                                                                                hash-table? grid
                                                                                              )
                                                                                               (
                                                                                                hash-table-ref grid y
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              else (
                                                                                                list-ref grid y
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           x (
                                                                                            + x 1
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        (
                                                                                          hash-table? (
                                                                                            cond (
                                                                                              (
                                                                                                string? grid
                                                                                              )
                                                                                               (
                                                                                                _substring grid y (
                                                                                                  + y 1
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              (
                                                                                                hash-table? grid
                                                                                              )
                                                                                               (
                                                                                                hash-table-ref grid y
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              else (
                                                                                                list-ref grid y
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          hash-table-ref (
                                                                                            cond (
                                                                                              (
                                                                                                string? grid
                                                                                              )
                                                                                               (
                                                                                                _substring grid y (
                                                                                                  + y 1
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              (
                                                                                                hash-table? grid
                                                                                              )
                                                                                               (
                                                                                                hash-table-ref grid y
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              else (
                                                                                                list-ref grid y
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           x
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        else (
                                                                                          list-ref (
                                                                                            cond (
                                                                                              (
                                                                                                string? grid
                                                                                              )
                                                                                               (
                                                                                                _substring grid y (
                                                                                                  + y 1
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              (
                                                                                                hash-table? grid
                                                                                              )
                                                                                               (
                                                                                                hash-table-ref grid y
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              else (
                                                                                                list-ref grid y
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           x
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    let (
                                                                                      (
                                                                                        z_poly (
                                                                                          iterate_function eval_quadratic c_poly 20 z0 4.0
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        let (
                                                                                          (
                                                                                            z_exp (
                                                                                              iterate_function eval_exponential c_exp 10 z0 10000000000.0
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            set! row_poly (
                                                                                              append row_poly (
                                                                                                _list (
                                                                                                  if (
                                                                                                    _lt (
                                                                                                      complex_abs z_poly
                                                                                                    )
                                                                                                     2.0
                                                                                                  )
                                                                                                   1 0
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            set! row_exp (
                                                                                              append row_exp (
                                                                                                _list (
                                                                                                  if (
                                                                                                    _lt (
                                                                                                      complex_abs z_exp
                                                                                                    )
                                                                                                     10000.0
                                                                                                  )
                                                                                                   1 0
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            set! x (
                                                                                              + x 1
                                                                                            )
                                                                                          )
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
                                                               (
                                                                set! poly_result (
                                                                  append poly_result (
                                                                    _list row_poly
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                set! exp_result (
                                                                  append exp_result (
                                                                    _list row_exp
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                set! y (
                                                                  + y 1
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    loop28
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
                                          loop28
                                        )
                                      )
                                    )
                                  )
                                   (
                                    _display (
                                      if (
                                        string? poly_result
                                      )
                                       poly_result (
                                        to-str poly_result
                                      )
                                    )
                                  )
                                   (
                                    newline
                                  )
                                   (
                                    _display (
                                      if (
                                        string? exp_result
                                      )
                                       exp_result (
                                        to-str exp_result
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
     (
      julia_demo
    )
     (
      let (
        (
          end33 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur34 (
              quotient (
                * (
                  - end33 start32
                )
                 1000000
              )
               jps35
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur34
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
