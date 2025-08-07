;; Generated on 2025-08-07 16:11 +0700
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
      start43 (
        current-jiffy
      )
    )
     (
      jps46 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        c_add a b
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
        c_sub a b
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            ret2 (
              alist->hash-table (
                _list (
                  cons "re" (
                    - (
                      hash-table-ref a "re"
                    )
                     (
                      hash-table-ref b "re"
                    )
                  )
                )
                 (
                  cons "im" (
                    - (
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
        c_mul a b
      )
       (
        call/cc (
          lambda (
            ret3
          )
           (
            ret3 (
              alist->hash-table (
                _list (
                  cons "re" (
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
                 (
                  cons "im" (
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
            )
          )
        )
      )
    )
     (
      define (
        c_mul_scalar a s
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            ret4 (
              alist->hash-table (
                _list (
                  cons "re" (
                    * (
                      hash-table-ref a "re"
                    )
                     s
                  )
                )
                 (
                  cons "im" (
                    * (
                      hash-table-ref a "im"
                    )
                     s
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
        c_div_scalar a s
      )
       (
        call/cc (
          lambda (
            ret5
          )
           (
            ret5 (
              alist->hash-table (
                _list (
                  cons "re" (
                    _div (
                      hash-table-ref a "re"
                    )
                     s
                  )
                )
                 (
                  cons "im" (
                    _div (
                      hash-table-ref a "im"
                    )
                     s
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
          PI 3.141592653589793
        )
      )
       (
        begin (
          define (
            sin_taylor x
          )
           (
            call/cc (
              lambda (
                ret6
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
                                            loop7
                                          )
                                        )
                                         '(
                                          
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
                           (
                            ret6 sum
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
                ret9
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
                                            loop10
                                          )
                                        )
                                         '(
                                          
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
                            ret9 sum
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
            exp_i theta
          )
           (
            call/cc (
              lambda (
                ret12
              )
               (
                ret12 (
                  alist->hash-table (
                    _list (
                      cons "re" (
                        cos_taylor theta
                      )
                    )
                     (
                      cons "im" (
                        sin_taylor theta
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
            make_complex_list n value
          )
           (
            call/cc (
              lambda (
                ret13
              )
               (
                let (
                  (
                    arr (
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
                                      < i n
                                    )
                                     (
                                      begin (
                                        set! arr (
                                          append arr (
                                            _list value
                                          )
                                        )
                                      )
                                       (
                                        set! i (
                                          + i 1
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
                        ret13 arr
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
            fft a invert
          )
           (
            call/cc (
              lambda (
                ret16
              )
               (
                let (
                  (
                    n (
                      _len a
                    )
                  )
                )
                 (
                  begin (
                    if (
                      equal? n 1
                    )
                     (
                      begin (
                        ret16 (
                          _list (
                            list-ref-safe a 0
                          )
                        )
                      )
                    )
                     '(
                      
                    )
                  )
                   (
                    let (
                      (
                        a0 (
                          _list
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            a1 (
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
                                    break18
                                  )
                                   (
                                    letrec (
                                      (
                                        loop17 (
                                          lambda (
                                            
                                          )
                                           (
                                            if (
                                              < i (
                                                _div n 2
                                              )
                                            )
                                             (
                                              begin (
                                                set! a0 (
                                                  append a0 (
                                                    _list (
                                                      list-ref-safe a (
                                                        * 2 i
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                set! a1 (
                                                  append a1 (
                                                    _list (
                                                      list-ref-safe a (
                                                        + (
                                                          * 2 i
                                                        )
                                                         1
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
                                                loop17
                                              )
                                            )
                                             '(
                                              
                                            )
                                          )
                                        )
                                      )
                                    )
                                     (
                                      loop17
                                    )
                                  )
                                )
                              )
                               (
                                let (
                                  (
                                    y0 (
                                      fft a0 invert
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        y1 (
                                          fft a1 invert
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            angle (
                                              * (
                                                _div (
                                                  * 2.0 PI
                                                )
                                                 (
                                                  + 0.0 n
                                                )
                                              )
                                               (
                                                if invert (
                                                  - 1.0
                                                )
                                                 1.0
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                w (
                                                  alist->hash-table (
                                                    _list (
                                                      cons "re" 1.0
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
                                                    wn (
                                                      exp_i angle
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        y (
                                                          make_complex_list n (
                                                            alist->hash-table (
                                                              _list (
                                                                cons "re" 0.0
                                                              )
                                                               (
                                                                cons "im" 0.0
                                                              )
                                                            )
                                                          )
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
                                                                        _div n 2
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            t (
                                                                              c_mul w (
                                                                                cond (
                                                                                  (
                                                                                    string? y1
                                                                                  )
                                                                                   (
                                                                                    _substring y1 i (
                                                                                      + i 1
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  (
                                                                                    hash-table? y1
                                                                                  )
                                                                                   (
                                                                                    hash-table-ref y1 i
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  else (
                                                                                    list-ref-safe y1 i
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
                                                                                u (
                                                                                  cond (
                                                                                    (
                                                                                      string? y0
                                                                                    )
                                                                                     (
                                                                                      _substring y0 i (
                                                                                        + i 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? y0
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref y0 i
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref-safe y0 i
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                let (
                                                                                  (
                                                                                    even (
                                                                                      c_add u t
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    let (
                                                                                      (
                                                                                        odd (
                                                                                          c_sub u t
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        if invert (
                                                                                          begin (
                                                                                            set! even (
                                                                                              c_div_scalar even 2.0
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            set! odd (
                                                                                              c_div_scalar odd 2.0
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         '(
                                                                                          
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        list-set! y i even
                                                                                      )
                                                                                       (
                                                                                        list-set! y (
                                                                                          + i (
                                                                                            _div n 2
                                                                                          )
                                                                                        )
                                                                                         odd
                                                                                      )
                                                                                       (
                                                                                        set! w (
                                                                                          c_mul w wn
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
                                                                       (
                                                                        loop19
                                                                      )
                                                                    )
                                                                     '(
                                                                      
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
                                                        ret16 y
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
            floor x
          )
           (
            call/cc (
              lambda (
                ret21
              )
               (
                let (
                  (
                    i (
                      let (
                        (
                          v22 x
                        )
                      )
                       (
                        cond (
                          (
                            string? v22
                          )
                           (
                            inexact->exact (
                              floor (
                                string->number v22
                              )
                            )
                          )
                        )
                         (
                          (
                            boolean? v22
                          )
                           (
                            if v22 1 0
                          )
                        )
                         (
                          else (
                            inexact->exact (
                              floor v22
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
                     '(
                      
                    )
                  )
                   (
                    ret21 (
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
                ret23
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
                                        loop24
                                      )
                                    )
                                     '(
                                      
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
                        ret23 p
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
            round_to x ndigits
          )
           (
            call/cc (
              lambda (
                ret26
              )
               (
                let (
                  (
                    m (
                      pow10 ndigits
                    )
                  )
                )
                 (
                  begin (
                    ret26 (
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
            list_to_string l
          )
           (
            call/cc (
              lambda (
                ret27
              )
               (
                let (
                  (
                    s "["
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
                                      < i (
                                        _len l
                                      )
                                    )
                                     (
                                      begin (
                                        set! s (
                                          string-append s (
                                            to-str-space (
                                              list-ref-safe l i
                                            )
                                          )
                                        )
                                      )
                                       (
                                        if (
                                          < (
                                            + i 1
                                          )
                                           (
                                            _len l
                                          )
                                        )
                                         (
                                          begin (
                                            set! s (
                                              string-append s ", "
                                            )
                                          )
                                        )
                                         '(
                                          
                                        )
                                      )
                                       (
                                        set! i (
                                          + i 1
                                        )
                                      )
                                       (
                                        loop28
                                      )
                                    )
                                     '(
                                      
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
                        set! s (
                          string-append s "]"
                        )
                      )
                       (
                        ret27 s
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
            multiply_poly a b
          )
           (
            call/cc (
              lambda (
                ret30
              )
               (
                let (
                  (
                    n 1
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
                                  < n (
                                    - (
                                      + (
                                        _len a
                                      )
                                       (
                                        _len b
                                      )
                                    )
                                     1
                                  )
                                )
                                 (
                                  begin (
                                    set! n (
                                      * n 2
                                    )
                                  )
                                   (
                                    loop31
                                  )
                                )
                                 '(
                                  
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
                    let (
                      (
                        fa (
                          make_complex_list n (
                            alist->hash-table (
                              _list (
                                cons "re" 0.0
                              )
                               (
                                cons "im" 0.0
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
                            fb (
                              make_complex_list n (
                                alist->hash-table (
                                  _list (
                                    cons "re" 0.0
                                  )
                                   (
                                    cons "im" 0.0
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
                                              < i (
                                                _len a
                                              )
                                            )
                                             (
                                              begin (
                                                list-set! fa i (
                                                  alist->hash-table (
                                                    _list (
                                                      cons "re" (
                                                        list-ref-safe a i
                                                      )
                                                    )
                                                     (
                                                      cons "im" 0.0
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
                                                loop33
                                              )
                                            )
                                             '(
                                              
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
                               (
                                set! i 0
                              )
                               (
                                call/cc (
                                  lambda (
                                    break36
                                  )
                                   (
                                    letrec (
                                      (
                                        loop35 (
                                          lambda (
                                            
                                          )
                                           (
                                            if (
                                              < i (
                                                _len b
                                              )
                                            )
                                             (
                                              begin (
                                                list-set! fb i (
                                                  alist->hash-table (
                                                    _list (
                                                      cons "re" (
                                                        list-ref-safe b i
                                                      )
                                                    )
                                                     (
                                                      cons "im" 0.0
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
                                                loop35
                                              )
                                            )
                                             '(
                                              
                                            )
                                          )
                                        )
                                      )
                                    )
                                     (
                                      loop35
                                    )
                                  )
                                )
                              )
                               (
                                set! fa (
                                  fft fa #f
                                )
                              )
                               (
                                set! fb (
                                  fft fb #f
                                )
                              )
                               (
                                set! i 0
                              )
                               (
                                call/cc (
                                  lambda (
                                    break38
                                  )
                                   (
                                    letrec (
                                      (
                                        loop37 (
                                          lambda (
                                            
                                          )
                                           (
                                            if (
                                              < i n
                                            )
                                             (
                                              begin (
                                                list-set! fa i (
                                                  c_mul (
                                                    cond (
                                                      (
                                                        string? fa
                                                      )
                                                       (
                                                        _substring fa i (
                                                          + i 1
                                                        )
                                                      )
                                                    )
                                                     (
                                                      (
                                                        hash-table? fa
                                                      )
                                                       (
                                                        hash-table-ref fa i
                                                      )
                                                    )
                                                     (
                                                      else (
                                                        list-ref-safe fa i
                                                      )
                                                    )
                                                  )
                                                   (
                                                    cond (
                                                      (
                                                        string? fb
                                                      )
                                                       (
                                                        _substring fb i (
                                                          + i 1
                                                        )
                                                      )
                                                    )
                                                     (
                                                      (
                                                        hash-table? fb
                                                      )
                                                       (
                                                        hash-table-ref fb i
                                                      )
                                                    )
                                                     (
                                                      else (
                                                        list-ref-safe fb i
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
                                                loop37
                                              )
                                            )
                                             '(
                                              
                                            )
                                          )
                                        )
                                      )
                                    )
                                     (
                                      loop37
                                    )
                                  )
                                )
                              )
                               (
                                set! fa (
                                  fft fa #t
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
                                                  < i (
                                                    - (
                                                      + (
                                                        _len a
                                                      )
                                                       (
                                                        _len b
                                                      )
                                                    )
                                                     1
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        val (
                                                          cond (
                                                            (
                                                              string? fa
                                                            )
                                                             (
                                                              _substring fa i (
                                                                + i 1
                                                              )
                                                            )
                                                          )
                                                           (
                                                            (
                                                              hash-table? fa
                                                            )
                                                             (
                                                              hash-table-ref fa i
                                                            )
                                                          )
                                                           (
                                                            else (
                                                              list-ref-safe fa i
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        set! res (
                                                          append res (
                                                            _list (
                                                              round_to (
                                                                hash-table-ref val "re"
                                                              )
                                                               8
                                                            )
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
                                                    loop39
                                                  )
                                                )
                                                 '(
                                                  
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
                                                  and (
                                                    > (
                                                      _len res
                                                    )
                                                     0
                                                  )
                                                   (
                                                    equal? (
                                                      list-ref-safe res (
                                                        - (
                                                          _len res
                                                        )
                                                         1
                                                      )
                                                    )
                                                     0.0
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    set! res (
                                                      take (
                                                        drop res 0
                                                      )
                                                       (
                                                        - (
                                                          - (
                                                            _len res
                                                          )
                                                           1
                                                        )
                                                         0
                                                      )
                                                    )
                                                  )
                                                   (
                                                    loop41
                                                  )
                                                )
                                                 '(
                                                  
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
                                    ret30 res
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
              A (
                _list 0.0 1.0 0.0 2.0
              )
            )
          )
           (
            begin (
              let (
                (
                  B (
                    _list 2.0 3.0 4.0 0.0
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      product (
                        multiply_poly A B
                      )
                    )
                  )
                   (
                    begin (
                      _display (
                        if (
                          string? (
                            list_to_string product
                          )
                        )
                         (
                          list_to_string product
                        )
                         (
                          to-str (
                            list_to_string product
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
      let (
        (
          end44 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur45 (
              quotient (
                * (
                  - end44 start43
                )
                 1000000
              )
               jps46
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur45
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
