;; Generated on 2025-08-07 08:20 +0700
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
      start39 (
        current-jiffy
      )
    )
     (
      jps42 (
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
            abs x
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
            sqrtApprox x
          )
           (
            call/cc (
              lambda (
                ret2
              )
               (
                begin (
                  if (
                    <= x 0.0
                  )
                   (
                    begin (
                      ret2 0.0
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
            expApprox x
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
                            n 1
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
            vec_gaussian mat variance
          )
           (
            call/cc (
              lambda (
                ret8
              )
               (
                let (
                  (
                    i 0
                  )
                )
                 (
                  begin (
                    let (
                      (
                        out (
                          _list
                        )
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
                                      < i (
                                        _len mat
                                      )
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
                                                              < j (
                                                                _len (
                                                                  list-ref mat i
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                let (
                                                                  (
                                                                    v (
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
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        e (
                                                                          _div (
                                                                            - (
                                                                              * v v
                                                                            )
                                                                          )
                                                                           (
                                                                            * 2.0 variance
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        set! row (
                                                                          append row (
                                                                            _list (
                                                                              expApprox e
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
                                                set! out (
                                                  append out (
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
                        ret8 out
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
            get_slice img x y kernel_size
          )
           (
            call/cc (
              lambda (
                ret13
              )
               (
                let (
                  (
                    half (
                      _div kernel_size 2
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        i (
                          - x half
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            slice (
                              _list
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
                                          <= i (
                                            + x half
                                          )
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
                                                    j (
                                                      - y half
                                                    )
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
                                                                  <= j (
                                                                    + y half
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! row (
                                                                      append row (
                                                                        _list (
                                                                          cond (
                                                                            (
                                                                              string? (
                                                                                list-ref img i
                                                                              )
                                                                            )
                                                                             (
                                                                              _substring (
                                                                                list-ref img i
                                                                              )
                                                                               j (
                                                                                + j 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            (
                                                                              hash-table? (
                                                                                list-ref img i
                                                                              )
                                                                            )
                                                                             (
                                                                              hash-table-ref (
                                                                                list-ref img i
                                                                              )
                                                                               j
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref (
                                                                                list-ref img i
                                                                              )
                                                                               j
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
                                                    set! slice (
                                                      append slice (
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
                            ret13 slice
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
            get_gauss_kernel kernel_size spatial_variance
          )
           (
            call/cc (
              lambda (
                ret18
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
                                      < i kernel_size
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
                                                              < j kernel_size
                                                            )
                                                             (
                                                              begin (
                                                                let (
                                                                  (
                                                                    di (
                                                                      + 0.0 (
                                                                        - i (
                                                                          _div kernel_size 2
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        dj (
                                                                          + 0.0 (
                                                                            - j (
                                                                              _div kernel_size 2
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            dist (
                                                                              sqrtApprox (
                                                                                _add (
                                                                                  * di di
                                                                                )
                                                                                 (
                                                                                  * dj dj
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            set! row (
                                                                              append row (
                                                                                _list dist
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
                                                set! arr (
                                                  append arr (
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
                        ret18 (
                          vec_gaussian arr spatial_variance
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
            elementwise_sub mat value
          )
           (
            call/cc (
              lambda (
                ret23
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
                                      < i (
                                        _len mat
                                      )
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
                                                              < j (
                                                                _len (
                                                                  list-ref mat i
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                set! row (
                                                                  append row (
                                                                    _list (
                                                                      - (
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
                                                                       value
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
                                                set! res (
                                                  append res (
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
                        ret23 res
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
            elementwise_mul a b
          )
           (
            call/cc (
              lambda (
                ret28
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
                                      < i (
                                        _len a
                                      )
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
                                                              < j (
                                                                _len (
                                                                  list-ref a i
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                set! row (
                                                                  append row (
                                                                    _list (
                                                                      * (
                                                                        cond (
                                                                          (
                                                                            string? (
                                                                              list-ref a i
                                                                            )
                                                                          )
                                                                           (
                                                                            _substring (
                                                                              list-ref a i
                                                                            )
                                                                             j (
                                                                              + j 1
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          (
                                                                            hash-table? (
                                                                              list-ref a i
                                                                            )
                                                                          )
                                                                           (
                                                                            hash-table-ref (
                                                                              list-ref a i
                                                                            )
                                                                             j
                                                                          )
                                                                        )
                                                                         (
                                                                          else (
                                                                            list-ref (
                                                                              list-ref a i
                                                                            )
                                                                             j
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        cond (
                                                                          (
                                                                            string? (
                                                                              list-ref b i
                                                                            )
                                                                          )
                                                                           (
                                                                            _substring (
                                                                              list-ref b i
                                                                            )
                                                                             j (
                                                                              + j 1
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          (
                                                                            hash-table? (
                                                                              list-ref b i
                                                                            )
                                                                          )
                                                                           (
                                                                            hash-table-ref (
                                                                              list-ref b i
                                                                            )
                                                                             j
                                                                          )
                                                                        )
                                                                         (
                                                                          else (
                                                                            list-ref (
                                                                              list-ref b i
                                                                            )
                                                                             j
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
                                                set! res (
                                                  append res (
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
                        ret28 res
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
            matrix_sum mat
          )
           (
            call/cc (
              lambda (
                ret33
              )
               (
                let (
                  (
                    total 0.0
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
                                                            _len (
                                                              list-ref mat i
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            set! total (
                                                              + total (
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
                                            set! i (
                                              + i 1
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
                        ret33 total
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
            bilateral_filter img spatial_variance intensity_variance kernel_size
          )
           (
            call/cc (
              lambda (
                ret38
              )
               (
                let (
                  (
                    gauss_ker (
                      get_gauss_kernel kernel_size spatial_variance
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        img_s img
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            center (
                              cond (
                                (
                                  string? (
                                    list-ref img_s (
                                      _div kernel_size 2
                                    )
                                  )
                                )
                                 (
                                  _substring (
                                    list-ref img_s (
                                      _div kernel_size 2
                                    )
                                  )
                                   (
                                    _div kernel_size 2
                                  )
                                   (
                                    + (
                                      _div kernel_size 2
                                    )
                                     1
                                  )
                                )
                              )
                               (
                                (
                                  hash-table? (
                                    list-ref img_s (
                                      _div kernel_size 2
                                    )
                                  )
                                )
                                 (
                                  hash-table-ref (
                                    list-ref img_s (
                                      _div kernel_size 2
                                    )
                                  )
                                   (
                                    _div kernel_size 2
                                  )
                                )
                              )
                               (
                                else (
                                  list-ref (
                                    list-ref img_s (
                                      _div kernel_size 2
                                    )
                                  )
                                   (
                                    _div kernel_size 2
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
                                img_i (
                                  elementwise_sub img_s center
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    img_ig (
                                      vec_gaussian img_i intensity_variance
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        weights (
                                          elementwise_mul gauss_ker img_ig
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            vals (
                                              elementwise_mul img_s weights
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                sum_weights (
                                                  matrix_sum weights
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    val 0.0
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    if (
                                                      not (
                                                        equal? sum_weights 0.0
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        set! val (
                                                          _div (
                                                            matrix_sum vals
                                                          )
                                                           sum_weights
                                                        )
                                                      )
                                                    )
                                                     (
                                                      quote (
                                                        
                                                      )
                                                    )
                                                  )
                                                   (
                                                    ret38 val
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
              img (
                _list (
                  _list 0.2 0.3 0.4
                )
                 (
                  _list 0.3 0.4 0.5
                )
                 (
                  _list 0.4 0.5 0.6
                )
              )
            )
          )
           (
            begin (
              let (
                (
                  result (
                    bilateral_filter img 1.0 1.0 3
                  )
                )
              )
               (
                begin (
                  _display (
                    if (
                      string? result
                    )
                     result (
                      to-str result
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
     (
      let (
        (
          end40 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur41 (
              quotient (
                * (
                  - end40 start39
                )
                 1000000
              )
               jps42
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur41
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
