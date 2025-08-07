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
      start49 (
        current-jiffy
      )
    )
     (
      jps52 (
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
          let (
            (
              GAUSSIAN_KERNEL (
                _list (
                  _list 0.0625 0.125 0.0625
                )
                 (
                  _list 0.125 0.25 0.125
                )
                 (
                  _list 0.0625 0.125 0.0625
                )
              )
            )
          )
           (
            begin (
              let (
                (
                  SOBEL_GX (
                    _list (
                      _list (
                        - 1.0
                      )
                       0.0 1.0
                    )
                     (
                      _list (
                        - 2.0
                      )
                       0.0 2.0
                    )
                     (
                      _list (
                        - 1.0
                      )
                       0.0 1.0
                    )
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      SOBEL_GY (
                        _list (
                          _list 1.0 2.0 1.0
                        )
                         (
                          _list 0.0 0.0 0.0
                        )
                         (
                          _list (
                            - 1.0
                          )
                           (
                            - 2.0
                          )
                           (
                            - 1.0
                          )
                        )
                      )
                    )
                  )
                   (
                    begin (
                      define (
                        zero_matrix h w
                      )
                       (
                        call/cc (
                          lambda (
                            ret7
                          )
                           (
                            let (
                              (
                                out (
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
                                                  < i h
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
                                                                          < j w
                                                                        )
                                                                         (
                                                                          begin (
                                                                            set! row (
                                                                              append row (
                                                                                _list 0.0
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            set! j (
                                                                              + j 1
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
                                    ret7 out
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
                        convolve img kernel
                      )
                       (
                        call/cc (
                          lambda (
                            ret12
                          )
                           (
                            let (
                              (
                                h (
                                  _len img
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    w (
                                      _len (
                                        list-ref img 0
                                      )
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        k (
                                          _len kernel
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            pad (
                                              _div k 2
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                out (
                                                  zero_matrix h w
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    y pad
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
                                                                  < y (
                                                                    - h pad
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        x pad
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
                                                                                      < x (
                                                                                        - w pad
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
                                                                                                ky 0
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
                                                                                                              < ky k
                                                                                                            )
                                                                                                             (
                                                                                                              begin (
                                                                                                                let (
                                                                                                                  (
                                                                                                                    kx 0
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
                                                                                                                                  < kx k
                                                                                                                                )
                                                                                                                                 (
                                                                                                                                  begin (
                                                                                                                                    let (
                                                                                                                                      (
                                                                                                                                        pixel (
                                                                                                                                          cond (
                                                                                                                                            (
                                                                                                                                              string? (
                                                                                                                                                list-ref img (
                                                                                                                                                  + (
                                                                                                                                                    - y pad
                                                                                                                                                  )
                                                                                                                                                   ky
                                                                                                                                                )
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                             (
                                                                                                                                              _substring (
                                                                                                                                                list-ref img (
                                                                                                                                                  + (
                                                                                                                                                    - y pad
                                                                                                                                                  )
                                                                                                                                                   ky
                                                                                                                                                )
                                                                                                                                              )
                                                                                                                                               (
                                                                                                                                                + (
                                                                                                                                                  - x pad
                                                                                                                                                )
                                                                                                                                                 kx
                                                                                                                                              )
                                                                                                                                               (
                                                                                                                                                + (
                                                                                                                                                  + (
                                                                                                                                                    - x pad
                                                                                                                                                  )
                                                                                                                                                   kx
                                                                                                                                                )
                                                                                                                                                 1
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                           (
                                                                                                                                            (
                                                                                                                                              hash-table? (
                                                                                                                                                list-ref img (
                                                                                                                                                  + (
                                                                                                                                                    - y pad
                                                                                                                                                  )
                                                                                                                                                   ky
                                                                                                                                                )
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                             (
                                                                                                                                              hash-table-ref (
                                                                                                                                                list-ref img (
                                                                                                                                                  + (
                                                                                                                                                    - y pad
                                                                                                                                                  )
                                                                                                                                                   ky
                                                                                                                                                )
                                                                                                                                              )
                                                                                                                                               (
                                                                                                                                                + (
                                                                                                                                                  - x pad
                                                                                                                                                )
                                                                                                                                                 kx
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                           (
                                                                                                                                            else (
                                                                                                                                              list-ref (
                                                                                                                                                list-ref img (
                                                                                                                                                  + (
                                                                                                                                                    - y pad
                                                                                                                                                  )
                                                                                                                                                   ky
                                                                                                                                                )
                                                                                                                                              )
                                                                                                                                               (
                                                                                                                                                + (
                                                                                                                                                  - x pad
                                                                                                                                                )
                                                                                                                                                 kx
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
                                                                                                                                            weight (
                                                                                                                                              cond (
                                                                                                                                                (
                                                                                                                                                  string? (
                                                                                                                                                    list-ref kernel ky
                                                                                                                                                  )
                                                                                                                                                )
                                                                                                                                                 (
                                                                                                                                                  _substring (
                                                                                                                                                    list-ref kernel ky
                                                                                                                                                  )
                                                                                                                                                   kx (
                                                                                                                                                    + kx 1
                                                                                                                                                  )
                                                                                                                                                )
                                                                                                                                              )
                                                                                                                                               (
                                                                                                                                                (
                                                                                                                                                  hash-table? (
                                                                                                                                                    list-ref kernel ky
                                                                                                                                                  )
                                                                                                                                                )
                                                                                                                                                 (
                                                                                                                                                  hash-table-ref (
                                                                                                                                                    list-ref kernel ky
                                                                                                                                                  )
                                                                                                                                                   kx
                                                                                                                                                )
                                                                                                                                              )
                                                                                                                                               (
                                                                                                                                                else (
                                                                                                                                                  list-ref (
                                                                                                                                                    list-ref kernel ky
                                                                                                                                                  )
                                                                                                                                                   kx
                                                                                                                                                )
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                         (
                                                                                                                                          begin (
                                                                                                                                            set! sum (
                                                                                                                                              _add sum (
                                                                                                                                                * pixel weight
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                           (
                                                                                                                                            set! kx (
                                                                                                                                              + kx 1
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
                                                                                                                    set! ky (
                                                                                                                      + ky 1
                                                                                                                    )
                                                                                                                  )
                                                                                                                )
                                                                                                              )
                                                                                                               (
                                                                                                                loop17
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
                                                                                                      loop17
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                list-set! (
                                                                                                  list-ref out y
                                                                                                )
                                                                                                 x sum
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
                                                                        set! y (
                                                                          + y 1
                                                                        )
                                                                      )
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
                                                    ret12 out
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
                        gaussian_blur img
                      )
                       (
                        call/cc (
                          lambda (
                            ret21
                          )
                           (
                            ret21 (
                              convolve img GAUSSIAN_KERNEL
                            )
                          )
                        )
                      )
                    )
                     (
                      define (
                        sobel_filter img
                      )
                       (
                        call/cc (
                          lambda (
                            ret22
                          )
                           (
                            let (
                              (
                                gx (
                                  convolve img SOBEL_GX
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    gy (
                                      convolve img SOBEL_GY
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        h (
                                          _len img
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            w (
                                              _len (
                                                list-ref img 0
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                grad (
                                                  zero_matrix h w
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    dir (
                                                      zero_matrix h w
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
                                                                      < i h
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
                                                                                          < j w
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            let (
                                                                                              (
                                                                                                gxx (
                                                                                                  cond (
                                                                                                    (
                                                                                                      string? (
                                                                                                        cond (
                                                                                                          (
                                                                                                            string? gx
                                                                                                          )
                                                                                                           (
                                                                                                            _substring gx i (
                                                                                                              + i 1
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          (
                                                                                                            hash-table? gx
                                                                                                          )
                                                                                                           (
                                                                                                            hash-table-ref gx i
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          else (
                                                                                                            list-ref gx i
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      _substring (
                                                                                                        cond (
                                                                                                          (
                                                                                                            string? gx
                                                                                                          )
                                                                                                           (
                                                                                                            _substring gx i (
                                                                                                              + i 1
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          (
                                                                                                            hash-table? gx
                                                                                                          )
                                                                                                           (
                                                                                                            hash-table-ref gx i
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          else (
                                                                                                            list-ref gx i
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       j (
                                                                                                        + j 1
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    (
                                                                                                      hash-table? (
                                                                                                        cond (
                                                                                                          (
                                                                                                            string? gx
                                                                                                          )
                                                                                                           (
                                                                                                            _substring gx i (
                                                                                                              + i 1
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          (
                                                                                                            hash-table? gx
                                                                                                          )
                                                                                                           (
                                                                                                            hash-table-ref gx i
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          else (
                                                                                                            list-ref gx i
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      hash-table-ref (
                                                                                                        cond (
                                                                                                          (
                                                                                                            string? gx
                                                                                                          )
                                                                                                           (
                                                                                                            _substring gx i (
                                                                                                              + i 1
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          (
                                                                                                            hash-table? gx
                                                                                                          )
                                                                                                           (
                                                                                                            hash-table-ref gx i
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          else (
                                                                                                            list-ref gx i
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       j
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    else (
                                                                                                      list-ref (
                                                                                                        cond (
                                                                                                          (
                                                                                                            string? gx
                                                                                                          )
                                                                                                           (
                                                                                                            _substring gx i (
                                                                                                              + i 1
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          (
                                                                                                            hash-table? gx
                                                                                                          )
                                                                                                           (
                                                                                                            hash-table-ref gx i
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          else (
                                                                                                            list-ref gx i
                                                                                                          )
                                                                                                        )
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
                                                                                                    gyy (
                                                                                                      cond (
                                                                                                        (
                                                                                                          string? (
                                                                                                            cond (
                                                                                                              (
                                                                                                                string? gy
                                                                                                              )
                                                                                                               (
                                                                                                                _substring gy i (
                                                                                                                  + i 1
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              (
                                                                                                                hash-table? gy
                                                                                                              )
                                                                                                               (
                                                                                                                hash-table-ref gy i
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              else (
                                                                                                                list-ref gy i
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          _substring (
                                                                                                            cond (
                                                                                                              (
                                                                                                                string? gy
                                                                                                              )
                                                                                                               (
                                                                                                                _substring gy i (
                                                                                                                  + i 1
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              (
                                                                                                                hash-table? gy
                                                                                                              )
                                                                                                               (
                                                                                                                hash-table-ref gy i
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              else (
                                                                                                                list-ref gy i
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                           j (
                                                                                                            + j 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? (
                                                                                                            cond (
                                                                                                              (
                                                                                                                string? gy
                                                                                                              )
                                                                                                               (
                                                                                                                _substring gy i (
                                                                                                                  + i 1
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              (
                                                                                                                hash-table? gy
                                                                                                              )
                                                                                                               (
                                                                                                                hash-table-ref gy i
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              else (
                                                                                                                list-ref gy i
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref (
                                                                                                            cond (
                                                                                                              (
                                                                                                                string? gy
                                                                                                              )
                                                                                                               (
                                                                                                                _substring gy i (
                                                                                                                  + i 1
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              (
                                                                                                                hash-table? gy
                                                                                                              )
                                                                                                               (
                                                                                                                hash-table-ref gy i
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              else (
                                                                                                                list-ref gy i
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                           j
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref (
                                                                                                            cond (
                                                                                                              (
                                                                                                                string? gy
                                                                                                              )
                                                                                                               (
                                                                                                                _substring gy i (
                                                                                                                  + i 1
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              (
                                                                                                                hash-table? gy
                                                                                                              )
                                                                                                               (
                                                                                                                hash-table-ref gy i
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              else (
                                                                                                                list-ref gy i
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                           j
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  begin (
                                                                                                    list-set! (
                                                                                                      list-ref grad i
                                                                                                    )
                                                                                                     j (
                                                                                                      sqrtApprox (
                                                                                                        _add (
                                                                                                          * gxx gxx
                                                                                                        )
                                                                                                         (
                                                                                                          * gyy gyy
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    list-set! (
                                                                                                      list-ref dir i
                                                                                                    )
                                                                                                     j (
                                                                                                      _add (
                                                                                                        deg (
                                                                                                          atan2Approx gyy gxx
                                                                                                        )
                                                                                                      )
                                                                                                       180.0
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
                                                                            set! i (
                                                                              + i 1
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
                                                        ret22 (
                                                          alist->hash-table (
                                                            _list (
                                                              cons "grad" grad
                                                            )
                                                             (
                                                              cons "dir" dir
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
                        suppress_non_maximum h w direction grad
                      )
                       (
                        call/cc (
                          lambda (
                            ret27
                          )
                           (
                            let (
                              (
                                dest (
                                  zero_matrix h w
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    r 1
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
                                                  < r (
                                                    - h 1
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        c 1
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
                                                                      < c (
                                                                        - w 1
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            angle (
                                                                              cond (
                                                                                (
                                                                                  string? (
                                                                                    list-ref direction r
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  _substring (
                                                                                    list-ref direction r
                                                                                  )
                                                                                   c (
                                                                                    + c 1
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                (
                                                                                  hash-table? (
                                                                                    list-ref direction r
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  hash-table-ref (
                                                                                    list-ref direction r
                                                                                  )
                                                                                   c
                                                                                )
                                                                              )
                                                                               (
                                                                                else (
                                                                                  list-ref (
                                                                                    list-ref direction r
                                                                                  )
                                                                                   c
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                q 0.0
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                let (
                                                                                  (
                                                                                    p 0.0
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    if (
                                                                                      or (
                                                                                        or (
                                                                                          and (
                                                                                            >= angle 0.0
                                                                                          )
                                                                                           (
                                                                                            < angle 22.5
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          and (
                                                                                            >= angle 157.5
                                                                                          )
                                                                                           (
                                                                                            <= angle 180.0
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        >= angle 337.5
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        set! q (
                                                                                          cond (
                                                                                            (
                                                                                              string? (
                                                                                                list-ref grad r
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              _substring (
                                                                                                list-ref grad r
                                                                                              )
                                                                                               (
                                                                                                + c 1
                                                                                              )
                                                                                               (
                                                                                                + (
                                                                                                  + c 1
                                                                                                )
                                                                                                 1
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            (
                                                                                              hash-table? (
                                                                                                list-ref grad r
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              hash-table-ref (
                                                                                                list-ref grad r
                                                                                              )
                                                                                               (
                                                                                                + c 1
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            else (
                                                                                              list-ref (
                                                                                                list-ref grad r
                                                                                              )
                                                                                               (
                                                                                                + c 1
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        set! p (
                                                                                          cond (
                                                                                            (
                                                                                              string? (
                                                                                                list-ref grad r
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              _substring (
                                                                                                list-ref grad r
                                                                                              )
                                                                                               (
                                                                                                - c 1
                                                                                              )
                                                                                               (
                                                                                                + (
                                                                                                  - c 1
                                                                                                )
                                                                                                 1
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            (
                                                                                              hash-table? (
                                                                                                list-ref grad r
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              hash-table-ref (
                                                                                                list-ref grad r
                                                                                              )
                                                                                               (
                                                                                                - c 1
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            else (
                                                                                              list-ref (
                                                                                                list-ref grad r
                                                                                              )
                                                                                               (
                                                                                                - c 1
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      if (
                                                                                        or (
                                                                                          and (
                                                                                            >= angle 22.5
                                                                                          )
                                                                                           (
                                                                                            < angle 67.5
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          and (
                                                                                            >= angle 202.5
                                                                                          )
                                                                                           (
                                                                                            < angle 247.5
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        begin (
                                                                                          set! q (
                                                                                            cond (
                                                                                              (
                                                                                                string? (
                                                                                                  list-ref grad (
                                                                                                    + r 1
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                _substring (
                                                                                                  list-ref grad (
                                                                                                    + r 1
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  - c 1
                                                                                                )
                                                                                                 (
                                                                                                  + (
                                                                                                    - c 1
                                                                                                  )
                                                                                                   1
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              (
                                                                                                hash-table? (
                                                                                                  list-ref grad (
                                                                                                    + r 1
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                hash-table-ref (
                                                                                                  list-ref grad (
                                                                                                    + r 1
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  - c 1
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              else (
                                                                                                list-ref (
                                                                                                  list-ref grad (
                                                                                                    + r 1
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  - c 1
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          set! p (
                                                                                            cond (
                                                                                              (
                                                                                                string? (
                                                                                                  list-ref grad (
                                                                                                    - r 1
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                _substring (
                                                                                                  list-ref grad (
                                                                                                    - r 1
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  + c 1
                                                                                                )
                                                                                                 (
                                                                                                  + (
                                                                                                    + c 1
                                                                                                  )
                                                                                                   1
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              (
                                                                                                hash-table? (
                                                                                                  list-ref grad (
                                                                                                    - r 1
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                hash-table-ref (
                                                                                                  list-ref grad (
                                                                                                    - r 1
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  + c 1
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              else (
                                                                                                list-ref (
                                                                                                  list-ref grad (
                                                                                                    - r 1
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  + c 1
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        if (
                                                                                          or (
                                                                                            and (
                                                                                              >= angle 67.5
                                                                                            )
                                                                                             (
                                                                                              < angle 112.5
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            and (
                                                                                              >= angle 247.5
                                                                                            )
                                                                                             (
                                                                                              < angle 292.5
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            set! q (
                                                                                              cond (
                                                                                                (
                                                                                                  string? (
                                                                                                    list-ref grad (
                                                                                                      + r 1
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  _substring (
                                                                                                    list-ref grad (
                                                                                                      + r 1
                                                                                                    )
                                                                                                  )
                                                                                                   c (
                                                                                                    + c 1
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                (
                                                                                                  hash-table? (
                                                                                                    list-ref grad (
                                                                                                      + r 1
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  hash-table-ref (
                                                                                                    list-ref grad (
                                                                                                      + r 1
                                                                                                    )
                                                                                                  )
                                                                                                   c
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                else (
                                                                                                  list-ref (
                                                                                                    list-ref grad (
                                                                                                      + r 1
                                                                                                    )
                                                                                                  )
                                                                                                   c
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            set! p (
                                                                                              cond (
                                                                                                (
                                                                                                  string? (
                                                                                                    list-ref grad (
                                                                                                      - r 1
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  _substring (
                                                                                                    list-ref grad (
                                                                                                      - r 1
                                                                                                    )
                                                                                                  )
                                                                                                   c (
                                                                                                    + c 1
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                (
                                                                                                  hash-table? (
                                                                                                    list-ref grad (
                                                                                                      - r 1
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  hash-table-ref (
                                                                                                    list-ref grad (
                                                                                                      - r 1
                                                                                                    )
                                                                                                  )
                                                                                                   c
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                else (
                                                                                                  list-ref (
                                                                                                    list-ref grad (
                                                                                                      - r 1
                                                                                                    )
                                                                                                  )
                                                                                                   c
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            set! q (
                                                                                              cond (
                                                                                                (
                                                                                                  string? (
                                                                                                    list-ref grad (
                                                                                                      - r 1
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  _substring (
                                                                                                    list-ref grad (
                                                                                                      - r 1
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    - c 1
                                                                                                  )
                                                                                                   (
                                                                                                    + (
                                                                                                      - c 1
                                                                                                    )
                                                                                                     1
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                (
                                                                                                  hash-table? (
                                                                                                    list-ref grad (
                                                                                                      - r 1
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  hash-table-ref (
                                                                                                    list-ref grad (
                                                                                                      - r 1
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    - c 1
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                else (
                                                                                                  list-ref (
                                                                                                    list-ref grad (
                                                                                                      - r 1
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    - c 1
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            set! p (
                                                                                              cond (
                                                                                                (
                                                                                                  string? (
                                                                                                    list-ref grad (
                                                                                                      + r 1
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  _substring (
                                                                                                    list-ref grad (
                                                                                                      + r 1
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    + c 1
                                                                                                  )
                                                                                                   (
                                                                                                    + (
                                                                                                      + c 1
                                                                                                    )
                                                                                                     1
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                (
                                                                                                  hash-table? (
                                                                                                    list-ref grad (
                                                                                                      + r 1
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  hash-table-ref (
                                                                                                    list-ref grad (
                                                                                                      + r 1
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    + c 1
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                else (
                                                                                                  list-ref (
                                                                                                    list-ref grad (
                                                                                                      + r 1
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    + c 1
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
                                                                                    if (
                                                                                      and (
                                                                                        >= (
                                                                                          cond (
                                                                                            (
                                                                                              string? (
                                                                                                list-ref grad r
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              _substring (
                                                                                                list-ref grad r
                                                                                              )
                                                                                               c (
                                                                                                + c 1
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            (
                                                                                              hash-table? (
                                                                                                list-ref grad r
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              hash-table-ref (
                                                                                                list-ref grad r
                                                                                              )
                                                                                               c
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            else (
                                                                                              list-ref (
                                                                                                list-ref grad r
                                                                                              )
                                                                                               c
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         q
                                                                                      )
                                                                                       (
                                                                                        >= (
                                                                                          cond (
                                                                                            (
                                                                                              string? (
                                                                                                list-ref grad r
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              _substring (
                                                                                                list-ref grad r
                                                                                              )
                                                                                               c (
                                                                                                + c 1
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            (
                                                                                              hash-table? (
                                                                                                list-ref grad r
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              hash-table-ref (
                                                                                                list-ref grad r
                                                                                              )
                                                                                               c
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            else (
                                                                                              list-ref (
                                                                                                list-ref grad r
                                                                                              )
                                                                                               c
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         p
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        list-set! (
                                                                                          list-ref dest r
                                                                                        )
                                                                                         c (
                                                                                          cond (
                                                                                            (
                                                                                              string? (
                                                                                                list-ref grad r
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              _substring (
                                                                                                list-ref grad r
                                                                                              )
                                                                                               c (
                                                                                                + c 1
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            (
                                                                                              hash-table? (
                                                                                                list-ref grad r
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              hash-table-ref (
                                                                                                list-ref grad r
                                                                                              )
                                                                                               c
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            else (
                                                                                              list-ref (
                                                                                                list-ref grad r
                                                                                              )
                                                                                               c
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
                                                                                    set! c (
                                                                                      + c 1
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
                                                        set! r (
                                                          + r 1
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
                                    ret27 dest
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
                        double_threshold h w img low high weak strong
                      )
                       (
                        call/cc (
                          lambda (
                            ret32
                          )
                           (
                            let (
                              (
                                r 0
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
                                              < r h
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    c 0
                                                  )
                                                )
                                                 (
                                                  begin (
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
                                                                  < c w
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        v (
                                                                          cond (
                                                                            (
                                                                              string? (
                                                                                list-ref img r
                                                                              )
                                                                            )
                                                                             (
                                                                              _substring (
                                                                                list-ref img r
                                                                              )
                                                                               c (
                                                                                + c 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            (
                                                                              hash-table? (
                                                                                list-ref img r
                                                                              )
                                                                            )
                                                                             (
                                                                              hash-table-ref (
                                                                                list-ref img r
                                                                              )
                                                                               c
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref (
                                                                                list-ref img r
                                                                              )
                                                                               c
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        if (
                                                                          >= v high
                                                                        )
                                                                         (
                                                                          begin (
                                                                            list-set! (
                                                                              list-ref img r
                                                                            )
                                                                             c strong
                                                                          )
                                                                        )
                                                                         (
                                                                          if (
                                                                            < v low
                                                                          )
                                                                           (
                                                                            begin (
                                                                              list-set! (
                                                                                list-ref img r
                                                                              )
                                                                               c 0.0
                                                                            )
                                                                          )
                                                                           (
                                                                            begin (
                                                                              list-set! (
                                                                                list-ref img r
                                                                              )
                                                                               c weak
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        set! c (
                                                                          + c 1
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    loop35
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
                                                          loop35
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    set! r (
                                                      + r 1
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
                     (
                      define (
                        track_edge h w img weak strong
                      )
                       (
                        call/cc (
                          lambda (
                            ret37
                          )
                           (
                            let (
                              (
                                r 1
                              )
                            )
                             (
                              begin (
                                call/cc (
                                  lambda (
                                    break39
                                  )
                                   (
                                    letrec (
                                      (
                                        loop38 (
                                          lambda (
                                            
                                          )
                                           (
                                            if (
                                              < r (
                                                - h 1
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    c 1
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    call/cc (
                                                      lambda (
                                                        break41
                                                      )
                                                       (
                                                        letrec (
                                                          (
                                                            loop40 (
                                                              lambda (
                                                                
                                                              )
                                                               (
                                                                if (
                                                                  < c (
                                                                    - w 1
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    if (
                                                                      equal? (
                                                                        cond (
                                                                          (
                                                                            string? (
                                                                              list-ref img r
                                                                            )
                                                                          )
                                                                           (
                                                                            _substring (
                                                                              list-ref img r
                                                                            )
                                                                             c (
                                                                              + c 1
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          (
                                                                            hash-table? (
                                                                              list-ref img r
                                                                            )
                                                                          )
                                                                           (
                                                                            hash-table-ref (
                                                                              list-ref img r
                                                                            )
                                                                             c
                                                                          )
                                                                        )
                                                                         (
                                                                          else (
                                                                            list-ref (
                                                                              list-ref img r
                                                                            )
                                                                             c
                                                                          )
                                                                        )
                                                                      )
                                                                       weak
                                                                    )
                                                                     (
                                                                      begin (
                                                                        if (
                                                                          or (
                                                                            or (
                                                                              or (
                                                                                or (
                                                                                  or (
                                                                                    or (
                                                                                      or (
                                                                                        equal? (
                                                                                          cond (
                                                                                            (
                                                                                              string? (
                                                                                                list-ref img (
                                                                                                  + r 1
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              _substring (
                                                                                                list-ref img (
                                                                                                  + r 1
                                                                                                )
                                                                                              )
                                                                                               c (
                                                                                                + c 1
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            (
                                                                                              hash-table? (
                                                                                                list-ref img (
                                                                                                  + r 1
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              hash-table-ref (
                                                                                                list-ref img (
                                                                                                  + r 1
                                                                                                )
                                                                                              )
                                                                                               c
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            else (
                                                                                              list-ref (
                                                                                                list-ref img (
                                                                                                  + r 1
                                                                                                )
                                                                                              )
                                                                                               c
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         strong
                                                                                      )
                                                                                       (
                                                                                        equal? (
                                                                                          cond (
                                                                                            (
                                                                                              string? (
                                                                                                list-ref img (
                                                                                                  - r 1
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              _substring (
                                                                                                list-ref img (
                                                                                                  - r 1
                                                                                                )
                                                                                              )
                                                                                               c (
                                                                                                + c 1
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            (
                                                                                              hash-table? (
                                                                                                list-ref img (
                                                                                                  - r 1
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              hash-table-ref (
                                                                                                list-ref img (
                                                                                                  - r 1
                                                                                                )
                                                                                              )
                                                                                               c
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            else (
                                                                                              list-ref (
                                                                                                list-ref img (
                                                                                                  - r 1
                                                                                                )
                                                                                              )
                                                                                               c
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         strong
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      equal? (
                                                                                        cond (
                                                                                          (
                                                                                            string? (
                                                                                              list-ref img r
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            _substring (
                                                                                              list-ref img r
                                                                                            )
                                                                                             (
                                                                                              + c 1
                                                                                            )
                                                                                             (
                                                                                              + (
                                                                                                + c 1
                                                                                              )
                                                                                               1
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          (
                                                                                            hash-table? (
                                                                                              list-ref img r
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            hash-table-ref (
                                                                                              list-ref img r
                                                                                            )
                                                                                             (
                                                                                              + c 1
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          else (
                                                                                            list-ref (
                                                                                              list-ref img r
                                                                                            )
                                                                                             (
                                                                                              + c 1
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       strong
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    equal? (
                                                                                      cond (
                                                                                        (
                                                                                          string? (
                                                                                            list-ref img r
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          _substring (
                                                                                            list-ref img r
                                                                                          )
                                                                                           (
                                                                                            - c 1
                                                                                          )
                                                                                           (
                                                                                            + (
                                                                                              - c 1
                                                                                            )
                                                                                             1
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        (
                                                                                          hash-table? (
                                                                                            list-ref img r
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          hash-table-ref (
                                                                                            list-ref img r
                                                                                          )
                                                                                           (
                                                                                            - c 1
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        else (
                                                                                          list-ref (
                                                                                            list-ref img r
                                                                                          )
                                                                                           (
                                                                                            - c 1
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     strong
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  equal? (
                                                                                    cond (
                                                                                      (
                                                                                        string? (
                                                                                          list-ref img (
                                                                                            - r 1
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        _substring (
                                                                                          list-ref img (
                                                                                            - r 1
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          - c 1
                                                                                        )
                                                                                         (
                                                                                          + (
                                                                                            - c 1
                                                                                          )
                                                                                           1
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      (
                                                                                        hash-table? (
                                                                                          list-ref img (
                                                                                            - r 1
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        hash-table-ref (
                                                                                          list-ref img (
                                                                                            - r 1
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          - c 1
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      else (
                                                                                        list-ref (
                                                                                          list-ref img (
                                                                                            - r 1
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          - c 1
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   strong
                                                                                )
                                                                              )
                                                                               (
                                                                                equal? (
                                                                                  cond (
                                                                                    (
                                                                                      string? (
                                                                                        list-ref img (
                                                                                          - r 1
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref img (
                                                                                          - r 1
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        + c 1
                                                                                      )
                                                                                       (
                                                                                        + (
                                                                                          + c 1
                                                                                        )
                                                                                         1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref img (
                                                                                          - r 1
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref img (
                                                                                          - r 1
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        + c 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref img (
                                                                                          - r 1
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        + c 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 strong
                                                                              )
                                                                            )
                                                                             (
                                                                              equal? (
                                                                                cond (
                                                                                  (
                                                                                    string? (
                                                                                      list-ref img (
                                                                                        + r 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    _substring (
                                                                                      list-ref img (
                                                                                        + r 1
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      - c 1
                                                                                    )
                                                                                     (
                                                                                      + (
                                                                                        - c 1
                                                                                      )
                                                                                       1
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  (
                                                                                    hash-table? (
                                                                                      list-ref img (
                                                                                        + r 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    hash-table-ref (
                                                                                      list-ref img (
                                                                                        + r 1
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      - c 1
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  else (
                                                                                    list-ref (
                                                                                      list-ref img (
                                                                                        + r 1
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      - c 1
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               strong
                                                                            )
                                                                          )
                                                                           (
                                                                            equal? (
                                                                              cond (
                                                                                (
                                                                                  string? (
                                                                                    list-ref img (
                                                                                      + r 1
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  _substring (
                                                                                    list-ref img (
                                                                                      + r 1
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    + c 1
                                                                                  )
                                                                                   (
                                                                                    + (
                                                                                      + c 1
                                                                                    )
                                                                                     1
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                (
                                                                                  hash-table? (
                                                                                    list-ref img (
                                                                                      + r 1
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  hash-table-ref (
                                                                                    list-ref img (
                                                                                      + r 1
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    + c 1
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                else (
                                                                                  list-ref (
                                                                                    list-ref img (
                                                                                      + r 1
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    + c 1
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                             strong
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            list-set! (
                                                                              list-ref img r
                                                                            )
                                                                             c strong
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            list-set! (
                                                                              list-ref img r
                                                                            )
                                                                             c 0.0
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
                                                                    set! c (
                                                                      + c 1
                                                                    )
                                                                  )
                                                                   (
                                                                    loop40
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
                                                          loop40
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    set! r (
                                                      + r 1
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                loop38
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
                                      loop38
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
                        canny image low high weak strong
                      )
                       (
                        call/cc (
                          lambda (
                            ret42
                          )
                           (
                            let (
                              (
                                blurred (
                                  gaussian_blur image
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    sob (
                                      sobel_filter blurred
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        grad (
                                          cond (
                                            (
                                              string? sob
                                            )
                                             (
                                              _substring sob "grad" (
                                                + "grad" 1
                                              )
                                            )
                                          )
                                           (
                                            (
                                              hash-table? sob
                                            )
                                             (
                                              hash-table-ref sob "grad"
                                            )
                                          )
                                           (
                                            else (
                                              list-ref sob "grad"
                                            )
                                          )
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            direction (
                                              cond (
                                                (
                                                  string? sob
                                                )
                                                 (
                                                  _substring sob "dir" (
                                                    + "dir" 1
                                                  )
                                                )
                                              )
                                               (
                                                (
                                                  hash-table? sob
                                                )
                                                 (
                                                  hash-table-ref sob "dir"
                                                )
                                              )
                                               (
                                                else (
                                                  list-ref sob "dir"
                                                )
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                h (
                                                  _len image
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    w (
                                                      _len (
                                                        list-ref image 0
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        suppressed (
                                                          suppress_non_maximum h w direction grad
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        double_threshold h w suppressed low high weak strong
                                                      )
                                                       (
                                                        track_edge h w suppressed weak strong
                                                      )
                                                       (
                                                        ret42 suppressed
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
                        print_image img
                      )
                       (
                        call/cc (
                          lambda (
                            ret43
                          )
                           (
                            let (
                              (
                                r 0
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
                                              < r (
                                                _len img
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    c 0
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        line ""
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
                                                                      < c (
                                                                        _len (
                                                                          list-ref img r
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        set! line (
                                                                          string-append (
                                                                            string-append line (
                                                                              to-str-space (
                                                                                let (
                                                                                  (
                                                                                    v48 (
                                                                                      cond (
                                                                                        (
                                                                                          string? (
                                                                                            list-ref img r
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          _substring (
                                                                                            list-ref img r
                                                                                          )
                                                                                           c (
                                                                                            + c 1
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        (
                                                                                          hash-table? (
                                                                                            list-ref img r
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          hash-table-ref (
                                                                                            list-ref img r
                                                                                          )
                                                                                           c
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        else (
                                                                                          list-ref (
                                                                                            list-ref img r
                                                                                          )
                                                                                           c
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  cond (
                                                                                    (
                                                                                      string? v48
                                                                                    )
                                                                                     (
                                                                                      inexact->exact (
                                                                                        floor (
                                                                                          string->number v48
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      boolean? v48
                                                                                    )
                                                                                     (
                                                                                      if v48 1 0
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      inexact->exact (
                                                                                        floor v48
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                           " "
                                                                        )
                                                                      )
                                                                       (
                                                                        set! c (
                                                                          + c 1
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
                                                        _display (
                                                          if (
                                                            string? line
                                                          )
                                                           line (
                                                            to-str line
                                                          )
                                                        )
                                                      )
                                                       (
                                                        newline
                                                      )
                                                       (
                                                        set! r (
                                                          + r 1
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
                            )
                          )
                        )
                      )
                    )
                     (
                      let (
                        (
                          image (
                            _list (
                              _list 0.0 0.0 0.0 0.0 0.0
                            )
                             (
                              _list 0.0 255.0 255.0 255.0 0.0
                            )
                             (
                              _list 0.0 255.0 255.0 255.0 0.0
                            )
                             (
                              _list 0.0 255.0 255.0 255.0 0.0
                            )
                             (
                              _list 0.0 0.0 0.0 0.0 0.0
                            )
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              edges (
                                canny image 20.0 40.0 128.0 255.0
                              )
                            )
                          )
                           (
                            begin (
                              print_image edges
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
          end50 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur51 (
              quotient (
                * (
                  - end50 start49
                )
                 1000000
              )
               jps52
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur51
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
