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
      start22 (
        current-jiffy
      )
    )
     (
      jps25 (
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
            expApprox x
          )
           (
            call/cc (
              lambda (
                ret1
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
            gen_gaussian_kernel k_size sigma
          )
           (
            call/cc (
              lambda (
                ret4
              )
               (
                let (
                  (
                    center (
                      _div k_size 2
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        kernel (
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
                                          < i k_size
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
                                                                  < j k_size
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        x (
                                                                          + 0.0 (
                                                                            - i center
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            y (
                                                                              + 0.0 (
                                                                                - j center
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                exponent (
                                                                                  - (
                                                                                    _div (
                                                                                      _add (
                                                                                        * x x
                                                                                      )
                                                                                       (
                                                                                        * y y
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      * (
                                                                                        * 2.0 sigma
                                                                                      )
                                                                                       sigma
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                let (
                                                                                  (
                                                                                    value (
                                                                                      * (
                                                                                        _div 1.0 (
                                                                                          * (
                                                                                            * 2.0 PI
                                                                                          )
                                                                                           sigma
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        expApprox exponent
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    set! row (
                                                                                      append row (
                                                                                        _list value
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
                                                   (
                                                    set! kernel (
                                                      append kernel (
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
                            ret4 kernel
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
            gaussian_filter image k_size sigma
          )
           (
            call/cc (
              lambda (
                ret9
              )
               (
                let (
                  (
                    height (
                      _len image
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        width (
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
                            dst_height (
                              + (
                                - height k_size
                              )
                               1
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                dst_width (
                                  + (
                                    - width k_size
                                  )
                                   1
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    kernel (
                                      gen_gaussian_kernel k_size sigma
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        dst (
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
                                                          < i dst_height
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
                                                                        break13
                                                                      )
                                                                       (
                                                                        letrec (
                                                                          (
                                                                            loop12 (
                                                                              lambda (
                                                                                
                                                                              )
                                                                               (
                                                                                if (
                                                                                  < j dst_width
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
                                                                                            ki 0
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
                                                                                                          < ki k_size
                                                                                                        )
                                                                                                         (
                                                                                                          begin (
                                                                                                            let (
                                                                                                              (
                                                                                                                kj 0
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
                                                                                                                              < kj k_size
                                                                                                                            )
                                                                                                                             (
                                                                                                                              begin (
                                                                                                                                set! sum (
                                                                                                                                  _add sum (
                                                                                                                                    * (
                                                                                                                                      + 0.0 (
                                                                                                                                        cond (
                                                                                                                                          (
                                                                                                                                            string? (
                                                                                                                                              list-ref image (
                                                                                                                                                + i ki
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                           (
                                                                                                                                            _substring (
                                                                                                                                              list-ref image (
                                                                                                                                                + i ki
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                             (
                                                                                                                                              + j kj
                                                                                                                                            )
                                                                                                                                             (
                                                                                                                                              + (
                                                                                                                                                + j kj
                                                                                                                                              )
                                                                                                                                               1
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                         (
                                                                                                                                          (
                                                                                                                                            hash-table? (
                                                                                                                                              list-ref image (
                                                                                                                                                + i ki
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                           (
                                                                                                                                            hash-table-ref (
                                                                                                                                              list-ref image (
                                                                                                                                                + i ki
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                             (
                                                                                                                                              + j kj
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                         (
                                                                                                                                          else (
                                                                                                                                            list-ref (
                                                                                                                                              list-ref image (
                                                                                                                                                + i ki
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                             (
                                                                                                                                              + j kj
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      cond (
                                                                                                                                        (
                                                                                                                                          string? (
                                                                                                                                            cond (
                                                                                                                                              (
                                                                                                                                                string? kernel
                                                                                                                                              )
                                                                                                                                               (
                                                                                                                                                _substring kernel ki (
                                                                                                                                                  + ki 1
                                                                                                                                                )
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                             (
                                                                                                                                              (
                                                                                                                                                hash-table? kernel
                                                                                                                                              )
                                                                                                                                               (
                                                                                                                                                hash-table-ref kernel ki
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                             (
                                                                                                                                              else (
                                                                                                                                                list-ref kernel ki
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                         (
                                                                                                                                          _substring (
                                                                                                                                            cond (
                                                                                                                                              (
                                                                                                                                                string? kernel
                                                                                                                                              )
                                                                                                                                               (
                                                                                                                                                _substring kernel ki (
                                                                                                                                                  + ki 1
                                                                                                                                                )
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                             (
                                                                                                                                              (
                                                                                                                                                hash-table? kernel
                                                                                                                                              )
                                                                                                                                               (
                                                                                                                                                hash-table-ref kernel ki
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                             (
                                                                                                                                              else (
                                                                                                                                                list-ref kernel ki
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                           kj (
                                                                                                                                            + kj 1
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                       (
                                                                                                                                        (
                                                                                                                                          hash-table? (
                                                                                                                                            cond (
                                                                                                                                              (
                                                                                                                                                string? kernel
                                                                                                                                              )
                                                                                                                                               (
                                                                                                                                                _substring kernel ki (
                                                                                                                                                  + ki 1
                                                                                                                                                )
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                             (
                                                                                                                                              (
                                                                                                                                                hash-table? kernel
                                                                                                                                              )
                                                                                                                                               (
                                                                                                                                                hash-table-ref kernel ki
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                             (
                                                                                                                                              else (
                                                                                                                                                list-ref kernel ki
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                         (
                                                                                                                                          hash-table-ref (
                                                                                                                                            cond (
                                                                                                                                              (
                                                                                                                                                string? kernel
                                                                                                                                              )
                                                                                                                                               (
                                                                                                                                                _substring kernel ki (
                                                                                                                                                  + ki 1
                                                                                                                                                )
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                             (
                                                                                                                                              (
                                                                                                                                                hash-table? kernel
                                                                                                                                              )
                                                                                                                                               (
                                                                                                                                                hash-table-ref kernel ki
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                             (
                                                                                                                                              else (
                                                                                                                                                list-ref kernel ki
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                           kj
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                       (
                                                                                                                                        else (
                                                                                                                                          list-ref (
                                                                                                                                            cond (
                                                                                                                                              (
                                                                                                                                                string? kernel
                                                                                                                                              )
                                                                                                                                               (
                                                                                                                                                _substring kernel ki (
                                                                                                                                                  + ki 1
                                                                                                                                                )
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                             (
                                                                                                                                              (
                                                                                                                                                hash-table? kernel
                                                                                                                                              )
                                                                                                                                               (
                                                                                                                                                hash-table-ref kernel ki
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                             (
                                                                                                                                              else (
                                                                                                                                                list-ref kernel ki
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                           kj
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                )
                                                                                                                              )
                                                                                                                               (
                                                                                                                                set! kj (
                                                                                                                                  + kj 1
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
                                                                                                                set! ki (
                                                                                                                  + ki 1
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
                                                                                            set! row (
                                                                                              append row (
                                                                                                _list (
                                                                                                  let (
                                                                                                    (
                                                                                                      v18 sum
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    cond (
                                                                                                      (
                                                                                                        string? v18
                                                                                                      )
                                                                                                       (
                                                                                                        inexact->exact (
                                                                                                          floor (
                                                                                                            string->number v18
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      (
                                                                                                        boolean? v18
                                                                                                      )
                                                                                                       (
                                                                                                        if v18 1 0
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      else (
                                                                                                        inexact->exact (
                                                                                                          floor v18
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
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    loop12
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
                                                                          loop12
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    set! dst (
                                                                      append dst (
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
                                            ret9 dst
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
            print_image image
          )
           (
            call/cc (
              lambda (
                ret19
              )
               (
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
                                  < i (
                                    _len image
                                  )
                                )
                                 (
                                  begin (
                                    _display (
                                      if (
                                        string? (
                                          list-ref image i
                                        )
                                      )
                                       (
                                        list-ref image i
                                      )
                                       (
                                        to-str (
                                          list-ref image i
                                        )
                                      )
                                    )
                                  )
                                   (
                                    newline
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
                  _list 52 55 61 59 79
                )
                 (
                  _list 62 59 55 104 94
                )
                 (
                  _list 63 65 66 113 144
                )
                 (
                  _list 68 70 70 126 154
                )
                 (
                  _list 70 72 69 128 155
                )
              )
            )
          )
           (
            begin (
              let (
                (
                  gaussian3 (
                    gaussian_filter img 3 1.0
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      gaussian5 (
                        gaussian_filter img 5 0.8
                      )
                    )
                  )
                   (
                    begin (
                      print_image gaussian3
                    )
                     (
                      print_image gaussian5
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
          end23 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur24 (
              quotient (
                * (
                  - end23 start22
                )
                 1000000
              )
               jps25
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur24
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
