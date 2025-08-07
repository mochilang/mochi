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
      start67 (
        current-jiffy
      )
    )
     (
      jps70 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        clamp_byte x
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            begin (
              if (
                < x 0
              )
               (
                begin (
                  ret1 0
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                > x 255
              )
               (
                begin (
                  ret1 255
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
        convert_to_negative img
      )
       (
        call/cc (
          lambda (
            ret2
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
                        out (
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
                                          < y h
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
                                                    x 0
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
                                                                  < x w
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! row (
                                                                      append row (
                                                                        _list (
                                                                          - 255 (
                                                                            cond (
                                                                              (
                                                                                string? (
                                                                                  list-ref img y
                                                                                )
                                                                              )
                                                                               (
                                                                                _substring (
                                                                                  list-ref img y
                                                                                )
                                                                                 x (
                                                                                  + x 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? (
                                                                                  list-ref img y
                                                                                )
                                                                              )
                                                                               (
                                                                                hash-table-ref (
                                                                                  list-ref img y
                                                                                )
                                                                                 x
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref (
                                                                                  list-ref img y
                                                                                )
                                                                                 x
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    set! x (
                                                                      + x 1
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
                                                    set! out (
                                                      append out (
                                                        _list row
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
                            ret2 out
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
        change_contrast img factor
      )
       (
        call/cc (
          lambda (
            ret7
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
                        out (
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
                                          < y h
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
                                                    x 0
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
                                                                  < x w
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        p (
                                                                          cond (
                                                                            (
                                                                              string? (
                                                                                list-ref img y
                                                                              )
                                                                            )
                                                                             (
                                                                              _substring (
                                                                                list-ref img y
                                                                              )
                                                                               x (
                                                                                + x 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            (
                                                                              hash-table? (
                                                                                list-ref img y
                                                                              )
                                                                            )
                                                                             (
                                                                              hash-table-ref (
                                                                                list-ref img y
                                                                              )
                                                                               x
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref (
                                                                                list-ref img y
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
                                                                            v (
                                                                              + (
                                                                                _div (
                                                                                  * (
                                                                                    - p 128
                                                                                  )
                                                                                   factor
                                                                                )
                                                                                 100
                                                                              )
                                                                               128
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            set! v (
                                                                              clamp_byte v
                                                                            )
                                                                          )
                                                                           (
                                                                            set! row (
                                                                              append row (
                                                                                _list v
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
                                                    set! y (
                                                      + y 1
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
          )
        )
      )
    )
     (
      define (
        gen_gaussian_kernel n sigma
      )
       (
        call/cc (
          lambda (
            ret12
          )
           (
            begin (
              if (
                equal? n 3
              )
               (
                begin (
                  ret12 (
                    _list (
                      _list (
                        _div 1.0 16.0
                      )
                       (
                        _div 2.0 16.0
                      )
                       (
                        _div 1.0 16.0
                      )
                    )
                     (
                      _list (
                        _div 2.0 16.0
                      )
                       (
                        _div 4.0 16.0
                      )
                       (
                        _div 2.0 16.0
                      )
                    )
                     (
                      _list (
                        _div 1.0 16.0
                      )
                       (
                        _div 2.0 16.0
                      )
                       (
                        _div 1.0 16.0
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
              let (
                (
                  k (
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
                                    < i n
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
                                                            < j n
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
                                              set! k (
                                                append k (
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
                      ret12 k
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
        img_convolve img kernel
      )
       (
        call/cc (
          lambda (
            ret17
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
                        out (
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
                                          < y h
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
                                                    x 0
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
                                                                  < x w
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        acc 0.0
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
                                                                                break23
                                                                              )
                                                                               (
                                                                                letrec (
                                                                                  (
                                                                                    loop22 (
                                                                                      lambda (
                                                                                        
                                                                                      )
                                                                                       (
                                                                                        if (
                                                                                          < ky (
                                                                                            _len kernel
                                                                                          )
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
                                                                                                              < kx (
                                                                                                                _len (
                                                                                                                  list-ref kernel 0
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              begin (
                                                                                                                let (
                                                                                                                  (
                                                                                                                    iy (
                                                                                                                      - (
                                                                                                                        + y ky
                                                                                                                      )
                                                                                                                       1
                                                                                                                    )
                                                                                                                  )
                                                                                                                )
                                                                                                                 (
                                                                                                                  begin (
                                                                                                                    let (
                                                                                                                      (
                                                                                                                        ix (
                                                                                                                          - (
                                                                                                                            + x kx
                                                                                                                          )
                                                                                                                           1
                                                                                                                        )
                                                                                                                      )
                                                                                                                    )
                                                                                                                     (
                                                                                                                      begin (
                                                                                                                        let (
                                                                                                                          (
                                                                                                                            pixel 0
                                                                                                                          )
                                                                                                                        )
                                                                                                                         (
                                                                                                                          begin (
                                                                                                                            if (
                                                                                                                              and (
                                                                                                                                and (
                                                                                                                                  and (
                                                                                                                                    >= iy 0
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    < iy h
                                                                                                                                  )
                                                                                                                                )
                                                                                                                                 (
                                                                                                                                  >= ix 0
                                                                                                                                )
                                                                                                                              )
                                                                                                                               (
                                                                                                                                < ix w
                                                                                                                              )
                                                                                                                            )
                                                                                                                             (
                                                                                                                              begin (
                                                                                                                                set! pixel (
                                                                                                                                  cond (
                                                                                                                                    (
                                                                                                                                      string? (
                                                                                                                                        list-ref img iy
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      _substring (
                                                                                                                                        list-ref img iy
                                                                                                                                      )
                                                                                                                                       ix (
                                                                                                                                        + ix 1
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    (
                                                                                                                                      hash-table? (
                                                                                                                                        list-ref img iy
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      hash-table-ref (
                                                                                                                                        list-ref img iy
                                                                                                                                      )
                                                                                                                                       ix
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    else (
                                                                                                                                      list-ref (
                                                                                                                                        list-ref img iy
                                                                                                                                      )
                                                                                                                                       ix
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
                                                                                                                            set! acc (
                                                                                                                              _add acc (
                                                                                                                                * (
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
                                                                                                                                 (
                                                                                                                                  * 1.0 pixel
                                                                                                                                )
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
                                                                                                set! ky (
                                                                                                  + ky 1
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            loop22
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
                                                                                  loop22
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
                                                                                      v26 acc
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    cond (
                                                                                      (
                                                                                        string? v26
                                                                                      )
                                                                                       (
                                                                                        exact (
                                                                                          floor (
                                                                                            string->number v26
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      (
                                                                                        boolean? v26
                                                                                      )
                                                                                       (
                                                                                        if v26 1 0
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      else (
                                                                                        exact (
                                                                                          floor v26
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
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
                                                    set! out (
                                                      append out (
                                                        _list row
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
                            ret17 out
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
        sort_ints xs
      )
       (
        call/cc (
          lambda (
            ret27
          )
           (
            let (
              (
                arr xs
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
                                    _len arr
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
                                                      < j (
                                                        - (
                                                          - (
                                                            _len arr
                                                          )
                                                           1
                                                        )
                                                         i
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        if (
                                                          > (
                                                            list-ref arr j
                                                          )
                                                           (
                                                            list-ref arr (
                                                              + j 1
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                tmp (
                                                                  list-ref arr j
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                list-set! arr j (
                                                                  list-ref arr (
                                                                    + j 1
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                list-set! arr (
                                                                  + j 1
                                                                )
                                                                 tmp
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
                                                        set! j (
                                                          + j 1
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
                                        set! i (
                                          + i 1
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
                    ret27 arr
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
        median_filter img k
      )
       (
        call/cc (
          lambda (
            ret32
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
                        offset (
                          _div k 2
                        )
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
                            let (
                              (
                                y 0
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
                                              < y h
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
                                                        x 0
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
                                                                      < x w
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            vals (
                                                                              _list
                                                                            )
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
                                                                                                                  < kx k
                                                                                                                )
                                                                                                                 (
                                                                                                                  begin (
                                                                                                                    let (
                                                                                                                      (
                                                                                                                        iy (
                                                                                                                          - (
                                                                                                                            + y ky
                                                                                                                          )
                                                                                                                           offset
                                                                                                                        )
                                                                                                                      )
                                                                                                                    )
                                                                                                                     (
                                                                                                                      begin (
                                                                                                                        let (
                                                                                                                          (
                                                                                                                            ix (
                                                                                                                              - (
                                                                                                                                + x kx
                                                                                                                              )
                                                                                                                               offset
                                                                                                                            )
                                                                                                                          )
                                                                                                                        )
                                                                                                                         (
                                                                                                                          begin (
                                                                                                                            let (
                                                                                                                              (
                                                                                                                                pixel 0
                                                                                                                              )
                                                                                                                            )
                                                                                                                             (
                                                                                                                              begin (
                                                                                                                                if (
                                                                                                                                  and (
                                                                                                                                    and (
                                                                                                                                      and (
                                                                                                                                        >= iy 0
                                                                                                                                      )
                                                                                                                                       (
                                                                                                                                        < iy h
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      >= ix 0
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    < ix w
                                                                                                                                  )
                                                                                                                                )
                                                                                                                                 (
                                                                                                                                  begin (
                                                                                                                                    set! pixel (
                                                                                                                                      cond (
                                                                                                                                        (
                                                                                                                                          string? (
                                                                                                                                            list-ref img iy
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                         (
                                                                                                                                          _substring (
                                                                                                                                            list-ref img iy
                                                                                                                                          )
                                                                                                                                           ix (
                                                                                                                                            + ix 1
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                       (
                                                                                                                                        (
                                                                                                                                          hash-table? (
                                                                                                                                            list-ref img iy
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                         (
                                                                                                                                          hash-table-ref (
                                                                                                                                            list-ref img iy
                                                                                                                                          )
                                                                                                                                           ix
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                       (
                                                                                                                                        else (
                                                                                                                                          list-ref (
                                                                                                                                            list-ref img iy
                                                                                                                                          )
                                                                                                                                           ix
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
                                                                                                                                set! vals (
                                                                                                                                  append vals (
                                                                                                                                    _list pixel
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
                                                                                                    set! ky (
                                                                                                      + ky 1
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                loop37
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
                                                                                      loop37
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                let (
                                                                                  (
                                                                                    sorted (
                                                                                      sort_ints vals
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    set! row (
                                                                                      append row (
                                                                                        _list (
                                                                                          cond (
                                                                                            (
                                                                                              string? sorted
                                                                                            )
                                                                                             (
                                                                                              _substring sorted (
                                                                                                _div (
                                                                                                  _len sorted
                                                                                                )
                                                                                                 2
                                                                                              )
                                                                                               (
                                                                                                + (
                                                                                                  _div (
                                                                                                    _len sorted
                                                                                                  )
                                                                                                   2
                                                                                                )
                                                                                                 1
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            (
                                                                                              hash-table? sorted
                                                                                            )
                                                                                             (
                                                                                              hash-table-ref sorted (
                                                                                                _div (
                                                                                                  _len sorted
                                                                                                )
                                                                                                 2
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            else (
                                                                                              list-ref sorted (
                                                                                                _div (
                                                                                                  _len sorted
                                                                                                )
                                                                                                 2
                                                                                              )
                                                                                            )
                                                                                          )
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
                                                        set! out (
                                                          append out (
                                                            _list row
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
                               (
                                ret32 out
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
        iabs x
      )
       (
        call/cc (
          lambda (
            ret41
          )
           (
            begin (
              if (
                < x 0
              )
               (
                begin (
                  ret41 (
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
              ret41 x
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
            ret42
          )
           (
            let (
              (
                gx (
                  _list (
                    _list 1 0 (
                      - 1
                    )
                  )
                   (
                    _list 2 0 (
                      - 2
                    )
                  )
                   (
                    _list 1 0 (
                      - 1
                    )
                  )
                )
              )
            )
             (
              begin (
                let (
                  (
                    gy (
                      _list (
                        _list 1 2 1
                      )
                       (
                        _list 0 0 0
                      )
                       (
                        _list (
                          - 1
                        )
                         (
                          - 2
                        )
                         (
                          - 1
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
                                out (
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
                                        break44
                                      )
                                       (
                                        letrec (
                                          (
                                            loop43 (
                                              lambda (
                                                
                                              )
                                               (
                                                if (
                                                  < y h
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
                                                            x 0
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            call/cc (
                                                              lambda (
                                                                break46
                                                              )
                                                               (
                                                                letrec (
                                                                  (
                                                                    loop45 (
                                                                      lambda (
                                                                        
                                                                      )
                                                                       (
                                                                        if (
                                                                          < x w
                                                                        )
                                                                         (
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                sx 0
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                let (
                                                                                  (
                                                                                    sy 0
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
                                                                                            break48
                                                                                          )
                                                                                           (
                                                                                            letrec (
                                                                                              (
                                                                                                loop47 (
                                                                                                  lambda (
                                                                                                    
                                                                                                  )
                                                                                                   (
                                                                                                    if (
                                                                                                      < ky 3
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
                                                                                                                break50
                                                                                                              )
                                                                                                               (
                                                                                                                letrec (
                                                                                                                  (
                                                                                                                    loop49 (
                                                                                                                      lambda (
                                                                                                                        
                                                                                                                      )
                                                                                                                       (
                                                                                                                        if (
                                                                                                                          < kx 3
                                                                                                                        )
                                                                                                                         (
                                                                                                                          begin (
                                                                                                                            let (
                                                                                                                              (
                                                                                                                                iy (
                                                                                                                                  - (
                                                                                                                                    + y ky
                                                                                                                                  )
                                                                                                                                   1
                                                                                                                                )
                                                                                                                              )
                                                                                                                            )
                                                                                                                             (
                                                                                                                              begin (
                                                                                                                                let (
                                                                                                                                  (
                                                                                                                                    ix (
                                                                                                                                      - (
                                                                                                                                        + x kx
                                                                                                                                      )
                                                                                                                                       1
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                )
                                                                                                                                 (
                                                                                                                                  begin (
                                                                                                                                    let (
                                                                                                                                      (
                                                                                                                                        pixel 0
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      begin (
                                                                                                                                        if (
                                                                                                                                          and (
                                                                                                                                            and (
                                                                                                                                              and (
                                                                                                                                                >= iy 0
                                                                                                                                              )
                                                                                                                                               (
                                                                                                                                                < iy h
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                             (
                                                                                                                                              >= ix 0
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                           (
                                                                                                                                            < ix w
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                         (
                                                                                                                                          begin (
                                                                                                                                            set! pixel (
                                                                                                                                              cond (
                                                                                                                                                (
                                                                                                                                                  string? (
                                                                                                                                                    list-ref img iy
                                                                                                                                                  )
                                                                                                                                                )
                                                                                                                                                 (
                                                                                                                                                  _substring (
                                                                                                                                                    list-ref img iy
                                                                                                                                                  )
                                                                                                                                                   ix (
                                                                                                                                                    + ix 1
                                                                                                                                                  )
                                                                                                                                                )
                                                                                                                                              )
                                                                                                                                               (
                                                                                                                                                (
                                                                                                                                                  hash-table? (
                                                                                                                                                    list-ref img iy
                                                                                                                                                  )
                                                                                                                                                )
                                                                                                                                                 (
                                                                                                                                                  hash-table-ref (
                                                                                                                                                    list-ref img iy
                                                                                                                                                  )
                                                                                                                                                   ix
                                                                                                                                                )
                                                                                                                                              )
                                                                                                                                               (
                                                                                                                                                else (
                                                                                                                                                  list-ref (
                                                                                                                                                    list-ref img iy
                                                                                                                                                  )
                                                                                                                                                   ix
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
                                                                                                                                        set! sx (
                                                                                                                                          + sx (
                                                                                                                                            * (
                                                                                                                                              cond (
                                                                                                                                                (
                                                                                                                                                  string? (
                                                                                                                                                    list-ref gx ky
                                                                                                                                                  )
                                                                                                                                                )
                                                                                                                                                 (
                                                                                                                                                  _substring (
                                                                                                                                                    list-ref gx ky
                                                                                                                                                  )
                                                                                                                                                   kx (
                                                                                                                                                    + kx 1
                                                                                                                                                  )
                                                                                                                                                )
                                                                                                                                              )
                                                                                                                                               (
                                                                                                                                                (
                                                                                                                                                  hash-table? (
                                                                                                                                                    list-ref gx ky
                                                                                                                                                  )
                                                                                                                                                )
                                                                                                                                                 (
                                                                                                                                                  hash-table-ref (
                                                                                                                                                    list-ref gx ky
                                                                                                                                                  )
                                                                                                                                                   kx
                                                                                                                                                )
                                                                                                                                              )
                                                                                                                                               (
                                                                                                                                                else (
                                                                                                                                                  list-ref (
                                                                                                                                                    list-ref gx ky
                                                                                                                                                  )
                                                                                                                                                   kx
                                                                                                                                                )
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                             pixel
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                       (
                                                                                                                                        set! sy (
                                                                                                                                          + sy (
                                                                                                                                            * (
                                                                                                                                              cond (
                                                                                                                                                (
                                                                                                                                                  string? (
                                                                                                                                                    list-ref gy ky
                                                                                                                                                  )
                                                                                                                                                )
                                                                                                                                                 (
                                                                                                                                                  _substring (
                                                                                                                                                    list-ref gy ky
                                                                                                                                                  )
                                                                                                                                                   kx (
                                                                                                                                                    + kx 1
                                                                                                                                                  )
                                                                                                                                                )
                                                                                                                                              )
                                                                                                                                               (
                                                                                                                                                (
                                                                                                                                                  hash-table? (
                                                                                                                                                    list-ref gy ky
                                                                                                                                                  )
                                                                                                                                                )
                                                                                                                                                 (
                                                                                                                                                  hash-table-ref (
                                                                                                                                                    list-ref gy ky
                                                                                                                                                  )
                                                                                                                                                   kx
                                                                                                                                                )
                                                                                                                                              )
                                                                                                                                               (
                                                                                                                                                else (
                                                                                                                                                  list-ref (
                                                                                                                                                    list-ref gy ky
                                                                                                                                                  )
                                                                                                                                                   kx
                                                                                                                                                )
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                             pixel
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
                                                                                                                            )
                                                                                                                          )
                                                                                                                           (
                                                                                                                            loop49
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
                                                                                                                  loop49
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
                                                                                                        loop47
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
                                                                                              loop47
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        set! row (
                                                                                          append row (
                                                                                            _list (
                                                                                              _add (
                                                                                                iabs sx
                                                                                              )
                                                                                               (
                                                                                                iabs sy
                                                                                              )
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
                                                                            loop45
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
                                                                  loop45
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
                                                            set! y (
                                                              + y 1
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    loop43
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
                                          loop43
                                        )
                                      )
                                    )
                                  )
                                   (
                                    ret42 out
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
        get_neighbors_pixel img x y
      )
       (
        call/cc (
          lambda (
            ret51
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
                        neighbors (
                          _list
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            dy (
                              - 1
                            )
                          )
                        )
                         (
                          begin (
                            call/cc (
                              lambda (
                                break53
                              )
                               (
                                letrec (
                                  (
                                    loop52 (
                                      lambda (
                                        
                                      )
                                       (
                                        if (
                                          <= dy 1
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                dx (
                                                  - 1
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                call/cc (
                                                  lambda (
                                                    break55
                                                  )
                                                   (
                                                    letrec (
                                                      (
                                                        loop54 (
                                                          lambda (
                                                            
                                                          )
                                                           (
                                                            if (
                                                              <= dx 1
                                                            )
                                                             (
                                                              begin (
                                                                if (
                                                                  not (
                                                                    and (
                                                                      equal? dx 0
                                                                    )
                                                                     (
                                                                      equal? dy 0
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        ny (
                                                                          + y dy
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            nx (
                                                                              + x dx
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                val 0
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                if (
                                                                                  and (
                                                                                    and (
                                                                                      and (
                                                                                        >= ny 0
                                                                                      )
                                                                                       (
                                                                                        < ny h
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      >= nx 0
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    < nx w
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    set! val (
                                                                                      cond (
                                                                                        (
                                                                                          string? (
                                                                                            list-ref img ny
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          _substring (
                                                                                            list-ref img ny
                                                                                          )
                                                                                           nx (
                                                                                            + nx 1
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        (
                                                                                          hash-table? (
                                                                                            list-ref img ny
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          hash-table-ref (
                                                                                            list-ref img ny
                                                                                          )
                                                                                           nx
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        else (
                                                                                          list-ref (
                                                                                            list-ref img ny
                                                                                          )
                                                                                           nx
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
                                                                                set! neighbors (
                                                                                  append neighbors (
                                                                                    _list val
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
                                                                  quote (
                                                                    
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                set! dx (
                                                                  + dx 1
                                                                )
                                                              )
                                                               (
                                                                loop54
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
                                                      loop54
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                set! dy (
                                                  + dy 1
                                                )
                                              )
                                            )
                                          )
                                           (
                                            loop52
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
                                  loop52
                                )
                              )
                            )
                          )
                           (
                            ret51 neighbors
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
        pow2 e
      )
       (
        call/cc (
          lambda (
            ret56
          )
           (
            let (
              (
                r 1
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
                        break58
                      )
                       (
                        letrec (
                          (
                            loop57 (
                              lambda (
                                
                              )
                               (
                                if (
                                  < i e
                                )
                                 (
                                  begin (
                                    set! r (
                                      * r 2
                                    )
                                  )
                                   (
                                    set! i (
                                      + i 1
                                    )
                                  )
                                   (
                                    loop57
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
                          loop57
                        )
                      )
                    )
                  )
                   (
                    ret56 r
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
        local_binary_value img x y
      )
       (
        call/cc (
          lambda (
            ret59
          )
           (
            let (
              (
                center (
                  cond (
                    (
                      string? (
                        list-ref img y
                      )
                    )
                     (
                      _substring (
                        list-ref img y
                      )
                       x (
                        + x 1
                      )
                    )
                  )
                   (
                    (
                      hash-table? (
                        list-ref img y
                      )
                    )
                     (
                      hash-table-ref (
                        list-ref img y
                      )
                       x
                    )
                  )
                   (
                    else (
                      list-ref (
                        list-ref img y
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
                    neighbors (
                      get_neighbors_pixel img x y
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        v 0
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
                                break61
                              )
                               (
                                letrec (
                                  (
                                    loop60 (
                                      lambda (
                                        
                                      )
                                       (
                                        if (
                                          < i (
                                            _len neighbors
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              _ge (
                                                cond (
                                                  (
                                                    string? neighbors
                                                  )
                                                   (
                                                    _substring neighbors i (
                                                      + i 1
                                                    )
                                                  )
                                                )
                                                 (
                                                  (
                                                    hash-table? neighbors
                                                  )
                                                   (
                                                    hash-table-ref neighbors i
                                                  )
                                                )
                                                 (
                                                  else (
                                                    list-ref neighbors i
                                                  )
                                                )
                                              )
                                               center
                                            )
                                             (
                                              begin (
                                                set! v (
                                                  _add v (
                                                    pow2 i
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
                                            set! i (
                                              + i 1
                                            )
                                          )
                                           (
                                            loop60
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
                                  loop60
                                )
                              )
                            )
                          )
                           (
                            ret59 v
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
        local_binary_pattern img
      )
       (
        call/cc (
          lambda (
            ret62
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
                        out (
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
                                break64
                              )
                               (
                                letrec (
                                  (
                                    loop63 (
                                      lambda (
                                        
                                      )
                                       (
                                        if (
                                          < y h
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
                                                    x 0
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    call/cc (
                                                      lambda (
                                                        break66
                                                      )
                                                       (
                                                        letrec (
                                                          (
                                                            loop65 (
                                                              lambda (
                                                                
                                                              )
                                                               (
                                                                if (
                                                                  < x w
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! row (
                                                                      append row (
                                                                        _list (
                                                                          local_binary_value img x y
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    set! x (
                                                                      + x 1
                                                                    )
                                                                  )
                                                                   (
                                                                    loop65
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
                                                          loop65
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
                                                    set! y (
                                                      + y 1
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                           (
                                            loop63
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
                                  loop63
                                )
                              )
                            )
                          )
                           (
                            ret62 out
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
              _list 52 55 61
            )
             (
              _list 62 59 55
            )
             (
              _list 63 65 66
            )
          )
        )
      )
       (
        begin (
          let (
            (
              negative (
                convert_to_negative img
              )
            )
          )
           (
            begin (
              let (
                (
                  contrast (
                    change_contrast img 110
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      kernel (
                        gen_gaussian_kernel 3 1.0
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          laplace (
                            _list (
                              _list 0.25 0.5 0.25
                            )
                             (
                              _list 0.5 (
                                - 3.0
                              )
                               0.5
                            )
                             (
                              _list 0.25 0.5 0.25
                            )
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              convolved (
                                img_convolve img laplace
                              )
                            )
                          )
                           (
                            begin (
                              let (
                                (
                                  medianed (
                                    median_filter img 3
                                  )
                                )
                              )
                               (
                                begin (
                                  let (
                                    (
                                      sobel (
                                        sobel_filter img
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      let (
                                        (
                                          lbp_img (
                                            local_binary_pattern img
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          _display (
                                            if (
                                              string? negative
                                            )
                                             negative (
                                              to-str negative
                                            )
                                          )
                                        )
                                         (
                                          newline
                                        )
                                         (
                                          _display (
                                            if (
                                              string? contrast
                                            )
                                             contrast (
                                              to-str contrast
                                            )
                                          )
                                        )
                                         (
                                          newline
                                        )
                                         (
                                          _display (
                                            if (
                                              string? kernel
                                            )
                                             kernel (
                                              to-str kernel
                                            )
                                          )
                                        )
                                         (
                                          newline
                                        )
                                         (
                                          _display (
                                            if (
                                              string? convolved
                                            )
                                             convolved (
                                              to-str convolved
                                            )
                                          )
                                        )
                                         (
                                          newline
                                        )
                                         (
                                          _display (
                                            if (
                                              string? medianed
                                            )
                                             medianed (
                                              to-str medianed
                                            )
                                          )
                                        )
                                         (
                                          newline
                                        )
                                         (
                                          _display (
                                            if (
                                              string? sobel
                                            )
                                             sobel (
                                              to-str sobel
                                            )
                                          )
                                        )
                                         (
                                          newline
                                        )
                                         (
                                          _display (
                                            if (
                                              string? lbp_img
                                            )
                                             lbp_img (
                                              to-str lbp_img
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
        )
      )
    )
     (
      let (
        (
          end68 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur69 (
              quotient (
                * (
                  - end68 start67
                )
                 1000000
              )
               jps70
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur69
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
