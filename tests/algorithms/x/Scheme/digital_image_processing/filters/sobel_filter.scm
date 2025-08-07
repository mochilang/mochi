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
      start62 (
        current-jiffy
      )
    )
     (
      jps65 (
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
            atanApprox x
          )
           (
            call/cc (
              lambda (
                ret5
              )
               (
                begin (
                  if (
                    > x 1.0
                  )
                   (
                    begin (
                      ret5 (
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
                      ret5 (
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
                  ret5 (
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
                ret6
              )
               (
                begin (
                  if (
                    equal? x 0.0
                  )
                   (
                    begin (
                      if (
                        > y 0.0
                      )
                       (
                        begin (
                          ret6 (
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
                          ret6 (
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
                      ret6 0.0
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
                      a (
                        atanApprox (
                          _div y x
                        )
                      )
                    )
                  )
                   (
                    begin (
                      if (
                        > x 0.0
                      )
                       (
                        begin (
                          ret6 a
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      if (
                        >= y 0.0
                      )
                       (
                        begin (
                          ret6 (
                            _add a PI
                          )
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      ret6 (
                        - a PI
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
            zeros h w
          )
           (
            call/cc (
              lambda (
                ret7
              )
               (
                let (
                  (
                    m (
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
                                                                set! row (
                                                                  append row (
                                                                    _list 0.0
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                set! x (
                                                                  + x 1
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
                                                set! m (
                                                  append m (
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
                        ret7 m
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
            pad_edge img pad
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
                            out (
                              zeros (
                                + h (
                                  * pad 2
                                )
                              )
                               (
                                + w (
                                  * pad 2
                                )
                              )
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
                                                + h (
                                                  * pad 2
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
                                                                    + w (
                                                                      * pad 2
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        sy (
                                                                          - y pad
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        if (
                                                                          < sy 0
                                                                        )
                                                                         (
                                                                          begin (
                                                                            set! sy 0
                                                                          )
                                                                        )
                                                                         (
                                                                          quote (
                                                                            
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        if (
                                                                          >= sy h
                                                                        )
                                                                         (
                                                                          begin (
                                                                            set! sy (
                                                                              - h 1
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
                                                                            sx (
                                                                              - x pad
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            if (
                                                                              < sx 0
                                                                            )
                                                                             (
                                                                              begin (
                                                                                set! sx 0
                                                                              )
                                                                            )
                                                                             (
                                                                              quote (
                                                                                
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            if (
                                                                              >= sx w
                                                                            )
                                                                             (
                                                                              begin (
                                                                                set! sx (
                                                                                  - w 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              quote (
                                                                                
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            list-set! (
                                                                              list-ref out y
                                                                            )
                                                                             x (
                                                                              cond (
                                                                                (
                                                                                  string? (
                                                                                    list-ref img sy
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  _substring (
                                                                                    list-ref img sy
                                                                                  )
                                                                                   sx (
                                                                                    + sx 1
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                (
                                                                                  hash-table? (
                                                                                    list-ref img sy
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  hash-table-ref (
                                                                                    list-ref img sy
                                                                                  )
                                                                                   sx
                                                                                )
                                                                              )
                                                                               (
                                                                                else (
                                                                                  list-ref (
                                                                                    list-ref img sy
                                                                                  )
                                                                                   sx
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
                                    padded (
                                      pad_edge img pad
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        out (
                                          zeros h w
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
                                                                                    sum 0.0
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
                                                                                                      < i k
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
                                                                                                                          < j k
                                                                                                                        )
                                                                                                                         (
                                                                                                                          begin (
                                                                                                                            set! sum (
                                                                                                                              _add sum (
                                                                                                                                * (
                                                                                                                                  cond (
                                                                                                                                    (
                                                                                                                                      string? (
                                                                                                                                        cond (
                                                                                                                                          (
                                                                                                                                            string? padded
                                                                                                                                          )
                                                                                                                                           (
                                                                                                                                            _substring padded (
                                                                                                                                              + y i
                                                                                                                                            )
                                                                                                                                             (
                                                                                                                                              + (
                                                                                                                                                + y i
                                                                                                                                              )
                                                                                                                                               1
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                         (
                                                                                                                                          (
                                                                                                                                            hash-table? padded
                                                                                                                                          )
                                                                                                                                           (
                                                                                                                                            hash-table-ref padded (
                                                                                                                                              + y i
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                         (
                                                                                                                                          else (
                                                                                                                                            list-ref padded (
                                                                                                                                              + y i
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      _substring (
                                                                                                                                        cond (
                                                                                                                                          (
                                                                                                                                            string? padded
                                                                                                                                          )
                                                                                                                                           (
                                                                                                                                            _substring padded (
                                                                                                                                              + y i
                                                                                                                                            )
                                                                                                                                             (
                                                                                                                                              + (
                                                                                                                                                + y i
                                                                                                                                              )
                                                                                                                                               1
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                         (
                                                                                                                                          (
                                                                                                                                            hash-table? padded
                                                                                                                                          )
                                                                                                                                           (
                                                                                                                                            hash-table-ref padded (
                                                                                                                                              + y i
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                         (
                                                                                                                                          else (
                                                                                                                                            list-ref padded (
                                                                                                                                              + y i
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                       (
                                                                                                                                        + x j
                                                                                                                                      )
                                                                                                                                       (
                                                                                                                                        + (
                                                                                                                                          + x j
                                                                                                                                        )
                                                                                                                                         1
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    (
                                                                                                                                      hash-table? (
                                                                                                                                        cond (
                                                                                                                                          (
                                                                                                                                            string? padded
                                                                                                                                          )
                                                                                                                                           (
                                                                                                                                            _substring padded (
                                                                                                                                              + y i
                                                                                                                                            )
                                                                                                                                             (
                                                                                                                                              + (
                                                                                                                                                + y i
                                                                                                                                              )
                                                                                                                                               1
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                         (
                                                                                                                                          (
                                                                                                                                            hash-table? padded
                                                                                                                                          )
                                                                                                                                           (
                                                                                                                                            hash-table-ref padded (
                                                                                                                                              + y i
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                         (
                                                                                                                                          else (
                                                                                                                                            list-ref padded (
                                                                                                                                              + y i
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      hash-table-ref (
                                                                                                                                        cond (
                                                                                                                                          (
                                                                                                                                            string? padded
                                                                                                                                          )
                                                                                                                                           (
                                                                                                                                            _substring padded (
                                                                                                                                              + y i
                                                                                                                                            )
                                                                                                                                             (
                                                                                                                                              + (
                                                                                                                                                + y i
                                                                                                                                              )
                                                                                                                                               1
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                         (
                                                                                                                                          (
                                                                                                                                            hash-table? padded
                                                                                                                                          )
                                                                                                                                           (
                                                                                                                                            hash-table-ref padded (
                                                                                                                                              + y i
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                         (
                                                                                                                                          else (
                                                                                                                                            list-ref padded (
                                                                                                                                              + y i
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                       (
                                                                                                                                        + x j
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    else (
                                                                                                                                      list-ref (
                                                                                                                                        cond (
                                                                                                                                          (
                                                                                                                                            string? padded
                                                                                                                                          )
                                                                                                                                           (
                                                                                                                                            _substring padded (
                                                                                                                                              + y i
                                                                                                                                            )
                                                                                                                                             (
                                                                                                                                              + (
                                                                                                                                                + y i
                                                                                                                                              )
                                                                                                                                               1
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                         (
                                                                                                                                          (
                                                                                                                                            hash-table? padded
                                                                                                                                          )
                                                                                                                                           (
                                                                                                                                            hash-table-ref padded (
                                                                                                                                              + y i
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                         (
                                                                                                                                          else (
                                                                                                                                            list-ref padded (
                                                                                                                                              + y i
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                       (
                                                                                                                                        + x j
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                )
                                                                                                                                 (
                                                                                                                                  + 0.0 (
                                                                                                                                    cond (
                                                                                                                                      (
                                                                                                                                        string? (
                                                                                                                                          list-ref kernel i
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                       (
                                                                                                                                        _substring (
                                                                                                                                          list-ref kernel i
                                                                                                                                        )
                                                                                                                                         j (
                                                                                                                                          + j 1
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      (
                                                                                                                                        hash-table? (
                                                                                                                                          list-ref kernel i
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                       (
                                                                                                                                        hash-table-ref (
                                                                                                                                          list-ref kernel i
                                                                                                                                        )
                                                                                                                                         j
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      else (
                                                                                                                                        list-ref (
                                                                                                                                          list-ref kernel i
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
                                                                                                            set! i (
                                                                                                              + i 1
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
                                                                set! y (
                                                                  + y 1
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
                  )
                )
              )
            )
          )
        )
         (
          define (
            abs_matrix mat
          )
           (
            call/cc (
              lambda (
                ret26
              )
               (
                let (
                  (
                    h (
                      _len mat
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        w (
                          _len (
                            list-ref mat 0
                          )
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            out (
                              zeros h w
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
                                    break28
                                  )
                                   (
                                    letrec (
                                      (
                                        loop27 (
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
                                                    x 0
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
                                                                  < x w
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        v (
                                                                          cond (
                                                                            (
                                                                              string? (
                                                                                list-ref mat y
                                                                              )
                                                                            )
                                                                             (
                                                                              _substring (
                                                                                list-ref mat y
                                                                              )
                                                                               x (
                                                                                + x 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            (
                                                                              hash-table? (
                                                                                list-ref mat y
                                                                              )
                                                                            )
                                                                             (
                                                                              hash-table-ref (
                                                                                list-ref mat y
                                                                              )
                                                                               x
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref (
                                                                                list-ref mat y
                                                                              )
                                                                               x
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        if (
                                                                          < v 0.0
                                                                        )
                                                                         (
                                                                          begin (
                                                                            list-set! (
                                                                              list-ref out y
                                                                            )
                                                                             x (
                                                                              - v
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            list-set! (
                                                                              list-ref out y
                                                                            )
                                                                             x v
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
                                                    set! y (
                                                      + y 1
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                loop27
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
                                      loop27
                                    )
                                  )
                                )
                              )
                               (
                                ret26 out
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
            max_matrix mat
          )
           (
            call/cc (
              lambda (
                ret31
              )
               (
                let (
                  (
                    max_val (
                      cond (
                        (
                          string? (
                            list-ref mat 0
                          )
                        )
                         (
                          _substring (
                            list-ref mat 0
                          )
                           0 (
                            + 0 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? (
                            list-ref mat 0
                          )
                        )
                         (
                          hash-table-ref (
                            list-ref mat 0
                          )
                           0
                        )
                      )
                       (
                        else (
                          list-ref (
                            list-ref mat 0
                          )
                           0
                        )
                      )
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
                            break33
                          )
                           (
                            letrec (
                              (
                                loop32 (
                                  lambda (
                                    
                                  )
                                   (
                                    if (
                                      < y (
                                        _len mat
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
                                                          < x (
                                                            _len (
                                                              list-ref mat 0
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            if (
                                                              > (
                                                                cond (
                                                                  (
                                                                    string? (
                                                                      list-ref mat y
                                                                    )
                                                                  )
                                                                   (
                                                                    _substring (
                                                                      list-ref mat y
                                                                    )
                                                                     x (
                                                                      + x 1
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  (
                                                                    hash-table? (
                                                                      list-ref mat y
                                                                    )
                                                                  )
                                                                   (
                                                                    hash-table-ref (
                                                                      list-ref mat y
                                                                    )
                                                                     x
                                                                  )
                                                                )
                                                                 (
                                                                  else (
                                                                    list-ref (
                                                                      list-ref mat y
                                                                    )
                                                                     x
                                                                  )
                                                                )
                                                              )
                                                               max_val
                                                            )
                                                             (
                                                              begin (
                                                                set! max_val (
                                                                  cond (
                                                                    (
                                                                      string? (
                                                                        list-ref mat y
                                                                      )
                                                                    )
                                                                     (
                                                                      _substring (
                                                                        list-ref mat y
                                                                      )
                                                                       x (
                                                                        + x 1
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    (
                                                                      hash-table? (
                                                                        list-ref mat y
                                                                      )
                                                                    )
                                                                     (
                                                                      hash-table-ref (
                                                                        list-ref mat y
                                                                      )
                                                                       x
                                                                    )
                                                                  )
                                                                   (
                                                                    else (
                                                                      list-ref (
                                                                        list-ref mat y
                                                                      )
                                                                       x
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
                                                            set! x (
                                                              + x 1
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
                                            set! y (
                                              + y 1
                                            )
                                          )
                                        )
                                      )
                                       (
                                        loop32
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
                              loop32
                            )
                          )
                        )
                      )
                       (
                        ret31 max_val
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
            scale_matrix mat factor
          )
           (
            call/cc (
              lambda (
                ret36
              )
               (
                let (
                  (
                    h (
                      _len mat
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        w (
                          _len (
                            list-ref mat 0
                          )
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            out (
                              zeros h w
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
                                              < y h
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
                                                                  < x w
                                                                )
                                                                 (
                                                                  begin (
                                                                    list-set! (
                                                                      list-ref out y
                                                                    )
                                                                     x (
                                                                      * (
                                                                        cond (
                                                                          (
                                                                            string? (
                                                                              list-ref mat y
                                                                            )
                                                                          )
                                                                           (
                                                                            _substring (
                                                                              list-ref mat y
                                                                            )
                                                                             x (
                                                                              + x 1
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          (
                                                                            hash-table? (
                                                                              list-ref mat y
                                                                            )
                                                                          )
                                                                           (
                                                                            hash-table-ref (
                                                                              list-ref mat y
                                                                            )
                                                                             x
                                                                          )
                                                                        )
                                                                         (
                                                                          else (
                                                                            list-ref (
                                                                              list-ref mat y
                                                                            )
                                                                             x
                                                                          )
                                                                        )
                                                                      )
                                                                       factor
                                                                    )
                                                                  )
                                                                   (
                                                                    set! x (
                                                                      + x 1
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
                                                    set! y (
                                                      + y 1
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
                                ret36 out
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
            sobel_filter image
          )
           (
            call/cc (
              lambda (
                ret41
              )
               (
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
                            img (
                              _list
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                y0 0
                              )
                            )
                             (
                              begin (
                                call/cc (
                                  lambda (
                                    break43
                                  )
                                   (
                                    letrec (
                                      (
                                        loop42 (
                                          lambda (
                                            
                                          )
                                           (
                                            if (
                                              < y0 h
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
                                                        x0 0
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
                                                                      < x0 w
                                                                    )
                                                                     (
                                                                      begin (
                                                                        set! row (
                                                                          append row (
                                                                            _list (
                                                                              + 0.0 (
                                                                                cond (
                                                                                  (
                                                                                    string? (
                                                                                      list-ref image y0
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    _substring (
                                                                                      list-ref image y0
                                                                                    )
                                                                                     x0 (
                                                                                      + x0 1
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  (
                                                                                    hash-table? (
                                                                                      list-ref image y0
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    hash-table-ref (
                                                                                      list-ref image y0
                                                                                    )
                                                                                     x0
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  else (
                                                                                    list-ref (
                                                                                      list-ref image y0
                                                                                    )
                                                                                     x0
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        set! x0 (
                                                                          + x0 1
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
                                                        set! img (
                                                          append img (
                                                            _list row
                                                          )
                                                        )
                                                      )
                                                       (
                                                        set! y0 (
                                                          + y0 1
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                loop42
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
                                      loop42
                                    )
                                  )
                                )
                              )
                               (
                                let (
                                  (
                                    kernel_x (
                                      _list (
                                        _list (
                                          - 1
                                        )
                                         0 1
                                      )
                                       (
                                        _list (
                                          - 2
                                        )
                                         0 2
                                      )
                                       (
                                        _list (
                                          - 1
                                        )
                                         0 1
                                      )
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        kernel_y (
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
                                            dst_x (
                                              abs_matrix (
                                                img_convolve img kernel_x
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                dst_y (
                                                  abs_matrix (
                                                    img_convolve img kernel_y
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    max_x (
                                                      max_matrix dst_x
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        max_y (
                                                          max_matrix dst_y
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        set! dst_x (
                                                          scale_matrix dst_x (
                                                            _div 255.0 max_x
                                                          )
                                                        )
                                                      )
                                                       (
                                                        set! dst_y (
                                                          scale_matrix dst_y (
                                                            _div 255.0 max_y
                                                          )
                                                        )
                                                      )
                                                       (
                                                        let (
                                                          (
                                                            mag (
                                                              zeros h w
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                theta (
                                                                  zeros h w
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
                                                                                  < y h
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
                                                                                            break49
                                                                                          )
                                                                                           (
                                                                                            letrec (
                                                                                              (
                                                                                                loop48 (
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
                                                                                                            gx (
                                                                                                              cond (
                                                                                                                (
                                                                                                                  string? (
                                                                                                                    cond (
                                                                                                                      (
                                                                                                                        string? dst_x
                                                                                                                      )
                                                                                                                       (
                                                                                                                        _substring dst_x y (
                                                                                                                          + y 1
                                                                                                                        )
                                                                                                                      )
                                                                                                                    )
                                                                                                                     (
                                                                                                                      (
                                                                                                                        hash-table? dst_x
                                                                                                                      )
                                                                                                                       (
                                                                                                                        hash-table-ref dst_x y
                                                                                                                      )
                                                                                                                    )
                                                                                                                     (
                                                                                                                      else (
                                                                                                                        list-ref dst_x y
                                                                                                                      )
                                                                                                                    )
                                                                                                                  )
                                                                                                                )
                                                                                                                 (
                                                                                                                  _substring (
                                                                                                                    cond (
                                                                                                                      (
                                                                                                                        string? dst_x
                                                                                                                      )
                                                                                                                       (
                                                                                                                        _substring dst_x y (
                                                                                                                          + y 1
                                                                                                                        )
                                                                                                                      )
                                                                                                                    )
                                                                                                                     (
                                                                                                                      (
                                                                                                                        hash-table? dst_x
                                                                                                                      )
                                                                                                                       (
                                                                                                                        hash-table-ref dst_x y
                                                                                                                      )
                                                                                                                    )
                                                                                                                     (
                                                                                                                      else (
                                                                                                                        list-ref dst_x y
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
                                                                                                                        string? dst_x
                                                                                                                      )
                                                                                                                       (
                                                                                                                        _substring dst_x y (
                                                                                                                          + y 1
                                                                                                                        )
                                                                                                                      )
                                                                                                                    )
                                                                                                                     (
                                                                                                                      (
                                                                                                                        hash-table? dst_x
                                                                                                                      )
                                                                                                                       (
                                                                                                                        hash-table-ref dst_x y
                                                                                                                      )
                                                                                                                    )
                                                                                                                     (
                                                                                                                      else (
                                                                                                                        list-ref dst_x y
                                                                                                                      )
                                                                                                                    )
                                                                                                                  )
                                                                                                                )
                                                                                                                 (
                                                                                                                  hash-table-ref (
                                                                                                                    cond (
                                                                                                                      (
                                                                                                                        string? dst_x
                                                                                                                      )
                                                                                                                       (
                                                                                                                        _substring dst_x y (
                                                                                                                          + y 1
                                                                                                                        )
                                                                                                                      )
                                                                                                                    )
                                                                                                                     (
                                                                                                                      (
                                                                                                                        hash-table? dst_x
                                                                                                                      )
                                                                                                                       (
                                                                                                                        hash-table-ref dst_x y
                                                                                                                      )
                                                                                                                    )
                                                                                                                     (
                                                                                                                      else (
                                                                                                                        list-ref dst_x y
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
                                                                                                                        string? dst_x
                                                                                                                      )
                                                                                                                       (
                                                                                                                        _substring dst_x y (
                                                                                                                          + y 1
                                                                                                                        )
                                                                                                                      )
                                                                                                                    )
                                                                                                                     (
                                                                                                                      (
                                                                                                                        hash-table? dst_x
                                                                                                                      )
                                                                                                                       (
                                                                                                                        hash-table-ref dst_x y
                                                                                                                      )
                                                                                                                    )
                                                                                                                     (
                                                                                                                      else (
                                                                                                                        list-ref dst_x y
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
                                                                                                                gy (
                                                                                                                  cond (
                                                                                                                    (
                                                                                                                      string? (
                                                                                                                        cond (
                                                                                                                          (
                                                                                                                            string? dst_y
                                                                                                                          )
                                                                                                                           (
                                                                                                                            _substring dst_y y (
                                                                                                                              + y 1
                                                                                                                            )
                                                                                                                          )
                                                                                                                        )
                                                                                                                         (
                                                                                                                          (
                                                                                                                            hash-table? dst_y
                                                                                                                          )
                                                                                                                           (
                                                                                                                            hash-table-ref dst_y y
                                                                                                                          )
                                                                                                                        )
                                                                                                                         (
                                                                                                                          else (
                                                                                                                            list-ref dst_y y
                                                                                                                          )
                                                                                                                        )
                                                                                                                      )
                                                                                                                    )
                                                                                                                     (
                                                                                                                      _substring (
                                                                                                                        cond (
                                                                                                                          (
                                                                                                                            string? dst_y
                                                                                                                          )
                                                                                                                           (
                                                                                                                            _substring dst_y y (
                                                                                                                              + y 1
                                                                                                                            )
                                                                                                                          )
                                                                                                                        )
                                                                                                                         (
                                                                                                                          (
                                                                                                                            hash-table? dst_y
                                                                                                                          )
                                                                                                                           (
                                                                                                                            hash-table-ref dst_y y
                                                                                                                          )
                                                                                                                        )
                                                                                                                         (
                                                                                                                          else (
                                                                                                                            list-ref dst_y y
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
                                                                                                                            string? dst_y
                                                                                                                          )
                                                                                                                           (
                                                                                                                            _substring dst_y y (
                                                                                                                              + y 1
                                                                                                                            )
                                                                                                                          )
                                                                                                                        )
                                                                                                                         (
                                                                                                                          (
                                                                                                                            hash-table? dst_y
                                                                                                                          )
                                                                                                                           (
                                                                                                                            hash-table-ref dst_y y
                                                                                                                          )
                                                                                                                        )
                                                                                                                         (
                                                                                                                          else (
                                                                                                                            list-ref dst_y y
                                                                                                                          )
                                                                                                                        )
                                                                                                                      )
                                                                                                                    )
                                                                                                                     (
                                                                                                                      hash-table-ref (
                                                                                                                        cond (
                                                                                                                          (
                                                                                                                            string? dst_y
                                                                                                                          )
                                                                                                                           (
                                                                                                                            _substring dst_y y (
                                                                                                                              + y 1
                                                                                                                            )
                                                                                                                          )
                                                                                                                        )
                                                                                                                         (
                                                                                                                          (
                                                                                                                            hash-table? dst_y
                                                                                                                          )
                                                                                                                           (
                                                                                                                            hash-table-ref dst_y y
                                                                                                                          )
                                                                                                                        )
                                                                                                                         (
                                                                                                                          else (
                                                                                                                            list-ref dst_y y
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
                                                                                                                            string? dst_y
                                                                                                                          )
                                                                                                                           (
                                                                                                                            _substring dst_y y (
                                                                                                                              + y 1
                                                                                                                            )
                                                                                                                          )
                                                                                                                        )
                                                                                                                         (
                                                                                                                          (
                                                                                                                            hash-table? dst_y
                                                                                                                          )
                                                                                                                           (
                                                                                                                            hash-table-ref dst_y y
                                                                                                                          )
                                                                                                                        )
                                                                                                                         (
                                                                                                                          else (
                                                                                                                            list-ref dst_y y
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
                                                                                                                list-set! (
                                                                                                                  list-ref mag y
                                                                                                                )
                                                                                                                 x (
                                                                                                                  sqrtApprox (
                                                                                                                    _add (
                                                                                                                      * gx gx
                                                                                                                    )
                                                                                                                     (
                                                                                                                      * gy gy
                                                                                                                    )
                                                                                                                  )
                                                                                                                )
                                                                                                              )
                                                                                                               (
                                                                                                                list-set! (
                                                                                                                  list-ref theta y
                                                                                                                )
                                                                                                                 x (
                                                                                                                  atan2Approx gy gx
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
                                                                                                        loop48
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
                                                                                              loop48
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
                                                                        max_m (
                                                                          max_matrix mag
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        set! mag (
                                                                          scale_matrix mag (
                                                                            _div 255.0 max_m
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        ret41 (
                                                                          _list mag theta
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
            )
          )
        )
         (
          define (
            print_matrix_int mat
          )
           (
            call/cc (
              lambda (
                ret50
              )
               (
                let (
                  (
                    y 0
                  )
                )
                 (
                  begin (
                    call/cc (
                      lambda (
                        break52
                      )
                       (
                        letrec (
                          (
                            loop51 (
                              lambda (
                                
                              )
                               (
                                if (
                                  < y (
                                    _len mat
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
                                        let (
                                          (
                                            x 0
                                          )
                                        )
                                         (
                                          begin (
                                            call/cc (
                                              lambda (
                                                break54
                                              )
                                               (
                                                letrec (
                                                  (
                                                    loop53 (
                                                      lambda (
                                                        
                                                      )
                                                       (
                                                        if (
                                                          < x (
                                                            _len (
                                                              list-ref mat y
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            set! line (
                                                              string-append line (
                                                                to-str-space (
                                                                  let (
                                                                    (
                                                                      v55 (
                                                                        cond (
                                                                          (
                                                                            string? (
                                                                              list-ref mat y
                                                                            )
                                                                          )
                                                                           (
                                                                            _substring (
                                                                              list-ref mat y
                                                                            )
                                                                             x (
                                                                              + x 1
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          (
                                                                            hash-table? (
                                                                              list-ref mat y
                                                                            )
                                                                          )
                                                                           (
                                                                            hash-table-ref (
                                                                              list-ref mat y
                                                                            )
                                                                             x
                                                                          )
                                                                        )
                                                                         (
                                                                          else (
                                                                            list-ref (
                                                                              list-ref mat y
                                                                            )
                                                                             x
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    cond (
                                                                      (
                                                                        string? v55
                                                                      )
                                                                       (
                                                                        inexact->exact (
                                                                          floor (
                                                                            string->number v55
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      (
                                                                        boolean? v55
                                                                      )
                                                                       (
                                                                        if v55 1 0
                                                                      )
                                                                    )
                                                                     (
                                                                      else (
                                                                        inexact->exact (
                                                                          floor v55
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
                                                              < x (
                                                                - (
                                                                  _len (
                                                                    list-ref mat y
                                                                  )
                                                                )
                                                                 1
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                set! line (
                                                                  string-append line " "
                                                                )
                                                              )
                                                            )
                                                             (
                                                              quote (
                                                                
                                                              )
                                                            )
                                                          )
                                                           (
                                                            set! x (
                                                              + x 1
                                                            )
                                                          )
                                                           (
                                                            loop53
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
                                                  loop53
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
                                            set! y (
                                              + y 1
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                   (
                                    loop51
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
                          loop51
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
            print_matrix_float mat
          )
           (
            call/cc (
              lambda (
                ret56
              )
               (
                let (
                  (
                    y 0
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
                                  < y (
                                    _len mat
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
                                        let (
                                          (
                                            x 0
                                          )
                                        )
                                         (
                                          begin (
                                            call/cc (
                                              lambda (
                                                break60
                                              )
                                               (
                                                letrec (
                                                  (
                                                    loop59 (
                                                      lambda (
                                                        
                                                      )
                                                       (
                                                        if (
                                                          < x (
                                                            _len (
                                                              list-ref mat y
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            set! line (
                                                              string-append line (
                                                                to-str-space (
                                                                  cond (
                                                                    (
                                                                      string? (
                                                                        list-ref mat y
                                                                      )
                                                                    )
                                                                     (
                                                                      _substring (
                                                                        list-ref mat y
                                                                      )
                                                                       x (
                                                                        + x 1
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    (
                                                                      hash-table? (
                                                                        list-ref mat y
                                                                      )
                                                                    )
                                                                     (
                                                                      hash-table-ref (
                                                                        list-ref mat y
                                                                      )
                                                                       x
                                                                    )
                                                                  )
                                                                   (
                                                                    else (
                                                                      list-ref (
                                                                        list-ref mat y
                                                                      )
                                                                       x
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                           (
                                                            if (
                                                              < x (
                                                                - (
                                                                  _len (
                                                                    list-ref mat y
                                                                  )
                                                                )
                                                                 1
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                set! line (
                                                                  string-append line " "
                                                                )
                                                              )
                                                            )
                                                             (
                                                              quote (
                                                                
                                                              )
                                                            )
                                                          )
                                                           (
                                                            set! x (
                                                              + x 1
                                                            )
                                                          )
                                                           (
                                                            loop59
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
                                                  loop59
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
                                            set! y (
                                              + y 1
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
                ret61
              )
               (
                let (
                  (
                    img (
                      _list (
                        _list 10 10 10 10 10
                      )
                       (
                        _list 10 50 50 50 10
                      )
                       (
                        _list 10 50 80 50 10
                      )
                       (
                        _list 10 50 50 50 10
                      )
                       (
                        _list 10 10 10 10 10
                      )
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        res (
                          sobel_filter img
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            mag (
                              cond (
                                (
                                  string? res
                                )
                                 (
                                  _substring res 0 (
                                    + 0 1
                                  )
                                )
                              )
                               (
                                (
                                  hash-table? res
                                )
                                 (
                                  hash-table-ref res 0
                                )
                              )
                               (
                                else (
                                  list-ref res 0
                                )
                              )
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                theta (
                                  cond (
                                    (
                                      string? res
                                    )
                                     (
                                      _substring res 1 (
                                        + 1 1
                                      )
                                    )
                                  )
                                   (
                                    (
                                      hash-table? res
                                    )
                                     (
                                      hash-table-ref res 1
                                    )
                                  )
                                   (
                                    else (
                                      list-ref res 1
                                    )
                                  )
                                )
                              )
                            )
                             (
                              begin (
                                print_matrix_int mag
                              )
                               (
                                print_matrix_float theta
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
          main
        )
      )
    )
     (
      let (
        (
          end63 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur64 (
              quotient (
                * (
                  - end63 start62
                )
                 1000000
              )
               jps65
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur64
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
