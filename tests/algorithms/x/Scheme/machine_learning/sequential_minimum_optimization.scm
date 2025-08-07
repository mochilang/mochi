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
      start18 (
        current-jiffy
      )
    )
     (
      jps21 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        dot a b
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
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
                                  < i (
                                    _len a
                                  )
                                )
                                 (
                                  begin (
                                    set! sum (
                                      _add sum (
                                        * (
                                          list-ref a i
                                        )
                                         (
                                          list-ref b i
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
     (
      define (
        maxf a b
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            begin (
              if (
                > a b
              )
               (
                begin (
                  ret4 a
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              ret4 b
            )
          )
        )
      )
    )
     (
      define (
        minf a b
      )
       (
        call/cc (
          lambda (
            ret5
          )
           (
            begin (
              if (
                < a b
              )
               (
                begin (
                  ret5 a
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              ret5 b
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
            ret6
          )
           (
            begin (
              if (
                >= x 0.0
              )
               (
                begin (
                  ret6 x
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              ret6 (
                - 0.0 x
              )
            )
          )
        )
      )
    )
     (
      define (
        predict_raw samples labels alphas b x
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            let (
              (
                res 0.0
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
                                  < i (
                                    _len samples
                                  )
                                )
                                 (
                                  begin (
                                    set! res (
                                      _add res (
                                        * (
                                          * (
                                            list-ref alphas i
                                          )
                                           (
                                            list-ref labels i
                                          )
                                        )
                                         (
                                          dot (
                                            list-ref samples i
                                          )
                                           x
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
                    ret7 (
                      + res b
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
        smo_train samples labels c tol max_passes
      )
       (
        call/cc (
          lambda (
            ret10
          )
           (
            let (
              (
                m (
                  _len samples
                )
              )
            )
             (
              begin (
                let (
                  (
                    alphas (
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
                                      < i m
                                    )
                                     (
                                      begin (
                                        set! alphas (
                                          append alphas (
                                            _list 0.0
                                          )
                                        )
                                      )
                                       (
                                        set! i (
                                          + i 1
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
                        let (
                          (
                            b 0.0
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                passes 0
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
                                              < passes max_passes
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    num_changed 0
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        i1 0
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
                                                                      < i1 m
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            Ei (
                                                                              - (
                                                                                predict_raw samples labels alphas b (
                                                                                  list-ref samples i1
                                                                                )
                                                                              )
                                                                               (
                                                                                list-ref labels i1
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            if (
                                                                              or (
                                                                                and (
                                                                                  _lt (
                                                                                    * (
                                                                                      list-ref labels i1
                                                                                    )
                                                                                     Ei
                                                                                  )
                                                                                   (
                                                                                    - 0.0 tol
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  < (
                                                                                    list-ref alphas i1
                                                                                  )
                                                                                   c
                                                                                )
                                                                              )
                                                                               (
                                                                                and (
                                                                                  _gt (
                                                                                    * (
                                                                                      list-ref labels i1
                                                                                    )
                                                                                     Ei
                                                                                  )
                                                                                   tol
                                                                                )
                                                                                 (
                                                                                  > (
                                                                                    list-ref alphas i1
                                                                                  )
                                                                                   0.0
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                let (
                                                                                  (
                                                                                    i2 (
                                                                                      _mod (
                                                                                        + i1 1
                                                                                      )
                                                                                       m
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    let (
                                                                                      (
                                                                                        Ej (
                                                                                          - (
                                                                                            predict_raw samples labels alphas b (
                                                                                              list-ref samples i2
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            list-ref labels i2
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        let (
                                                                                          (
                                                                                            alpha1_old (
                                                                                              list-ref alphas i1
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            let (
                                                                                              (
                                                                                                alpha2_old (
                                                                                                  list-ref alphas i2
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                let (
                                                                                                  (
                                                                                                    L 0.0
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  begin (
                                                                                                    let (
                                                                                                      (
                                                                                                        H 0.0
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      begin (
                                                                                                        if (
                                                                                                          not (
                                                                                                            equal? (
                                                                                                              list-ref labels i1
                                                                                                            )
                                                                                                             (
                                                                                                              list-ref labels i2
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          begin (
                                                                                                            set! L (
                                                                                                              maxf 0.0 (
                                                                                                                - alpha2_old alpha1_old
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                           (
                                                                                                            set! H (
                                                                                                              minf c (
                                                                                                                - (
                                                                                                                  + c alpha2_old
                                                                                                                )
                                                                                                                 alpha1_old
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          begin (
                                                                                                            set! L (
                                                                                                              maxf 0.0 (
                                                                                                                - (
                                                                                                                  + alpha2_old alpha1_old
                                                                                                                )
                                                                                                                 c
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                           (
                                                                                                            set! H (
                                                                                                              minf c (
                                                                                                                + alpha2_old alpha1_old
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        if (
                                                                                                          equal? L H
                                                                                                        )
                                                                                                         (
                                                                                                          begin (
                                                                                                            set! i1 (
                                                                                                              + i1 1
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
                                                                                                       (
                                                                                                        let (
                                                                                                          (
                                                                                                            eta (
                                                                                                              - (
                                                                                                                - (
                                                                                                                  * 2.0 (
                                                                                                                    dot (
                                                                                                                      list-ref samples i1
                                                                                                                    )
                                                                                                                     (
                                                                                                                      list-ref samples i2
                                                                                                                    )
                                                                                                                  )
                                                                                                                )
                                                                                                                 (
                                                                                                                  dot (
                                                                                                                    list-ref samples i1
                                                                                                                  )
                                                                                                                   (
                                                                                                                    list-ref samples i1
                                                                                                                  )
                                                                                                                )
                                                                                                              )
                                                                                                               (
                                                                                                                dot (
                                                                                                                  list-ref samples i2
                                                                                                                )
                                                                                                                 (
                                                                                                                  list-ref samples i2
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          begin (
                                                                                                            if (
                                                                                                              _ge eta 0.0
                                                                                                            )
                                                                                                             (
                                                                                                              begin (
                                                                                                                set! i1 (
                                                                                                                  + i1 1
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
                                                                                                           (
                                                                                                            list-set! alphas i2 (
                                                                                                              - alpha2_old (
                                                                                                                _div (
                                                                                                                  * (
                                                                                                                    list-ref labels i2
                                                                                                                  )
                                                                                                                   (
                                                                                                                    - Ei Ej
                                                                                                                  )
                                                                                                                )
                                                                                                                 eta
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                           (
                                                                                                            if (
                                                                                                              > (
                                                                                                                list-ref alphas i2
                                                                                                              )
                                                                                                               H
                                                                                                            )
                                                                                                             (
                                                                                                              begin (
                                                                                                                list-set! alphas i2 H
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              quote (
                                                                                                                
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                           (
                                                                                                            if (
                                                                                                              < (
                                                                                                                list-ref alphas i2
                                                                                                              )
                                                                                                               L
                                                                                                            )
                                                                                                             (
                                                                                                              begin (
                                                                                                                list-set! alphas i2 L
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              quote (
                                                                                                                
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                           (
                                                                                                            if (
                                                                                                              _lt (
                                                                                                                absf (
                                                                                                                  - (
                                                                                                                    list-ref alphas i2
                                                                                                                  )
                                                                                                                   alpha2_old
                                                                                                                )
                                                                                                              )
                                                                                                               1e-05
                                                                                                            )
                                                                                                             (
                                                                                                              begin (
                                                                                                                set! i1 (
                                                                                                                  + i1 1
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
                                                                                                           (
                                                                                                            list-set! alphas i1 (
                                                                                                              _add alpha1_old (
                                                                                                                * (
                                                                                                                  * (
                                                                                                                    list-ref labels i1
                                                                                                                  )
                                                                                                                   (
                                                                                                                    list-ref labels i2
                                                                                                                  )
                                                                                                                )
                                                                                                                 (
                                                                                                                  - alpha2_old (
                                                                                                                    list-ref alphas i2
                                                                                                                  )
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                           (
                                                                                                            let (
                                                                                                              (
                                                                                                                b1 (
                                                                                                                  - (
                                                                                                                    - (
                                                                                                                      - b Ei
                                                                                                                    )
                                                                                                                     (
                                                                                                                      * (
                                                                                                                        * (
                                                                                                                          list-ref labels i1
                                                                                                                        )
                                                                                                                         (
                                                                                                                          - (
                                                                                                                            list-ref alphas i1
                                                                                                                          )
                                                                                                                           alpha1_old
                                                                                                                        )
                                                                                                                      )
                                                                                                                       (
                                                                                                                        dot (
                                                                                                                          list-ref samples i1
                                                                                                                        )
                                                                                                                         (
                                                                                                                          list-ref samples i1
                                                                                                                        )
                                                                                                                      )
                                                                                                                    )
                                                                                                                  )
                                                                                                                   (
                                                                                                                    * (
                                                                                                                      * (
                                                                                                                        list-ref labels i2
                                                                                                                      )
                                                                                                                       (
                                                                                                                        - (
                                                                                                                          list-ref alphas i2
                                                                                                                        )
                                                                                                                         alpha2_old
                                                                                                                      )
                                                                                                                    )
                                                                                                                     (
                                                                                                                      dot (
                                                                                                                        list-ref samples i1
                                                                                                                      )
                                                                                                                       (
                                                                                                                        list-ref samples i2
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
                                                                                                                    b2 (
                                                                                                                      - (
                                                                                                                        - (
                                                                                                                          - b Ej
                                                                                                                        )
                                                                                                                         (
                                                                                                                          * (
                                                                                                                            * (
                                                                                                                              list-ref labels i1
                                                                                                                            )
                                                                                                                             (
                                                                                                                              - (
                                                                                                                                list-ref alphas i1
                                                                                                                              )
                                                                                                                               alpha1_old
                                                                                                                            )
                                                                                                                          )
                                                                                                                           (
                                                                                                                            dot (
                                                                                                                              list-ref samples i1
                                                                                                                            )
                                                                                                                             (
                                                                                                                              list-ref samples i2
                                                                                                                            )
                                                                                                                          )
                                                                                                                        )
                                                                                                                      )
                                                                                                                       (
                                                                                                                        * (
                                                                                                                          * (
                                                                                                                            list-ref labels i2
                                                                                                                          )
                                                                                                                           (
                                                                                                                            - (
                                                                                                                              list-ref alphas i2
                                                                                                                            )
                                                                                                                             alpha2_old
                                                                                                                          )
                                                                                                                        )
                                                                                                                         (
                                                                                                                          dot (
                                                                                                                            list-ref samples i2
                                                                                                                          )
                                                                                                                           (
                                                                                                                            list-ref samples i2
                                                                                                                          )
                                                                                                                        )
                                                                                                                      )
                                                                                                                    )
                                                                                                                  )
                                                                                                                )
                                                                                                                 (
                                                                                                                  begin (
                                                                                                                    if (
                                                                                                                      and (
                                                                                                                        > (
                                                                                                                          list-ref alphas i1
                                                                                                                        )
                                                                                                                         0.0
                                                                                                                      )
                                                                                                                       (
                                                                                                                        < (
                                                                                                                          list-ref alphas i1
                                                                                                                        )
                                                                                                                         c
                                                                                                                      )
                                                                                                                    )
                                                                                                                     (
                                                                                                                      begin (
                                                                                                                        set! b b1
                                                                                                                      )
                                                                                                                    )
                                                                                                                     (
                                                                                                                      if (
                                                                                                                        and (
                                                                                                                          > (
                                                                                                                            list-ref alphas i2
                                                                                                                          )
                                                                                                                           0.0
                                                                                                                        )
                                                                                                                         (
                                                                                                                          < (
                                                                                                                            list-ref alphas i2
                                                                                                                          )
                                                                                                                           c
                                                                                                                        )
                                                                                                                      )
                                                                                                                       (
                                                                                                                        begin (
                                                                                                                          set! b b2
                                                                                                                        )
                                                                                                                      )
                                                                                                                       (
                                                                                                                        begin (
                                                                                                                          set! b (
                                                                                                                            _div (
                                                                                                                              _add b1 b2
                                                                                                                            )
                                                                                                                             2.0
                                                                                                                          )
                                                                                                                        )
                                                                                                                      )
                                                                                                                    )
                                                                                                                  )
                                                                                                                   (
                                                                                                                    set! num_changed (
                                                                                                                      + num_changed 1
                                                                                                                    )
                                                                                                                  )
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                              )
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
                                                                            set! i1 (
                                                                              + i1 1
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
                                                        if (
                                                          equal? num_changed 0
                                                        )
                                                         (
                                                          begin (
                                                            set! passes (
                                                              + passes 1
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            set! passes 0
                                                          )
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
                                ret10 (
                                  _list alphas (
                                    _list b
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
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
        predict samples labels model x
      )
       (
        call/cc (
          lambda (
            ret17
          )
           (
            let (
              (
                alphas (
                  list-ref model 0
                )
              )
            )
             (
              begin (
                let (
                  (
                    b (
                      cond (
                        (
                          string? (
                            list-ref model 1
                          )
                        )
                         (
                          _substring (
                            list-ref model 1
                          )
                           0 (
                            + 0 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? (
                            list-ref model 1
                          )
                        )
                         (
                          hash-table-ref (
                            list-ref model 1
                          )
                           0
                        )
                      )
                       (
                        else (
                          list-ref (
                            list-ref model 1
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
                        val (
                          predict_raw samples labels alphas b x
                        )
                      )
                    )
                     (
                      begin (
                        if (
                          _ge val 0.0
                        )
                         (
                          begin (
                            ret17 1.0
                          )
                        )
                         (
                          quote (
                            
                          )
                        )
                      )
                       (
                        ret17 (
                          - 1.0
                        )
                      )
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
          samples (
            _list (
              _list 2.0 2.0
            )
             (
              _list 1.5 1.5
            )
             (
              _list 0.0 0.0
            )
             (
              _list 0.5 0.0
            )
          )
        )
      )
       (
        begin (
          let (
            (
              labels (
                _list 1.0 1.0 (
                  - 1.0
                )
                 (
                  - 1.0
                )
              )
            )
          )
           (
            begin (
              let (
                (
                  model (
                    smo_train samples labels 1.0 0.001 10
                  )
                )
              )
               (
                begin (
                  _display (
                    if (
                      string? (
                        predict samples labels model (
                          _list 1.5 1.0
                        )
                      )
                    )
                     (
                      predict samples labels model (
                        _list 1.5 1.0
                      )
                    )
                     (
                      to-str (
                        predict samples labels model (
                          _list 1.5 1.0
                        )
                      )
                    )
                  )
                )
                 (
                  newline
                )
                 (
                  _display (
                    if (
                      string? (
                        predict samples labels model (
                          _list 0.2 0.1
                        )
                      )
                    )
                     (
                      predict samples labels model (
                        _list 0.2 0.1
                      )
                    )
                     (
                      to-str (
                        predict samples labels model (
                          _list 0.2 0.1
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
      )
    )
     (
      let (
        (
          end19 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur20 (
              quotient (
                * (
                  - end19 start18
                )
                 1000000
              )
               jps21
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur20
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
