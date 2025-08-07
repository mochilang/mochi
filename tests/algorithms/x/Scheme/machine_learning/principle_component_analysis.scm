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
      start48 (
        current-jiffy
      )
    )
     (
      jps51 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        sqrt x
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
                  if (
                    > x 1.0
                  )
                   (
                    _div x 2.0
                  )
                   1.0
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
                                      * 0.5 (
                                        _add guess (
                                          _div x guess
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
        mean xs
      )
       (
        call/cc (
          lambda (
            ret4
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
                                  < i (
                                    _len xs
                                  )
                                )
                                 (
                                  begin (
                                    set! sum (
                                      + sum (
                                        list-ref xs i
                                      )
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
                   (
                    ret4 (
                      _div sum (
                        _len xs
                      )
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
        standardize data
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            let (
              (
                n_samples (
                  _len data
                )
              )
            )
             (
              begin (
                let (
                  (
                    n_features (
                      _len (
                        list-ref data 0
                      )
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        means (
                          _list
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            stds (
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
                                              < j n_features
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    column (
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
                                                                      < i n_samples
                                                                    )
                                                                     (
                                                                      begin (
                                                                        set! column (
                                                                          append column (
                                                                            _list (
                                                                              cond (
                                                                                (
                                                                                  string? (
                                                                                    list-ref data i
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  _substring (
                                                                                    list-ref data i
                                                                                  )
                                                                                   j (
                                                                                    + j 1
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                (
                                                                                  hash-table? (
                                                                                    list-ref data i
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  hash-table-ref (
                                                                                    list-ref data i
                                                                                  )
                                                                                   j
                                                                                )
                                                                              )
                                                                               (
                                                                                else (
                                                                                  list-ref (
                                                                                    list-ref data i
                                                                                  )
                                                                                   j
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
                                                        let (
                                                          (
                                                            m (
                                                              mean column
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            set! means (
                                                              append means (
                                                                _list m
                                                              )
                                                            )
                                                          )
                                                           (
                                                            let (
                                                              (
                                                                variance 0.0
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
                                                                                  < k n_samples
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    let (
                                                                                      (
                                                                                        diff (
                                                                                          - (
                                                                                            list-ref column k
                                                                                          )
                                                                                           m
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        set! variance (
                                                                                          _add variance (
                                                                                            * diff diff
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
                                                                    set! stds (
                                                                      append stds (
                                                                        _list (
                                                                          sqrt (
                                                                            _div variance (
                                                                              - n_samples 1
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
                                let (
                                  (
                                    standardized (
                                      _list
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        r 0
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
                                                      < r n_samples
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
                                                                c 0
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
                                                                              < c n_features
                                                                            )
                                                                             (
                                                                              begin (
                                                                                set! row (
                                                                                  append row (
                                                                                    _list (
                                                                                      _div (
                                                                                        - (
                                                                                          cond (
                                                                                            (
                                                                                              string? (
                                                                                                list-ref data r
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              _substring (
                                                                                                list-ref data r
                                                                                              )
                                                                                               c (
                                                                                                + c 1
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            (
                                                                                              hash-table? (
                                                                                                list-ref data r
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              hash-table-ref (
                                                                                                list-ref data r
                                                                                              )
                                                                                               c
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            else (
                                                                                              list-ref (
                                                                                                list-ref data r
                                                                                              )
                                                                                               c
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          list-ref means c
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        list-ref stds c
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                set! c (
                                                                                  + c 1
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
                                                                set! standardized (
                                                                  append standardized (
                                                                    _list row
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
                                        ret7 standardized
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
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
        covariance_matrix data
      )
       (
        call/cc (
          lambda (
            ret18
          )
           (
            let (
              (
                n_samples (
                  _len data
                )
              )
            )
             (
              begin (
                let (
                  (
                    n_features (
                      _len (
                        list-ref data 0
                      )
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        cov (
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
                                          < i n_features
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
                                                                  < j n_features
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
                                                                            k 0
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
                                                                                          < k n_samples
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            set! sum (
                                                                                              _add sum (
                                                                                                * (
                                                                                                  cond (
                                                                                                    (
                                                                                                      string? (
                                                                                                        list-ref data k
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      _substring (
                                                                                                        list-ref data k
                                                                                                      )
                                                                                                       i (
                                                                                                        + i 1
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    (
                                                                                                      hash-table? (
                                                                                                        list-ref data k
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      hash-table-ref (
                                                                                                        list-ref data k
                                                                                                      )
                                                                                                       i
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    else (
                                                                                                      list-ref (
                                                                                                        list-ref data k
                                                                                                      )
                                                                                                       i
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  cond (
                                                                                                    (
                                                                                                      string? (
                                                                                                        list-ref data k
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      _substring (
                                                                                                        list-ref data k
                                                                                                      )
                                                                                                       j (
                                                                                                        + j 1
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    (
                                                                                                      hash-table? (
                                                                                                        list-ref data k
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      hash-table-ref (
                                                                                                        list-ref data k
                                                                                                      )
                                                                                                       j
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    else (
                                                                                                      list-ref (
                                                                                                        list-ref data k
                                                                                                      )
                                                                                                       j
                                                                                                    )
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
                                                                            set! row (
                                                                              append row (
                                                                                _list (
                                                                                  _div sum (
                                                                                    - n_samples 1
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
                                                    set! cov (
                                                      append cov (
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
                            ret18 cov
                          )
                        )
                      )
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
        normalize vec
      )
       (
        call/cc (
          lambda (
            ret25
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
                                  < i (
                                    _len vec
                                  )
                                )
                                 (
                                  begin (
                                    set! sum (
                                      _add sum (
                                        * (
                                          list-ref vec i
                                        )
                                         (
                                          list-ref vec i
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
                    let (
                      (
                        n (
                          sqrt sum
                        )
                      )
                    )
                     (
                      begin (
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
                                j 0
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
                                              < j (
                                                _len vec
                                              )
                                            )
                                             (
                                              begin (
                                                set! res (
                                                  append res (
                                                    _list (
                                                      _div (
                                                        list-ref vec j
                                                      )
                                                       n
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
                                ret25 res
                              )
                            )
                          )
                        )
                      )
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
        eigen_decomposition_2x2 matrix
      )
       (
        call/cc (
          lambda (
            ret30
          )
           (
            let (
              (
                a (
                  cond (
                    (
                      string? (
                        list-ref matrix 0
                      )
                    )
                     (
                      _substring (
                        list-ref matrix 0
                      )
                       0 (
                        + 0 1
                      )
                    )
                  )
                   (
                    (
                      hash-table? (
                        list-ref matrix 0
                      )
                    )
                     (
                      hash-table-ref (
                        list-ref matrix 0
                      )
                       0
                    )
                  )
                   (
                    else (
                      list-ref (
                        list-ref matrix 0
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
                    b (
                      cond (
                        (
                          string? (
                            list-ref matrix 0
                          )
                        )
                         (
                          _substring (
                            list-ref matrix 0
                          )
                           1 (
                            + 1 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? (
                            list-ref matrix 0
                          )
                        )
                         (
                          hash-table-ref (
                            list-ref matrix 0
                          )
                           1
                        )
                      )
                       (
                        else (
                          list-ref (
                            list-ref matrix 0
                          )
                           1
                        )
                      )
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        c (
                          cond (
                            (
                              string? (
                                list-ref matrix 1
                              )
                            )
                             (
                              _substring (
                                list-ref matrix 1
                              )
                               1 (
                                + 1 1
                              )
                            )
                          )
                           (
                            (
                              hash-table? (
                                list-ref matrix 1
                              )
                            )
                             (
                              hash-table-ref (
                                list-ref matrix 1
                              )
                               1
                            )
                          )
                           (
                            else (
                              list-ref (
                                list-ref matrix 1
                              )
                               1
                            )
                          )
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            diff (
                              - a c
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                discriminant (
                                  sqrt (
                                    _add (
                                      * diff diff
                                    )
                                     (
                                      * (
                                        * 4.0 b
                                      )
                                       b
                                    )
                                  )
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    lambda1 (
                                      _div (
                                        _add (
                                          + a c
                                        )
                                         discriminant
                                      )
                                       2.0
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        lambda2 (
                                          _div (
                                            - (
                                              + a c
                                            )
                                             discriminant
                                          )
                                           2.0
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            v1 (
                                              quote (
                                                
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                v2 (
                                                  quote (
                                                    
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                if (
                                                  not (
                                                    equal? b 0.0
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    set! v1 (
                                                      normalize (
                                                        _list (
                                                          - lambda1 c
                                                        )
                                                         b
                                                      )
                                                    )
                                                  )
                                                   (
                                                    set! v2 (
                                                      normalize (
                                                        _list (
                                                          - lambda2 c
                                                        )
                                                         b
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    set! v1 (
                                                      _list 1.0 0.0
                                                    )
                                                  )
                                                   (
                                                    set! v2 (
                                                      _list 0.0 1.0
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                let (
                                                  (
                                                    eigenvalues (
                                                      _list lambda1 lambda2
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        eigenvectors (
                                                          _list v1 v2
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        if (
                                                          < (
                                                            list-ref eigenvalues 0
                                                          )
                                                           (
                                                            list-ref eigenvalues 1
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                tmp_val (
                                                                  list-ref eigenvalues 0
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                list-set! eigenvalues 0 (
                                                                  list-ref eigenvalues 1
                                                                )
                                                              )
                                                               (
                                                                list-set! eigenvalues 1 tmp_val
                                                              )
                                                               (
                                                                let (
                                                                  (
                                                                    tmp_vec (
                                                                      list-ref eigenvectors 0
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    list-set! eigenvectors 0 (
                                                                      list-ref eigenvectors 1
                                                                    )
                                                                  )
                                                                   (
                                                                    list-set! eigenvectors 1 tmp_vec
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
                                                        ret30 (
                                                          alist->hash-table (
                                                            _list (
                                                              cons "values" eigenvalues
                                                            )
                                                             (
                                                              cons "vectors" eigenvectors
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
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
        transpose matrix
      )
       (
        call/cc (
          lambda (
            ret31
          )
           (
            let (
              (
                rows (
                  _len matrix
                )
              )
            )
             (
              begin (
                let (
                  (
                    cols (
                      _len (
                        list-ref matrix 0
                      )
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        trans (
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
                                          < i cols
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
                                                                  < j rows
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! row (
                                                                      append row (
                                                                        _list (
                                                                          cond (
                                                                            (
                                                                              string? (
                                                                                list-ref matrix j
                                                                              )
                                                                            )
                                                                             (
                                                                              _substring (
                                                                                list-ref matrix j
                                                                              )
                                                                               i (
                                                                                + i 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            (
                                                                              hash-table? (
                                                                                list-ref matrix j
                                                                              )
                                                                            )
                                                                             (
                                                                              hash-table-ref (
                                                                                list-ref matrix j
                                                                              )
                                                                               i
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref (
                                                                                list-ref matrix j
                                                                              )
                                                                               i
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
                                                    set! trans (
                                                      append trans (
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
                            ret31 trans
                          )
                        )
                      )
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
        matrix_multiply a b
      )
       (
        call/cc (
          lambda (
            ret36
          )
           (
            let (
              (
                rows_a (
                  _len a
                )
              )
            )
             (
              begin (
                let (
                  (
                    cols_a (
                      _len (
                        list-ref a 0
                      )
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        rows_b (
                          _len b
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            cols_b (
                              _len (
                                list-ref b 0
                              )
                            )
                          )
                        )
                         (
                          begin (
                            if (
                              not (
                                equal? cols_a rows_b
                              )
                            )
                             (
                              begin (
                                panic "Incompatible matrices"
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
                                result (
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
                                                  < i rows_a
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
                                                                          < j cols_b
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
                                                                                    k 0
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
                                                                                                  < k cols_a
                                                                                                )
                                                                                                 (
                                                                                                  begin (
                                                                                                    set! sum (
                                                                                                      _add sum (
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
                                                                                                               k (
                                                                                                                + k 1
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
                                                                                                               k
                                                                                                            )
                                                                                                          )
                                                                                                           (
                                                                                                            else (
                                                                                                              list-ref (
                                                                                                                list-ref a i
                                                                                                              )
                                                                                                               k
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          cond (
                                                                                                            (
                                                                                                              string? (
                                                                                                                list-ref b k
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              _substring (
                                                                                                                list-ref b k
                                                                                                              )
                                                                                                               j (
                                                                                                                + j 1
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                           (
                                                                                                            (
                                                                                                              hash-table? (
                                                                                                                list-ref b k
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              hash-table-ref (
                                                                                                                list-ref b k
                                                                                                              )
                                                                                                               j
                                                                                                            )
                                                                                                          )
                                                                                                           (
                                                                                                            else (
                                                                                                              list-ref (
                                                                                                                list-ref b k
                                                                                                              )
                                                                                                               j
                                                                                                            )
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
                                                                                    set! row (
                                                                                      append row (
                                                                                        _list sum
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
                                                            set! result (
                                                              append result (
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
                                    ret36 result
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
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
        apply_pca data n_components
      )
       (
        call/cc (
          lambda (
            ret43
          )
           (
            let (
              (
                standardized (
                  standardize data
                )
              )
            )
             (
              begin (
                let (
                  (
                    cov (
                      covariance_matrix standardized
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        eig (
                          eigen_decomposition_2x2 cov
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            eigenvalues (
                              hash-table-ref eig "values"
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                eigenvectors (
                                  hash-table-ref eig "vectors"
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    components (
                                      transpose eigenvectors
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        transformed (
                                          matrix_multiply standardized components
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            total (
                                              _add (
                                                cond (
                                                  (
                                                    string? eigenvalues
                                                  )
                                                   (
                                                    _substring eigenvalues 0 (
                                                      + 0 1
                                                    )
                                                  )
                                                )
                                                 (
                                                  (
                                                    hash-table? eigenvalues
                                                  )
                                                   (
                                                    hash-table-ref eigenvalues 0
                                                  )
                                                )
                                                 (
                                                  else (
                                                    list-ref eigenvalues 0
                                                  )
                                                )
                                              )
                                               (
                                                cond (
                                                  (
                                                    string? eigenvalues
                                                  )
                                                   (
                                                    _substring eigenvalues 1 (
                                                      + 1 1
                                                    )
                                                  )
                                                )
                                                 (
                                                  (
                                                    hash-table? eigenvalues
                                                  )
                                                   (
                                                    hash-table-ref eigenvalues 1
                                                  )
                                                )
                                                 (
                                                  else (
                                                    list-ref eigenvalues 1
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
                                                ratios (
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
                                                                  < i n_components
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! ratios (
                                                                      append ratios (
                                                                        _list (
                                                                          _div (
                                                                            cond (
                                                                              (
                                                                                string? eigenvalues
                                                                              )
                                                                               (
                                                                                _substring eigenvalues i (
                                                                                  + i 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? eigenvalues
                                                                              )
                                                                               (
                                                                                hash-table-ref eigenvalues i
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref eigenvalues i
                                                                              )
                                                                            )
                                                                          )
                                                                           total
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
                                                    ret43 (
                                                      alist->hash-table (
                                                        _list (
                                                          cons "transformed" transformed
                                                        )
                                                         (
                                                          cons "variance_ratio" ratios
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
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
          data (
            _list (
              _list 2.5 2.4
            )
             (
              _list 0.5 0.7
            )
             (
              _list 2.2 2.9
            )
             (
              _list 1.9 2.2
            )
             (
              _list 3.1 3.0
            )
             (
              _list 2.3 2.7
            )
             (
              _list 2.0 1.6
            )
             (
              _list 1.0 1.1
            )
             (
              _list 1.5 1.6
            )
             (
              _list 1.1 0.9
            )
          )
        )
      )
       (
        begin (
          let (
            (
              result (
                apply_pca data 2
              )
            )
          )
           (
            begin (
              _display (
                if (
                  string? "Transformed Data (first 5 rows):"
                )
                 "Transformed Data (first 5 rows):" (
                  to-str "Transformed Data (first 5 rows):"
                )
              )
            )
             (
              newline
            )
             (
              let (
                (
                  idx 0
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
                                < idx 5
                              )
                               (
                                begin (
                                  _display (
                                    if (
                                      string? (
                                        cond (
                                          (
                                            string? (
                                              hash-table-ref result "transformed"
                                            )
                                          )
                                           (
                                            _substring (
                                              hash-table-ref result "transformed"
                                            )
                                             idx (
                                              + idx 1
                                            )
                                          )
                                        )
                                         (
                                          (
                                            hash-table? (
                                              hash-table-ref result "transformed"
                                            )
                                          )
                                           (
                                            hash-table-ref (
                                              hash-table-ref result "transformed"
                                            )
                                             idx
                                          )
                                        )
                                         (
                                          else (
                                            list-ref (
                                              hash-table-ref result "transformed"
                                            )
                                             idx
                                          )
                                        )
                                      )
                                    )
                                     (
                                      cond (
                                        (
                                          string? (
                                            hash-table-ref result "transformed"
                                          )
                                        )
                                         (
                                          _substring (
                                            hash-table-ref result "transformed"
                                          )
                                           idx (
                                            + idx 1
                                          )
                                        )
                                      )
                                       (
                                        (
                                          hash-table? (
                                            hash-table-ref result "transformed"
                                          )
                                        )
                                         (
                                          hash-table-ref (
                                            hash-table-ref result "transformed"
                                          )
                                           idx
                                        )
                                      )
                                       (
                                        else (
                                          list-ref (
                                            hash-table-ref result "transformed"
                                          )
                                           idx
                                        )
                                      )
                                    )
                                     (
                                      to-str (
                                        cond (
                                          (
                                            string? (
                                              hash-table-ref result "transformed"
                                            )
                                          )
                                           (
                                            _substring (
                                              hash-table-ref result "transformed"
                                            )
                                             idx (
                                              + idx 1
                                            )
                                          )
                                        )
                                         (
                                          (
                                            hash-table? (
                                              hash-table-ref result "transformed"
                                            )
                                          )
                                           (
                                            hash-table-ref (
                                              hash-table-ref result "transformed"
                                            )
                                             idx
                                          )
                                        )
                                         (
                                          else (
                                            list-ref (
                                              hash-table-ref result "transformed"
                                            )
                                             idx
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                                 (
                                  newline
                                )
                                 (
                                  set! idx (
                                    + idx 1
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
                      string? "Explained Variance Ratio:"
                    )
                     "Explained Variance Ratio:" (
                      to-str "Explained Variance Ratio:"
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
                        hash-table-ref result "variance_ratio"
                      )
                    )
                     (
                      hash-table-ref result "variance_ratio"
                    )
                     (
                      to-str (
                        hash-table-ref result "variance_ratio"
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
          end49 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur50 (
              quotient (
                * (
                  - end49 start48
                )
                 1000000
              )
               jps51
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur50
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
