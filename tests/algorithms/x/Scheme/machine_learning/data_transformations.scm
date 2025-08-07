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
      define (
        floor x
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                i (
                  let (
                    (
                      v2 x
                    )
                  )
                   (
                    cond (
                      (
                        string? v2
                      )
                       (
                        inexact->exact (
                          floor (
                            string->number v2
                          )
                        )
                      )
                    )
                     (
                      (
                        boolean? v2
                      )
                       (
                        if v2 1 0
                      )
                    )
                     (
                      else (
                        inexact->exact (
                          floor v2
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
                 (
                  quote (
                    
                  )
                )
              )
               (
                ret1 (
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
            ret3
          )
           (
            let (
              (
                result 1.0
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
                                  < i n
                                )
                                 (
                                  begin (
                                    set! result (
                                      * result 10.0
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
                    ret3 result
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
        round x n
      )
       (
        call/cc (
          lambda (
            ret6
          )
           (
            let (
              (
                m (
                  pow10 n
                )
              )
            )
             (
              begin (
                let (
                  (
                    y (
                      + 0.0 (
                        floor (
                          _add (
                            * x m
                          )
                           0.5
                        )
                      )
                    )
                  )
                )
                 (
                  begin (
                    ret6 (
                      _div y m
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
            ret7
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
                    ret7 guess
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
        mean data
      )
       (
        call/cc (
          lambda (
            ret10
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
                    let (
                      (
                        n (
                          _len data
                        )
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
                                      < i n
                                    )
                                     (
                                      begin (
                                        set! total (
                                          + total (
                                            list-ref data i
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
                        ret10 (
                          _div total (
                            + 0.0 n
                          )
                        )
                      )
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
        stdev data
      )
       (
        call/cc (
          lambda (
            ret13
          )
           (
            let (
              (
                n (
                  _len data
                )
              )
            )
             (
              begin (
                if (
                  <= n 1
                )
                 (
                  begin (
                    panic "data length must be > 1"
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
                    m (
                      mean data
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        sum_sq 0.0
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
                                            let (
                                              (
                                                diff (
                                                  - (
                                                    list-ref data i
                                                  )
                                                   m
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                set! sum_sq (
                                                  _add sum_sq (
                                                    * diff diff
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
                            ret13 (
                              sqrtApprox (
                                _div sum_sq (
                                  + 0.0 (
                                    - n 1
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
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
        normalization data ndigits
      )
       (
        call/cc (
          lambda (
            ret16
          )
           (
            let (
              (
                x_min (
                  + 0.0 (
                    apply min data
                  )
                )
              )
            )
             (
              begin (
                let (
                  (
                    x_max (
                      + 0.0 (
                        apply max data
                      )
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        denom (
                          - x_max x_min
                        )
                      )
                    )
                     (
                      begin (
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
                                let (
                                  (
                                    n (
                                      _len data
                                    )
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
                                                  < i n
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        norm (
                                                          _div (
                                                            - (
                                                              list-ref data i
                                                            )
                                                             x_min
                                                          )
                                                           denom
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        set! result (
                                                          append result (
                                                            _list (
                                                              round norm ndigits
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
                                    ret16 result
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
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
        standardization data ndigits
      )
       (
        call/cc (
          lambda (
            ret19
          )
           (
            let (
              (
                mu (
                  mean data
                )
              )
            )
             (
              begin (
                let (
                  (
                    sigma (
                      stdev data
                    )
                  )
                )
                 (
                  begin (
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
                            let (
                              (
                                n (
                                  _len data
                                )
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
                                              < i n
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    z (
                                                      _div (
                                                        - (
                                                          list-ref data i
                                                        )
                                                         mu
                                                      )
                                                       sigma
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    set! result (
                                                      append result (
                                                        _list (
                                                          round z ndigits
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
                                ret19 result
                              )
                            )
                          )
                        )
                      )
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
      _display (
        if (
          string? (
            to-str-space (
              normalization (
                _list 2.0 7.0 10.0 20.0 30.0 50.0
              )
               3
            )
          )
        )
         (
          to-str-space (
            normalization (
              _list 2.0 7.0 10.0 20.0 30.0 50.0
            )
             3
          )
        )
         (
          to-str (
            to-str-space (
              normalization (
                _list 2.0 7.0 10.0 20.0 30.0 50.0
              )
               3
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
            to-str-space (
              normalization (
                _list 5.0 10.0 15.0 20.0 25.0
              )
               3
            )
          )
        )
         (
          to-str-space (
            normalization (
              _list 5.0 10.0 15.0 20.0 25.0
            )
             3
          )
        )
         (
          to-str (
            to-str-space (
              normalization (
                _list 5.0 10.0 15.0 20.0 25.0
              )
               3
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
            to-str-space (
              standardization (
                _list 2.0 7.0 10.0 20.0 30.0 50.0
              )
               3
            )
          )
        )
         (
          to-str-space (
            standardization (
              _list 2.0 7.0 10.0 20.0 30.0 50.0
            )
             3
          )
        )
         (
          to-str (
            to-str-space (
              standardization (
                _list 2.0 7.0 10.0 20.0 30.0 50.0
              )
               3
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
            to-str-space (
              standardization (
                _list 5.0 10.0 15.0 20.0 25.0
              )
               3
            )
          )
        )
         (
          to-str-space (
            standardization (
              _list 5.0 10.0 15.0 20.0 25.0
            )
             3
          )
        )
         (
          to-str (
            to-str-space (
              standardization (
                _list 5.0 10.0 15.0 20.0 25.0
              )
               3
            )
          )
        )
      )
    )
     (
      newline
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
