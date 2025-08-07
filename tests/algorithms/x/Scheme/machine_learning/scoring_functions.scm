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
      start30 (
        current-jiffy
      )
    )
     (
      jps33 (
        jiffies-per-second
      )
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
                    - 0.0 x
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
        ln_series x
      )
       (
        call/cc (
          lambda (
            ret5
          )
           (
            let (
              (
                t (
                  _div (
                    - x 1.0
                  )
                   (
                    + x 1.0
                  )
                )
              )
            )
             (
              begin (
                let (
                  (
                    term t
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
                                          <= n 19
                                        )
                                         (
                                          begin (
                                            set! sum (
                                              _add sum (
                                                _div term (
                                                  + 0.0 n
                                                )
                                              )
                                            )
                                          )
                                           (
                                            set! term (
                                              * (
                                                * term t
                                              )
                                               t
                                            )
                                          )
                                           (
                                            set! n (
                                              + n 2
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
                            ret5 (
                              * 2.0 sum
                            )
                          )
                        )
                      )
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
        ln x
      )
       (
        call/cc (
          lambda (
            ret8
          )
           (
            let (
              (
                y x
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
                                  >= y 10.0
                                )
                                 (
                                  begin (
                                    set! y (
                                      _div y 10.0
                                    )
                                  )
                                   (
                                    set! k (
                                      + k 1
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
                                  < y 1.0
                                )
                                 (
                                  begin (
                                    set! y (
                                      * y 10.0
                                    )
                                  )
                                   (
                                    set! k (
                                      - k 1
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
                    ret8 (
                      _add (
                        ln_series y
                      )
                       (
                        * (
                          + 0.0 k
                        )
                         (
                          ln_series 10.0
                        )
                      )
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
        mae predict actual
      )
       (
        call/cc (
          lambda (
            ret13
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
                                  < i (
                                    _len predict
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        diff (
                                          - (
                                            list-ref predict i
                                          )
                                           (
                                            list-ref actual i
                                          )
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        set! sum (
                                          _add sum (
                                            absf diff
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
                      _div sum (
                        + 0.0 (
                          _len predict
                        )
                      )
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
        mse predict actual
      )
       (
        call/cc (
          lambda (
            ret16
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
                                    _len predict
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        diff (
                                          - (
                                            list-ref predict i
                                          )
                                           (
                                            list-ref actual i
                                          )
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        set! sum (
                                          _add sum (
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
                    ret16 (
                      _div sum (
                        + 0.0 (
                          _len predict
                        )
                      )
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
        rmse predict actual
      )
       (
        call/cc (
          lambda (
            ret19
          )
           (
            ret19 (
              sqrtApprox (
                mse predict actual
              )
            )
          )
        )
      )
    )
     (
      define (
        rmsle predict actual
      )
       (
        call/cc (
          lambda (
            ret20
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
                                  < i (
                                    _len predict
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        lp (
                                          ln (
                                            + (
                                              list-ref predict i
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
                                            la (
                                              ln (
                                                + (
                                                  list-ref actual i
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
                                                diff (
                                                  - lp la
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                set! sum (
                                                  _add sum (
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
                    ret20 (
                      sqrtApprox (
                        _div sum (
                          + 0.0 (
                            _len predict
                          )
                        )
                      )
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
        mbd predict actual
      )
       (
        call/cc (
          lambda (
            ret23
          )
           (
            let (
              (
                diff_sum 0.0
              )
            )
             (
              begin (
                let (
                  (
                    actual_sum 0.0
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
                                        _len predict
                                      )
                                    )
                                     (
                                      begin (
                                        set! diff_sum (
                                          + diff_sum (
                                            - (
                                              list-ref predict i
                                            )
                                             (
                                              list-ref actual i
                                            )
                                          )
                                        )
                                      )
                                       (
                                        set! actual_sum (
                                          + actual_sum (
                                            list-ref actual i
                                          )
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
                        let (
                          (
                            n (
                              + 0.0 (
                                _len predict
                              )
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                numerator (
                                  _div diff_sum n
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    denominator (
                                      _div actual_sum n
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    ret23 (
                                      * (
                                        _div numerator denominator
                                      )
                                       100.0
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
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
        manual_accuracy predict actual
      )
       (
        call/cc (
          lambda (
            ret26
          )
           (
            let (
              (
                correct 0
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
                                  < i (
                                    _len predict
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      equal? (
                                        list-ref predict i
                                      )
                                       (
                                        list-ref actual i
                                      )
                                    )
                                     (
                                      begin (
                                        set! correct (
                                          + correct 1
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
                    ret26 (
                      _div (
                        + 0.0 correct
                      )
                       (
                        + 0.0 (
                          _len predict
                        )
                      )
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
            ret29
          )
           (
            let (
              (
                actual (
                  _list 1.0 2.0 3.0
                )
              )
            )
             (
              begin (
                let (
                  (
                    predict (
                      _list 1.0 4.0 3.0
                    )
                  )
                )
                 (
                  begin (
                    _display (
                      if (
                        string? (
                          to-str-space (
                            mae predict actual
                          )
                        )
                      )
                       (
                        to-str-space (
                          mae predict actual
                        )
                      )
                       (
                        to-str (
                          to-str-space (
                            mae predict actual
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
                            mse predict actual
                          )
                        )
                      )
                       (
                        to-str-space (
                          mse predict actual
                        )
                      )
                       (
                        to-str (
                          to-str-space (
                            mse predict actual
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
                            rmse predict actual
                          )
                        )
                      )
                       (
                        to-str-space (
                          rmse predict actual
                        )
                      )
                       (
                        to-str (
                          to-str-space (
                            rmse predict actual
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
                            rmsle (
                              _list 10.0 2.0 30.0
                            )
                             (
                              _list 10.0 10.0 30.0
                            )
                          )
                        )
                      )
                       (
                        to-str-space (
                          rmsle (
                            _list 10.0 2.0 30.0
                          )
                           (
                            _list 10.0 10.0 30.0
                          )
                        )
                      )
                       (
                        to-str (
                          to-str-space (
                            rmsle (
                              _list 10.0 2.0 30.0
                            )
                             (
                              _list 10.0 10.0 30.0
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
                    _display (
                      if (
                        string? (
                          to-str-space (
                            mbd (
                              _list 2.0 3.0 4.0
                            )
                             (
                              _list 1.0 2.0 3.0
                            )
                          )
                        )
                      )
                       (
                        to-str-space (
                          mbd (
                            _list 2.0 3.0 4.0
                          )
                           (
                            _list 1.0 2.0 3.0
                          )
                        )
                      )
                       (
                        to-str (
                          to-str-space (
                            mbd (
                              _list 2.0 3.0 4.0
                            )
                             (
                              _list 1.0 2.0 3.0
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
                    _display (
                      if (
                        string? (
                          to-str-space (
                            mbd (
                              _list 0.0 1.0 1.0
                            )
                             (
                              _list 1.0 2.0 3.0
                            )
                          )
                        )
                      )
                       (
                        to-str-space (
                          mbd (
                            _list 0.0 1.0 1.0
                          )
                           (
                            _list 1.0 2.0 3.0
                          )
                        )
                      )
                       (
                        to-str (
                          to-str-space (
                            mbd (
                              _list 0.0 1.0 1.0
                            )
                             (
                              _list 1.0 2.0 3.0
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
                    _display (
                      if (
                        string? (
                          to-str-space (
                            manual_accuracy predict actual
                          )
                        )
                      )
                       (
                        to-str-space (
                          manual_accuracy predict actual
                        )
                      )
                       (
                        to-str (
                          to-str-space (
                            manual_accuracy predict actual
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
    )
     (
      main
    )
     (
      let (
        (
          end31 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur32 (
              quotient (
                * (
                  - end31 start30
                )
                 1000000
              )
               jps33
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur32
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
