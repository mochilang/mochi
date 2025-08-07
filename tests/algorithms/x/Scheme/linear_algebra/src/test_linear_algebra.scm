;; Generated on 2025-08-07 14:57 +0700
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
      start90 (
        current-jiffy
      )
    )
     (
      jps93 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        int_to_string n
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            begin (
              if (
                equal? n 0
              )
               (
                begin (
                  ret1 "0"
                )
              )
               '(
                
              )
            )
             (
              let (
                (
                  num n
                )
              )
               (
                begin (
                  let (
                    (
                      neg #f
                    )
                  )
                   (
                    begin (
                      if (
                        < num 0
                      )
                       (
                        begin (
                          set! neg #t
                        )
                         (
                          set! num (
                            - num
                          )
                        )
                      )
                       '(
                        
                      )
                    )
                     (
                      let (
                        (
                          res ""
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
                                        > num 0
                                      )
                                       (
                                        begin (
                                          let (
                                            (
                                              digit (
                                                _mod num 10
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              let (
                                                (
                                                  ch (
                                                    _substring "0123456789" digit (
                                                      + digit 1
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                begin (
                                                  set! res (
                                                    string-append ch res
                                                  )
                                                )
                                                 (
                                                  set! num (
                                                    _div num 10
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
                                       '(
                                        
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
                          if neg (
                            begin (
                              set! res (
                                string-append "-" res
                              )
                            )
                          )
                           '(
                            
                          )
                        )
                         (
                          ret1 res
                        )
                      )
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
        float_to_string x dec
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            let (
              (
                neg #f
              )
            )
             (
              begin (
                let (
                  (
                    num x
                  )
                )
                 (
                  begin (
                    if (
                      < num 0.0
                    )
                     (
                      begin (
                        set! neg #t
                      )
                       (
                        set! num (
                          - num
                        )
                      )
                    )
                     '(
                      
                    )
                  )
                   (
                    let (
                      (
                        int_part (
                          let (
                            (
                              v5 num
                            )
                          )
                           (
                            cond (
                              (
                                string? v5
                              )
                               (
                                inexact->exact (
                                  floor (
                                    string->number v5
                                  )
                                )
                              )
                            )
                             (
                              (
                                boolean? v5
                              )
                               (
                                if v5 1 0
                              )
                            )
                             (
                              else (
                                inexact->exact (
                                  floor v5
                                )
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
                            res (
                              int_to_string int_part
                            )
                          )
                        )
                         (
                          begin (
                            if (
                              > dec 0
                            )
                             (
                              begin (
                                set! res (
                                  string-append res "."
                                )
                              )
                               (
                                let (
                                  (
                                    frac (
                                      - num (
                                        + 0.0 int_part
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
                                                      < i dec
                                                    )
                                                     (
                                                      begin (
                                                        set! frac (
                                                          * frac 10.0
                                                        )
                                                      )
                                                       (
                                                        let (
                                                          (
                                                            digit (
                                                              let (
                                                                (
                                                                  v8 frac
                                                                )
                                                              )
                                                               (
                                                                cond (
                                                                  (
                                                                    string? v8
                                                                  )
                                                                   (
                                                                    inexact->exact (
                                                                      floor (
                                                                        string->number v8
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  (
                                                                    boolean? v8
                                                                  )
                                                                   (
                                                                    if v8 1 0
                                                                  )
                                                                )
                                                                 (
                                                                  else (
                                                                    inexact->exact (
                                                                      floor v8
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            set! res (
                                                              string-append res (
                                                                _substring "0123456789" digit (
                                                                  + digit 1
                                                                )
                                                              )
                                                            )
                                                          )
                                                           (
                                                            set! frac (
                                                              - frac (
                                                                + 0.0 digit
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
                                                        loop6
                                                      )
                                                    )
                                                     '(
                                                      
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
                                    )
                                  )
                                )
                              )
                            )
                             '(
                              
                            )
                          )
                           (
                            if neg (
                              begin (
                                set! res (
                                  string-append "-" res
                                )
                              )
                            )
                             '(
                              
                            )
                          )
                           (
                            ret4 res
                          )
                        )
                      )
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
        vector_component v i
      )
       (
        call/cc (
          lambda (
            ret9
          )
           (
            ret9 (
              list-ref-safe v i
            )
          )
        )
      )
    )
     (
      define (
        vector_str_int v
      )
       (
        call/cc (
          lambda (
            ret10
          )
           (
            let (
              (
                s "("
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
                                  < i (
                                    _len v
                                  )
                                )
                                 (
                                  begin (
                                    set! s (
                                      string-append s (
                                        int_to_string (
                                          list-ref-safe v i
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
                                        _len v
                                      )
                                    )
                                     (
                                      begin (
                                        set! s (
                                          string-append s ","
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
                                    loop11
                                  )
                                )
                                 '(
                                  
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
                    set! s (
                      string-append s ")"
                    )
                  )
                   (
                    ret10 s
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
        vector_str_float v dec
      )
       (
        call/cc (
          lambda (
            ret13
          )
           (
            let (
              (
                s "("
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
                                    _len v
                                  )
                                )
                                 (
                                  begin (
                                    set! s (
                                      string-append s (
                                        float_to_string (
                                          list-ref-safe v i
                                        )
                                         dec
                                      )
                                    )
                                  )
                                   (
                                    if (
                                      < (
                                        + i 1
                                      )
                                       (
                                        _len v
                                      )
                                    )
                                     (
                                      begin (
                                        set! s (
                                          string-append s ","
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
                    set! s (
                      string-append s ")"
                    )
                  )
                   (
                    ret13 s
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
        vector_add a b
      )
       (
        call/cc (
          lambda (
            ret16
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
                                    _len a
                                  )
                                )
                                 (
                                  begin (
                                    set! res (
                                      append res (
                                        _list (
                                          + (
                                            list-ref-safe a i
                                          )
                                           (
                                            list-ref-safe b i
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
                    ret16 res
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
        vector_sub a b
      )
       (
        call/cc (
          lambda (
            ret19
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
                                    _len a
                                  )
                                )
                                 (
                                  begin (
                                    set! res (
                                      append res (
                                        _list (
                                          - (
                                            list-ref-safe a i
                                          )
                                           (
                                            list-ref-safe b i
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
                                    loop20
                                  )
                                )
                                 '(
                                  
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
                    ret19 res
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
        vector_scalar_mul v s
      )
       (
        call/cc (
          lambda (
            ret22
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
                                  < i (
                                    _len v
                                  )
                                )
                                 (
                                  begin (
                                    set! res (
                                      append res (
                                        _list (
                                          * (
                                            + 0.0 (
                                              list-ref-safe v i
                                            )
                                          )
                                           s
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
                                    loop23
                                  )
                                )
                                 '(
                                  
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
                    ret22 res
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
        vector_dot a b
      )
       (
        call/cc (
          lambda (
            ret25
          )
           (
            let (
              (
                sum 0
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
                                    _len a
                                  )
                                )
                                 (
                                  begin (
                                    set! sum (
                                      + sum (
                                        * (
                                          list-ref-safe a i
                                        )
                                         (
                                          list-ref-safe b i
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
                                 '(
                                  
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
                    ret25 sum
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
        sqrt_newton x
      )
       (
        call/cc (
          lambda (
            ret28
          )
           (
            begin (
              if (
                equal? x 0.0
              )
               (
                begin (
                  ret28 0.0
                )
              )
               '(
                
              )
            )
             (
              let (
                (
                  low 0.0
                )
              )
               (
                begin (
                  let (
                    (
                      high x
                    )
                  )
                   (
                    begin (
                      if (
                        < x 1.0
                      )
                       (
                        begin (
                          set! high 1.0
                        )
                      )
                       '(
                        
                      )
                    )
                     (
                      let (
                        (
                          mid 0.0
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
                                            < i 40
                                          )
                                           (
                                            begin (
                                              set! mid (
                                                _div (
                                                  + low high
                                                )
                                                 2.0
                                              )
                                            )
                                             (
                                              if (
                                                _gt (
                                                  * mid mid
                                                )
                                                 x
                                              )
                                               (
                                                begin (
                                                  set! high mid
                                                )
                                              )
                                               (
                                                begin (
                                                  set! low mid
                                                )
                                              )
                                            )
                                             (
                                              set! i (
                                                + i 1
                                              )
                                            )
                                             (
                                              loop29
                                            )
                                          )
                                           '(
                                            
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
                              ret28 mid
                            )
                          )
                        )
                      )
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
        euclidean_length v
      )
       (
        call/cc (
          lambda (
            ret31
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
                                  < i (
                                    _len v
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        val (
                                          + 0.0 (
                                            list-ref-safe v i
                                          )
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        set! sum (
                                          _add sum (
                                            * val val
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
                                    loop32
                                  )
                                )
                                 '(
                                  
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
                    ret31 (
                      sqrt_newton sum
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
        zero_vector n
      )
       (
        call/cc (
          lambda (
            ret34
          )
           (
            let (
              (
                v (
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
                                  < i n
                                )
                                 (
                                  begin (
                                    set! v (
                                      append v (
                                        _list 0
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
                    ret34 v
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
        unit_basis_vector n idx
      )
       (
        call/cc (
          lambda (
            ret37
          )
           (
            let (
              (
                v (
                  zero_vector n
                )
              )
            )
             (
              begin (
                list-set! v idx 1
              )
               (
                ret37 v
              )
            )
          )
        )
      )
    )
     (
      define (
        axpy a x y
      )
       (
        call/cc (
          lambda (
            ret38
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
                                    _len x
                                  )
                                )
                                 (
                                  begin (
                                    set! res (
                                      append res (
                                        _list (
                                          + (
                                            * a (
                                              list-ref-safe x i
                                            )
                                          )
                                           (
                                            list-ref-safe y i
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
                    ret38 res
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
        copy_vector x
      )
       (
        call/cc (
          lambda (
            ret41
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
                                  < i (
                                    _len x
                                  )
                                )
                                 (
                                  begin (
                                    set! res (
                                      append res (
                                        _list (
                                          list-ref-safe x i
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
                                    loop42
                                  )
                                )
                                 '(
                                  
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
                    ret41 res
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
        change_component v idx val
      )
       (
        call/cc (
          lambda (
            ret44
          )
           (
            list-set! v idx val
          )
        )
      )
    )
     (
      define (
        matrix_str m
      )
       (
        call/cc (
          lambda (
            ret45
          )
           (
            let (
              (
                s ""
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
                                  < i (
                                    _len m
                                  )
                                )
                                 (
                                  begin (
                                    set! s (
                                      string-append s "|"
                                    )
                                  )
                                   (
                                    let (
                                      (
                                        j 0
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
                                                      < j (
                                                        _len (
                                                          list-ref-safe m 0
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        set! s (
                                                          string-append s (
                                                            int_to_string (
                                                              cond (
                                                                (
                                                                  string? (
                                                                    list-ref-safe m i
                                                                  )
                                                                )
                                                                 (
                                                                  _substring (
                                                                    list-ref-safe m i
                                                                  )
                                                                   j (
                                                                    + j 1
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                (
                                                                  hash-table? (
                                                                    list-ref-safe m i
                                                                  )
                                                                )
                                                                 (
                                                                  hash-table-ref (
                                                                    list-ref-safe m i
                                                                  )
                                                                   j
                                                                )
                                                              )
                                                               (
                                                                else (
                                                                  list-ref-safe (
                                                                    list-ref-safe m i
                                                                  )
                                                                   j
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        if (
                                                          < (
                                                            + j 1
                                                          )
                                                           (
                                                            _len (
                                                              list-ref-safe m 0
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            set! s (
                                                              string-append s ","
                                                            )
                                                          )
                                                        )
                                                         '(
                                                          
                                                        )
                                                      )
                                                       (
                                                        set! j (
                                                          + j 1
                                                        )
                                                      )
                                                       (
                                                        loop48
                                                      )
                                                    )
                                                     '(
                                                      
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
                                        set! s (
                                          string-append s "|\n"
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
                                    loop46
                                  )
                                )
                                 '(
                                  
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
                    ret45 s
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
        submatrix m row col
      )
       (
        call/cc (
          lambda (
            ret50
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
                                  < i (
                                    _len m
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      not (
                                        equal? i row
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            r (
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
                                                              < j (
                                                                _len (
                                                                  list-ref-safe m 0
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                if (
                                                                  not (
                                                                    equal? j col
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! r (
                                                                      append r (
                                                                        _list (
                                                                          cond (
                                                                            (
                                                                              string? (
                                                                                list-ref-safe m i
                                                                              )
                                                                            )
                                                                             (
                                                                              _substring (
                                                                                list-ref-safe m i
                                                                              )
                                                                               j (
                                                                                + j 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            (
                                                                              hash-table? (
                                                                                list-ref-safe m i
                                                                              )
                                                                            )
                                                                             (
                                                                              hash-table-ref (
                                                                                list-ref-safe m i
                                                                              )
                                                                               j
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref-safe (
                                                                                list-ref-safe m i
                                                                              )
                                                                               j
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                                 '(
                                                                  
                                                                )
                                                              )
                                                               (
                                                                set! j (
                                                                  + j 1
                                                                )
                                                              )
                                                               (
                                                                loop53
                                                              )
                                                            )
                                                             '(
                                                              
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
                                                set! res (
                                                  append res (
                                                    _list r
                                                  )
                                                )
                                              )
                                            )
                                          )
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
                                    loop51
                                  )
                                )
                                 '(
                                  
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
                   (
                    ret50 res
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
        determinant m
      )
       (
        call/cc (
          lambda (
            ret55
          )
           (
            let (
              (
                n (
                  _len m
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
                    ret55 (
                      cond (
                        (
                          string? (
                            list-ref-safe m 0
                          )
                        )
                         (
                          _substring (
                            list-ref-safe m 0
                          )
                           0 (
                            + 0 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? (
                            list-ref-safe m 0
                          )
                        )
                         (
                          hash-table-ref (
                            list-ref-safe m 0
                          )
                           0
                        )
                      )
                       (
                        else (
                          list-ref-safe (
                            list-ref-safe m 0
                          )
                           0
                        )
                      )
                    )
                  )
                )
                 '(
                  
                )
              )
               (
                if (
                  equal? n 2
                )
                 (
                  begin (
                    ret55 (
                      - (
                        * (
                          cond (
                            (
                              string? (
                                list-ref-safe m 0
                              )
                            )
                             (
                              _substring (
                                list-ref-safe m 0
                              )
                               0 (
                                + 0 1
                              )
                            )
                          )
                           (
                            (
                              hash-table? (
                                list-ref-safe m 0
                              )
                            )
                             (
                              hash-table-ref (
                                list-ref-safe m 0
                              )
                               0
                            )
                          )
                           (
                            else (
                              list-ref-safe (
                                list-ref-safe m 0
                              )
                               0
                            )
                          )
                        )
                         (
                          cond (
                            (
                              string? (
                                list-ref-safe m 1
                              )
                            )
                             (
                              _substring (
                                list-ref-safe m 1
                              )
                               1 (
                                + 1 1
                              )
                            )
                          )
                           (
                            (
                              hash-table? (
                                list-ref-safe m 1
                              )
                            )
                             (
                              hash-table-ref (
                                list-ref-safe m 1
                              )
                               1
                            )
                          )
                           (
                            else (
                              list-ref-safe (
                                list-ref-safe m 1
                              )
                               1
                            )
                          )
                        )
                      )
                       (
                        * (
                          cond (
                            (
                              string? (
                                list-ref-safe m 0
                              )
                            )
                             (
                              _substring (
                                list-ref-safe m 0
                              )
                               1 (
                                + 1 1
                              )
                            )
                          )
                           (
                            (
                              hash-table? (
                                list-ref-safe m 0
                              )
                            )
                             (
                              hash-table-ref (
                                list-ref-safe m 0
                              )
                               1
                            )
                          )
                           (
                            else (
                              list-ref-safe (
                                list-ref-safe m 0
                              )
                               1
                            )
                          )
                        )
                         (
                          cond (
                            (
                              string? (
                                list-ref-safe m 1
                              )
                            )
                             (
                              _substring (
                                list-ref-safe m 1
                              )
                               0 (
                                + 0 1
                              )
                            )
                          )
                           (
                            (
                              hash-table? (
                                list-ref-safe m 1
                              )
                            )
                             (
                              hash-table-ref (
                                list-ref-safe m 1
                              )
                               0
                            )
                          )
                           (
                            else (
                              list-ref-safe (
                                list-ref-safe m 1
                              )
                               0
                            )
                          )
                        )
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
                    det 0
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
                            break57
                          )
                           (
                            letrec (
                              (
                                loop56 (
                                  lambda (
                                    
                                  )
                                   (
                                    if (
                                      < c n
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            sub (
                                              submatrix m 0 c
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                sign 1
                                              )
                                            )
                                             (
                                              begin (
                                                if (
                                                  equal? (
                                                    _mod c 2
                                                  )
                                                   1
                                                )
                                                 (
                                                  begin (
                                                    set! sign (
                                                      - 1
                                                    )
                                                  )
                                                )
                                                 '(
                                                  
                                                )
                                              )
                                               (
                                                set! det (
                                                  _add det (
                                                    * (
                                                      * sign (
                                                        cond (
                                                          (
                                                            string? (
                                                              list-ref-safe m 0
                                                            )
                                                          )
                                                           (
                                                            _substring (
                                                              list-ref-safe m 0
                                                            )
                                                             c (
                                                              + c 1
                                                            )
                                                          )
                                                        )
                                                         (
                                                          (
                                                            hash-table? (
                                                              list-ref-safe m 0
                                                            )
                                                          )
                                                           (
                                                            hash-table-ref (
                                                              list-ref-safe m 0
                                                            )
                                                             c
                                                          )
                                                        )
                                                         (
                                                          else (
                                                            list-ref-safe (
                                                              list-ref-safe m 0
                                                            )
                                                             c
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      determinant sub
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
                                        )
                                      )
                                       (
                                        loop56
                                      )
                                    )
                                     '(
                                      
                                    )
                                  )
                                )
                              )
                            )
                             (
                              loop56
                            )
                          )
                        )
                      )
                       (
                        ret55 det
                      )
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
        matrix_minor m row col
      )
       (
        call/cc (
          lambda (
            ret58
          )
           (
            ret58 (
              determinant (
                submatrix m row col
              )
            )
          )
        )
      )
    )
     (
      define (
        matrix_cofactor m row col
      )
       (
        call/cc (
          lambda (
            ret59
          )
           (
            let (
              (
                sign 1
              )
            )
             (
              begin (
                if (
                  equal? (
                    _mod (
                      + row col
                    )
                     2
                  )
                   1
                )
                 (
                  begin (
                    set! sign (
                      - 1
                    )
                  )
                )
                 '(
                  
                )
              )
               (
                ret59 (
                  * sign (
                    matrix_minor m row col
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
        matrix_mul_vector m v
      )
       (
        call/cc (
          lambda (
            ret60
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
                        break62
                      )
                       (
                        letrec (
                          (
                            loop61 (
                              lambda (
                                
                              )
                               (
                                if (
                                  < i (
                                    _len m
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        sum 0
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
                                                          < j (
                                                            _len (
                                                              list-ref-safe m 0
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            set! sum (
                                                              + sum (
                                                                * (
                                                                  cond (
                                                                    (
                                                                      string? (
                                                                        list-ref-safe m i
                                                                      )
                                                                    )
                                                                     (
                                                                      _substring (
                                                                        list-ref-safe m i
                                                                      )
                                                                       j (
                                                                        + j 1
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    (
                                                                      hash-table? (
                                                                        list-ref-safe m i
                                                                      )
                                                                    )
                                                                     (
                                                                      hash-table-ref (
                                                                        list-ref-safe m i
                                                                      )
                                                                       j
                                                                    )
                                                                  )
                                                                   (
                                                                    else (
                                                                      list-ref-safe (
                                                                        list-ref-safe m i
                                                                      )
                                                                       j
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  list-ref-safe v j
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
                                                            loop63
                                                          )
                                                        )
                                                         '(
                                                          
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
                                            set! res (
                                              append res (
                                                _list sum
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
                                    loop61
                                  )
                                )
                                 '(
                                  
                                )
                              )
                            )
                          )
                        )
                         (
                          loop61
                        )
                      )
                    )
                  )
                   (
                    ret60 res
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
        matrix_mul_scalar m s
      )
       (
        call/cc (
          lambda (
            ret65
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
                        break67
                      )
                       (
                        letrec (
                          (
                            loop66 (
                              lambda (
                                
                              )
                               (
                                if (
                                  < i (
                                    _len m
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
                                                break69
                                              )
                                               (
                                                letrec (
                                                  (
                                                    loop68 (
                                                      lambda (
                                                        
                                                      )
                                                       (
                                                        if (
                                                          < j (
                                                            _len (
                                                              list-ref-safe m 0
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
                                                                          list-ref-safe m i
                                                                        )
                                                                      )
                                                                       (
                                                                        _substring (
                                                                          list-ref-safe m i
                                                                        )
                                                                         j (
                                                                          + j 1
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      (
                                                                        hash-table? (
                                                                          list-ref-safe m i
                                                                        )
                                                                      )
                                                                       (
                                                                        hash-table-ref (
                                                                          list-ref-safe m i
                                                                        )
                                                                         j
                                                                      )
                                                                    )
                                                                     (
                                                                      else (
                                                                        list-ref-safe (
                                                                          list-ref-safe m i
                                                                        )
                                                                         j
                                                                      )
                                                                    )
                                                                  )
                                                                   s
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
                                                            loop68
                                                          )
                                                        )
                                                         '(
                                                          
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  loop68
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
                                    loop66
                                  )
                                )
                                 '(
                                  
                                )
                              )
                            )
                          )
                        )
                         (
                          loop66
                        )
                      )
                    )
                  )
                   (
                    ret65 res
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
        matrix_change_component m i j val
      )
       (
        call/cc (
          lambda (
            ret70
          )
           (
            list-set! (
              list-ref-safe m i
            )
             j val
          )
        )
      )
    )
     (
      define (
        matrix_component m i j
      )
       (
        call/cc (
          lambda (
            ret71
          )
           (
            ret71 (
              cond (
                (
                  string? (
                    list-ref-safe m i
                  )
                )
                 (
                  _substring (
                    list-ref-safe m i
                  )
                   j (
                    + j 1
                  )
                )
              )
               (
                (
                  hash-table? (
                    list-ref-safe m i
                  )
                )
                 (
                  hash-table-ref (
                    list-ref-safe m i
                  )
                   j
                )
              )
               (
                else (
                  list-ref-safe (
                    list-ref-safe m i
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
      define (
        matrix_add a b
      )
       (
        call/cc (
          lambda (
            ret72
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
                        break74
                      )
                       (
                        letrec (
                          (
                            loop73 (
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
                                                break76
                                              )
                                               (
                                                letrec (
                                                  (
                                                    loop75 (
                                                      lambda (
                                                        
                                                      )
                                                       (
                                                        if (
                                                          < j (
                                                            _len (
                                                              list-ref-safe a 0
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            set! row (
                                                              append row (
                                                                _list (
                                                                  + (
                                                                    cond (
                                                                      (
                                                                        string? (
                                                                          list-ref-safe a i
                                                                        )
                                                                      )
                                                                       (
                                                                        _substring (
                                                                          list-ref-safe a i
                                                                        )
                                                                         j (
                                                                          + j 1
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      (
                                                                        hash-table? (
                                                                          list-ref-safe a i
                                                                        )
                                                                      )
                                                                       (
                                                                        hash-table-ref (
                                                                          list-ref-safe a i
                                                                        )
                                                                         j
                                                                      )
                                                                    )
                                                                     (
                                                                      else (
                                                                        list-ref-safe (
                                                                          list-ref-safe a i
                                                                        )
                                                                         j
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    cond (
                                                                      (
                                                                        string? (
                                                                          list-ref-safe b i
                                                                        )
                                                                      )
                                                                       (
                                                                        _substring (
                                                                          list-ref-safe b i
                                                                        )
                                                                         j (
                                                                          + j 1
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      (
                                                                        hash-table? (
                                                                          list-ref-safe b i
                                                                        )
                                                                      )
                                                                       (
                                                                        hash-table-ref (
                                                                          list-ref-safe b i
                                                                        )
                                                                         j
                                                                      )
                                                                    )
                                                                     (
                                                                      else (
                                                                        list-ref-safe (
                                                                          list-ref-safe b i
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
                                                            loop75
                                                          )
                                                        )
                                                         '(
                                                          
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  loop75
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
                                    loop73
                                  )
                                )
                                 '(
                                  
                                )
                              )
                            )
                          )
                        )
                         (
                          loop73
                        )
                      )
                    )
                  )
                   (
                    ret72 res
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
        matrix_sub a b
      )
       (
        call/cc (
          lambda (
            ret77
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
                        break79
                      )
                       (
                        letrec (
                          (
                            loop78 (
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
                                                break81
                                              )
                                               (
                                                letrec (
                                                  (
                                                    loop80 (
                                                      lambda (
                                                        
                                                      )
                                                       (
                                                        if (
                                                          < j (
                                                            _len (
                                                              list-ref-safe a 0
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
                                                                          list-ref-safe a i
                                                                        )
                                                                      )
                                                                       (
                                                                        _substring (
                                                                          list-ref-safe a i
                                                                        )
                                                                         j (
                                                                          + j 1
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      (
                                                                        hash-table? (
                                                                          list-ref-safe a i
                                                                        )
                                                                      )
                                                                       (
                                                                        hash-table-ref (
                                                                          list-ref-safe a i
                                                                        )
                                                                         j
                                                                      )
                                                                    )
                                                                     (
                                                                      else (
                                                                        list-ref-safe (
                                                                          list-ref-safe a i
                                                                        )
                                                                         j
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    cond (
                                                                      (
                                                                        string? (
                                                                          list-ref-safe b i
                                                                        )
                                                                      )
                                                                       (
                                                                        _substring (
                                                                          list-ref-safe b i
                                                                        )
                                                                         j (
                                                                          + j 1
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      (
                                                                        hash-table? (
                                                                          list-ref-safe b i
                                                                        )
                                                                      )
                                                                       (
                                                                        hash-table-ref (
                                                                          list-ref-safe b i
                                                                        )
                                                                         j
                                                                      )
                                                                    )
                                                                     (
                                                                      else (
                                                                        list-ref-safe (
                                                                          list-ref-safe b i
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
                                                            loop80
                                                          )
                                                        )
                                                         '(
                                                          
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  loop80
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
                                    loop78
                                  )
                                )
                                 '(
                                  
                                )
                              )
                            )
                          )
                        )
                         (
                          loop78
                        )
                      )
                    )
                  )
                   (
                    ret77 res
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
        square_zero_matrix n
      )
       (
        call/cc (
          lambda (
            ret82
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
                    i 0
                  )
                )
                 (
                  begin (
                    call/cc (
                      lambda (
                        break84
                      )
                       (
                        letrec (
                          (
                            loop83 (
                              lambda (
                                
                              )
                               (
                                if (
                                  < i n
                                )
                                 (
                                  begin (
                                    set! m (
                                      append m (
                                        _list (
                                          zero_vector n
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
                                    loop83
                                  )
                                )
                                 '(
                                  
                                )
                              )
                            )
                          )
                        )
                         (
                          loop83
                        )
                      )
                    )
                  )
                   (
                    ret82 m
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
        assert_int name actual expected
      )
       (
        call/cc (
          lambda (
            ret85
          )
           (
            if (
              equal? actual expected
            )
             (
              begin (
                _display (
                  if (
                    string? (
                      string-append name " ok"
                    )
                  )
                   (
                    string-append name " ok"
                  )
                   (
                    to-str (
                      string-append name " ok"
                    )
                  )
                )
              )
               (
                newline
              )
            )
             (
              begin (
                _display (
                  if (
                    string? (
                      string-append (
                        string-append (
                          string-append (
                            string-append name " fail "
                          )
                           (
                            int_to_string actual
                          )
                        )
                         " != "
                      )
                       (
                        int_to_string expected
                      )
                    )
                  )
                   (
                    string-append (
                      string-append (
                        string-append (
                          string-append name " fail "
                        )
                         (
                          int_to_string actual
                        )
                      )
                       " != "
                    )
                     (
                      int_to_string expected
                    )
                  )
                   (
                    to-str (
                      string-append (
                        string-append (
                          string-append (
                            string-append name " fail "
                          )
                           (
                            int_to_string actual
                          )
                        )
                         " != "
                      )
                       (
                        int_to_string expected
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
     (
      define (
        assert_str name actual expected
      )
       (
        call/cc (
          lambda (
            ret86
          )
           (
            if (
              string=? actual expected
            )
             (
              begin (
                _display (
                  if (
                    string? (
                      string-append name " ok"
                    )
                  )
                   (
                    string-append name " ok"
                  )
                   (
                    to-str (
                      string-append name " ok"
                    )
                  )
                )
              )
               (
                newline
              )
            )
             (
              begin (
                _display (
                  if (
                    string? (
                      string-append name " fail"
                    )
                  )
                   (
                    string-append name " fail"
                  )
                   (
                    to-str (
                      string-append name " fail"
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
                    string? actual
                  )
                   actual (
                    to-str actual
                  )
                )
              )
               (
                newline
              )
               (
                _display (
                  if (
                    string? expected
                  )
                   expected (
                    to-str expected
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
     (
      define (
        assert_float name actual expected eps
      )
       (
        call/cc (
          lambda (
            ret87
          )
           (
            let (
              (
                diff (
                  - actual expected
                )
              )
            )
             (
              begin (
                if (
                  < diff 0.0
                )
                 (
                  begin (
                    set! diff (
                      - diff
                    )
                  )
                )
                 '(
                  
                )
              )
               (
                if (
                  <= diff eps
                )
                 (
                  begin (
                    _display (
                      if (
                        string? (
                          string-append name " ok"
                        )
                      )
                       (
                        string-append name " ok"
                      )
                       (
                        to-str (
                          string-append name " ok"
                        )
                      )
                    )
                  )
                   (
                    newline
                  )
                )
                 (
                  begin (
                    _display (
                      if (
                        string? (
                          string-append name " fail"
                        )
                      )
                       (
                        string-append name " fail"
                      )
                       (
                        to-str (
                          string-append name " fail"
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
      let (
        (
          vx (
            _list 1 2 3
          )
        )
      )
       (
        begin (
          assert_int "component0" (
            vector_component vx 0
          )
           1
        )
         (
          assert_int "component2" (
            vector_component vx 2
          )
           3
        )
         (
          let (
            (
              vs (
                _list 0 0 0 0 0 1
              )
            )
          )
           (
            begin (
              assert_str "str_vector" (
                vector_str_int vs
              )
               "(0,0,0,0,0,1)"
            )
             (
              let (
                (
                  vsize (
                    _list 1 2 3 4
                  )
                )
              )
               (
                begin (
                  assert_int "size" (
                    _len vsize
                  )
                   4
                )
                 (
                  let (
                    (
                      va (
                        _list 1 2 3
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          vb (
                            _list 1 1 1
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              vsum (
                                vector_add va vb
                              )
                            )
                          )
                           (
                            begin (
                              assert_int "add0" (
                                vector_component vsum 0
                              )
                               2
                            )
                             (
                              assert_int "add1" (
                                vector_component vsum 1
                              )
                               3
                            )
                             (
                              assert_int "add2" (
                                vector_component vsum 2
                              )
                               4
                            )
                             (
                              let (
                                (
                                  vsub (
                                    vector_sub va vb
                                  )
                                )
                              )
                               (
                                begin (
                                  assert_int "sub0" (
                                    vector_component vsub 0
                                  )
                                   0
                                )
                                 (
                                  assert_int "sub1" (
                                    vector_component vsub 1
                                  )
                                   1
                                )
                                 (
                                  assert_int "sub2" (
                                    vector_component vsub 2
                                  )
                                   2
                                )
                                 (
                                  let (
                                    (
                                      vmul (
                                        vector_scalar_mul va 3.0
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      assert_str "scalar_mul" (
                                        vector_str_float vmul 1
                                      )
                                       "(3.0,6.0,9.0)"
                                    )
                                     (
                                      assert_int "dot_product" (
                                        vector_dot (
                                          _list 2 (
                                            - 1
                                          )
                                           4
                                        )
                                         (
                                          _list 1 (
                                            - 2
                                          )
                                           (
                                            - 1
                                          )
                                        )
                                      )
                                       0
                                    )
                                     (
                                      let (
                                        (
                                          zvec (
                                            zero_vector 10
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          let (
                                            (
                                              zstr (
                                                vector_str_int zvec
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              let (
                                                (
                                                  zcount 0
                                                )
                                              )
                                               (
                                                begin (
                                                  let (
                                                    (
                                                      zi 0
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      call/cc (
                                                        lambda (
                                                          break89
                                                        )
                                                         (
                                                          letrec (
                                                            (
                                                              loop88 (
                                                                lambda (
                                                                  
                                                                )
                                                                 (
                                                                  if (
                                                                    < zi (
                                                                      _len zstr
                                                                    )
                                                                  )
                                                                   (
                                                                    begin (
                                                                      if (
                                                                        string=? (
                                                                          _substring zstr zi (
                                                                            + zi 1
                                                                          )
                                                                        )
                                                                         "0"
                                                                      )
                                                                       (
                                                                        begin (
                                                                          set! zcount (
                                                                            + zcount 1
                                                                          )
                                                                        )
                                                                      )
                                                                       '(
                                                                        
                                                                      )
                                                                    )
                                                                     (
                                                                      set! zi (
                                                                        + zi 1
                                                                      )
                                                                    )
                                                                     (
                                                                      loop88
                                                                    )
                                                                  )
                                                                   '(
                                                                    
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                           (
                                                            loop88
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      assert_int "zero_vector" zcount 10
                                                    )
                                                     (
                                                      assert_str "unit_basis" (
                                                        vector_str_int (
                                                          unit_basis_vector 3 1
                                                        )
                                                      )
                                                       "(0,1,0)"
                                                    )
                                                     (
                                                      assert_str "axpy" (
                                                        vector_str_int (
                                                          axpy 2 (
                                                            _list 1 2 3
                                                          )
                                                           (
                                                            _list 1 0 1
                                                          )
                                                        )
                                                      )
                                                       "(3,4,7)"
                                                    )
                                                     (
                                                      let (
                                                        (
                                                          vcopy (
                                                            copy_vector (
                                                              _list 1 0 0 0 0 0
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        begin (
                                                          assert_str "copy" (
                                                            vector_str_int vcopy
                                                          )
                                                           "(1,0,0,0,0,0)"
                                                        )
                                                         (
                                                          let (
                                                            (
                                                              vchange (
                                                                _list 1 0 0
                                                              )
                                                            )
                                                          )
                                                           (
                                                            begin (
                                                              change_component vchange 0 0
                                                            )
                                                             (
                                                              change_component vchange 1 1
                                                            )
                                                             (
                                                              assert_str "change_component" (
                                                                vector_str_int vchange
                                                              )
                                                               "(0,1,0)"
                                                            )
                                                             (
                                                              let (
                                                                (
                                                                  ma (
                                                                    _list (
                                                                      _list 1 2 3
                                                                    )
                                                                     (
                                                                      _list 2 4 5
                                                                    )
                                                                     (
                                                                      _list 6 7 8
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                begin (
                                                                  assert_str "matrix_str" (
                                                                    matrix_str ma
                                                                  )
                                                                   "|1,2,3|\n|2,4,5|\n|6,7,8|\n"
                                                                )
                                                                 (
                                                                  assert_int "determinant" (
                                                                    determinant ma
                                                                  )
                                                                   (
                                                                    - 5
                                                                  )
                                                                )
                                                                 (
                                                                  let (
                                                                    (
                                                                      mb (
                                                                        _list (
                                                                          _list 1 2 3
                                                                        )
                                                                         (
                                                                          _list 4 5 6
                                                                        )
                                                                         (
                                                                          _list 7 8 9
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    begin (
                                                                      let (
                                                                        (
                                                                          mv (
                                                                            matrix_mul_vector mb (
                                                                              _list 1 2 3
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        begin (
                                                                          assert_str "matrix_vec_mul" (
                                                                            vector_str_int mv
                                                                          )
                                                                           "(14,32,50)"
                                                                        )
                                                                         (
                                                                          let (
                                                                            (
                                                                              msc (
                                                                                matrix_mul_scalar mb 2
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            begin (
                                                                              assert_str "matrix_scalar_mul" (
                                                                                matrix_str msc
                                                                              )
                                                                               "|2,4,6|\n|8,10,12|\n|14,16,18|\n"
                                                                            )
                                                                             (
                                                                              let (
                                                                                (
                                                                                  mc (
                                                                                    _list (
                                                                                      _list 1 2 3
                                                                                    )
                                                                                     (
                                                                                      _list 2 4 5
                                                                                    )
                                                                                     (
                                                                                      _list 6 7 8
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                begin (
                                                                                  matrix_change_component mc 0 2 5
                                                                                )
                                                                                 (
                                                                                  assert_str "change_component_matrix" (
                                                                                    matrix_str mc
                                                                                  )
                                                                                   "|1,2,5|\n|2,4,5|\n|6,7,8|\n"
                                                                                )
                                                                                 (
                                                                                  assert_int "matrix_component" (
                                                                                    matrix_component mc 2 1
                                                                                  )
                                                                                   7
                                                                                )
                                                                                 (
                                                                                  let (
                                                                                    (
                                                                                      madd (
                                                                                        matrix_add (
                                                                                          _list (
                                                                                            _list 1 2 3
                                                                                          )
                                                                                           (
                                                                                            _list 2 4 5
                                                                                          )
                                                                                           (
                                                                                            _list 6 7 8
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          _list (
                                                                                            _list 1 2 7
                                                                                          )
                                                                                           (
                                                                                            _list 2 4 5
                                                                                          )
                                                                                           (
                                                                                            _list 6 7 10
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    begin (
                                                                                      assert_str "matrix_add" (
                                                                                        matrix_str madd
                                                                                      )
                                                                                       "|2,4,10|\n|4,8,10|\n|12,14,18|\n"
                                                                                    )
                                                                                     (
                                                                                      let (
                                                                                        (
                                                                                          msub (
                                                                                            matrix_sub (
                                                                                              _list (
                                                                                                _list 1 2 3
                                                                                              )
                                                                                               (
                                                                                                _list 2 4 5
                                                                                              )
                                                                                               (
                                                                                                _list 6 7 8
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              _list (
                                                                                                _list 1 2 7
                                                                                              )
                                                                                               (
                                                                                                _list 2 4 5
                                                                                              )
                                                                                               (
                                                                                                _list 6 7 10
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        begin (
                                                                                          assert_str "matrix_sub" (
                                                                                            matrix_str msub
                                                                                          )
                                                                                           "|0,0,-4|\n|0,0,0|\n|0,0,-2|\n"
                                                                                        )
                                                                                         (
                                                                                          let (
                                                                                            (
                                                                                              mzero (
                                                                                                square_zero_matrix 5
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            begin (
                                                                                              assert_str "square_zero_matrix" (
                                                                                                matrix_str mzero
                                                                                              )
                                                                                               "|0,0,0,0,0|\n|0,0,0,0,0|\n|0,0,0,0,0|\n|0,0,0,0,0|\n|0,0,0,0,0|\n"
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
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
          end91 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur92 (
              quotient (
                * (
                  - end91 start90
                )
                 1000000
              )
               jps93
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur92
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
