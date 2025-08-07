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
        pow10 n
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                result 1
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
                                  < i n
                                )
                                 (
                                  begin (
                                    set! result (
                                      * result 10
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
                    ret1 result
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
        gcd a b
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            let (
              (
                x a
              )
            )
             (
              begin (
                let (
                  (
                    y b
                  )
                )
                 (
                  begin (
                    if (
                      < x 0
                    )
                     (
                      begin (
                        set! x (
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
                    if (
                      < y 0
                    )
                     (
                      begin (
                        set! y (
                          - y
                        )
                      )
                    )
                     (
                      quote (
                        
                      )
                    )
                  )
                   (
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
                                  not (
                                    equal? y 0
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        r (
                                          _mod x y
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        set! x y
                                      )
                                       (
                                        set! y r
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
                    ret4 x
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
        parse_decimal s
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            begin (
              if (
                equal? (
                  _len s
                )
                 0
              )
               (
                begin (
                  panic "invalid number"
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
                  idx 0
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
                      let (
                        (
                          first (
                            _substring s 0 1
                          )
                        )
                      )
                       (
                        begin (
                          if (
                            string=? first "-"
                          )
                           (
                            begin (
                              set! sign (
                                - 1
                              )
                            )
                             (
                              set! idx 1
                            )
                          )
                           (
                            if (
                              string=? first "+"
                            )
                             (
                              begin (
                                set! idx 1
                              )
                            )
                             (
                              quote (
                                
                              )
                            )
                          )
                        )
                         (
                          let (
                            (
                              int_part ""
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
                                            < idx (
                                              _len s
                                            )
                                          )
                                           (
                                            begin (
                                              let (
                                                (
                                                  c (
                                                    _substring s idx (
                                                      + idx 1
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                begin (
                                                  if (
                                                    and (
                                                      string>=? c "0"
                                                    )
                                                     (
                                                      string<=? c "9"
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      set! int_part (
                                                        string-append int_part c
                                                      )
                                                    )
                                                     (
                                                      set! idx (
                                                        + idx 1
                                                      )
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      break9 (
                                                        quote (
                                                          
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
                                  frac_part ""
                                )
                              )
                               (
                                begin (
                                  if (
                                    and (
                                      < idx (
                                        _len s
                                      )
                                    )
                                     (
                                      string=? (
                                        _substring s idx (
                                          + idx 1
                                        )
                                      )
                                       "."
                                    )
                                  )
                                   (
                                    begin (
                                      set! idx (
                                        + idx 1
                                      )
                                    )
                                     (
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
                                                    < idx (
                                                      _len s
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      let (
                                                        (
                                                          c (
                                                            _substring s idx (
                                                              + idx 1
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        begin (
                                                          if (
                                                            and (
                                                              string>=? c "0"
                                                            )
                                                             (
                                                              string<=? c "9"
                                                            )
                                                          )
                                                           (
                                                            begin (
                                                              set! frac_part (
                                                                string-append frac_part c
                                                              )
                                                            )
                                                             (
                                                              set! idx (
                                                                + idx 1
                                                              )
                                                            )
                                                          )
                                                           (
                                                            begin (
                                                              break11 (
                                                                quote (
                                                                  
                                                                )
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
                                  )
                                   (
                                    quote (
                                      
                                    )
                                  )
                                )
                                 (
                                  let (
                                    (
                                      exp 0
                                    )
                                  )
                                   (
                                    begin (
                                      if (
                                        and (
                                          < idx (
                                            _len s
                                          )
                                        )
                                         (
                                          or (
                                            string=? (
                                              _substring s idx (
                                                + idx 1
                                              )
                                            )
                                             "e"
                                          )
                                           (
                                            string=? (
                                              _substring s idx (
                                                + idx 1
                                              )
                                            )
                                             "E"
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          set! idx (
                                            + idx 1
                                          )
                                        )
                                         (
                                          let (
                                            (
                                              exp_sign 1
                                            )
                                          )
                                           (
                                            begin (
                                              if (
                                                and (
                                                  < idx (
                                                    _len s
                                                  )
                                                )
                                                 (
                                                  string=? (
                                                    _substring s idx (
                                                      + idx 1
                                                    )
                                                  )
                                                   "-"
                                                )
                                              )
                                               (
                                                begin (
                                                  set! exp_sign (
                                                    - 1
                                                  )
                                                )
                                                 (
                                                  set! idx (
                                                    + idx 1
                                                  )
                                                )
                                              )
                                               (
                                                if (
                                                  and (
                                                    < idx (
                                                      _len s
                                                    )
                                                  )
                                                   (
                                                    string=? (
                                                      _substring s idx (
                                                        + idx 1
                                                      )
                                                    )
                                                     "+"
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    set! idx (
                                                      + idx 1
                                                    )
                                                  )
                                                )
                                                 (
                                                  quote (
                                                    
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              let (
                                                (
                                                  exp_str ""
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
                                                                < idx (
                                                                  _len s
                                                                )
                                                              )
                                                               (
                                                                begin (
                                                                  let (
                                                                    (
                                                                      c (
                                                                        _substring s idx (
                                                                          + idx 1
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    begin (
                                                                      if (
                                                                        and (
                                                                          string>=? c "0"
                                                                        )
                                                                         (
                                                                          string<=? c "9"
                                                                        )
                                                                      )
                                                                       (
                                                                        begin (
                                                                          set! exp_str (
                                                                            string-append exp_str c
                                                                          )
                                                                        )
                                                                         (
                                                                          set! idx (
                                                                            + idx 1
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        begin (
                                                                          panic "invalid number"
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
                                                  if (
                                                    equal? (
                                                      _len exp_str
                                                    )
                                                     0
                                                  )
                                                   (
                                                    begin (
                                                      panic "invalid number"
                                                    )
                                                  )
                                                   (
                                                    quote (
                                                      
                                                    )
                                                  )
                                                )
                                                 (
                                                  set! exp (
                                                    * exp_sign (
                                                      let (
                                                        (
                                                          v14 exp_str
                                                        )
                                                      )
                                                       (
                                                        cond (
                                                          (
                                                            string? v14
                                                          )
                                                           (
                                                            exact (
                                                              floor (
                                                                string->number v14
                                                              )
                                                            )
                                                          )
                                                        )
                                                         (
                                                          (
                                                            boolean? v14
                                                          )
                                                           (
                                                            if v14 1 0
                                                          )
                                                        )
                                                         (
                                                          else (
                                                            exact (
                                                              floor v14
                                                            )
                                                          )
                                                        )
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
                                      if (
                                        not (
                                          equal? idx (
                                            _len s
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          panic "invalid number"
                                        )
                                      )
                                       (
                                        quote (
                                          
                                        )
                                      )
                                    )
                                     (
                                      if (
                                        equal? (
                                          _len int_part
                                        )
                                         0
                                      )
                                       (
                                        begin (
                                          set! int_part "0"
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
                                          num_str (
                                            string-append int_part frac_part
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          let (
                                            (
                                              numerator (
                                                let (
                                                  (
                                                    v15 num_str
                                                  )
                                                )
                                                 (
                                                  cond (
                                                    (
                                                      string? v15
                                                    )
                                                     (
                                                      exact (
                                                        floor (
                                                          string->number v15
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    (
                                                      boolean? v15
                                                    )
                                                     (
                                                      if v15 1 0
                                                    )
                                                  )
                                                   (
                                                    else (
                                                      exact (
                                                        floor v15
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
                                                equal? sign (
                                                  - 0 1
                                                )
                                              )
                                               (
                                                begin (
                                                  set! numerator (
                                                    - 0 numerator
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
                                                  denominator (
                                                    pow10 (
                                                      _len frac_part
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                begin (
                                                  if (
                                                    > exp 0
                                                  )
                                                   (
                                                    begin (
                                                      set! numerator (
                                                        * numerator (
                                                          pow10 exp
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    if (
                                                      < exp 0
                                                    )
                                                     (
                                                      begin (
                                                        set! denominator (
                                                          * denominator (
                                                            pow10 (
                                                              - exp
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
                                                )
                                                 (
                                                  ret7 (
                                                    alist->hash-table (
                                                      _list (
                                                        cons "numerator" numerator
                                                      )
                                                       (
                                                        cons "denominator" denominator
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
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
        reduce fr
      )
       (
        call/cc (
          lambda (
            ret16
          )
           (
            let (
              (
                g (
                  gcd (
                    hash-table-ref fr "numerator"
                  )
                   (
                    hash-table-ref fr "denominator"
                  )
                )
              )
            )
             (
              begin (
                ret16 (
                  alist->hash-table (
                    _list (
                      cons "numerator" (
                        _div (
                          hash-table-ref fr "numerator"
                        )
                         g
                      )
                    )
                     (
                      cons "denominator" (
                        _div (
                          hash-table-ref fr "denominator"
                        )
                         g
                      )
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
        decimal_to_fraction_str s
      )
       (
        call/cc (
          lambda (
            ret17
          )
           (
            ret17 (
              reduce (
                parse_decimal s
              )
            )
          )
        )
      )
    )
     (
      define (
        decimal_to_fraction x
      )
       (
        call/cc (
          lambda (
            ret18
          )
           (
            ret18 (
              decimal_to_fraction_str (
                to-str-space x
              )
            )
          )
        )
      )
    )
     (
      define (
        assert_fraction name fr num den
      )
       (
        call/cc (
          lambda (
            ret19
          )
           (
            if (
              or (
                not (
                  equal? (
                    hash-table-ref fr "numerator"
                  )
                   num
                )
              )
               (
                not (
                  equal? (
                    hash-table-ref fr "denominator"
                  )
                   den
                )
              )
            )
             (
              begin (
                panic name
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
      define (
        test_decimal_to_fraction
      )
       (
        call/cc (
          lambda (
            ret20
          )
           (
            begin (
              assert_fraction "case1" (
                decimal_to_fraction 2.0
              )
               2 1
            )
             (
              assert_fraction "case2" (
                decimal_to_fraction 89.0
              )
               89 1
            )
             (
              assert_fraction "case3" (
                decimal_to_fraction_str "67"
              )
               67 1
            )
             (
              assert_fraction "case4" (
                decimal_to_fraction_str "45.0"
              )
               45 1
            )
             (
              assert_fraction "case5" (
                decimal_to_fraction 1.5
              )
               3 2
            )
             (
              assert_fraction "case6" (
                decimal_to_fraction_str "6.25"
              )
               25 4
            )
             (
              assert_fraction "case7" (
                decimal_to_fraction 0.0
              )
               0 1
            )
             (
              assert_fraction "case8" (
                decimal_to_fraction (
                  - 2.5
                )
              )
               (
                - 5
              )
               2
            )
             (
              assert_fraction "case9" (
                decimal_to_fraction 0.125
              )
               1 8
            )
             (
              assert_fraction "case10" (
                decimal_to_fraction 1.00000025e+06
              )
               4000001 4
            )
             (
              assert_fraction "case11" (
                decimal_to_fraction 1.3333
              )
               13333 10000
            )
             (
              assert_fraction "case12" (
                decimal_to_fraction_str "1.23e2"
              )
               123 1
            )
             (
              assert_fraction "case13" (
                decimal_to_fraction_str "0.500"
              )
               1 2
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
            ret21
          )
           (
            begin (
              test_decimal_to_fraction
            )
             (
              let (
                (
                  fr (
                    decimal_to_fraction 1.5
                  )
                )
              )
               (
                begin (
                  _display (
                    if (
                      string? (
                        string-append (
                          string-append (
                            to-str-space (
                              hash-table-ref fr "numerator"
                            )
                          )
                           "/"
                        )
                         (
                          to-str-space (
                            hash-table-ref fr "denominator"
                          )
                        )
                      )
                    )
                     (
                      string-append (
                        string-append (
                          to-str-space (
                            hash-table-ref fr "numerator"
                          )
                        )
                         "/"
                      )
                       (
                        to-str-space (
                          hash-table-ref fr "denominator"
                        )
                      )
                    )
                     (
                      to-str (
                        string-append (
                          string-append (
                            to-str-space (
                              hash-table-ref fr "numerator"
                            )
                          )
                           "/"
                        )
                         (
                          to-str-space (
                            hash-table-ref fr "denominator"
                          )
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
      main
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
