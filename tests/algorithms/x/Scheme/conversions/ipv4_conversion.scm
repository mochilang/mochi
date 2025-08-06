;; Generated on 2025-08-06 23:15 +0700
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
      start28 (
        current-jiffy
      )
    )
     (
      jps31 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      let (
        (
          hex_digits "0123456789abcdef"
        )
      )
       (
        begin (
          define (
            split_by_dot s
          )
           (
            call/cc (
              lambda (
                ret1
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
                        current ""
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
                                            _len s
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                c (
                                                  _substring s i (
                                                    + i 1
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                if (
                                                  string=? c "."
                                                )
                                                 (
                                                  begin (
                                                    set! res (
                                                      append res (
                                                        _list current
                                                      )
                                                    )
                                                  )
                                                   (
                                                    set! current ""
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    set! current (
                                                      string-append current c
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
                            set! res (
                              append res (
                                _list current
                              )
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
         (
          define (
            parse_decimal s
          )
           (
            call/cc (
              lambda (
                ret4
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
                      panic "Invalid IPv4 address format"
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
                      value 0
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
                                          _len s
                                        )
                                      )
                                       (
                                        begin (
                                          let (
                                            (
                                              c (
                                                _substring s i (
                                                  + i 1
                                                )
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              if (
                                                or (
                                                  string<? c "0"
                                                )
                                                 (
                                                  string>? c "9"
                                                )
                                              )
                                               (
                                                begin (
                                                  panic "Invalid IPv4 address format"
                                                )
                                              )
                                               (
                                                quote (
                                                  
                                                )
                                              )
                                            )
                                             (
                                              set! value (
                                                + (
                                                  * value 10
                                                )
                                                 (
                                                  let (
                                                    (
                                                      v7 c
                                                    )
                                                  )
                                                   (
                                                    cond (
                                                      (
                                                        string? v7
                                                      )
                                                       (
                                                        inexact->exact (
                                                          floor (
                                                            string->number v7
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      (
                                                        boolean? v7
                                                      )
                                                       (
                                                        if v7 1 0
                                                      )
                                                    )
                                                     (
                                                      else (
                                                        inexact->exact (
                                                          floor v7
                                                        )
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
                          ret4 value
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
            to_hex2 n
          )
           (
            call/cc (
              lambda (
                ret8
              )
               (
                let (
                  (
                    x n
                  )
                )
                 (
                  begin (
                    let (
                      (
                        res ""
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
                                      > x 0
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            d (
                                              _mod x 16
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            set! res (
                                              string-append (
                                                _substring hex_digits d (
                                                  + d 1
                                                )
                                              )
                                               res
                                            )
                                          )
                                           (
                                            set! x (
                                              _div x 16
                                            )
                                          )
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
                                      < (
                                        _len res
                                      )
                                       2
                                    )
                                     (
                                      begin (
                                        set! res (
                                          string-append "0" res
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
                        ret8 res
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
            ipv4_to_decimal ipv4_address
          )
           (
            call/cc (
              lambda (
                ret13
              )
               (
                let (
                  (
                    parts (
                      split_by_dot ipv4_address
                    )
                  )
                )
                 (
                  begin (
                    if (
                      not (
                        equal? (
                          _len parts
                        )
                         4
                      )
                    )
                     (
                      begin (
                        panic "Invalid IPv4 address format"
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
                        result 0
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
                                          < i 4
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                oct (
                                                  parse_decimal (
                                                    cond (
                                                      (
                                                        string? parts
                                                      )
                                                       (
                                                        _substring parts i (
                                                          + i 1
                                                        )
                                                      )
                                                    )
                                                     (
                                                      (
                                                        hash-table? parts
                                                      )
                                                       (
                                                        hash-table-ref parts i
                                                      )
                                                    )
                                                     (
                                                      else (
                                                        list-ref parts i
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                if (
                                                  or (
                                                    _lt oct 0
                                                  )
                                                   (
                                                    _gt oct 255
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    panic (
                                                      string-append "Invalid IPv4 octet " (
                                                        to-str-space oct
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
                                                set! result (
                                                  _add (
                                                    * result 256
                                                  )
                                                   oct
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
                            ret13 result
                          )
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
            alt_ipv4_to_decimal ipv4_address
          )
           (
            call/cc (
              lambda (
                ret16
              )
               (
                let (
                  (
                    parts (
                      split_by_dot ipv4_address
                    )
                  )
                )
                 (
                  begin (
                    if (
                      not (
                        equal? (
                          _len parts
                        )
                         4
                      )
                    )
                     (
                      begin (
                        panic "Invalid IPv4 address format"
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
                        hex_str ""
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
                                          < i 4
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                oct (
                                                  parse_decimal (
                                                    cond (
                                                      (
                                                        string? parts
                                                      )
                                                       (
                                                        _substring parts i (
                                                          + i 1
                                                        )
                                                      )
                                                    )
                                                     (
                                                      (
                                                        hash-table? parts
                                                      )
                                                       (
                                                        hash-table-ref parts i
                                                      )
                                                    )
                                                     (
                                                      else (
                                                        list-ref parts i
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                if (
                                                  or (
                                                    _lt oct 0
                                                  )
                                                   (
                                                    _gt oct 255
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    panic (
                                                      string-append "Invalid IPv4 octet " (
                                                        to-str-space oct
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
                                                set! hex_str (
                                                  string-append hex_str (
                                                    to_hex2 oct
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
                            let (
                              (
                                value 0
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
                                                  < k (
                                                    _len hex_str
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        c (
                                                          _substring hex_str k (
                                                            + k 1
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            digit (
                                                              - 0 1
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
                                                                              < j (
                                                                                _len hex_digits
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                if (
                                                                                  string=? (
                                                                                    _substring hex_digits j (
                                                                                      + j 1
                                                                                    )
                                                                                  )
                                                                                   c
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    set! digit j
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
                                                                if (
                                                                  < digit 0
                                                                )
                                                                 (
                                                                  begin (
                                                                    panic "Invalid hex digit"
                                                                  )
                                                                )
                                                                 (
                                                                  quote (
                                                                    
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                set! value (
                                                                  + (
                                                                    * value 16
                                                                  )
                                                                   digit
                                                                )
                                                              )
                                                               (
                                                                set! k (
                                                                  + k 1
                                                                )
                                                              )
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
                                    ret16 value
                                  )
                                )
                              )
                            )
                          )
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
            decimal_to_ipv4 decimal_ipv4
          )
           (
            call/cc (
              lambda (
                ret23
              )
               (
                begin (
                  if (
                    or (
                      < decimal_ipv4 0
                    )
                     (
                      > decimal_ipv4 4294967295
                    )
                  )
                   (
                    begin (
                      panic "Invalid decimal IPv4 address"
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
                      n decimal_ipv4
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          parts (
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
                                            < i 4
                                          )
                                           (
                                            begin (
                                              let (
                                                (
                                                  octet (
                                                    _mod n 256
                                                  )
                                                )
                                              )
                                               (
                                                begin (
                                                  set! parts (
                                                    append parts (
                                                      _list (
                                                        to-str-space octet
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  set! n (
                                                    _div n 256
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
                                  res ""
                                )
                              )
                               (
                                begin (
                                  let (
                                    (
                                      j (
                                        - (
                                          _len parts
                                        )
                                         1
                                      )
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
                                                    >= j 0
                                                  )
                                                   (
                                                    begin (
                                                      set! res (
                                                        string-append res (
                                                          list-ref parts j
                                                        )
                                                      )
                                                    )
                                                     (
                                                      if (
                                                        > j 0
                                                      )
                                                       (
                                                        begin (
                                                          set! res (
                                                            string-append res "."
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
                                                        - j 1
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
                                      ret23 res
                                    )
                                  )
                                )
                              )
                            )
                          )
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
                ipv4_to_decimal "192.168.0.1"
              )
            )
             (
              ipv4_to_decimal "192.168.0.1"
            )
             (
              to-str (
                ipv4_to_decimal "192.168.0.1"
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
                ipv4_to_decimal "10.0.0.255"
              )
            )
             (
              ipv4_to_decimal "10.0.0.255"
            )
             (
              to-str (
                ipv4_to_decimal "10.0.0.255"
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
                alt_ipv4_to_decimal "192.168.0.1"
              )
            )
             (
              alt_ipv4_to_decimal "192.168.0.1"
            )
             (
              to-str (
                alt_ipv4_to_decimal "192.168.0.1"
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
                alt_ipv4_to_decimal "10.0.0.255"
              )
            )
             (
              alt_ipv4_to_decimal "10.0.0.255"
            )
             (
              to-str (
                alt_ipv4_to_decimal "10.0.0.255"
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
                decimal_to_ipv4 3232235521
              )
            )
             (
              decimal_to_ipv4 3232235521
            )
             (
              to-str (
                decimal_to_ipv4 3232235521
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
                decimal_to_ipv4 167772415
              )
            )
             (
              decimal_to_ipv4 167772415
            )
             (
              to-str (
                decimal_to_ipv4 167772415
              )
            )
          )
        )
         (
          newline
        )
      )
    )
     (
      let (
        (
          end29 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur30 (
              quotient (
                * (
                  - end29 start28
                )
                 1000000
              )
               jps31
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur30
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
