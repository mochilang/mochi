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
      start50 (
        current-jiffy
      )
    )
     (
      jps53 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        index_of s ch
      )
       (
        call/cc (
          lambda (
            ret1
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
                                if (
                                  string=? (
                                    _substring s i (
                                      + i 1
                                    )
                                  )
                                   ch
                                )
                                 (
                                  begin (
                                    ret1 i
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
                ret1 (
                  - 1
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        ord ch
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            let (
              (
                upper "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
              )
            )
             (
              begin (
                let (
                  (
                    lower "abcdefghijklmnopqrstuvwxyz"
                  )
                )
                 (
                  begin (
                    let (
                      (
                        idx (
                          index_of upper ch
                        )
                      )
                    )
                     (
                      begin (
                        if (
                          _ge idx 0
                        )
                         (
                          begin (
                            ret4 (
                              _add 65 idx
                            )
                          )
                        )
                         (
                          quote (
                            
                          )
                        )
                      )
                       (
                        set! idx (
                          index_of lower ch
                        )
                      )
                       (
                        if (
                          _ge idx 0
                        )
                         (
                          begin (
                            ret4 (
                              _add 97 idx
                            )
                          )
                        )
                         (
                          quote (
                            
                          )
                        )
                      )
                       (
                        ret4 0
                      )
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
        chr n
      )
       (
        call/cc (
          lambda (
            ret5
          )
           (
            let (
              (
                upper "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
              )
            )
             (
              begin (
                let (
                  (
                    lower "abcdefghijklmnopqrstuvwxyz"
                  )
                )
                 (
                  begin (
                    if (
                      and (
                        >= n 65
                      )
                       (
                        < n 91
                      )
                    )
                     (
                      begin (
                        ret5 (
                          _substring upper (
                            - n 65
                          )
                           (
                            - n 64
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
                      and (
                        >= n 97
                      )
                       (
                        < n 123
                      )
                    )
                     (
                      begin (
                        ret5 (
                          _substring lower (
                            - n 97
                          )
                           (
                            - n 96
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
                    ret5 "?"
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
        text_to_bits text
      )
       (
        call/cc (
          lambda (
            ret6
          )
           (
            let (
              (
                bits ""
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
                                  < i (
                                    _len text
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        code (
                                          ord (
                                            _substring text i (
                                              + i 1
                                            )
                                          )
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            j 7
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
                                                          >= j 0
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                p (
                                                                  pow2 j
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                if (
                                                                  equal? (
                                                                    _mod (
                                                                      _div code p
                                                                    )
                                                                     2
                                                                  )
                                                                   1
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! bits (
                                                                      string-append bits "1"
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! bits (
                                                                      string-append bits "0"
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                set! j (
                                                                  - j 1
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
                                            set! i (
                                              + i 1
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
                    ret6 bits
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
        text_from_bits bits
      )
       (
        call/cc (
          lambda (
            ret11
          )
           (
            let (
              (
                text ""
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
                                  < i (
                                    _len bits
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        code 0
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
                                                          and (
                                                            < j 8
                                                          )
                                                           (
                                                            < (
                                                              + i j
                                                            )
                                                             (
                                                              _len bits
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            set! code (
                                                              * code 2
                                                            )
                                                          )
                                                           (
                                                            if (
                                                              string=? (
                                                                _substring bits (
                                                                  + i j
                                                                )
                                                                 (
                                                                  + (
                                                                    + i j
                                                                  )
                                                                   1
                                                                )
                                                              )
                                                               "1"
                                                            )
                                                             (
                                                              begin (
                                                                set! code (
                                                                  + code 1
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
                                            set! text (
                                              string-append text (
                                                chr code
                                              )
                                            )
                                          )
                                           (
                                            set! i (
                                              + i 8
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
                    ret11 text
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
        bool_to_string b
      )
       (
        call/cc (
          lambda (
            ret16
          )
           (
            begin (
              if b (
                begin (
                  ret16 "True"
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              ret16 "False"
            )
          )
        )
      )
    )
     (
      define (
        string_to_bitlist s
      )
       (
        call/cc (
          lambda (
            ret17
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
                                  < i (
                                    _len s
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      string=? (
                                        _substring s i (
                                          + i 1
                                        )
                                      )
                                       "1"
                                    )
                                     (
                                      begin (
                                        set! res (
                                          append res (
                                            _list 1
                                          )
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        set! res (
                                          append res (
                                            _list 0
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
                    ret17 res
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
        bitlist_to_string bits
      )
       (
        call/cc (
          lambda (
            ret20
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
                                    _len bits
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      equal? (
                                        list-ref-safe bits i
                                      )
                                       1
                                    )
                                     (
                                      begin (
                                        set! s (
                                          string-append s "1"
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        set! s (
                                          string-append s "0"
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
                    ret20 s
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
        is_power_of_two x
      )
       (
        call/cc (
          lambda (
            ret23
          )
           (
            begin (
              if (
                < x 1
              )
               (
                begin (
                  ret23 #f
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
                  p 1
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
                                < p x
                              )
                               (
                                begin (
                                  set! p (
                                    * p 2
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
                  ret23 (
                    equal? p x
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
        list_eq a b
      )
       (
        call/cc (
          lambda (
            ret26
          )
           (
            begin (
              if (
                not (
                  equal? (
                    _len a
                  )
                   (
                    _len b
                  )
                )
              )
               (
                begin (
                  ret26 #f
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
                                  _len a
                                )
                              )
                               (
                                begin (
                                  if (
                                    not (
                                      equal? (
                                        list-ref-safe a i
                                      )
                                       (
                                        list-ref-safe b i
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      ret26 #f
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
                  ret26 #t
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
            ret29
          )
           (
            let (
              (
                res 1
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
                                  < i e
                                )
                                 (
                                  begin (
                                    set! res (
                                      * res 2
                                    )
                                  )
                                   (
                                    set! i (
                                      + i 1
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
                    ret29 res
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
        has_bit n b
      )
       (
        call/cc (
          lambda (
            ret32
          )
           (
            let (
              (
                p (
                  pow2 b
                )
              )
            )
             (
              begin (
                if (
                  equal? (
                    _mod (
                      _div n p
                    )
                     2
                  )
                   1
                )
                 (
                  begin (
                    ret32 #t
                  )
                )
                 (
                  quote (
                    
                  )
                )
              )
               (
                ret32 #f
              )
            )
          )
        )
      )
    )
     (
      define (
        hamming_encode r data_bits
      )
       (
        call/cc (
          lambda (
            ret33
          )
           (
            let (
              (
                total (
                  + r (
                    _len data_bits
                  )
                )
              )
            )
             (
              begin (
                let (
                  (
                    data_ord (
                      _list
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        cont_data 0
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            x 1
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
                                          <= x total
                                        )
                                         (
                                          begin (
                                            if (
                                              is_power_of_two x
                                            )
                                             (
                                              begin (
                                                set! data_ord (
                                                  append data_ord (
                                                    _list (
                                                      - 1
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                set! data_ord (
                                                  append data_ord (
                                                    _list (
                                                      list-ref-safe data_bits cont_data
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                set! cont_data (
                                                  + cont_data 1
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
                            let (
                              (
                                parity (
                                  _list
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    bp 0
                                  )
                                )
                                 (
                                  begin (
                                    call/cc (
                                      lambda (
                                        break37
                                      )
                                       (
                                        letrec (
                                          (
                                            loop36 (
                                              lambda (
                                                
                                              )
                                               (
                                                if (
                                                  < bp r
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        cont_bo 0
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
                                                                break39
                                                              )
                                                               (
                                                                letrec (
                                                                  (
                                                                    loop38 (
                                                                      lambda (
                                                                        
                                                                      )
                                                                       (
                                                                        if (
                                                                          < j (
                                                                            _len data_ord
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                bit (
                                                                                  list-ref-safe data_ord j
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                if (
                                                                                  >= bit 0
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    let (
                                                                                      (
                                                                                        pos (
                                                                                          + j 1
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        if (
                                                                                          and (
                                                                                            has_bit pos bp
                                                                                          )
                                                                                           (
                                                                                            equal? bit 1
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            set! cont_bo (
                                                                                              + cont_bo 1
                                                                                            )
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
                                                                                  quote (
                                                                                    
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
                                                                           (
                                                                            loop38
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
                                                                  loop38
                                                                )
                                                              )
                                                            )
                                                          )
                                                           (
                                                            set! parity (
                                                              append parity (
                                                                _list (
                                                                  _mod cont_bo 2
                                                                )
                                                              )
                                                            )
                                                          )
                                                           (
                                                            set! bp (
                                                              + bp 1
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    loop36
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
                                          loop36
                                        )
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
                                            cont_bp 0
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
                                                    break41
                                                  )
                                                   (
                                                    letrec (
                                                      (
                                                        loop40 (
                                                          lambda (
                                                            
                                                          )
                                                           (
                                                            if (
                                                              < i (
                                                                _len data_ord
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                if (
                                                                  < (
                                                                    list-ref-safe data_ord i
                                                                  )
                                                                   0
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! result (
                                                                      append result (
                                                                        _list (
                                                                          list-ref-safe parity cont_bp
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    set! cont_bp (
                                                                      + cont_bp 1
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! result (
                                                                      append result (
                                                                        _list (
                                                                          list-ref-safe data_ord i
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
                                                                loop40
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
                                                      loop40
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                ret33 result
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
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
        hamming_decode r code
      )
       (
        call/cc (
          lambda (
            ret42
          )
           (
            let (
              (
                data_output (
                  _list
                )
              )
            )
             (
              begin (
                let (
                  (
                    parity_received (
                      _list
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        i 1
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            idx 0
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
                                          <= i (
                                            _len code
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              is_power_of_two i
                                            )
                                             (
                                              begin (
                                                set! parity_received (
                                                  append parity_received (
                                                    _list (
                                                      list-ref-safe code idx
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                set! data_output (
                                                  append data_output (
                                                    _list (
                                                      list-ref-safe code idx
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                           (
                                            set! idx (
                                              + idx 1
                                            )
                                          )
                                           (
                                            set! i (
                                              + i 1
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
                            let (
                              (
                                recomputed (
                                  hamming_encode r data_output
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    parity_calc (
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
                                                      < j (
                                                        _len recomputed
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        if (
                                                          is_power_of_two (
                                                            + j 1
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            set! parity_calc (
                                                              append parity_calc (
                                                                _list (
                                                                  cond (
                                                                    (
                                                                      string? recomputed
                                                                    )
                                                                     (
                                                                      _substring recomputed j (
                                                                        + j 1
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    (
                                                                      hash-table? recomputed
                                                                    )
                                                                     (
                                                                      hash-table-ref recomputed j
                                                                    )
                                                                  )
                                                                   (
                                                                    else (
                                                                      list-ref-safe recomputed j
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
                                                        set! j (
                                                          + j 1
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
                                        let (
                                          (
                                            ack (
                                              list_eq parity_received parity_calc
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            ret42 (
                                              alist->hash-table (
                                                _list (
                                                  cons "data" data_output
                                                )
                                                 (
                                                  cons "ack" ack
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
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
            ret47
          )
           (
            let (
              (
                sizePari 4
              )
            )
             (
              begin (
                let (
                  (
                    be 2
                  )
                )
                 (
                  begin (
                    let (
                      (
                        text "Message01"
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            binary (
                              text_to_bits text
                            )
                          )
                        )
                         (
                          begin (
                            _display (
                              if (
                                string? (
                                  string-append (
                                    string-append "Text input in binary is '" binary
                                  )
                                   "'"
                                )
                              )
                               (
                                string-append (
                                  string-append "Text input in binary is '" binary
                                )
                                 "'"
                              )
                               (
                                to-str (
                                  string-append (
                                    string-append "Text input in binary is '" binary
                                  )
                                   "'"
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
                                data_bits (
                                  string_to_bitlist binary
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    encoded (
                                      hamming_encode sizePari data_bits
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    _display (
                                      if (
                                        string? (
                                          string-append "Data converted ----------> " (
                                            bitlist_to_string encoded
                                          )
                                        )
                                      )
                                       (
                                        string-append "Data converted ----------> " (
                                          bitlist_to_string encoded
                                        )
                                      )
                                       (
                                        to-str (
                                          string-append "Data converted ----------> " (
                                            bitlist_to_string encoded
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
                                        decoded (
                                          hamming_decode sizePari encoded
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
                                                  string-append "Data receive ------------> " (
                                                    bitlist_to_string (
                                                      hash-table-ref decoded "data"
                                                    )
                                                  )
                                                )
                                                 " -- Data integrity: "
                                              )
                                               (
                                                bool_to_string (
                                                  hash-table-ref decoded "ack"
                                                )
                                              )
                                            )
                                          )
                                           (
                                            string-append (
                                              string-append (
                                                string-append "Data receive ------------> " (
                                                  bitlist_to_string (
                                                    hash-table-ref decoded "data"
                                                  )
                                                )
                                              )
                                               " -- Data integrity: "
                                            )
                                             (
                                              bool_to_string (
                                                hash-table-ref decoded "ack"
                                              )
                                            )
                                          )
                                           (
                                            to-str (
                                              string-append (
                                                string-append (
                                                  string-append "Data receive ------------> " (
                                                    bitlist_to_string (
                                                      hash-table-ref decoded "data"
                                                    )
                                                  )
                                                )
                                                 " -- Data integrity: "
                                              )
                                               (
                                                bool_to_string (
                                                  hash-table-ref decoded "ack"
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
                                        let (
                                          (
                                            corrupted (
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
                                                              < i (
                                                                _len encoded
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                set! corrupted (
                                                                  append corrupted (
                                                                    _list (
                                                                      cond (
                                                                        (
                                                                          string? encoded
                                                                        )
                                                                         (
                                                                          _substring encoded i (
                                                                            + i 1
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        (
                                                                          hash-table? encoded
                                                                        )
                                                                         (
                                                                          hash-table-ref encoded i
                                                                        )
                                                                      )
                                                                       (
                                                                        else (
                                                                          list-ref-safe encoded i
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
                                                let (
                                                  (
                                                    pos (
                                                      - be 1
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    if (
                                                      equal? (
                                                        list-ref-safe corrupted pos
                                                      )
                                                       0
                                                    )
                                                     (
                                                      begin (
                                                        list-set! corrupted pos 1
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        list-set! corrupted pos 0
                                                      )
                                                    )
                                                  )
                                                   (
                                                    let (
                                                      (
                                                        decoded_err (
                                                          hamming_decode sizePari corrupted
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
                                                                  string-append "Data receive (error) ----> " (
                                                                    bitlist_to_string (
                                                                      hash-table-ref decoded_err "data"
                                                                    )
                                                                  )
                                                                )
                                                                 " -- Data integrity: "
                                                              )
                                                               (
                                                                bool_to_string (
                                                                  hash-table-ref decoded_err "ack"
                                                                )
                                                              )
                                                            )
                                                          )
                                                           (
                                                            string-append (
                                                              string-append (
                                                                string-append "Data receive (error) ----> " (
                                                                  bitlist_to_string (
                                                                    hash-table-ref decoded_err "data"
                                                                  )
                                                                )
                                                              )
                                                               " -- Data integrity: "
                                                            )
                                                             (
                                                              bool_to_string (
                                                                hash-table-ref decoded_err "ack"
                                                              )
                                                            )
                                                          )
                                                           (
                                                            to-str (
                                                              string-append (
                                                                string-append (
                                                                  string-append "Data receive (error) ----> " (
                                                                    bitlist_to_string (
                                                                      hash-table-ref decoded_err "data"
                                                                    )
                                                                  )
                                                                )
                                                                 " -- Data integrity: "
                                                              )
                                                               (
                                                                bool_to_string (
                                                                  hash-table-ref decoded_err "ack"
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
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
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
     (
      let (
        (
          end51 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur52 (
              quotient (
                * (
                  - end51 start50
                )
                 1000000
              )
               jps53
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur52
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
