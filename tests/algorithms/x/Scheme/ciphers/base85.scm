;; Generated on 2025-08-06 22:04 +0700
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
      start35 (
        current-jiffy
      )
    )
     (
      jps38 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      let (
        (
          ascii85_chars "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstu"
        )
      )
       (
        begin (
          define (
            indexOf s ch
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
                    idx (
                      indexOf ascii85_chars ch
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
                          _add 33 idx
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
                begin (
                  if (
                    and (
                      >= n 33
                    )
                     (
                      <= n 117
                    )
                  )
                   (
                    begin (
                      ret5 (
                        _substring ascii85_chars (
                          - n 33
                        )
                         (
                          - n 32
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
         (
          define (
            to_binary n bits
          )
           (
            call/cc (
              lambda (
                ret6
              )
               (
                let (
                  (
                    b ""
                  )
                )
                 (
                  begin (
                    let (
                      (
                        val n
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
                                      > val 0
                                    )
                                     (
                                      begin (
                                        set! b (
                                          string-append (
                                            to-str-space (
                                              modulo val 2
                                            )
                                          )
                                           b
                                        )
                                      )
                                       (
                                        set! val (
                                          quotient val 2
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
                                      < (
                                        _len b
                                      )
                                       bits
                                    )
                                     (
                                      begin (
                                        set! b (
                                          string-append "0" b
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
                        if (
                          equal? (
                            _len b
                          )
                           0
                        )
                         (
                          begin (
                            set! b "0"
                          )
                        )
                         (
                          quote (
                            
                          )
                        )
                      )
                       (
                        ret6 b
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
            bin_to_int bits
          )
           (
            call/cc (
              lambda (
                ret11
              )
               (
                let (
                  (
                    n 0
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
                                        if (
                                          string=? (
                                            _substring bits i (
                                              + i 1
                                            )
                                          )
                                           "1"
                                        )
                                         (
                                          begin (
                                            set! n (
                                              + (
                                                * n 2
                                              )
                                               1
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            set! n (
                                              * n 2
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
                        ret11 n
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
            reverse s
          )
           (
            call/cc (
              lambda (
                ret14
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
                        i (
                          - (
                            _len s
                          )
                           1
                        )
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
                                      >= i 0
                                    )
                                     (
                                      begin (
                                        set! res (
                                          string-append res (
                                            _substring s i (
                                              + i 1
                                            )
                                          )
                                        )
                                      )
                                       (
                                        set! i (
                                          - i 1
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
                        ret14 res
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
            base10_to_85 d
          )
           (
            call/cc (
              lambda (
                ret17
              )
               (
                begin (
                  if (
                    > d 0
                  )
                   (
                    begin (
                      ret17 (
                        _add (
                          chr (
                            + (
                              modulo d 85
                            )
                             33
                          )
                        )
                         (
                          base10_to_85 (
                            quotient d 85
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
                  ret17 ""
                )
              )
            )
          )
        )
         (
          define (
            base85_to_10 digits
          )
           (
            call/cc (
              lambda (
                ret18
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
                                      < i (
                                        _len digits
                                      )
                                    )
                                     (
                                      begin (
                                        set! value (
                                          _add (
                                            * value 85
                                          )
                                           (
                                            - (
                                              ord (
                                                _substring digits i (
                                                  + i 1
                                                )
                                              )
                                            )
                                             33
                                          )
                                        )
                                      )
                                       (
                                        set! i (
                                          + i 1
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
                        ret18 value
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
            ascii85_encode data
          )
           (
            call/cc (
              lambda (
                ret21
              )
               (
                let (
                  (
                    binary_data ""
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
                                xs
                              )
                               (
                                if (
                                  null? xs
                                )
                                 (
                                  quote (
                                    
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        ch (
                                          string (
                                            car xs
                                          )
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        set! binary_data (
                                          string-append binary_data (
                                            to_binary (
                                              ord ch
                                            )
                                             8
                                          )
                                        )
                                      )
                                    )
                                  )
                                   (
                                    loop22 (
                                      cdr xs
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                         (
                          loop22 (
                            string->list data
                          )
                        )
                      )
                    )
                  )
                   (
                    let (
                      (
                        null_values (
                          quotient (
                            - (
                              * 32 (
                                + (
                                  quotient (
                                    _len binary_data
                                  )
                                   32
                                )
                                 1
                              )
                            )
                             (
                              _len binary_data
                            )
                          )
                           8
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            total_bits (
                              * 32 (
                                + (
                                  quotient (
                                    _len binary_data
                                  )
                                   32
                                )
                                 1
                              )
                            )
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
                                          < (
                                            _len binary_data
                                          )
                                           total_bits
                                        )
                                         (
                                          begin (
                                            set! binary_data (
                                              string-append binary_data "0"
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
                                result ""
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
                                                    _len binary_data
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        chunk_bits (
                                                          _substring binary_data i (
                                                            + i 32
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            chunk_val (
                                                              bin_to_int chunk_bits
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                encoded (
                                                                  reverse (
                                                                    base10_to_85 chunk_val
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                set! result (
                                                                  string-append result encoded
                                                                )
                                                              )
                                                               (
                                                                set! i (
                                                                  + i 32
                                                                )
                                                              )
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
                                    if (
                                      not (
                                        equal? (
                                          modulo null_values 4
                                        )
                                         0
                                      )
                                    )
                                     (
                                      begin (
                                        set! result (
                                          _substring result 0 (
                                            - (
                                              _len result
                                            )
                                             null_values
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
                                    ret21 result
                                  )
                                )
                              )
                            )
                          )
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
            ascii85_decode data
          )
           (
            call/cc (
              lambda (
                ret28
              )
               (
                let (
                  (
                    null_values (
                      - (
                        * 5 (
                          + (
                            quotient (
                              _len data
                            )
                             5
                          )
                           1
                        )
                      )
                       (
                        _len data
                      )
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        binary_data data
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
                                          < i null_values
                                        )
                                         (
                                          begin (
                                            set! binary_data (
                                              string-append binary_data "u"
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
                            let (
                              (
                                result ""
                              )
                            )
                             (
                              begin (
                                set! i 0
                              )
                               (
                                call/cc (
                                  lambda (
                                    break32
                                  )
                                   (
                                    letrec (
                                      (
                                        loop31 (
                                          lambda (
                                            
                                          )
                                           (
                                            if (
                                              < i (
                                                _len binary_data
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    chunk (
                                                      _substring binary_data i (
                                                        + i 5
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        value (
                                                          base85_to_10 chunk
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            bits (
                                                              to_binary value 32
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
                                                                              < j 32
                                                                            )
                                                                             (
                                                                              begin (
                                                                                let (
                                                                                  (
                                                                                    byte_bits (
                                                                                      if (
                                                                                        string? bits
                                                                                      )
                                                                                       (
                                                                                        _substring bits j (
                                                                                          + j 8
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        take (
                                                                                          drop bits j
                                                                                        )
                                                                                         (
                                                                                          - (
                                                                                            + j 8
                                                                                          )
                                                                                           j
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
                                                                                          chr (
                                                                                            bin_to_int byte_bits
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        set! result (
                                                                                          string-append result c
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        set! j (
                                                                                          + j 8
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
                                                                set! i (
                                                                  + i 5
                                                                )
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
                                                loop31
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
                                      loop31
                                    )
                                  )
                                )
                              )
                               (
                                let (
                                  (
                                    trim null_values
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      equal? (
                                        modulo null_values 5
                                      )
                                       0
                                    )
                                     (
                                      begin (
                                        set! trim (
                                          - null_values 1
                                        )
                                      )
                                    )
                                     (
                                      quote (
                                        
                                      )
                                    )
                                  )
                                   (
                                    ret28 (
                                      _substring result 0 (
                                        - (
                                          _len result
                                        )
                                         trim
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
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
                ascii85_encode ""
              )
            )
             (
              ascii85_encode ""
            )
             (
              to-str (
                ascii85_encode ""
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
                ascii85_encode "12345"
              )
            )
             (
              ascii85_encode "12345"
            )
             (
              to-str (
                ascii85_encode "12345"
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
                ascii85_encode "base 85"
              )
            )
             (
              ascii85_encode "base 85"
            )
             (
              to-str (
                ascii85_encode "base 85"
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
                ascii85_decode ""
              )
            )
             (
              ascii85_decode ""
            )
             (
              to-str (
                ascii85_decode ""
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
                ascii85_decode "0etOA2#"
              )
            )
             (
              ascii85_decode "0etOA2#"
            )
             (
              to-str (
                ascii85_decode "0etOA2#"
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
                ascii85_decode "@UX=h+?24"
              )
            )
             (
              ascii85_decode "@UX=h+?24"
            )
             (
              to-str (
                ascii85_decode "@UX=h+?24"
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
          end36 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur37 (
              quotient (
                * (
                  - end36 start35
                )
                 1000000
              )
               jps38
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur37
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
