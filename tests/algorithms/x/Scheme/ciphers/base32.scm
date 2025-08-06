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
      start27 (
        current-jiffy
      )
    )
     (
      jps30 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      let (
        (
          B32_CHARSET "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567"
        )
      )
       (
        begin (
          define (
            indexOfChar s ch
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
                            digits "0123456789"
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                idx (
                                  indexOfChar upper ch
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
                                  indexOfChar lower ch
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
                                set! idx (
                                  indexOfChar digits ch
                                )
                              )
                               (
                                if (
                                  _ge idx 0
                                )
                                 (
                                  begin (
                                    ret4 (
                                      _add 48 idx
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
                                  string=? ch " "
                                )
                                 (
                                  begin (
                                    ret4 32
                                  )
                                )
                                 (
                                  quote (
                                    
                                  )
                                )
                              )
                               (
                                if (
                                  string=? ch "!"
                                )
                                 (
                                  begin (
                                    ret4 33
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
          )
        )
         (
          define (
            chr code
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
                        let (
                          (
                            digits "0123456789"
                          )
                        )
                         (
                          begin (
                            if (
                              equal? code 32
                            )
                             (
                              begin (
                                ret5 " "
                              )
                            )
                             (
                              quote (
                                
                              )
                            )
                          )
                           (
                            if (
                              equal? code 33
                            )
                             (
                              begin (
                                ret5 "!"
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
                                idx (
                                  - code 65
                                )
                              )
                            )
                             (
                              begin (
                                if (
                                  and (
                                    >= idx 0
                                  )
                                   (
                                    < idx (
                                      _len upper
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    ret5 (
                                      _substring upper idx (
                                        + idx 1
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
                                set! idx (
                                  - code 97
                                )
                              )
                               (
                                if (
                                  and (
                                    >= idx 0
                                  )
                                   (
                                    < idx (
                                      _len lower
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    ret5 (
                                      _substring lower idx (
                                        + idx 1
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
                                set! idx (
                                  - code 48
                                )
                              )
                               (
                                if (
                                  and (
                                    >= idx 0
                                  )
                                   (
                                    < idx (
                                      _len digits
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    ret5 (
                                      _substring digits idx (
                                        + idx 1
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
                                ret5 ""
                              )
                            )
                          )
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
            repeat s n
          )
           (
            call/cc (
              lambda (
                ret6
              )
               (
                let (
                  (
                    out ""
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
                                      < i n
                                    )
                                     (
                                      begin (
                                        set! out (
                                          string-append out s
                                        )
                                      )
                                       (
                                        set! i (
                                          + i 1
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
                        ret6 out
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
            to_binary n bits
          )
           (
            call/cc (
              lambda (
                ret9
              )
               (
                let (
                  (
                    v n
                  )
                )
                 (
                  begin (
                    let (
                      (
                        out ""
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
                                          < i bits
                                        )
                                         (
                                          begin (
                                            set! out (
                                              string-append (
                                                to-str-space (
                                                  modulo v 2
                                                )
                                              )
                                               out
                                            )
                                          )
                                           (
                                            set! v (
                                              quotient v 2
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
                            ret9 out
                          )
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
            binary_to_int bits
          )
           (
            call/cc (
              lambda (
                ret12
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
                                      < i (
                                        _len bits
                                      )
                                    )
                                     (
                                      begin (
                                        set! n (
                                          * n 2
                                        )
                                      )
                                       (
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
                                              + n 1
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
                        ret12 n
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
            base32_encode data
          )
           (
            call/cc (
              lambda (
                ret15
              )
               (
                let (
                  (
                    binary_data ""
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
                                      < i (
                                        _len data
                                      )
                                    )
                                     (
                                      begin (
                                        set! binary_data (
                                          string-append binary_data (
                                            to_binary (
                                              ord (
                                                _substring data i (
                                                  + i 1
                                                )
                                              )
                                            )
                                             8
                                          )
                                        )
                                      )
                                       (
                                        set! i (
                                          + i 1
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
                        let (
                          (
                            remainder (
                              modulo (
                                _len binary_data
                              )
                               5
                            )
                          )
                        )
                         (
                          begin (
                            if (
                              not (
                                equal? remainder 0
                              )
                            )
                             (
                              begin (
                                set! binary_data (
                                  string-append binary_data (
                                    repeat "0" (
                                      - 5 remainder
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
                                b32_result ""
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
                                                  < j (
                                                    _len binary_data
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        chunk (
                                                          _substring binary_data j (
                                                            + j 5
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            index (
                                                              binary_to_int chunk
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            set! b32_result (
                                                              string-append b32_result (
                                                                _substring B32_CHARSET index (
                                                                  + index 1
                                                                )
                                                              )
                                                            )
                                                          )
                                                           (
                                                            set! j (
                                                              + j 5
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
                                    let (
                                      (
                                        rem (
                                          modulo (
                                            _len b32_result
                                          )
                                           8
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        if (
                                          not (
                                            equal? rem 0
                                          )
                                        )
                                         (
                                          begin (
                                            set! b32_result (
                                              string-append b32_result (
                                                repeat "=" (
                                                  - 8 rem
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
                                        ret15 b32_result
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
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
            base32_decode data
          )
           (
            call/cc (
              lambda (
                ret20
              )
               (
                let (
                  (
                    clean ""
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
                                        _len data
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            ch (
                                              _substring data i (
                                                + i 1
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              not (
                                                string=? ch "="
                                              )
                                            )
                                             (
                                              begin (
                                                set! clean (
                                                  string-append clean ch
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
                        let (
                          (
                            binary_chunks ""
                          )
                        )
                         (
                          begin (
                            set! i 0
                          )
                           (
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
                                            _len clean
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                idx (
                                                  indexOfChar B32_CHARSET (
                                                    _substring clean i (
                                                      + i 1
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                set! binary_chunks (
                                                  string-append binary_chunks (
                                                    to_binary idx 5
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
                            let (
                              (
                                result ""
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
                                        break26
                                      )
                                       (
                                        letrec (
                                          (
                                            loop25 (
                                              lambda (
                                                
                                              )
                                               (
                                                if (
                                                  <= (
                                                    + j 8
                                                  )
                                                   (
                                                    _len binary_chunks
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        byte_bits (
                                                          _substring binary_chunks j (
                                                            + j 8
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            code (
                                                              binary_to_int byte_bits
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            set! result (
                                                              string-append result (
                                                                chr code
                                                              )
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
                                                    loop25
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
                                          loop25
                                        )
                                      )
                                    )
                                  )
                                   (
                                    ret20 result
                                  )
                                )
                              )
                            )
                          )
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
                base32_encode "Hello World!"
              )
            )
             (
              base32_encode "Hello World!"
            )
             (
              to-str (
                base32_encode "Hello World!"
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
                base32_encode "123456"
              )
            )
             (
              base32_encode "123456"
            )
             (
              to-str (
                base32_encode "123456"
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
                base32_encode "some long complex string"
              )
            )
             (
              base32_encode "some long complex string"
            )
             (
              to-str (
                base32_encode "some long complex string"
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
                base32_decode "JBSWY3DPEBLW64TMMQQQ===="
              )
            )
             (
              base32_decode "JBSWY3DPEBLW64TMMQQQ===="
            )
             (
              to-str (
                base32_decode "JBSWY3DPEBLW64TMMQQQ===="
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
                base32_decode "GEZDGNBVGY======"
              )
            )
             (
              base32_decode "GEZDGNBVGY======"
            )
             (
              to-str (
                base32_decode "GEZDGNBVGY======"
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
                base32_decode "ONXW2ZJANRXW4ZZAMNXW24DMMV4CA43UOJUW4ZY="
              )
            )
             (
              base32_decode "ONXW2ZJANRXW4ZZAMNXW24DMMV4CA43UOJUW4ZY="
            )
             (
              to-str (
                base32_decode "ONXW2ZJANRXW4ZZAMNXW24DMMV4CA43UOJUW4ZY="
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
          end28 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur29 (
              quotient (
                * (
                  - end28 start27
                )
                 1000000
              )
               jps30
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur29
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
