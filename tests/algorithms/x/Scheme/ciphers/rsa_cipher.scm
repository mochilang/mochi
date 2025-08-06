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
          BYTE_SIZE 256
        )
      )
       (
        begin (
          define (
            pow_int base exp
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
                                      < i exp
                                    )
                                     (
                                      begin (
                                        set! result (
                                          * result base
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
            mod_pow base exponent modulus
          )
           (
            call/cc (
              lambda (
                ret4
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
                        b (
                          _mod base modulus
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            e exponent
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
                                          > e 0
                                        )
                                         (
                                          begin (
                                            if (
                                              equal? (
                                                _mod e 2
                                              )
                                               1
                                            )
                                             (
                                              begin (
                                                set! result (
                                                  _mod (
                                                    * result b
                                                  )
                                                   modulus
                                                )
                                              )
                                            )
                                             (
                                              quote (
                                                
                                              )
                                            )
                                          )
                                           (
                                            set! e (
                                              _div e 2
                                            )
                                          )
                                           (
                                            set! b (
                                              _mod (
                                                * b b
                                              )
                                               modulus
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
                            ret4 result
                          )
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
            ord ch
          )
           (
            call/cc (
              lambda (
                ret7
              )
               (
                begin (
                  if (
                    string=? ch " "
                  )
                   (
                    begin (
                      ret7 32
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  if (
                    string=? ch "a"
                  )
                   (
                    begin (
                      ret7 97
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  if (
                    string=? ch "b"
                  )
                   (
                    begin (
                      ret7 98
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  if (
                    string=? ch "c"
                  )
                   (
                    begin (
                      ret7 99
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  if (
                    string=? ch "d"
                  )
                   (
                    begin (
                      ret7 100
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  if (
                    string=? ch "e"
                  )
                   (
                    begin (
                      ret7 101
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  if (
                    string=? ch "f"
                  )
                   (
                    begin (
                      ret7 102
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  if (
                    string=? ch "g"
                  )
                   (
                    begin (
                      ret7 103
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  if (
                    string=? ch "h"
                  )
                   (
                    begin (
                      ret7 104
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  if (
                    string=? ch "i"
                  )
                   (
                    begin (
                      ret7 105
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  if (
                    string=? ch "j"
                  )
                   (
                    begin (
                      ret7 106
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  if (
                    string=? ch "k"
                  )
                   (
                    begin (
                      ret7 107
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  if (
                    string=? ch "l"
                  )
                   (
                    begin (
                      ret7 108
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  if (
                    string=? ch "m"
                  )
                   (
                    begin (
                      ret7 109
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  if (
                    string=? ch "n"
                  )
                   (
                    begin (
                      ret7 110
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  if (
                    string=? ch "o"
                  )
                   (
                    begin (
                      ret7 111
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  if (
                    string=? ch "p"
                  )
                   (
                    begin (
                      ret7 112
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  if (
                    string=? ch "q"
                  )
                   (
                    begin (
                      ret7 113
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  if (
                    string=? ch "r"
                  )
                   (
                    begin (
                      ret7 114
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  if (
                    string=? ch "s"
                  )
                   (
                    begin (
                      ret7 115
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  if (
                    string=? ch "t"
                  )
                   (
                    begin (
                      ret7 116
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  if (
                    string=? ch "u"
                  )
                   (
                    begin (
                      ret7 117
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  if (
                    string=? ch "v"
                  )
                   (
                    begin (
                      ret7 118
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  if (
                    string=? ch "w"
                  )
                   (
                    begin (
                      ret7 119
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  if (
                    string=? ch "x"
                  )
                   (
                    begin (
                      ret7 120
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  if (
                    string=? ch "y"
                  )
                   (
                    begin (
                      ret7 121
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  if (
                    string=? ch "z"
                  )
                   (
                    begin (
                      ret7 122
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  ret7 0
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
                ret8
              )
               (
                begin (
                  if (
                    equal? code 32
                  )
                   (
                    begin (
                      ret8 " "
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  if (
                    equal? code 97
                  )
                   (
                    begin (
                      ret8 "a"
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  if (
                    equal? code 98
                  )
                   (
                    begin (
                      ret8 "b"
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  if (
                    equal? code 99
                  )
                   (
                    begin (
                      ret8 "c"
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  if (
                    equal? code 100
                  )
                   (
                    begin (
                      ret8 "d"
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  if (
                    equal? code 101
                  )
                   (
                    begin (
                      ret8 "e"
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  if (
                    equal? code 102
                  )
                   (
                    begin (
                      ret8 "f"
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  if (
                    equal? code 103
                  )
                   (
                    begin (
                      ret8 "g"
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  if (
                    equal? code 104
                  )
                   (
                    begin (
                      ret8 "h"
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  if (
                    equal? code 105
                  )
                   (
                    begin (
                      ret8 "i"
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  if (
                    equal? code 106
                  )
                   (
                    begin (
                      ret8 "j"
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  if (
                    equal? code 107
                  )
                   (
                    begin (
                      ret8 "k"
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  if (
                    equal? code 108
                  )
                   (
                    begin (
                      ret8 "l"
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  if (
                    equal? code 109
                  )
                   (
                    begin (
                      ret8 "m"
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  if (
                    equal? code 110
                  )
                   (
                    begin (
                      ret8 "n"
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  if (
                    equal? code 111
                  )
                   (
                    begin (
                      ret8 "o"
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  if (
                    equal? code 112
                  )
                   (
                    begin (
                      ret8 "p"
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  if (
                    equal? code 113
                  )
                   (
                    begin (
                      ret8 "q"
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  if (
                    equal? code 114
                  )
                   (
                    begin (
                      ret8 "r"
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  if (
                    equal? code 115
                  )
                   (
                    begin (
                      ret8 "s"
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  if (
                    equal? code 116
                  )
                   (
                    begin (
                      ret8 "t"
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  if (
                    equal? code 117
                  )
                   (
                    begin (
                      ret8 "u"
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  if (
                    equal? code 118
                  )
                   (
                    begin (
                      ret8 "v"
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  if (
                    equal? code 119
                  )
                   (
                    begin (
                      ret8 "w"
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  if (
                    equal? code 120
                  )
                   (
                    begin (
                      ret8 "x"
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  if (
                    equal? code 121
                  )
                   (
                    begin (
                      ret8 "y"
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  if (
                    equal? code 122
                  )
                   (
                    begin (
                      ret8 "z"
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  ret8 ""
                )
              )
            )
          )
        )
         (
          define (
            get_blocks_from_text message block_size
          )
           (
            call/cc (
              lambda (
                ret9
              )
               (
                let (
                  (
                    block_ints (
                      _list
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        block_start 0
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
                                      < block_start (
                                        _len message
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            block_int 0
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                i block_start
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
                                                              and (
                                                                < i (
                                                                  + block_start block_size
                                                                )
                                                              )
                                                               (
                                                                < i (
                                                                  _len message
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                set! block_int (
                                                                  _add block_int (
                                                                    * (
                                                                      ord (
                                                                        _substring message i (
                                                                          + i 1
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      pow_int BYTE_SIZE (
                                                                        - i block_start
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
                                                set! block_ints (
                                                  append block_ints (
                                                    _list block_int
                                                  )
                                                )
                                              )
                                               (
                                                set! block_start (
                                                  + block_start block_size
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
                       (
                        ret9 block_ints
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
            get_text_from_blocks block_ints message_length block_size
          )
           (
            call/cc (
              lambda (
                ret14
              )
               (
                let (
                  (
                    message ""
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
                                        block_int (
                                          car xs
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            block block_int
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                i (
                                                  - block_size 1
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    block_message ""
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
                                                                  >= i 0
                                                                )
                                                                 (
                                                                  begin (
                                                                    if (
                                                                      < (
                                                                        + (
                                                                          _len message
                                                                        )
                                                                         i
                                                                      )
                                                                       message_length
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            ascii_number (
                                                                              _div block (
                                                                                pow_int BYTE_SIZE i
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            set! block (
                                                                              _mod block (
                                                                                pow_int BYTE_SIZE i
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            set! block_message (
                                                                              string-append (
                                                                                chr ascii_number
                                                                              )
                                                                               block_message
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
                                                                    set! i (
                                                                      - i 1
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
                                                    set! message (
                                                      string-append message block_message
                                                    )
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
                                    loop15 (
                                      cdr xs
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                         (
                          loop15 block_ints
                        )
                      )
                    )
                  )
                   (
                    ret14 message
                  )
                )
              )
            )
          )
        )
         (
          define (
            encrypt_message message n e block_size
          )
           (
            call/cc (
              lambda (
                ret19
              )
               (
                let (
                  (
                    encrypted (
                      _list
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        blocks (
                          get_blocks_from_text message block_size
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
                                            block (
                                              car xs
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            set! encrypted (
                                              append encrypted (
                                                _list (
                                                  mod_pow block e n
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                       (
                                        loop20 (
                                          cdr xs
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                             (
                              loop20 blocks
                            )
                          )
                        )
                      )
                       (
                        ret19 encrypted
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
            decrypt_message blocks message_length n d block_size
          )
           (
            call/cc (
              lambda (
                ret22
              )
               (
                let (
                  (
                    decrypted_blocks (
                      _list
                    )
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
                                        block (
                                          car xs
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        set! decrypted_blocks (
                                          append decrypted_blocks (
                                            _list (
                                              mod_pow block d n
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                   (
                                    loop23 (
                                      cdr xs
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                         (
                          loop23 blocks
                        )
                      )
                    )
                  )
                   (
                    let (
                      (
                        message ""
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
                                            num (
                                              car xs
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            set! message (
                                              string-append message (
                                                chr num
                                              )
                                            )
                                          )
                                        )
                                      )
                                       (
                                        loop25 (
                                          cdr xs
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                             (
                              loop25 decrypted_blocks
                            )
                          )
                        )
                      )
                       (
                        ret22 message
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
                ret27
              )
               (
                let (
                  (
                    message "hello world"
                  )
                )
                 (
                  begin (
                    let (
                      (
                        n 3233
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            e 17
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                d 2753
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    block_size 1
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        encrypted (
                                          encrypt_message message n e block_size
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        _display (
                                          if (
                                            string? (
                                              to-str-space encrypted
                                            )
                                          )
                                           (
                                            to-str-space encrypted
                                          )
                                           (
                                            to-str (
                                              to-str-space encrypted
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
                                            decrypted (
                                              decrypt_message encrypted (
                                                _len message
                                              )
                                               n d block_size
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            _display (
                                              if (
                                                string? decrypted
                                              )
                                               decrypted (
                                                to-str decrypted
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
         (
          main
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
