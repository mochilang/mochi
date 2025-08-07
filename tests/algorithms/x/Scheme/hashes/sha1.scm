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
      start45 (
        current-jiffy
      )
    )
     (
      jps48 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      let (
        (
          MOD 4294967296
        )
      )
       (
        begin (
          let (
            (
              ASCII " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"
            )
          )
           (
            begin (
              define (
                ord ch
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
                                        _len ASCII
                                      )
                                    )
                                     (
                                      begin (
                                        if (
                                          string=? (
                                            _substring ASCII i (
                                              + i 1
                                            )
                                          )
                                           ch
                                        )
                                         (
                                          begin (
                                            ret1 (
                                              + 32 i
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
                        ret1 0
                      )
                    )
                  )
                )
              )
            )
             (
              define (
                pow2 n
              )
               (
                call/cc (
                  lambda (
                    ret4
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
                                          < i n
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
                            ret4 res
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
                bit_and a b
              )
               (
                call/cc (
                  lambda (
                    ret7
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
                            let (
                              (
                                res 0
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    bit 1
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
                                                      < i 32
                                                    )
                                                     (
                                                      begin (
                                                        if (
                                                          and (
                                                            equal? (
                                                              _mod x 2
                                                            )
                                                             1
                                                          )
                                                           (
                                                            equal? (
                                                              _mod y 2
                                                            )
                                                             1
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            set! res (
                                                              + res bit
                                                            )
                                                          )
                                                        )
                                                         (
                                                          quote (
                                                            
                                                          )
                                                        )
                                                      )
                                                       (
                                                        set! x (
                                                          _div x 2
                                                        )
                                                      )
                                                       (
                                                        set! y (
                                                          _div y 2
                                                        )
                                                      )
                                                       (
                                                        set! bit (
                                                          * bit 2
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
                                        ret7 res
                                      )
                                    )
                                  )
                                )
                              )
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
                bit_or a b
              )
               (
                call/cc (
                  lambda (
                    ret10
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
                            let (
                              (
                                res 0
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    bit 1
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
                                                      < i 32
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            abit (
                                                              _mod x 2
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                bbit (
                                                                  _mod y 2
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                if (
                                                                  or (
                                                                    equal? abit 1
                                                                  )
                                                                   (
                                                                    equal? bbit 1
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! res (
                                                                      + res bit
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  quote (
                                                                    
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                set! x (
                                                                  _div x 2
                                                                )
                                                              )
                                                               (
                                                                set! y (
                                                                  _div y 2
                                                                )
                                                              )
                                                               (
                                                                set! bit (
                                                                  * bit 2
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
                                        ret10 res
                                      )
                                    )
                                  )
                                )
                              )
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
                bit_xor a b
              )
               (
                call/cc (
                  lambda (
                    ret13
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
                            let (
                              (
                                res 0
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    bit 1
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
                                                      < i 32
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            abit (
                                                              _mod x 2
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                bbit (
                                                                  _mod y 2
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                if (
                                                                  or (
                                                                    and (
                                                                      equal? abit 1
                                                                    )
                                                                     (
                                                                      equal? bbit 0
                                                                    )
                                                                  )
                                                                   (
                                                                    and (
                                                                      equal? abit 0
                                                                    )
                                                                     (
                                                                      equal? bbit 1
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! res (
                                                                      + res bit
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  quote (
                                                                    
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                set! x (
                                                                  _div x 2
                                                                )
                                                              )
                                                               (
                                                                set! y (
                                                                  _div y 2
                                                                )
                                                              )
                                                               (
                                                                set! bit (
                                                                  * bit 2
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
                                        ret13 res
                                      )
                                    )
                                  )
                                )
                              )
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
                bit_not a
              )
               (
                call/cc (
                  lambda (
                    ret16
                  )
                   (
                    ret16 (
                      - (
                        - MOD 1
                      )
                       a
                    )
                  )
                )
              )
            )
             (
              define (
                rotate_left n b
              )
               (
                call/cc (
                  lambda (
                    ret17
                  )
                   (
                    let (
                      (
                        left (
                          _mod (
                            * n (
                              pow2 b
                            )
                          )
                           MOD
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            right (
                              _div n (
                                pow2 (
                                  - 32 b
                                )
                              )
                            )
                          )
                        )
                         (
                          begin (
                            ret17 (
                              _mod (
                                _add left right
                              )
                               MOD
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
                to_hex32 n
              )
               (
                call/cc (
                  lambda (
                    ret18
                  )
                   (
                    let (
                      (
                        digits "0123456789abcdef"
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            num n
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                s ""
                              )
                            )
                             (
                              begin (
                                if (
                                  equal? num 0
                                )
                                 (
                                  begin (
                                    set! s "0"
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
                                              > num 0
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    d (
                                                      _mod num 16
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    set! s (
                                                      string-append (
                                                        _substring digits d (
                                                          + d 1
                                                        )
                                                      )
                                                       s
                                                    )
                                                  )
                                                   (
                                                    set! num (
                                                      _div num 16
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
                                              < (
                                                _len s
                                              )
                                               8
                                            )
                                             (
                                              begin (
                                                set! s (
                                                  string-append "0" s
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
                                  > (
                                    _len s
                                  )
                                   8
                                )
                                 (
                                  begin (
                                    set! s (
                                      _substring s (
                                        - (
                                          _len s
                                        )
                                         8
                                      )
                                       (
                                        _len s
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
                                ret18 s
                              )
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
                sha1 message
              )
               (
                call/cc (
                  lambda (
                    ret23
                  )
                   (
                    let (
                      (
                        bytes (
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
                                          < i (
                                            _len message
                                          )
                                        )
                                         (
                                          begin (
                                            set! bytes (
                                              append bytes (
                                                _list (
                                                  ord (
                                                    _substring message i (
                                                      + i 1
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
                            set! bytes (
                              append bytes (
                                _list 128
                              )
                            )
                          )
                           (
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
                                          not (
                                            equal? (
                                              _mod (
                                                + (
                                                  _len bytes
                                                )
                                                 8
                                              )
                                               64
                                            )
                                             0
                                          )
                                        )
                                         (
                                          begin (
                                            set! bytes (
                                              append bytes (
                                                _list 0
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
                            let (
                              (
                                bit_len (
                                  * (
                                    _len message
                                  )
                                   8
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    len_bytes (
                                      _list 0 0 0 0 0 0 0 0
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        bl bit_len
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            k 7
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
                                                          >= k 0
                                                        )
                                                         (
                                                          begin (
                                                            list-set! len_bytes k (
                                                              _mod bl 256
                                                            )
                                                          )
                                                           (
                                                            set! bl (
                                                              _div bl 256
                                                            )
                                                          )
                                                           (
                                                            set! k (
                                                              - k 1
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
                                            let (
                                              (
                                                j 0
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
                                                              < j 8
                                                            )
                                                             (
                                                              begin (
                                                                set! bytes (
                                                                  append bytes (
                                                                    _list (
                                                                      list-ref-safe len_bytes j
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
                                                let (
                                                  (
                                                    blocks (
                                                      _list
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        pos 0
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
                                                                      < pos (
                                                                        _len bytes
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            block (
                                                                              _list
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                j2 0
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
                                                                                              < j2 64
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                set! block (
                                                                                                  append block (
                                                                                                    _list (
                                                                                                      list-ref-safe bytes (
                                                                                                        + pos j2
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                set! j2 (
                                                                                                  + j2 1
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
                                                                                set! blocks (
                                                                                  append blocks (
                                                                                    _list block
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                set! pos (
                                                                                  + pos 64
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
                                                        let (
                                                          (
                                                            h0 1732584193
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                h1 4023233417
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                let (
                                                                  (
                                                                    h2 2562383102
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        h3 271733878
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            h4 3285377520
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                bindex 0
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
                                                                                              < bindex (
                                                                                                _len blocks
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                let (
                                                                                                  (
                                                                                                    block (
                                                                                                      list-ref-safe blocks bindex
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  begin (
                                                                                                    let (
                                                                                                      (
                                                                                                        w (
                                                                                                          _list
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      begin (
                                                                                                        let (
                                                                                                          (
                                                                                                            t 0
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
                                                                                                                          < t 16
                                                                                                                        )
                                                                                                                         (
                                                                                                                          begin (
                                                                                                                            let (
                                                                                                                              (
                                                                                                                                j3 (
                                                                                                                                  * t 4
                                                                                                                                )
                                                                                                                              )
                                                                                                                            )
                                                                                                                             (
                                                                                                                              begin (
                                                                                                                                let (
                                                                                                                                  (
                                                                                                                                    word (
                                                                                                                                      + (
                                                                                                                                        * (
                                                                                                                                          + (
                                                                                                                                            * (
                                                                                                                                              + (
                                                                                                                                                * (
                                                                                                                                                  list-ref-safe block j3
                                                                                                                                                )
                                                                                                                                                 256
                                                                                                                                              )
                                                                                                                                               (
                                                                                                                                                list-ref-safe block (
                                                                                                                                                  + j3 1
                                                                                                                                                )
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                             256
                                                                                                                                          )
                                                                                                                                           (
                                                                                                                                            list-ref-safe block (
                                                                                                                                              + j3 2
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                         256
                                                                                                                                      )
                                                                                                                                       (
                                                                                                                                        list-ref-safe block (
                                                                                                                                          + j3 3
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                )
                                                                                                                                 (
                                                                                                                                  begin (
                                                                                                                                    set! w (
                                                                                                                                      append w (
                                                                                                                                        _list word
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    set! t (
                                                                                                                                      + t 1
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
                                                                                                                          < t 80
                                                                                                                        )
                                                                                                                         (
                                                                                                                          begin (
                                                                                                                            let (
                                                                                                                              (
                                                                                                                                tmp (
                                                                                                                                  bit_xor (
                                                                                                                                    bit_xor (
                                                                                                                                      bit_xor (
                                                                                                                                        list-ref-safe w (
                                                                                                                                          - t 3
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                       (
                                                                                                                                        list-ref-safe w (
                                                                                                                                          - t 8
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      list-ref-safe w (
                                                                                                                                        - t 14
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    list-ref-safe w (
                                                                                                                                      - t 16
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                )
                                                                                                                              )
                                                                                                                            )
                                                                                                                             (
                                                                                                                              begin (
                                                                                                                                set! w (
                                                                                                                                  append w (
                                                                                                                                    _list (
                                                                                                                                      rotate_left tmp 1
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                )
                                                                                                                              )
                                                                                                                               (
                                                                                                                                set! t (
                                                                                                                                  + t 1
                                                                                                                                )
                                                                                                                              )
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
                                                                                                            let (
                                                                                                              (
                                                                                                                a h0
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              begin (
                                                                                                                let (
                                                                                                                  (
                                                                                                                    b h1
                                                                                                                  )
                                                                                                                )
                                                                                                                 (
                                                                                                                  begin (
                                                                                                                    let (
                                                                                                                      (
                                                                                                                        c h2
                                                                                                                      )
                                                                                                                    )
                                                                                                                     (
                                                                                                                      begin (
                                                                                                                        let (
                                                                                                                          (
                                                                                                                            d h3
                                                                                                                          )
                                                                                                                        )
                                                                                                                         (
                                                                                                                          begin (
                                                                                                                            let (
                                                                                                                              (
                                                                                                                                e h4
                                                                                                                              )
                                                                                                                            )
                                                                                                                             (
                                                                                                                              begin (
                                                                                                                                let (
                                                                                                                                  (
                                                                                                                                    i2 0
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
                                                                                                                                                  < i2 80
                                                                                                                                                )
                                                                                                                                                 (
                                                                                                                                                  begin (
                                                                                                                                                    let (
                                                                                                                                                      (
                                                                                                                                                        f 0
                                                                                                                                                      )
                                                                                                                                                    )
                                                                                                                                                     (
                                                                                                                                                      begin (
                                                                                                                                                        let (
                                                                                                                                                          (
                                                                                                                                                            kconst 0
                                                                                                                                                          )
                                                                                                                                                        )
                                                                                                                                                         (
                                                                                                                                                          begin (
                                                                                                                                                            if (
                                                                                                                                                              < i2 20
                                                                                                                                                            )
                                                                                                                                                             (
                                                                                                                                                              begin (
                                                                                                                                                                set! f (
                                                                                                                                                                  bit_or (
                                                                                                                                                                    bit_and b c
                                                                                                                                                                  )
                                                                                                                                                                   (
                                                                                                                                                                    bit_and (
                                                                                                                                                                      bit_not b
                                                                                                                                                                    )
                                                                                                                                                                     d
                                                                                                                                                                  )
                                                                                                                                                                )
                                                                                                                                                              )
                                                                                                                                                               (
                                                                                                                                                                set! kconst 1518500249
                                                                                                                                                              )
                                                                                                                                                            )
                                                                                                                                                             (
                                                                                                                                                              if (
                                                                                                                                                                < i2 40
                                                                                                                                                              )
                                                                                                                                                               (
                                                                                                                                                                begin (
                                                                                                                                                                  set! f (
                                                                                                                                                                    bit_xor (
                                                                                                                                                                      bit_xor b c
                                                                                                                                                                    )
                                                                                                                                                                     d
                                                                                                                                                                  )
                                                                                                                                                                )
                                                                                                                                                                 (
                                                                                                                                                                  set! kconst 1859775393
                                                                                                                                                                )
                                                                                                                                                              )
                                                                                                                                                               (
                                                                                                                                                                if (
                                                                                                                                                                  < i2 60
                                                                                                                                                                )
                                                                                                                                                                 (
                                                                                                                                                                  begin (
                                                                                                                                                                    set! f (
                                                                                                                                                                      bit_or (
                                                                                                                                                                        bit_or (
                                                                                                                                                                          bit_and b c
                                                                                                                                                                        )
                                                                                                                                                                         (
                                                                                                                                                                          bit_and b d
                                                                                                                                                                        )
                                                                                                                                                                      )
                                                                                                                                                                       (
                                                                                                                                                                        bit_and c d
                                                                                                                                                                      )
                                                                                                                                                                    )
                                                                                                                                                                  )
                                                                                                                                                                   (
                                                                                                                                                                    set! kconst 2400959708
                                                                                                                                                                  )
                                                                                                                                                                )
                                                                                                                                                                 (
                                                                                                                                                                  begin (
                                                                                                                                                                    set! f (
                                                                                                                                                                      bit_xor (
                                                                                                                                                                        bit_xor b c
                                                                                                                                                                      )
                                                                                                                                                                       d
                                                                                                                                                                    )
                                                                                                                                                                  )
                                                                                                                                                                   (
                                                                                                                                                                    set! kconst 3395469782
                                                                                                                                                                  )
                                                                                                                                                                )
                                                                                                                                                              )
                                                                                                                                                            )
                                                                                                                                                          )
                                                                                                                                                           (
                                                                                                                                                            let (
                                                                                                                                                              (
                                                                                                                                                                temp (
                                                                                                                                                                  _mod (
                                                                                                                                                                    _add (
                                                                                                                                                                      _add (
                                                                                                                                                                        _add (
                                                                                                                                                                          _add (
                                                                                                                                                                            rotate_left a 5
                                                                                                                                                                          )
                                                                                                                                                                           f
                                                                                                                                                                        )
                                                                                                                                                                         e
                                                                                                                                                                      )
                                                                                                                                                                       kconst
                                                                                                                                                                    )
                                                                                                                                                                     (
                                                                                                                                                                      list-ref-safe w i2
                                                                                                                                                                    )
                                                                                                                                                                  )
                                                                                                                                                                   MOD
                                                                                                                                                                )
                                                                                                                                                              )
                                                                                                                                                            )
                                                                                                                                                             (
                                                                                                                                                              begin (
                                                                                                                                                                set! e d
                                                                                                                                                              )
                                                                                                                                                               (
                                                                                                                                                                set! d c
                                                                                                                                                              )
                                                                                                                                                               (
                                                                                                                                                                set! c (
                                                                                                                                                                  rotate_left b 30
                                                                                                                                                                )
                                                                                                                                                              )
                                                                                                                                                               (
                                                                                                                                                                set! b a
                                                                                                                                                              )
                                                                                                                                                               (
                                                                                                                                                                set! a temp
                                                                                                                                                              )
                                                                                                                                                               (
                                                                                                                                                                set! i2 (
                                                                                                                                                                  + i2 1
                                                                                                                                                                )
                                                                                                                                                              )
                                                                                                                                                            )
                                                                                                                                                          )
                                                                                                                                                        )
                                                                                                                                                      )
                                                                                                                                                    )
                                                                                                                                                  )
                                                                                                                                                   (
                                                                                                                                                    loop42
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
                                                                                                                                          loop42
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    set! h0 (
                                                                                                                                      _mod (
                                                                                                                                        + h0 a
                                                                                                                                      )
                                                                                                                                       MOD
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    set! h1 (
                                                                                                                                      _mod (
                                                                                                                                        + h1 b
                                                                                                                                      )
                                                                                                                                       MOD
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    set! h2 (
                                                                                                                                      _mod (
                                                                                                                                        + h2 c
                                                                                                                                      )
                                                                                                                                       MOD
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    set! h3 (
                                                                                                                                      _mod (
                                                                                                                                        + h3 d
                                                                                                                                      )
                                                                                                                                       MOD
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    set! h4 (
                                                                                                                                      _mod (
                                                                                                                                        + h4 e
                                                                                                                                      )
                                                                                                                                       MOD
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    set! bindex (
                                                                                                                                      + bindex 1
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                )
                                                                                                                              )
                                                                                                                            )
                                                                                                                          )
                                                                                                                        )
                                                                                                                      )
                                                                                                                    )
                                                                                                                  )
                                                                                                                )
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
                                                                                ret23 (
                                                                                  _add (
                                                                                    _add (
                                                                                      _add (
                                                                                        _add (
                                                                                          to_hex32 h0
                                                                                        )
                                                                                         (
                                                                                          to_hex32 h1
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        to_hex32 h2
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      to_hex32 h3
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    to_hex32 h4
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
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
                    ret44
                  )
                   (
                    begin (
                      _display (
                        if (
                          string? (
                            sha1 "Test String"
                          )
                        )
                         (
                          sha1 "Test String"
                        )
                         (
                          to-str (
                            sha1 "Test String"
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
             (
              main
            )
          )
        )
      )
    )
     (
      let (
        (
          end46 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur47 (
              quotient (
                * (
                  - end46 start45
                )
                 1000000
              )
               jps48
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur47
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
