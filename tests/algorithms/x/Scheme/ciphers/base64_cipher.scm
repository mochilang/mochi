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
      start31 (
        current-jiffy
      )
    )
     (
      jps34 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      let (
        (
          B64_CHARSET "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
        )
      )
       (
        begin (
          define (
            to_binary n
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
                   (
                    quote (
                      
                    )
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
                                              bit (
                                                modulo num 2
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              set! res (
                                                string-append (
                                                  to-str-space bit
                                                )
                                                 res
                                              )
                                            )
                                             (
                                              set! num (
                                                quotient num 2
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
         (
          define (
            zfill s width
          )
           (
            call/cc (
              lambda (
                ret4
              )
               (
                let (
                  (
                    res s
                  )
                )
                 (
                  begin (
                    let (
                      (
                        pad (
                          - width (
                            _len s
                          )
                        )
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
                                      > pad 0
                                    )
                                     (
                                      begin (
                                        set! res (
                                          string-append "0" res
                                        )
                                      )
                                       (
                                        set! pad (
                                          - pad 1
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
            from_binary s
          )
           (
            call/cc (
              lambda (
                ret7
              )
               (
                let (
                  (
                    i 0
                  )
                )
                 (
                  begin (
                    let (
                      (
                        result 0
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
                                      < i (
                                        _len s
                                      )
                                    )
                                     (
                                      begin (
                                        set! result (
                                          * result 2
                                        )
                                      )
                                       (
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
                                            set! result (
                                              + result 1
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
                        ret7 result
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
            repeat ch times
          )
           (
            call/cc (
              lambda (
                ret10
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
                                      < i times
                                    )
                                     (
                                      begin (
                                        set! res (
                                          string-append res ch
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
                        ret10 res
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
            char_index s c
          )
           (
            call/cc (
              lambda (
                ret13
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
                                       c
                                    )
                                     (
                                      begin (
                                        ret13 i
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
            base64_encode data
          )
           (
            call/cc (
              lambda (
                ret16
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
                                        _len data
                                      )
                                    )
                                     (
                                      begin (
                                        set! bits (
                                          string-append bits (
                                            zfill (
                                              to_binary (
                                                list-ref data i
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
                            pad_bits 0
                          )
                        )
                         (
                          begin (
                            if (
                              not (
                                equal? (
                                  modulo (
                                    _len bits
                                  )
                                   6
                                )
                                 0
                              )
                            )
                             (
                              begin (
                                set! pad_bits (
                                  - 6 (
                                    modulo (
                                      _len bits
                                    )
                                     6
                                  )
                                )
                              )
                               (
                                set! bits (
                                  string-append bits (
                                    repeat "0" pad_bits
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
                                j 0
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    encoded ""
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
                                                  < j (
                                                    _len bits
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        chunk (
                                                          _substring bits j (
                                                            + j 6
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            idx (
                                                              from_binary chunk
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            set! encoded (
                                                              string-append encoded (
                                                                _substring B64_CHARSET idx (
                                                                  _add idx 1
                                                                )
                                                              )
                                                            )
                                                          )
                                                           (
                                                            set! j (
                                                              + j 6
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
                                    let (
                                      (
                                        pad (
                                          quotient pad_bits 2
                                        )
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
                                                      > pad 0
                                                    )
                                                     (
                                                      begin (
                                                        set! encoded (
                                                          string-append encoded "="
                                                        )
                                                      )
                                                       (
                                                        set! pad (
                                                          - pad 1
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
                                        ret16 encoded
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
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
            base64_decode s
          )
           (
            call/cc (
              lambda (
                ret23
              )
               (
                let (
                  (
                    padding 0
                  )
                )
                 (
                  begin (
                    let (
                      (
                        end (
                          _len s
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
                                      and (
                                        > end 0
                                      )
                                       (
                                        string=? (
                                          _substring s (
                                            - end 1
                                          )
                                           end
                                        )
                                         "="
                                      )
                                    )
                                     (
                                      begin (
                                        set! padding (
                                          + padding 1
                                        )
                                      )
                                       (
                                        set! end (
                                          - end 1
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
                            bits ""
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
                                              < k end
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    c (
                                                      _substring s k (
                                                        + k 1
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        idx (
                                                          char_index B64_CHARSET c
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        set! bits (
                                                          string-append bits (
                                                            zfill (
                                                              to_binary idx
                                                            )
                                                             6
                                                          )
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
                                  > padding 0
                                )
                                 (
                                  begin (
                                    set! bits (
                                      _substring bits 0 (
                                        - (
                                          _len bits
                                        )
                                         (
                                          * padding 2
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
                                    bytes (
                                      _list
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        m 0
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
                                                      < m (
                                                        _len bits
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            byte (
                                                              from_binary (
                                                                _substring bits m (
                                                                  + m 8
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            set! bytes (
                                                              append bytes (
                                                                _list byte
                                                              )
                                                            )
                                                          )
                                                           (
                                                            set! m (
                                                              + m 8
                                                            )
                                                          )
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
                                        ret23 bytes
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
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
                ret30
              )
               (
                let (
                  (
                    data (
                      _list 77 111 99 104 105
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        encoded (
                          base64_encode data
                        )
                      )
                    )
                     (
                      begin (
                        _display (
                          if (
                            string? encoded
                          )
                           encoded (
                            to-str encoded
                          )
                        )
                      )
                       (
                        newline
                      )
                       (
                        _display (
                          base64_decode encoded
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
      )
    )
     (
      let (
        (
          end32 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur33 (
              quotient (
                * (
                  - end32 start31
                )
                 1000000
              )
               jps34
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur33
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
