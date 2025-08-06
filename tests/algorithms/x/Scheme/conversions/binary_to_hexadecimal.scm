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
        strip_spaces s
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                start 0
              )
            )
             (
              begin (
                let (
                  (
                    end (
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
                                  and (
                                    < start (
                                      _len s
                                    )
                                  )
                                   (
                                    string=? (
                                      _substring s start (
                                        + start 1
                                      )
                                    )
                                     " "
                                  )
                                )
                                 (
                                  begin (
                                    set! start (
                                      + start 1
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
                    call/cc (
                      lambda (
                        break5
                      )
                       (
                        letrec (
                          (
                            loop4 (
                              lambda (
                                
                              )
                               (
                                if (
                                  and (
                                    >= end start
                                  )
                                   (
                                    string=? (
                                      _substring s end (
                                        + end 1
                                      )
                                    )
                                     " "
                                  )
                                )
                                 (
                                  begin (
                                    set! end (
                                      - end 1
                                    )
                                  )
                                   (
                                    loop4
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
                          loop4
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
                            i start
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
                                          <= i end
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
                                              + i 1
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
    )
     (
      define (
        repeat_char ch count
      )
       (
        call/cc (
          lambda (
            ret8
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
                                  < i count
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
        slice s start end
      )
       (
        call/cc (
          lambda (
            ret11
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
                    i start
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
                                  < i end
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
                    ret11 res
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
        bits_to_int bits
      )
       (
        call/cc (
          lambda (
            ret14
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
                                  < i (
                                    _len bits
                                  )
                                )
                                 (
                                  begin (
                                    set! value (
                                      * value 2
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
                                        set! value (
                                          + value 1
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
                    ret14 value
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
        bin_to_hexadecimal binary_str
      )
       (
        call/cc (
          lambda (
            ret17
          )
           (
            let (
              (
                s (
                  strip_spaces binary_str
                )
              )
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
                    panic "Empty string was passed to the function"
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
                    is_negative #f
                  )
                )
                 (
                  begin (
                    if (
                      string=? (
                        cond (
                          (
                            string? s
                          )
                           (
                            _substring s 0 (
                              + 0 1
                            )
                          )
                        )
                         (
                          (
                            hash-table? s
                          )
                           (
                            hash-table-ref s 0
                          )
                        )
                         (
                          else (
                            list-ref s 0
                          )
                        )
                      )
                       "-"
                    )
                     (
                      begin (
                        set! is_negative #t
                      )
                       (
                        set! s (
                          slice s 1 (
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
                                        let (
                                          (
                                            c (
                                              cond (
                                                (
                                                  string? s
                                                )
                                                 (
                                                  _substring s i (
                                                    + i 1
                                                  )
                                                )
                                              )
                                               (
                                                (
                                                  hash-table? s
                                                )
                                                 (
                                                  hash-table-ref s i
                                                )
                                              )
                                               (
                                                else (
                                                  list-ref s i
                                                )
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              and (
                                                not (
                                                  string=? c "0"
                                                )
                                              )
                                               (
                                                not (
                                                  string=? c "1"
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                panic "Non-binary value was passed to the function"
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
                            groups (
                              + (
                                _div (
                                  _len s
                                )
                                 4
                              )
                               1
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                pad_len (
                                  - (
                                    * groups 4
                                  )
                                   (
                                    _len s
                                  )
                                )
                              )
                            )
                             (
                              begin (
                                set! s (
                                  _add (
                                    repeat_char "0" pad_len
                                  )
                                   s
                                )
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
                                        res "0x"
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
                                                          < j (
                                                            _len s
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                chunk (
                                                                  slice s j (
                                                                    + j 4
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                let (
                                                                  (
                                                                    val (
                                                                      bits_to_int chunk
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! res (
                                                                      string-append res (
                                                                        _substring digits val (
                                                                          + val 1
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    set! j (
                                                                      + j 4
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                           (
                                                            loop20
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
                                                  loop20
                                                )
                                              )
                                            )
                                          )
                                           (
                                            if is_negative (
                                              begin (
                                                ret17 (
                                                  string-append "-" res
                                                )
                                              )
                                            )
                                             (
                                              quote (
                                                
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
                          )
                        )
                      )
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
            bin_to_hexadecimal "101011111"
          )
        )
         (
          bin_to_hexadecimal "101011111"
        )
         (
          to-str (
            bin_to_hexadecimal "101011111"
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
            bin_to_hexadecimal " 1010   "
          )
        )
         (
          bin_to_hexadecimal " 1010   "
        )
         (
          to-str (
            bin_to_hexadecimal " 1010   "
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
            bin_to_hexadecimal "-11101"
          )
        )
         (
          bin_to_hexadecimal "-11101"
        )
         (
          to-str (
            bin_to_hexadecimal "-11101"
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
