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
      start10 (
        current-jiffy
      )
    )
     (
      jps13 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        base16_encode data
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                digits "0123456789ABCDEF"
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
                                        _len data
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            b (
                                              list-ref data i
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              or (
                                                < b 0
                                              )
                                               (
                                                > b 255
                                              )
                                            )
                                             (
                                              begin (
                                                panic "byte out of range"
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
                                                hi (
                                                  quotient b 16
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    lo (
                                                      modulo b 16
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    set! res (
                                                      string-append (
                                                        string-append res (
                                                          _substring digits hi (
                                                            + hi 1
                                                          )
                                                        )
                                                      )
                                                       (
                                                        _substring digits lo (
                                                          + lo 1
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
    )
     (
      define (
        base16_decode data
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            let (
              (
                digits "0123456789ABCDEF"
              )
            )
             (
              begin (
                if (
                  not (
                    equal? (
                      modulo (
                        _len data
                      )
                       2
                    )
                     0
                  )
                )
                 (
                  begin (
                    panic "Base16 encoded data is invalid: Data does not have an even number of hex digits."
                  )
                )
                 (
                  quote (
                    
                  )
                )
              )
               (
                define (
                  hex_value ch
                )
                 (
                  call/cc (
                    lambda (
                      ret5
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
                                        < j 16
                                      )
                                       (
                                        begin (
                                          if (
                                            string=? (
                                              _substring digits j (
                                                + j 1
                                              )
                                            )
                                             ch
                                          )
                                           (
                                            begin (
                                              ret5 j
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
                          ret5 (
                            - 1
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
                    out (
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
                                        _len data
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            hi_char (
                                              _substring data i (
                                                + i 1
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                lo_char (
                                                  _substring data (
                                                    + i 1
                                                  )
                                                   (
                                                    + i 2
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    hi (
                                                      hex_value hi_char
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        lo (
                                                          hex_value lo_char
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        if (
                                                          or (
                                                            _lt hi 0
                                                          )
                                                           (
                                                            _lt lo 0
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            panic "Base16 encoded data is invalid: Data is not uppercase hex or it contains invalid characters."
                                                          )
                                                        )
                                                         (
                                                          quote (
                                                            
                                                          )
                                                        )
                                                      )
                                                       (
                                                        set! out (
                                                          append out (
                                                            _list (
                                                              _add (
                                                                * hi 16
                                                              )
                                                               lo
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        set! i (
                                                          + i 2
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
                        ret4 out
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
          example1 (
            _list 72 101 108 108 111 32 87 111 114 108 100 33
          )
        )
      )
       (
        begin (
          let (
            (
              example2 (
                _list 72 69 76 76 79 32 87 79 82 76 68 33
              )
            )
          )
           (
            begin (
              _display (
                if (
                  string? (
                    base16_encode example1
                  )
                )
                 (
                  base16_encode example1
                )
                 (
                  to-str (
                    base16_encode example1
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
                    base16_encode example2
                  )
                )
                 (
                  base16_encode example2
                )
                 (
                  to-str (
                    base16_encode example2
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
                    base16_encode (
                      _list
                    )
                  )
                )
                 (
                  base16_encode (
                    _list
                  )
                )
                 (
                  to-str (
                    base16_encode (
                      _list
                    )
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
                    to-str-space (
                      base16_decode "48656C6C6F20576F726C6421"
                    )
                  )
                )
                 (
                  to-str-space (
                    base16_decode "48656C6C6F20576F726C6421"
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      base16_decode "48656C6C6F20576F726C6421"
                    )
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
                    to-str-space (
                      base16_decode "48454C4C4F20574F524C4421"
                    )
                  )
                )
                 (
                  to-str-space (
                    base16_decode "48454C4C4F20574F524C4421"
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      base16_decode "48454C4C4F20574F524C4421"
                    )
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
                    to-str-space (
                      base16_decode ""
                    )
                  )
                )
                 (
                  to-str-space (
                    base16_decode ""
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      base16_decode ""
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
     (
      let (
        (
          end11 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur12 (
              quotient (
                * (
                  - end11 start10
                )
                 1000000
              )
               jps13
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur12
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
