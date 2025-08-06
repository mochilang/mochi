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
      start6 (
        current-jiffy
      )
    )
     (
      jps9 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      let (
        (
          SI_UNITS (
            alist->hash-table (
              _list (
                cons "yotta" 24
              )
               (
                cons "zetta" 21
              )
               (
                cons "exa" 18
              )
               (
                cons "peta" 15
              )
               (
                cons "tera" 12
              )
               (
                cons "giga" 9
              )
               (
                cons "mega" 6
              )
               (
                cons "kilo" 3
              )
               (
                cons "hecto" 2
              )
               (
                cons "deca" 1
              )
               (
                cons "deci" (
                  - 1
                )
              )
               (
                cons "centi" (
                  - 2
                )
              )
               (
                cons "milli" (
                  - 3
                )
              )
               (
                cons "micro" (
                  - 6
                )
              )
               (
                cons "nano" (
                  - 9
                )
              )
               (
                cons "pico" (
                  - 12
                )
              )
               (
                cons "femto" (
                  - 15
                )
              )
               (
                cons "atto" (
                  - 18
                )
              )
               (
                cons "zepto" (
                  - 21
                )
              )
               (
                cons "yocto" (
                  - 24
                )
              )
            )
          )
        )
      )
       (
        begin (
          let (
            (
              BINARY_UNITS (
                alist->hash-table (
                  _list (
                    cons "yotta" 8
                  )
                   (
                    cons "zetta" 7
                  )
                   (
                    cons "exa" 6
                  )
                   (
                    cons "peta" 5
                  )
                   (
                    cons "tera" 4
                  )
                   (
                    cons "giga" 3
                  )
                   (
                    cons "mega" 2
                  )
                   (
                    cons "kilo" 1
                  )
                )
              )
            )
          )
           (
            begin (
              define (
                pow base exp
              )
               (
                call/cc (
                  lambda (
                    ret1
                  )
                   (
                    begin (
                      if (
                        equal? exp 0
                      )
                       (
                        begin (
                          ret1 1.0
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
                          e exp
                        )
                      )
                       (
                        begin (
                          if (
                            < e 0
                          )
                           (
                            begin (
                              set! e (
                                - e
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
                              result 1.0
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
                                                < i e
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
                                  if (
                                    < exp 0
                                  )
                                   (
                                    begin (
                                      ret1 (
                                        _div 1.0 result
                                      )
                                    )
                                  )
                                   (
                                    quote (
                                      
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
                )
              )
            )
             (
              define (
                convert_si_prefix known_amount known_prefix unknown_prefix
              )
               (
                call/cc (
                  lambda (
                    ret4
                  )
                   (
                    let (
                      (
                        kp (
                          lower known_prefix
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            up (
                              lower unknown_prefix
                            )
                          )
                        )
                         (
                          begin (
                            if (
                              not (
                                cond (
                                  (
                                    string? SI_UNITS
                                  )
                                   (
                                    if (
                                      string-contains SI_UNITS kp
                                    )
                                     #t #f
                                  )
                                )
                                 (
                                  (
                                    hash-table? SI_UNITS
                                  )
                                   (
                                    if (
                                      hash-table-exists? SI_UNITS kp
                                    )
                                     #t #f
                                  )
                                )
                                 (
                                  else (
                                    if (
                                      member kp SI_UNITS
                                    )
                                     #t #f
                                  )
                                )
                              )
                            )
                             (
                              begin (
                                panic (
                                  string-append "unknown prefix: " known_prefix
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
                                cond (
                                  (
                                    string? SI_UNITS
                                  )
                                   (
                                    if (
                                      string-contains SI_UNITS up
                                    )
                                     #t #f
                                  )
                                )
                                 (
                                  (
                                    hash-table? SI_UNITS
                                  )
                                   (
                                    if (
                                      hash-table-exists? SI_UNITS up
                                    )
                                     #t #f
                                  )
                                )
                                 (
                                  else (
                                    if (
                                      member up SI_UNITS
                                    )
                                     #t #f
                                  )
                                )
                              )
                            )
                             (
                              begin (
                                panic (
                                  string-append "unknown prefix: " unknown_prefix
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
                                diff (
                                  - (
                                    hash-table-ref/default SI_UNITS kp (
                                      quote (
                                        
                                      )
                                    )
                                  )
                                   (
                                    hash-table-ref/default SI_UNITS up (
                                      quote (
                                        
                                      )
                                    )
                                  )
                                )
                              )
                            )
                             (
                              begin (
                                ret4 (
                                  * known_amount (
                                    pow 10.0 diff
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
                convert_binary_prefix known_amount known_prefix unknown_prefix
              )
               (
                call/cc (
                  lambda (
                    ret5
                  )
                   (
                    let (
                      (
                        kp (
                          lower known_prefix
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            up (
                              lower unknown_prefix
                            )
                          )
                        )
                         (
                          begin (
                            if (
                              not (
                                cond (
                                  (
                                    string? BINARY_UNITS
                                  )
                                   (
                                    if (
                                      string-contains BINARY_UNITS kp
                                    )
                                     #t #f
                                  )
                                )
                                 (
                                  (
                                    hash-table? BINARY_UNITS
                                  )
                                   (
                                    if (
                                      hash-table-exists? BINARY_UNITS kp
                                    )
                                     #t #f
                                  )
                                )
                                 (
                                  else (
                                    if (
                                      member kp BINARY_UNITS
                                    )
                                     #t #f
                                  )
                                )
                              )
                            )
                             (
                              begin (
                                panic (
                                  string-append "unknown prefix: " known_prefix
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
                                cond (
                                  (
                                    string? BINARY_UNITS
                                  )
                                   (
                                    if (
                                      string-contains BINARY_UNITS up
                                    )
                                     #t #f
                                  )
                                )
                                 (
                                  (
                                    hash-table? BINARY_UNITS
                                  )
                                   (
                                    if (
                                      hash-table-exists? BINARY_UNITS up
                                    )
                                     #t #f
                                  )
                                )
                                 (
                                  else (
                                    if (
                                      member up BINARY_UNITS
                                    )
                                     #t #f
                                  )
                                )
                              )
                            )
                             (
                              begin (
                                panic (
                                  string-append "unknown prefix: " unknown_prefix
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
                                diff (
                                  * (
                                    - (
                                      hash-table-ref/default BINARY_UNITS kp (
                                        quote (
                                          
                                        )
                                      )
                                    )
                                     (
                                      hash-table-ref/default BINARY_UNITS up (
                                        quote (
                                          
                                        )
                                      )
                                    )
                                  )
                                   10
                                )
                              )
                            )
                             (
                              begin (
                                ret5 (
                                  * known_amount (
                                    pow 2.0 diff
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
                    to-str-space (
                      convert_si_prefix 1.0 "giga" "mega"
                    )
                  )
                )
                 (
                  to-str-space (
                    convert_si_prefix 1.0 "giga" "mega"
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      convert_si_prefix 1.0 "giga" "mega"
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
                      convert_si_prefix 1.0 "mega" "giga"
                    )
                  )
                )
                 (
                  to-str-space (
                    convert_si_prefix 1.0 "mega" "giga"
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      convert_si_prefix 1.0 "mega" "giga"
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
                      convert_si_prefix 1.0 "kilo" "kilo"
                    )
                  )
                )
                 (
                  to-str-space (
                    convert_si_prefix 1.0 "kilo" "kilo"
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      convert_si_prefix 1.0 "kilo" "kilo"
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
                      convert_binary_prefix 1.0 "giga" "mega"
                    )
                  )
                )
                 (
                  to-str-space (
                    convert_binary_prefix 1.0 "giga" "mega"
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      convert_binary_prefix 1.0 "giga" "mega"
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
                      convert_binary_prefix 1.0 "mega" "giga"
                    )
                  )
                )
                 (
                  to-str-space (
                    convert_binary_prefix 1.0 "mega" "giga"
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      convert_binary_prefix 1.0 "mega" "giga"
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
                      convert_binary_prefix 1.0 "kilo" "kilo"
                    )
                  )
                )
                 (
                  to-str-space (
                    convert_binary_prefix 1.0 "kilo" "kilo"
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      convert_binary_prefix 1.0 "kilo" "kilo"
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
          end7 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur8 (
              quotient (
                * (
                  - end7 start6
                )
                 1000000
              )
               jps9
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur8
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
