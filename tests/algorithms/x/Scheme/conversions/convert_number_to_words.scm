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
      start12 (
        current-jiffy
      )
    )
     (
      jps15 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      let (
        (
          ones (
            _list "zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"
          )
        )
      )
       (
        begin (
          let (
            (
              teens (
                _list "ten" "eleven" "twelve" "thirteen" "fourteen" "fifteen" "sixteen" "seventeen" "eighteen" "nineteen"
              )
            )
          )
           (
            begin (
              let (
                (
                  tens (
                    _list "" "" "twenty" "thirty" "forty" "fifty" "sixty" "seventy" "eighty" "ninety"
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      short_powers (
                        _list 15 12 9 6 3 2
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          short_units (
                            _list "quadrillion" "trillion" "billion" "million" "thousand" "hundred"
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              long_powers (
                                _list 15 9 6 3 2
                              )
                            )
                          )
                           (
                            begin (
                              let (
                                (
                                  long_units (
                                    _list "billiard" "milliard" "million" "thousand" "hundred"
                                  )
                                )
                              )
                               (
                                begin (
                                  let (
                                    (
                                      indian_powers (
                                        _list 14 12 7 5 3 2
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      let (
                                        (
                                          indian_units (
                                            _list "crore crore" "lakh crore" "crore" "lakh" "thousand" "hundred"
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          define (
                                            pow10 exp
                                          )
                                           (
                                            call/cc (
                                              lambda (
                                                ret1
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
                                                                        set! res (
                                                                          * res 10
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
                                                        ret1 res
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
                                            max_value system
                                          )
                                           (
                                            call/cc (
                                              lambda (
                                                ret4
                                              )
                                               (
                                                begin (
                                                  if (
                                                    string=? system "short"
                                                  )
                                                   (
                                                    begin (
                                                      ret4 (
                                                        - (
                                                          pow10 18
                                                        )
                                                         1
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
                                                    string=? system "long"
                                                  )
                                                   (
                                                    begin (
                                                      ret4 (
                                                        - (
                                                          pow10 21
                                                        )
                                                         1
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
                                                    string=? system "indian"
                                                  )
                                                   (
                                                    begin (
                                                      ret4 (
                                                        - (
                                                          pow10 19
                                                        )
                                                         1
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
                                         (
                                          define (
                                            join_words words
                                          )
                                           (
                                            call/cc (
                                              lambda (
                                                ret5
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
                                                                      < i (
                                                                        _len words
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        if (
                                                                          > i 0
                                                                        )
                                                                         (
                                                                          begin (
                                                                            set! res (
                                                                              string-append res " "
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          quote (
                                                                            
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        set! res (
                                                                          string-append res (
                                                                            list-ref words i
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
                                                        ret5 res
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
                                            convert_small_number num
                                          )
                                           (
                                            call/cc (
                                              lambda (
                                                ret8
                                              )
                                               (
                                                begin (
                                                  if (
                                                    < num 0
                                                  )
                                                   (
                                                    begin (
                                                      ret8 ""
                                                    )
                                                  )
                                                   (
                                                    quote (
                                                      
                                                    )
                                                  )
                                                )
                                                 (
                                                  if (
                                                    >= num 100
                                                  )
                                                   (
                                                    begin (
                                                      ret8 ""
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
                                                      tens_digit (
                                                        _div num 10
                                                      )
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      let (
                                                        (
                                                          ones_digit (
                                                            _mod num 10
                                                          )
                                                        )
                                                      )
                                                       (
                                                        begin (
                                                          if (
                                                            equal? tens_digit 0
                                                          )
                                                           (
                                                            begin (
                                                              ret8 (
                                                                list-ref ones ones_digit
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
                                                            equal? tens_digit 1
                                                          )
                                                           (
                                                            begin (
                                                              ret8 (
                                                                list-ref teens ones_digit
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
                                                              hyphen (
                                                                if (
                                                                  > ones_digit 0
                                                                )
                                                                 "-" ""
                                                              )
                                                            )
                                                          )
                                                           (
                                                            begin (
                                                              let (
                                                                (
                                                                  tail (
                                                                    if (
                                                                      > ones_digit 0
                                                                    )
                                                                     (
                                                                      list-ref ones ones_digit
                                                                    )
                                                                     ""
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                begin (
                                                                  ret8 (
                                                                    string-append (
                                                                      string-append (
                                                                        list-ref tens tens_digit
                                                                      )
                                                                       hyphen
                                                                    )
                                                                     tail
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
                                            convert_number num system
                                          )
                                           (
                                            call/cc (
                                              lambda (
                                                ret9
                                              )
                                               (
                                                let (
                                                  (
                                                    word_groups (
                                                      _list
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        n num
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        if (
                                                          < n 0
                                                        )
                                                         (
                                                          begin (
                                                            set! word_groups (
                                                              append word_groups (
                                                                _list "negative"
                                                              )
                                                            )
                                                          )
                                                           (
                                                            set! n (
                                                              - n
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
                                                          _gt n (
                                                            max_value system
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            ret9 ""
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
                                                            powers (
                                                              _list
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                units (
                                                                  _list
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                if (
                                                                  string=? system "short"
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! powers short_powers
                                                                  )
                                                                   (
                                                                    set! units short_units
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    if (
                                                                      string=? system "long"
                                                                    )
                                                                     (
                                                                      begin (
                                                                        set! powers long_powers
                                                                      )
                                                                       (
                                                                        set! units long_units
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        if (
                                                                          string=? system "indian"
                                                                        )
                                                                         (
                                                                          begin (
                                                                            set! powers indian_powers
                                                                          )
                                                                           (
                                                                            set! units indian_units
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            ret9 ""
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
                                                                                  < i (
                                                                                    _len powers
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    let (
                                                                                      (
                                                                                        power (
                                                                                          list-ref powers i
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        let (
                                                                                          (
                                                                                            unit (
                                                                                              list-ref units i
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            let (
                                                                                              (
                                                                                                divisor (
                                                                                                  pow10 power
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                let (
                                                                                                  (
                                                                                                    digit_group (
                                                                                                      _div n divisor
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  begin (
                                                                                                    set! n (
                                                                                                      _mod n divisor
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    if (
                                                                                                      _gt digit_group 0
                                                                                                    )
                                                                                                     (
                                                                                                      begin (
                                                                                                        let (
                                                                                                          (
                                                                                                            word_group (
                                                                                                              if (
                                                                                                                _ge digit_group 100
                                                                                                              )
                                                                                                               (
                                                                                                                convert_number digit_group system
                                                                                                              )
                                                                                                               (
                                                                                                                convert_small_number digit_group
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          begin (
                                                                                                            set! word_groups (
                                                                                                              append word_groups (
                                                                                                                _list (
                                                                                                                  string-append (
                                                                                                                    string-append word_group " "
                                                                                                                  )
                                                                                                                   unit
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
                                                                    if (
                                                                      or (
                                                                        > n 0
                                                                      )
                                                                       (
                                                                        equal? (
                                                                          _len word_groups
                                                                        )
                                                                         0
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        set! word_groups (
                                                                          append word_groups (
                                                                            _list (
                                                                              convert_small_number n
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
                                                                        joined (
                                                                          join_words word_groups
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        ret9 joined
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
                                                convert_number 123456789012345 "short"
                                              )
                                            )
                                             (
                                              convert_number 123456789012345 "short"
                                            )
                                             (
                                              to-str (
                                                convert_number 123456789012345 "short"
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
                                                convert_number 123456789012345 "long"
                                              )
                                            )
                                             (
                                              convert_number 123456789012345 "long"
                                            )
                                             (
                                              to-str (
                                                convert_number 123456789012345 "long"
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
                                                convert_number 123456789012345 "indian"
                                              )
                                            )
                                             (
                                              convert_number 123456789012345 "indian"
                                            )
                                             (
                                              to-str (
                                                convert_number 123456789012345 "indian"
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
     (
      let (
        (
          end13 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur14 (
              quotient (
                * (
                  - end13 start12
                )
                 1000000
              )
               jps15
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur14
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
