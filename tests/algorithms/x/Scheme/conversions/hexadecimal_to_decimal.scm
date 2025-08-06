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
      start11 (
        current-jiffy
      )
    )
     (
      jps14 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        strip s
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
                      _len s
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
                                    < start end
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
                                    > end start
                                  )
                                   (
                                    string=? (
                                      _substring s (
                                        - end 1
                                      )
                                       end
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
                    ret1 (
                      _substring s start end
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
        hex_digit_value c
      )
       (
        call/cc (
          lambda (
            ret6
          )
           (
            begin (
              if (
                string=? c "0"
              )
               (
                begin (
                  ret6 0
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                string=? c "1"
              )
               (
                begin (
                  ret6 1
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                string=? c "2"
              )
               (
                begin (
                  ret6 2
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                string=? c "3"
              )
               (
                begin (
                  ret6 3
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                string=? c "4"
              )
               (
                begin (
                  ret6 4
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                string=? c "5"
              )
               (
                begin (
                  ret6 5
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                string=? c "6"
              )
               (
                begin (
                  ret6 6
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                string=? c "7"
              )
               (
                begin (
                  ret6 7
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                string=? c "8"
              )
               (
                begin (
                  ret6 8
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                string=? c "9"
              )
               (
                begin (
                  ret6 9
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                or (
                  string=? c "a"
                )
                 (
                  string=? c "A"
                )
              )
               (
                begin (
                  ret6 10
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                or (
                  string=? c "b"
                )
                 (
                  string=? c "B"
                )
              )
               (
                begin (
                  ret6 11
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                or (
                  string=? c "c"
                )
                 (
                  string=? c "C"
                )
              )
               (
                begin (
                  ret6 12
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                or (
                  string=? c "d"
                )
                 (
                  string=? c "D"
                )
              )
               (
                begin (
                  ret6 13
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                or (
                  string=? c "e"
                )
                 (
                  string=? c "E"
                )
              )
               (
                begin (
                  ret6 14
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                or (
                  string=? c "f"
                )
                 (
                  string=? c "F"
                )
              )
               (
                begin (
                  ret6 15
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              _display (
                if (
                  string? "Non-hexadecimal value was passed to the function"
                )
                 "Non-hexadecimal value was passed to the function" (
                  to-str "Non-hexadecimal value was passed to the function"
                )
              )
            )
             (
              newline
            )
             (
              ret6 0
            )
          )
        )
      )
    )
     (
      define (
        hex_to_decimal hex_string
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            let (
              (
                s (
                  strip hex_string
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
                    _display (
                      if (
                        string? "Empty string was passed to the function"
                      )
                       "Empty string was passed to the function" (
                        to-str "Empty string was passed to the function"
                      )
                    )
                  )
                   (
                    newline
                  )
                   (
                    ret7 0
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
                        _substring s 0 1
                      )
                       "-"
                    )
                     (
                      begin (
                        set! is_negative #t
                      )
                       (
                        set! s (
                          _substring s 1 (
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
                        decimal_number 0
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
                                            _len s
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                c (
                                                  _substring s i (
                                                    + i 1
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    value (
                                                      hex_digit_value c
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    set! decimal_number (
                                                      _add (
                                                        * 16 decimal_number
                                                      )
                                                       value
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
                            if is_negative (
                              begin (
                                ret7 (
                                  - decimal_number
                                )
                              )
                            )
                             (
                              quote (
                                
                              )
                            )
                          )
                           (
                            ret7 decimal_number
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
            ret10
          )
           (
            begin (
              _display (
                if (
                  string? (
                    to-str-space (
                      hex_to_decimal "a"
                    )
                  )
                )
                 (
                  to-str-space (
                    hex_to_decimal "a"
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      hex_to_decimal "a"
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
                      hex_to_decimal "12f"
                    )
                  )
                )
                 (
                  to-str-space (
                    hex_to_decimal "12f"
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      hex_to_decimal "12f"
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
                      hex_to_decimal "   12f   "
                    )
                  )
                )
                 (
                  to-str-space (
                    hex_to_decimal "   12f   "
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      hex_to_decimal "   12f   "
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
                      hex_to_decimal "FfFf"
                    )
                  )
                )
                 (
                  to-str-space (
                    hex_to_decimal "FfFf"
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      hex_to_decimal "FfFf"
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
                      hex_to_decimal "-Ff"
                    )
                  )
                )
                 (
                  to-str-space (
                    hex_to_decimal "-Ff"
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      hex_to_decimal "-Ff"
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
      main
    )
     (
      let (
        (
          end12 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur13 (
              quotient (
                * (
                  - end12 start11
                )
                 1000000
              )
               jps14
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur13
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
