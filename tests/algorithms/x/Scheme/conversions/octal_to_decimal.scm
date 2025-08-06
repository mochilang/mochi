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
      define (
        panic msg
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            begin (
              _display (
                if (
                  string? msg
                )
                 msg (
                  to-str msg
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
      define (
        trim_spaces s
      )
       (
        call/cc (
          lambda (
            ret2
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
                        break4
                      )
                       (
                        letrec (
                          (
                            loop3 (
                              lambda (
                                
                              )
                               (
                                if (
                                  and (
                                    <= start end
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
                                    loop3
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
                          loop3
                        )
                      )
                    )
                  )
                   (
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
                    if (
                      > start end
                    )
                     (
                      begin (
                        ret2 ""
                      )
                    )
                     (
                      quote (
                        
                      )
                    )
                  )
                   (
                    ret2 (
                      _substring s start (
                        + end 1
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
        char_to_digit ch
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            begin (
              if (
                string=? ch "0"
              )
               (
                begin (
                  ret7 0
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                string=? ch "1"
              )
               (
                begin (
                  ret7 1
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                string=? ch "2"
              )
               (
                begin (
                  ret7 2
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                string=? ch "3"
              )
               (
                begin (
                  ret7 3
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                string=? ch "4"
              )
               (
                begin (
                  ret7 4
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                string=? ch "5"
              )
               (
                begin (
                  ret7 5
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                string=? ch "6"
              )
               (
                begin (
                  ret7 6
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                string=? ch "7"
              )
               (
                begin (
                  ret7 7
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              panic "Non-octal value was passed to the function"
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
        oct_to_decimal oct_string
      )
       (
        call/cc (
          lambda (
            ret8
          )
           (
            let (
              (
                s (
                  trim_spaces oct_string
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
                   (
                    ret8 0
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
                    if (
                      equal? (
                        _len s
                      )
                       0
                    )
                     (
                      begin (
                        panic "Non-octal value was passed to the function"
                      )
                       (
                        ret8 0
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
                                          < i (
                                            _len s
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                ch (
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
                                                    digit (
                                                      char_to_digit ch
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    set! decimal_number (
                                                      _add (
                                                        * 8 decimal_number
                                                      )
                                                       digit
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
                            if is_negative (
                              begin (
                                set! decimal_number (
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
                            ret8 decimal_number
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
            ret11
          )
           (
            begin (
              _display (
                if (
                  string? (
                    to-str-space (
                      oct_to_decimal "1"
                    )
                  )
                )
                 (
                  to-str-space (
                    oct_to_decimal "1"
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      oct_to_decimal "1"
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
                      oct_to_decimal "-1"
                    )
                  )
                )
                 (
                  to-str-space (
                    oct_to_decimal "-1"
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      oct_to_decimal "-1"
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
                      oct_to_decimal "12"
                    )
                  )
                )
                 (
                  to-str-space (
                    oct_to_decimal "12"
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      oct_to_decimal "12"
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
                      oct_to_decimal " 12   "
                    )
                  )
                )
                 (
                  to-str-space (
                    oct_to_decimal " 12   "
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      oct_to_decimal " 12   "
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
                      oct_to_decimal "-45"
                    )
                  )
                )
                 (
                  to-str-space (
                    oct_to_decimal "-45"
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      oct_to_decimal "-45"
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
                      oct_to_decimal "0"
                    )
                  )
                )
                 (
                  to-str-space (
                    oct_to_decimal "0"
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      oct_to_decimal "0"
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
