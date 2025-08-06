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
      start14 (
        current-jiffy
      )
    )
     (
      jps17 (
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
                      _len s
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
                    ret2 (
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
        hex_digit_value ch
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
              if (
                string=? ch "8"
              )
               (
                begin (
                  ret7 8
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                string=? ch "9"
              )
               (
                begin (
                  ret7 9
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
                  string=? ch "a"
                )
                 (
                  string=? ch "A"
                )
              )
               (
                begin (
                  ret7 10
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
                  string=? ch "b"
                )
                 (
                  string=? ch "B"
                )
              )
               (
                begin (
                  ret7 11
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
                  string=? ch "c"
                )
                 (
                  string=? ch "C"
                )
              )
               (
                begin (
                  ret7 12
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
                  string=? ch "d"
                )
                 (
                  string=? ch "D"
                )
              )
               (
                begin (
                  ret7 13
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
                  string=? ch "e"
                )
                 (
                  string=? ch "E"
                )
              )
               (
                begin (
                  ret7 14
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
                  string=? ch "f"
                )
                 (
                  string=? ch "F"
                )
              )
               (
                begin (
                  ret7 15
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              panic "Invalid value was passed to the function"
            )
          )
        )
      )
    )
     (
      define (
        hex_to_bin hex_num
      )
       (
        call/cc (
          lambda (
            ret8
          )
           (
            let (
              (
                trimmed (
                  trim_spaces hex_num
                )
              )
            )
             (
              begin (
                if (
                  equal? (
                    _len trimmed
                  )
                   0
                )
                 (
                  begin (
                    panic "No value was passed to the function"
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
                    s trimmed
                  )
                )
                 (
                  begin (
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
                            int_num 0
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
                                                        val (
                                                          hex_digit_value ch
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        set! int_num (
                                                          _add (
                                                            * int_num 16
                                                          )
                                                           val
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
                                let (
                                  (
                                    bin_str ""
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        n int_num
                                      )
                                    )
                                     (
                                      begin (
                                        if (
                                          equal? n 0
                                        )
                                         (
                                          begin (
                                            set! bin_str "0"
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
                                                      > n 0
                                                    )
                                                     (
                                                      begin (
                                                        set! bin_str (
                                                          string-append (
                                                            to-str-space (
                                                              _mod n 2
                                                            )
                                                          )
                                                           bin_str
                                                        )
                                                      )
                                                       (
                                                        set! n (
                                                          _div n 2
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
                                        let (
                                          (
                                            result (
                                              let (
                                                (
                                                  v13 bin_str
                                                )
                                              )
                                               (
                                                cond (
                                                  (
                                                    string? v13
                                                  )
                                                   (
                                                    inexact->exact (
                                                      floor (
                                                        string->number v13
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  (
                                                    boolean? v13
                                                  )
                                                   (
                                                    if v13 1 0
                                                  )
                                                )
                                                 (
                                                  else (
                                                    inexact->exact (
                                                      floor v13
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            if is_negative (
                                              begin (
                                                set! result (
                                                  - result
                                                )
                                              )
                                            )
                                             (
                                              quote (
                                                
                                              )
                                            )
                                          )
                                           (
                                            ret8 result
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
            to-str-space (
              hex_to_bin "AC"
            )
          )
        )
         (
          to-str-space (
            hex_to_bin "AC"
          )
        )
         (
          to-str (
            to-str-space (
              hex_to_bin "AC"
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
              hex_to_bin "9A4"
            )
          )
        )
         (
          to-str-space (
            hex_to_bin "9A4"
          )
        )
         (
          to-str (
            to-str-space (
              hex_to_bin "9A4"
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
              hex_to_bin "   12f   "
            )
          )
        )
         (
          to-str-space (
            hex_to_bin "   12f   "
          )
        )
         (
          to-str (
            to-str-space (
              hex_to_bin "   12f   "
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
              hex_to_bin "FfFf"
            )
          )
        )
         (
          to-str-space (
            hex_to_bin "FfFf"
          )
        )
         (
          to-str (
            to-str-space (
              hex_to_bin "FfFf"
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
              hex_to_bin "-fFfF"
            )
          )
        )
         (
          to-str-space (
            hex_to_bin "-fFfF"
          )
        )
         (
          to-str (
            to-str-space (
              hex_to_bin "-fFfF"
            )
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
          end15 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur16 (
              quotient (
                * (
                  - end15 start14
                )
                 1000000
              )
               jps17
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur16
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
