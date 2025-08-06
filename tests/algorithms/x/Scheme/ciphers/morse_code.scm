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
      start17 (
        current-jiffy
      )
    )
     (
      jps20 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      let (
        (
          CHARS (
            _list "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "&" "@" ":" "," "." "'" "\"" "?" "/" "=" "+" "-" "(" ")" "!" " "
          )
        )
      )
       (
        begin (
          let (
            (
              CODES (
                _list ".-" "-..." "-.-." "-.." "." "..-." "--." "...." ".." ".---" "-.-" ".-.." "--" "-." "---" ".--." "--.-" ".-." "..." "-" "..-" "...-" ".--" "-..-" "-.--" "--.." ".----" "..---" "...--" "....-" "....." "-...." "--..." "---.." "----." "-----" ".-..." ".--.-." "---..." "--..--" ".-.-.-" ".----." ".-..-." "..--.." "-..-." "-...-" ".-.-." "-....-" "-.--." "-.--.-" "-.-.--" "/"
              )
            )
          )
           (
            begin (
              define (
                to_upper_char c
              )
               (
                call/cc (
                  lambda (
                    ret1
                  )
                   (
                    begin (
                      if (
                        string=? c "a"
                      )
                       (
                        begin (
                          ret1 "A"
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      if (
                        string=? c "b"
                      )
                       (
                        begin (
                          ret1 "B"
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      if (
                        string=? c "c"
                      )
                       (
                        begin (
                          ret1 "C"
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      if (
                        string=? c "d"
                      )
                       (
                        begin (
                          ret1 "D"
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      if (
                        string=? c "e"
                      )
                       (
                        begin (
                          ret1 "E"
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      if (
                        string=? c "f"
                      )
                       (
                        begin (
                          ret1 "F"
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      if (
                        string=? c "g"
                      )
                       (
                        begin (
                          ret1 "G"
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      if (
                        string=? c "h"
                      )
                       (
                        begin (
                          ret1 "H"
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      if (
                        string=? c "i"
                      )
                       (
                        begin (
                          ret1 "I"
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      if (
                        string=? c "j"
                      )
                       (
                        begin (
                          ret1 "J"
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      if (
                        string=? c "k"
                      )
                       (
                        begin (
                          ret1 "K"
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      if (
                        string=? c "l"
                      )
                       (
                        begin (
                          ret1 "L"
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      if (
                        string=? c "m"
                      )
                       (
                        begin (
                          ret1 "M"
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      if (
                        string=? c "n"
                      )
                       (
                        begin (
                          ret1 "N"
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      if (
                        string=? c "o"
                      )
                       (
                        begin (
                          ret1 "O"
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      if (
                        string=? c "p"
                      )
                       (
                        begin (
                          ret1 "P"
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      if (
                        string=? c "q"
                      )
                       (
                        begin (
                          ret1 "Q"
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      if (
                        string=? c "r"
                      )
                       (
                        begin (
                          ret1 "R"
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      if (
                        string=? c "s"
                      )
                       (
                        begin (
                          ret1 "S"
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      if (
                        string=? c "t"
                      )
                       (
                        begin (
                          ret1 "T"
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      if (
                        string=? c "u"
                      )
                       (
                        begin (
                          ret1 "U"
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      if (
                        string=? c "v"
                      )
                       (
                        begin (
                          ret1 "V"
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      if (
                        string=? c "w"
                      )
                       (
                        begin (
                          ret1 "W"
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      if (
                        string=? c "x"
                      )
                       (
                        begin (
                          ret1 "X"
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      if (
                        string=? c "y"
                      )
                       (
                        begin (
                          ret1 "Y"
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      if (
                        string=? c "z"
                      )
                       (
                        begin (
                          ret1 "Z"
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      ret1 c
                    )
                  )
                )
              )
            )
             (
              define (
                to_upper s
              )
               (
                call/cc (
                  lambda (
                    ret2
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
                                          < i (
                                            _len s
                                          )
                                        )
                                         (
                                          begin (
                                            set! res (
                                              string-append res (
                                                to_upper_char (
                                                  _substring s i (
                                                    + i 1
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
                            ret2 res
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
                index_of xs target
              )
               (
                call/cc (
                  lambda (
                    ret5
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
                                        _len xs
                                      )
                                    )
                                     (
                                      begin (
                                        if (
                                          string=? (
                                            list-ref xs i
                                          )
                                           target
                                        )
                                         (
                                          begin (
                                            ret5 i
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
              define (
                encrypt message
              )
               (
                call/cc (
                  lambda (
                    ret8
                  )
                   (
                    let (
                      (
                        msg (
                          to_upper message
                        )
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
                                                _len msg
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    c (
                                                      cond (
                                                        (
                                                          string? msg
                                                        )
                                                         (
                                                          _substring msg i (
                                                            + i 1
                                                          )
                                                        )
                                                      )
                                                       (
                                                        (
                                                          hash-table? msg
                                                        )
                                                         (
                                                          hash-table-ref msg i
                                                        )
                                                      )
                                                       (
                                                        else (
                                                          list-ref msg i
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        idx (
                                                          index_of CHARS c
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        if (
                                                          _ge idx 0
                                                        )
                                                         (
                                                          begin (
                                                            if (
                                                              not (
                                                                string=? res ""
                                                              )
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
                                                                list-ref CODES idx
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
              )
            )
             (
              define (
                split_spaces s
              )
               (
                call/cc (
                  lambda (
                    ret11
                  )
                   (
                    let (
                      (
                        res (
                          _list
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            current ""
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
                                                    if (
                                                      string=? ch " "
                                                    )
                                                     (
                                                      begin (
                                                        if (
                                                          not (
                                                            string=? current ""
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            set! res (
                                                              append res (
                                                                _list current
                                                              )
                                                            )
                                                          )
                                                           (
                                                            set! current ""
                                                          )
                                                        )
                                                         (
                                                          quote (
                                                            
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        set! current (
                                                          string-append current ch
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
                                if (
                                  not (
                                    string=? current ""
                                  )
                                )
                                 (
                                  begin (
                                    set! res (
                                      append res (
                                        _list current
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
                                ret11 res
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
                decrypt message
              )
               (
                call/cc (
                  lambda (
                    ret14
                  )
                   (
                    let (
                      (
                        parts (
                          split_spaces message
                        )
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
                                                code (
                                                  car xs
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    idx (
                                                      index_of CODES code
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    if (
                                                      _ge idx 0
                                                    )
                                                     (
                                                      begin (
                                                        set! res (
                                                          string-append res (
                                                            list-ref CHARS idx
                                                          )
                                                        )
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
                                  loop15 parts
                                )
                              )
                            )
                          )
                           (
                            ret14 res
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
                  msg "Morse code here!"
                )
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
                 (
                  let (
                    (
                      enc (
                        encrypt msg
                      )
                    )
                  )
                   (
                    begin (
                      _display (
                        if (
                          string? enc
                        )
                         enc (
                          to-str enc
                        )
                      )
                    )
                     (
                      newline
                    )
                     (
                      let (
                        (
                          dec (
                            decrypt enc
                          )
                        )
                      )
                       (
                        begin (
                          _display (
                            if (
                              string? dec
                            )
                             dec (
                              to-str dec
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
                                encrypt "Sos!"
                              )
                            )
                             (
                              encrypt "Sos!"
                            )
                             (
                              to-str (
                                encrypt "Sos!"
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
                                decrypt "... --- ... -.-.--"
                              )
                            )
                             (
                              decrypt "... --- ... -.-.--"
                            )
                             (
                              to-str (
                                decrypt "... --- ... -.-.--"
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
     (
      let (
        (
          end18 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur19 (
              quotient (
                * (
                  - end18 start17
                )
                 1000000
              )
               jps20
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur19
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
