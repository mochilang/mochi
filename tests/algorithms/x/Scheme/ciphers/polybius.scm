;; Generated on 2025-08-06 22:42 +0700
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
      let (
        (
          square (
            _list (
              _list "a" "b" "c" "d" "e"
            )
             (
              _list "f" "g" "h" "i" "k"
            )
             (
              _list "l" "m" "n" "o" "p"
            )
             (
              _list "q" "r" "s" "t" "u"
            )
             (
              _list "v" "w" "x" "y" "z"
            )
          )
        )
      )
       (
        begin (
          define (
            letter_to_numbers letter
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
                                    _len square
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
                                                      < j (
                                                        _len (
                                                          list-ref square i
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        if (
                                                          string=? (
                                                            cond (
                                                              (
                                                                string? (
                                                                  list-ref square i
                                                                )
                                                              )
                                                               (
                                                                _substring (
                                                                  list-ref square i
                                                                )
                                                                 j (
                                                                  + j 1
                                                                )
                                                              )
                                                            )
                                                             (
                                                              (
                                                                hash-table? (
                                                                  list-ref square i
                                                                )
                                                              )
                                                               (
                                                                hash-table-ref (
                                                                  list-ref square i
                                                                )
                                                                 j
                                                              )
                                                            )
                                                             (
                                                              else (
                                                                list-ref (
                                                                  list-ref square i
                                                                )
                                                                 j
                                                              )
                                                            )
                                                          )
                                                           letter
                                                        )
                                                         (
                                                          begin (
                                                            ret1 (
                                                              _list (
                                                                + i 1
                                                              )
                                                               (
                                                                + j 1
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
                                                        set! j (
                                                          + j 1
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
                                        set! i (
                                          + i 1
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
                    ret1 (
                      _list 0 0
                    )
                  )
                )
              )
            )
          )
        )
         (
          define (
            numbers_to_letter index1 index2
          )
           (
            call/cc (
              lambda (
                ret6
              )
               (
                ret6 (
                  cond (
                    (
                      string? (
                        list-ref square (
                          - index1 1
                        )
                      )
                    )
                     (
                      _substring (
                        list-ref square (
                          - index1 1
                        )
                      )
                       (
                        - index2 1
                      )
                       (
                        + (
                          - index2 1
                        )
                         1
                      )
                    )
                  )
                   (
                    (
                      hash-table? (
                        list-ref square (
                          - index1 1
                        )
                      )
                    )
                     (
                      hash-table-ref (
                        list-ref square (
                          - index1 1
                        )
                      )
                       (
                        - index2 1
                      )
                    )
                  )
                   (
                    else (
                      list-ref (
                        list-ref square (
                          - index1 1
                        )
                      )
                       (
                        - index2 1
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
            char_to_int ch
          )
           (
            call/cc (
              lambda (
                ret7
              )
               (
                begin (
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
                  ret7 0
                )
              )
            )
          )
        )
         (
          define (
            encode message
          )
           (
            call/cc (
              lambda (
                ret8
              )
               (
                begin (
                  set! message (
                    lower message
                  )
                )
                 (
                  let (
                    (
                      encoded ""
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
                                          _len message
                                        )
                                      )
                                       (
                                        begin (
                                          let (
                                            (
                                              ch (
                                                _substring message i (
                                                  + i 1
                                                )
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              if (
                                                string=? ch "j"
                                              )
                                               (
                                                begin (
                                                  set! ch "i"
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
                                                  string=? ch " "
                                                )
                                              )
                                               (
                                                begin (
                                                  let (
                                                    (
                                                      nums (
                                                        letter_to_numbers ch
                                                      )
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      set! encoded (
                                                        string-append (
                                                          string-append encoded (
                                                            to-str-space (
                                                              cond (
                                                                (
                                                                  string? nums
                                                                )
                                                                 (
                                                                  _substring nums 0 (
                                                                    + 0 1
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                (
                                                                  hash-table? nums
                                                                )
                                                                 (
                                                                  hash-table-ref nums 0
                                                                )
                                                              )
                                                               (
                                                                else (
                                                                  list-ref nums 0
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                         (
                                                          to-str-space (
                                                            cond (
                                                              (
                                                                string? nums
                                                              )
                                                               (
                                                                _substring nums 1 (
                                                                  + 1 1
                                                                )
                                                              )
                                                            )
                                                             (
                                                              (
                                                                hash-table? nums
                                                              )
                                                               (
                                                                hash-table-ref nums 1
                                                              )
                                                            )
                                                             (
                                                              else (
                                                                list-ref nums 1
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
                                                begin (
                                                  set! encoded (
                                                    string-append encoded " "
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
                          ret8 encoded
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
            decode message
          )
           (
            call/cc (
              lambda (
                ret11
              )
               (
                let (
                  (
                    decoded ""
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
                                        _len message
                                      )
                                    )
                                     (
                                      begin (
                                        if (
                                          string=? (
                                            _substring message i (
                                              + i 1
                                            )
                                          )
                                           " "
                                        )
                                         (
                                          begin (
                                            set! decoded (
                                              string-append decoded " "
                                            )
                                          )
                                           (
                                            set! i (
                                              + i 1
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                index1 (
                                                  char_to_int (
                                                    _substring message i (
                                                      + i 1
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    index2 (
                                                      char_to_int (
                                                        _substring message (
                                                          + i 1
                                                        )
                                                         (
                                                          + (
                                                            + i 1
                                                          )
                                                           1
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        letter (
                                                          numbers_to_letter index1 index2
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        set! decoded (
                                                          string-append decoded letter
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
                        ret11 decoded
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
                encode "test message"
              )
            )
             (
              encode "test message"
            )
             (
              to-str (
                encode "test message"
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
                encode "Test Message"
              )
            )
             (
              encode "Test Message"
            )
             (
              to-str (
                encode "Test Message"
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
                decode "44154344 32154343112215"
              )
            )
             (
              decode "44154344 32154343112215"
            )
             (
              to-str (
                decode "44154344 32154343112215"
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
                decode "4415434432154343112215"
              )
            )
             (
              decode "4415434432154343112215"
            )
             (
              to-str (
                decode "4415434432154343112215"
              )
            )
          )
        )
         (
          newline
        )
      )
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
