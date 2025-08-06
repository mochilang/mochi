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
      let (
        (
          LETTERS "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        )
      )
       (
        begin (
          let (
            (
              LETTERS_LOWER "abcdefghijklmnopqrstuvwxyz"
            )
          )
           (
            begin (
              define (
                find_index s ch
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
                                           ch
                                        )
                                         (
                                          begin (
                                            ret1 i
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
                to_upper_char ch
              )
               (
                call/cc (
                  lambda (
                    ret4
                  )
                   (
                    let (
                      (
                        idx (
                          find_index LETTERS_LOWER ch
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
                            ret4 (
                              _substring LETTERS idx (
                                + idx 1
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
                        ret4 ch
                      )
                    )
                  )
                )
              )
            )
             (
              define (
                to_lower_char ch
              )
               (
                call/cc (
                  lambda (
                    ret5
                  )
                   (
                    let (
                      (
                        idx (
                          find_index LETTERS ch
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
                            ret5 (
                              _substring LETTERS_LOWER idx (
                                + idx 1
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
                        ret5 ch
                      )
                    )
                  )
                )
              )
            )
             (
              define (
                is_upper ch
              )
               (
                call/cc (
                  lambda (
                    ret6
                  )
                   (
                    ret6 (
                      _ge (
                        find_index LETTERS ch
                      )
                       0
                    )
                  )
                )
              )
            )
             (
              define (
                to_upper_string s
              )
               (
                call/cc (
                  lambda (
                    ret7
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
                            ret7 res
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
                  key "HDarji"
                )
              )
               (
                begin (
                  let (
                    (
                      message "This is Harshil Darji from Dharmaj."
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          key_up (
                            to_upper_string key
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              encrypted ""
                            )
                          )
                           (
                            begin (
                              let (
                                (
                                  key_index 0
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
                                                      _len message
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      let (
                                                        (
                                                          symbol (
                                                            _substring message i (
                                                              + i 1
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        begin (
                                                          let (
                                                            (
                                                              upper_symbol (
                                                                to_upper_char symbol
                                                              )
                                                            )
                                                          )
                                                           (
                                                            begin (
                                                              let (
                                                                (
                                                                  num (
                                                                    find_index LETTERS upper_symbol
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                begin (
                                                                  if (
                                                                    _ge num 0
                                                                  )
                                                                   (
                                                                    begin (
                                                                      set! num (
                                                                        _add num (
                                                                          find_index LETTERS (
                                                                            cond (
                                                                              (
                                                                                string? key_up
                                                                              )
                                                                               (
                                                                                _substring key_up key_index (
                                                                                  + key_index 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? key_up
                                                                              )
                                                                               (
                                                                                hash-table-ref key_up key_index
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref key_up key_index
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      set! num (
                                                                        _mod num (
                                                                          _len LETTERS
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      if (
                                                                        is_upper symbol
                                                                      )
                                                                       (
                                                                        begin (
                                                                          set! encrypted (
                                                                            string-append encrypted (
                                                                              _substring LETTERS num (
                                                                                + num 1
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        begin (
                                                                          set! encrypted (
                                                                            string-append encrypted (
                                                                              to_lower_char (
                                                                                _substring LETTERS num (
                                                                                  + num 1
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      set! key_index (
                                                                        + key_index 1
                                                                      )
                                                                    )
                                                                     (
                                                                      if (
                                                                        equal? key_index (
                                                                          _len key_up
                                                                        )
                                                                      )
                                                                       (
                                                                        begin (
                                                                          set! key_index 0
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
                                                                      set! encrypted (
                                                                        string-append encrypted symbol
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
                                      _display (
                                        if (
                                          string? encrypted
                                        )
                                         encrypted (
                                          to-str encrypted
                                        )
                                      )
                                    )
                                     (
                                      newline
                                    )
                                     (
                                      let (
                                        (
                                          decrypted ""
                                        )
                                      )
                                       (
                                        begin (
                                          set! key_index 0
                                        )
                                         (
                                          set! i 0
                                        )
                                         (
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
                                                          _len encrypted
                                                        )
                                                      )
                                                       (
                                                        begin (
                                                          let (
                                                            (
                                                              symbol (
                                                                _substring encrypted i (
                                                                  + i 1
                                                                )
                                                              )
                                                            )
                                                          )
                                                           (
                                                            begin (
                                                              let (
                                                                (
                                                                  upper_symbol (
                                                                    to_upper_char symbol
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                begin (
                                                                  let (
                                                                    (
                                                                      num (
                                                                        find_index LETTERS upper_symbol
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    begin (
                                                                      if (
                                                                        _ge num 0
                                                                      )
                                                                       (
                                                                        begin (
                                                                          set! num (
                                                                            - num (
                                                                              find_index LETTERS (
                                                                                cond (
                                                                                  (
                                                                                    string? key_up
                                                                                  )
                                                                                   (
                                                                                    _substring key_up key_index (
                                                                                      + key_index 1
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  (
                                                                                    hash-table? key_up
                                                                                  )
                                                                                   (
                                                                                    hash-table-ref key_up key_index
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  else (
                                                                                    list-ref key_up key_index
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          set! num (
                                                                            _mod num (
                                                                              _len LETTERS
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          if (
                                                                            is_upper symbol
                                                                          )
                                                                           (
                                                                            begin (
                                                                              set! decrypted (
                                                                                string-append decrypted (
                                                                                  _substring LETTERS num (
                                                                                    + num 1
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            begin (
                                                                              set! decrypted (
                                                                                string-append decrypted (
                                                                                  to_lower_char (
                                                                                    _substring LETTERS num (
                                                                                      + num 1
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          set! key_index (
                                                                            + key_index 1
                                                                          )
                                                                        )
                                                                         (
                                                                          if (
                                                                            equal? key_index (
                                                                              _len key_up
                                                                            )
                                                                          )
                                                                           (
                                                                            begin (
                                                                              set! key_index 0
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
                                                                          set! decrypted (
                                                                            string-append decrypted symbol
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
                                          _display (
                                            if (
                                              string? decrypted
                                            )
                                             decrypted (
                                              to-str decrypted
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
