;; Generated on 2025-08-07 16:11 +0700
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
        ((number? x)
         (if (integer? x)
             (number->string (inexact->exact x))
             (number->string x)))
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
(define (_div a b) (if (and (integer? a) (integer? b) (exact? a) (exact? b)) (quotient a b) (/ a b)))
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
(define (panic msg) (error msg))
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
(define (list-ref-safe lst idx) (if (and (integer? idx) (>= idx 0) (< idx (length lst))) (list-ref lst idx) '()))
(
  let (
    (
      start19 (
        current-jiffy
      )
    )
     (
      jps22 (
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
            begin
          )
        )
      )
    )
     (
      define (
        char_to_value c
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            let (
              (
                digits "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
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
                                    _len digits
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      string=? (
                                        _substring digits i (
                                          + i 1
                                        )
                                      )
                                       c
                                    )
                                     (
                                      begin (
                                        ret2 i
                                      )
                                    )
                                     '(
                                      
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
                                 '(
                                  
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
                    panic "invalid digit"
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
        int_to_base number base
      )
       (
        call/cc (
          lambda (
            ret5
          )
           (
            begin (
              if (
                or (
                  < base 2
                )
                 (
                  > base 36
                )
              )
               (
                begin (
                  panic "'base' must be between 2 and 36 inclusive"
                )
              )
               '(
                
              )
            )
             (
              if (
                < number 0
              )
               (
                begin (
                  panic "number must be a positive integer"
                )
              )
               '(
                
              )
            )
             (
              let (
                (
                  digits "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                )
              )
               (
                begin (
                  let (
                    (
                      n number
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          result ""
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
                                        > n 0
                                      )
                                       (
                                        begin (
                                          let (
                                            (
                                              remainder (
                                                _mod n base
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              set! result (
                                                string-append (
                                                  _substring digits remainder (
                                                    + remainder 1
                                                  )
                                                )
                                                 result
                                              )
                                            )
                                             (
                                              set! n (
                                                _div n base
                                              )
                                            )
                                          )
                                        )
                                         (
                                          loop6
                                        )
                                      )
                                       '(
                                        
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
                          if (
                            string=? result ""
                          )
                           (
                            begin (
                              set! result "0"
                            )
                          )
                           '(
                            
                          )
                        )
                         (
                          ret5 result
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
        base_to_int num_str base
      )
       (
        call/cc (
          lambda (
            ret8
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
                                    _len num_str
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        c (
                                          _substring num_str i (
                                            + i 1
                                          )
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        set! value (
                                          _add (
                                            * value base
                                          )
                                           (
                                            char_to_value c
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
                                 '(
                                  
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
                    ret8 value
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
        sum_of_digits num base
      )
       (
        call/cc (
          lambda (
            ret11
          )
           (
            begin (
              if (
                or (
                  < base 2
                )
                 (
                  > base 36
                )
              )
               (
                begin (
                  panic "'base' must be between 2 and 36 inclusive"
                )
              )
               '(
                
              )
            )
             (
              let (
                (
                  num_str (
                    int_to_base num base
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      total 0
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
                                          _len num_str
                                        )
                                      )
                                       (
                                        begin (
                                          let (
                                            (
                                              c (
                                                cond (
                                                  (
                                                    string? num_str
                                                  )
                                                   (
                                                    _substring num_str i (
                                                      + i 1
                                                    )
                                                  )
                                                )
                                                 (
                                                  (
                                                    hash-table? num_str
                                                  )
                                                   (
                                                    hash-table-ref num_str i
                                                  )
                                                )
                                                 (
                                                  else (
                                                    list-ref-safe num_str i
                                                  )
                                                )
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              set! total (
                                                _add total (
                                                  char_to_value c
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
                                       '(
                                        
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
                          ret11 (
                            int_to_base total base
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
        harshad_numbers_in_base limit base
      )
       (
        call/cc (
          lambda (
            ret14
          )
           (
            begin (
              if (
                or (
                  < base 2
                )
                 (
                  > base 36
                )
              )
               (
                begin (
                  panic "'base' must be between 2 and 36 inclusive"
                )
              )
               '(
                
              )
            )
             (
              if (
                < limit 0
              )
               (
                begin (
                  ret14 (
                    _list
                  )
                )
              )
               '(
                
              )
            )
             (
              let (
                (
                  numbers (
                    _list
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      i 1
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
                                    < i limit
                                  )
                                   (
                                    begin (
                                      let (
                                        (
                                          s (
                                            sum_of_digits i base
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          let (
                                            (
                                              divisor (
                                                base_to_int s base
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              if (
                                                equal? (
                                                  _mod i divisor
                                                )
                                                 0
                                              )
                                               (
                                                begin (
                                                  set! numbers (
                                                    append numbers (
                                                      _list (
                                                        int_to_base i base
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                               '(
                                                
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
                                      loop15
                                    )
                                  )
                                   '(
                                    
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
                      ret14 numbers
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
        is_harshad_number_in_base num base
      )
       (
        call/cc (
          lambda (
            ret17
          )
           (
            begin (
              if (
                or (
                  < base 2
                )
                 (
                  > base 36
                )
              )
               (
                begin (
                  panic "'base' must be between 2 and 36 inclusive"
                )
              )
               '(
                
              )
            )
             (
              if (
                < num 0
              )
               (
                begin (
                  ret17 #f
                )
              )
               '(
                
              )
            )
             (
              let (
                (
                  n (
                    int_to_base num base
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      d (
                        sum_of_digits num base
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          n_val (
                            base_to_int n base
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              d_val (
                                base_to_int d base
                              )
                            )
                          )
                           (
                            begin (
                              ret17 (
                                equal? (
                                  _mod n_val d_val
                                )
                                 0
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
        main
      )
       (
        call/cc (
          lambda (
            ret18
          )
           (
            begin (
              _display (
                if (
                  string? (
                    int_to_base 0 21
                  )
                )
                 (
                  int_to_base 0 21
                )
                 (
                  to-str (
                    int_to_base 0 21
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
                    int_to_base 23 2
                  )
                )
                 (
                  int_to_base 23 2
                )
                 (
                  to-str (
                    int_to_base 23 2
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
                    int_to_base 58 5
                  )
                )
                 (
                  int_to_base 58 5
                )
                 (
                  to-str (
                    int_to_base 58 5
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
                    int_to_base 167 16
                  )
                )
                 (
                  int_to_base 167 16
                )
                 (
                  to-str (
                    int_to_base 167 16
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
                    sum_of_digits 103 12
                  )
                )
                 (
                  sum_of_digits 103 12
                )
                 (
                  to-str (
                    sum_of_digits 103 12
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
                    sum_of_digits 1275 4
                  )
                )
                 (
                  sum_of_digits 1275 4
                )
                 (
                  to-str (
                    sum_of_digits 1275 4
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
                    sum_of_digits 6645 2
                  )
                )
                 (
                  sum_of_digits 6645 2
                )
                 (
                  to-str (
                    sum_of_digits 6645 2
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
                    harshad_numbers_in_base 15 2
                  )
                )
                 (
                  harshad_numbers_in_base 15 2
                )
                 (
                  to-str (
                    harshad_numbers_in_base 15 2
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
                    harshad_numbers_in_base 12 34
                  )
                )
                 (
                  harshad_numbers_in_base 12 34
                )
                 (
                  to-str (
                    harshad_numbers_in_base 12 34
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
                    harshad_numbers_in_base 12 4
                  )
                )
                 (
                  harshad_numbers_in_base 12 4
                )
                 (
                  to-str (
                    harshad_numbers_in_base 12 4
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
                    is_harshad_number_in_base 18 10
                  )
                )
                 (
                  is_harshad_number_in_base 18 10
                )
                 (
                  to-str (
                    is_harshad_number_in_base 18 10
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
                    is_harshad_number_in_base 21 10
                  )
                )
                 (
                  is_harshad_number_in_base 21 10
                )
                 (
                  to-str (
                    is_harshad_number_in_base 21 10
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
                    is_harshad_number_in_base (
                      - 21
                    )
                     5
                  )
                )
                 (
                  is_harshad_number_in_base (
                    - 21
                  )
                   5
                )
                 (
                  to-str (
                    is_harshad_number_in_base (
                      - 21
                    )
                     5
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
          end20 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur21 (
              quotient (
                * (
                  - end20 start19
                )
                 1000000
              )
               jps22
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur21
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
