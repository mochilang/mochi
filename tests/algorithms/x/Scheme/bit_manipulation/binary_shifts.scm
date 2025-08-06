;; Generated on 2025-08-06 21:38 +0700
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
      start15 (
        current-jiffy
      )
    )
     (
      jps18 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        repeat_char ch count
      )
       (
        call/cc (
          lambda (
            ret1
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
                                  < i count
                                )
                                 (
                                  begin (
                                    set! res (
                                      string-append res ch
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
        abs_int n
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            begin (
              if (
                < n 0
              )
               (
                begin (
                  ret4 (
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
              ret4 n
            )
          )
        )
      )
    )
     (
      define (
        pow2 exp
      )
       (
        call/cc (
          lambda (
            ret5
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
                                  < i exp
                                )
                                 (
                                  begin (
                                    set! res (
                                      * res 2
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
        to_binary_no_prefix n
      )
       (
        call/cc (
          lambda (
            ret8
          )
           (
            let (
              (
                v n
              )
            )
             (
              begin (
                if (
                  < v 0
                )
                 (
                  begin (
                    set! v (
                      - v
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
                  equal? v 0
                )
                 (
                  begin (
                    ret8 "0"
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
                    res ""
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
                                  > v 0
                                )
                                 (
                                  begin (
                                    set! res (
                                      string-append (
                                        to-str-space (
                                          modulo v 2
                                        )
                                      )
                                       res
                                    )
                                  )
                                   (
                                    set! v (
                                      quotient v 2
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
     (
      define (
        logical_left_shift number shift_amount
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
                  < number 0
                )
                 (
                  < shift_amount 0
                )
              )
               (
                begin (
                  panic "both inputs must be positive integers"
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
                  binary_number (
                    string-append "0b" (
                      to_binary_no_prefix number
                    )
                  )
                )
              )
               (
                begin (
                  ret11 (
                    _add binary_number (
                      repeat_char "0" shift_amount
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
        logical_right_shift number shift_amount
      )
       (
        call/cc (
          lambda (
            ret12
          )
           (
            begin (
              if (
                or (
                  < number 0
                )
                 (
                  < shift_amount 0
                )
              )
               (
                begin (
                  panic "both inputs must be positive integers"
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
                  binary_number (
                    to_binary_no_prefix number
                  )
                )
              )
               (
                begin (
                  if (
                    >= shift_amount (
                      _len binary_number
                    )
                  )
                   (
                    begin (
                      ret12 "0b0"
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
                      shifted (
                        _substring binary_number 0 (
                          - (
                            _len binary_number
                          )
                           shift_amount
                        )
                      )
                    )
                  )
                   (
                    begin (
                      ret12 (
                        string-append "0b" shifted
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
        arithmetic_right_shift number shift_amount
      )
       (
        call/cc (
          lambda (
            ret13
          )
           (
            let (
              (
                binary_number ""
              )
            )
             (
              begin (
                if (
                  >= number 0
                )
                 (
                  begin (
                    set! binary_number (
                      string-append "0" (
                        to_binary_no_prefix number
                      )
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        length (
                          _len (
                            to_binary_no_prefix (
                              - number
                            )
                          )
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            intermediate (
                              - (
                                abs_int number
                              )
                               (
                                pow2 length
                              )
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                bin_repr (
                                  to_binary_no_prefix intermediate
                                )
                              )
                            )
                             (
                              begin (
                                set! binary_number (
                                  string-append (
                                    string-append "1" (
                                      repeat_char "0" (
                                        - length (
                                          _len bin_repr
                                        )
                                      )
                                    )
                                  )
                                   bin_repr
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
                if (
                  >= shift_amount (
                    _len binary_number
                  )
                )
                 (
                  begin (
                    let (
                      (
                        sign (
                          _substring binary_number 0 1
                        )
                      )
                    )
                     (
                      begin (
                        ret13 (
                          string-append "0b" (
                            repeat_char sign (
                              _len binary_number
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
                let (
                  (
                    sign (
                      _substring binary_number 0 1
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        shifted (
                          _substring binary_number 0 (
                            - (
                              _len binary_number
                            )
                             shift_amount
                          )
                        )
                      )
                    )
                     (
                      begin (
                        ret13 (
                          string-append (
                            string-append "0b" (
                              repeat_char sign shift_amount
                            )
                          )
                           shifted
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
            ret14
          )
           (
            begin (
              _display (
                if (
                  string? (
                    logical_left_shift 17 2
                  )
                )
                 (
                  logical_left_shift 17 2
                )
                 (
                  to-str (
                    logical_left_shift 17 2
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
                    logical_right_shift 1983 4
                  )
                )
                 (
                  logical_right_shift 1983 4
                )
                 (
                  to-str (
                    logical_right_shift 1983 4
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
                    arithmetic_right_shift (
                      - 17
                    )
                     2
                  )
                )
                 (
                  arithmetic_right_shift (
                    - 17
                  )
                   2
                )
                 (
                  to-str (
                    arithmetic_right_shift (
                      - 17
                    )
                     2
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
          end16 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur17 (
              quotient (
                * (
                  - end16 start15
                )
                 1000000
              )
               jps18
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur17
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
