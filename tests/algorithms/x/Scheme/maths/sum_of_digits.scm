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
        abs_int n
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            begin (
              if (
                < n 0
              )
               (
                begin (
                  ret1 (
                    - n
                  )
                )
              )
               '(
                
              )
            )
             (
              ret1 n
            )
          )
        )
      )
    )
     (
      define (
        sum_of_digits n
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            let (
              (
                m (
                  abs_int n
                )
              )
            )
             (
              begin (
                let (
                  (
                    res 0
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
                                  _gt m 0
                                )
                                 (
                                  begin (
                                    set! res (
                                      _add res (
                                        _mod m 10
                                      )
                                    )
                                  )
                                   (
                                    set! m (
                                      _div m 10
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
        sum_of_digits_recursion n
      )
       (
        call/cc (
          lambda (
            ret5
          )
           (
            let (
              (
                m (
                  abs_int n
                )
              )
            )
             (
              begin (
                if (
                  _lt m 10
                )
                 (
                  begin (
                    ret5 m
                  )
                )
                 '(
                  
                )
              )
               (
                ret5 (
                  _add (
                    _mod m 10
                  )
                   (
                    sum_of_digits_recursion (
                      _div m 10
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
        sum_of_digits_compact n
      )
       (
        call/cc (
          lambda (
            ret6
          )
           (
            let (
              (
                s (
                  to-str-space (
                    abs_int n
                  )
                )
              )
            )
             (
              begin (
                let (
                  (
                    res 0
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
                            break8
                          )
                           (
                            letrec (
                              (
                                loop7 (
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
                                          + res (
                                            let (
                                              (
                                                v9 (
                                                  _substring s i (
                                                    + i 1
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              cond (
                                                (
                                                  string? v9
                                                )
                                                 (
                                                  inexact->exact (
                                                    floor (
                                                      string->number v9
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                (
                                                  boolean? v9
                                                )
                                                 (
                                                  if v9 1 0
                                                )
                                              )
                                               (
                                                else (
                                                  inexact->exact (
                                                    floor v9
                                                  )
                                                )
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
                                        loop7
                                      )
                                    )
                                     '(
                                      
                                    )
                                  )
                                )
                              )
                            )
                             (
                              loop7
                            )
                          )
                        )
                      )
                       (
                        ret6 res
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
        test_sum_of_digits
      )
       (
        call/cc (
          lambda (
            ret10
          )
           (
            begin (
              if (
                not (
                  equal? (
                    sum_of_digits 12345
                  )
                   15
                )
              )
               (
                begin (
                  panic "sum_of_digits 12345 failed"
                )
              )
               '(
                
              )
            )
             (
              if (
                not (
                  equal? (
                    sum_of_digits 123
                  )
                   6
                )
              )
               (
                begin (
                  panic "sum_of_digits 123 failed"
                )
              )
               '(
                
              )
            )
             (
              if (
                not (
                  equal? (
                    sum_of_digits (
                      - 123
                    )
                  )
                   6
                )
              )
               (
                begin (
                  panic "sum_of_digits -123 failed"
                )
              )
               '(
                
              )
            )
             (
              if (
                not (
                  equal? (
                    sum_of_digits 0
                  )
                   0
                )
              )
               (
                begin (
                  panic "sum_of_digits 0 failed"
                )
              )
               '(
                
              )
            )
             (
              if (
                not (
                  equal? (
                    sum_of_digits_recursion 12345
                  )
                   15
                )
              )
               (
                begin (
                  panic "recursion 12345 failed"
                )
              )
               '(
                
              )
            )
             (
              if (
                not (
                  equal? (
                    sum_of_digits_recursion 123
                  )
                   6
                )
              )
               (
                begin (
                  panic "recursion 123 failed"
                )
              )
               '(
                
              )
            )
             (
              if (
                not (
                  equal? (
                    sum_of_digits_recursion (
                      - 123
                    )
                  )
                   6
                )
              )
               (
                begin (
                  panic "recursion -123 failed"
                )
              )
               '(
                
              )
            )
             (
              if (
                not (
                  equal? (
                    sum_of_digits_recursion 0
                  )
                   0
                )
              )
               (
                begin (
                  panic "recursion 0 failed"
                )
              )
               '(
                
              )
            )
             (
              if (
                not (
                  equal? (
                    sum_of_digits_compact 12345
                  )
                   15
                )
              )
               (
                begin (
                  panic "compact 12345 failed"
                )
              )
               '(
                
              )
            )
             (
              if (
                not (
                  equal? (
                    sum_of_digits_compact 123
                  )
                   6
                )
              )
               (
                begin (
                  panic "compact 123 failed"
                )
              )
               '(
                
              )
            )
             (
              if (
                not (
                  equal? (
                    sum_of_digits_compact (
                      - 123
                    )
                  )
                   6
                )
              )
               (
                begin (
                  panic "compact -123 failed"
                )
              )
               '(
                
              )
            )
             (
              if (
                not (
                  equal? (
                    sum_of_digits_compact 0
                  )
                   0
                )
              )
               (
                begin (
                  panic "compact 0 failed"
                )
              )
               '(
                
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
              test_sum_of_digits
            )
             (
              _display (
                if (
                  string? (
                    to-str-space (
                      sum_of_digits 12345
                    )
                  )
                )
                 (
                  to-str-space (
                    sum_of_digits 12345
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      sum_of_digits 12345
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
