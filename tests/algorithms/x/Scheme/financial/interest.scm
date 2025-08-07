;; Generated on 2025-08-07 08:40 +0700
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
(
  let (
    (
      start10 (
        current-jiffy
      )
    )
     (
      jps13 (
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
        powf base exp
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            let (
              (
                result 1.0
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
                                  < i (
                                    let (
                                      (
                                        v3 exp
                                      )
                                    )
                                     (
                                      cond (
                                        (
                                          string? v3
                                        )
                                         (
                                          inexact->exact (
                                            floor (
                                              string->number v3
                                            )
                                          )
                                        )
                                      )
                                       (
                                        (
                                          boolean? v3
                                        )
                                         (
                                          if v3 1 0
                                        )
                                      )
                                       (
                                        else (
                                          inexact->exact (
                                            floor v3
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    set! result (
                                      * result base
                                    )
                                  )
                                   (
                                    set! i (
                                      + i 1
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
                    ret2 result
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
        simple_interest principal daily_rate days
      )
       (
        call/cc (
          lambda (
            ret6
          )
           (
            begin (
              if (
                <= days 0.0
              )
               (
                begin (
                  panic "days_between_payments must be > 0"
                )
                 (
                  ret6 0.0
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                < daily_rate 0.0
              )
               (
                begin (
                  panic "daily_interest_rate must be >= 0"
                )
                 (
                  ret6 0.0
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                <= principal 0.0
              )
               (
                begin (
                  panic "principal must be > 0"
                )
                 (
                  ret6 0.0
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              ret6 (
                * (
                  * principal daily_rate
                )
                 days
              )
            )
          )
        )
      )
    )
     (
      define (
        compound_interest principal nominal_rate periods
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            begin (
              if (
                <= periods 0.0
              )
               (
                begin (
                  panic "number_of_compounding_periods must be > 0"
                )
                 (
                  ret7 0.0
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                < nominal_rate 0.0
              )
               (
                begin (
                  panic "nominal_annual_interest_rate_percentage must be >= 0"
                )
                 (
                  ret7 0.0
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                <= principal 0.0
              )
               (
                begin (
                  panic "principal must be > 0"
                )
                 (
                  ret7 0.0
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              ret7 (
                * principal (
                  - (
                    powf (
                      + 1.0 nominal_rate
                    )
                     periods
                  )
                   1.0
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        apr_interest principal apr years
      )
       (
        call/cc (
          lambda (
            ret8
          )
           (
            begin (
              if (
                <= years 0.0
              )
               (
                begin (
                  panic "number_of_years must be > 0"
                )
                 (
                  ret8 0.0
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                < apr 0.0
              )
               (
                begin (
                  panic "nominal_annual_percentage_rate must be >= 0"
                )
                 (
                  ret8 0.0
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                <= principal 0.0
              )
               (
                begin (
                  panic "principal must be > 0"
                )
                 (
                  ret8 0.0
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              ret8 (
                compound_interest principal (
                  _div apr 365.0
                )
                 (
                  * years 365.0
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
            ret9
          )
           (
            begin (
              _display (
                if (
                  string? (
                    to-str-space (
                      simple_interest 18000.0 0.06 3.0
                    )
                  )
                )
                 (
                  to-str-space (
                    simple_interest 18000.0 0.06 3.0
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      simple_interest 18000.0 0.06 3.0
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
                      simple_interest 0.5 0.06 3.0
                    )
                  )
                )
                 (
                  to-str-space (
                    simple_interest 0.5 0.06 3.0
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      simple_interest 0.5 0.06 3.0
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
                      simple_interest 18000.0 0.01 10.0
                    )
                  )
                )
                 (
                  to-str-space (
                    simple_interest 18000.0 0.01 10.0
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      simple_interest 18000.0 0.01 10.0
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
                      compound_interest 10000.0 0.05 3.0
                    )
                  )
                )
                 (
                  to-str-space (
                    compound_interest 10000.0 0.05 3.0
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      compound_interest 10000.0 0.05 3.0
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
                      compound_interest 10000.0 0.05 1.0
                    )
                  )
                )
                 (
                  to-str-space (
                    compound_interest 10000.0 0.05 1.0
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      compound_interest 10000.0 0.05 1.0
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
                      apr_interest 10000.0 0.05 3.0
                    )
                  )
                )
                 (
                  to-str-space (
                    apr_interest 10000.0 0.05 3.0
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      apr_interest 10000.0 0.05 3.0
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
                      apr_interest 10000.0 0.05 1.0
                    )
                  )
                )
                 (
                  to-str-space (
                    apr_interest 10000.0 0.05 1.0
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      apr_interest 10000.0 0.05 1.0
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
          end11 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur12 (
              quotient (
                * (
                  - end11 start10
                )
                 1000000
              )
               jps13
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur12
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
