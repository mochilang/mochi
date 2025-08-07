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
        powf base exponent
      )
       (
        call/cc (
          lambda (
            ret1
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
                                  < i exponent
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
                    ret1 result
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
        round2 value
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            begin (
              if (
                >= value 0.0
              )
               (
                begin (
                  let (
                    (
                      scaled (
                        let (
                          (
                            v5 (
                              _add (
                                * value 100.0
                              )
                               0.5
                            )
                          )
                        )
                         (
                          cond (
                            (
                              string? v5
                            )
                             (
                              inexact->exact (
                                floor (
                                  string->number v5
                                )
                              )
                            )
                          )
                           (
                            (
                              boolean? v5
                            )
                             (
                              if v5 1 0
                            )
                          )
                           (
                            else (
                              inexact->exact (
                                floor v5
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                   (
                    begin (
                      ret4 (
                        _div (
                          + 0.0 scaled
                        )
                         100.0
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
                  scaled (
                    let (
                      (
                        v6 (
                          - (
                            * value 100.0
                          )
                           0.5
                        )
                      )
                    )
                     (
                      cond (
                        (
                          string? v6
                        )
                         (
                          inexact->exact (
                            floor (
                              string->number v6
                            )
                          )
                        )
                      )
                       (
                        (
                          boolean? v6
                        )
                         (
                          if v6 1 0
                        )
                      )
                       (
                        else (
                          inexact->exact (
                            floor v6
                          )
                        )
                      )
                    )
                  )
                )
              )
               (
                begin (
                  ret4 (
                    _div (
                      + 0.0 scaled
                    )
                     100.0
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
        present_value discount_rate cash_flows
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            begin (
              if (
                < discount_rate 0.0
              )
               (
                begin (
                  panic "Discount rate cannot be negative"
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
                  _len cash_flows
                )
                 0
              )
               (
                begin (
                  panic "Cash flows list cannot be empty"
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
                  pv 0.0
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
                      let (
                        (
                          factor (
                            + 1.0 discount_rate
                          )
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
                                          _len cash_flows
                                        )
                                      )
                                       (
                                        begin (
                                          let (
                                            (
                                              cf (
                                                list-ref cash_flows i
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              set! pv (
                                                _add pv (
                                                  _div cf (
                                                    powf factor i
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
                          ret7 (
                            round2 pv
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
              present_value 0.13 (
                _list 10.0 20.7 (
                  - 293.0
                )
                 297.0
              )
            )
          )
        )
         (
          to-str-space (
            present_value 0.13 (
              _list 10.0 20.7 (
                - 293.0
              )
               297.0
            )
          )
        )
         (
          to-str (
            to-str-space (
              present_value 0.13 (
                _list 10.0 20.7 (
                  - 293.0
                )
                 297.0
              )
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
              present_value 0.07 (
                _list (
                  - 109129.39
                )
                 30923.23 15098.93 29734.0 39.0
              )
            )
          )
        )
         (
          to-str-space (
            present_value 0.07 (
              _list (
                - 109129.39
              )
               30923.23 15098.93 29734.0 39.0
            )
          )
        )
         (
          to-str (
            to-str-space (
              present_value 0.07 (
                _list (
                  - 109129.39
                )
                 30923.23 15098.93 29734.0 39.0
              )
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
              present_value 0.07 (
                _list 109129.39 30923.23 15098.93 29734.0 39.0
              )
            )
          )
        )
         (
          to-str-space (
            present_value 0.07 (
              _list 109129.39 30923.23 15098.93 29734.0 39.0
            )
          )
        )
         (
          to-str (
            to-str-space (
              present_value 0.07 (
                _list 109129.39 30923.23 15098.93 29734.0 39.0
              )
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
