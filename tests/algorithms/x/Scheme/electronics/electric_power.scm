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
      start8 (
        current-jiffy
      )
    )
     (
      jps11 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        absf x
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            begin (
              if (
                < x 0.0
              )
               (
                begin (
                  ret1 (
                    - x
                  )
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              ret1 x
            )
          )
        )
      )
    )
     (
      define (
        pow10 n
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            let (
              (
                p 1.0
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
                                  < i n
                                )
                                 (
                                  begin (
                                    set! p (
                                      * p 10.0
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
                    ret2 p
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
        round_to x n
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
                  pow10 n
                )
              )
            )
             (
              begin (
                ret5 (
                  _div (
                    floor (
                      _add (
                        * x m
                      )
                       0.5
                    )
                  )
                   m
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        electric_power voltage current power
      )
       (
        call/cc (
          lambda (
            ret6
          )
           (
            let (
              (
                zeros 0
              )
            )
             (
              begin (
                if (
                  equal? voltage 0.0
                )
                 (
                  begin (
                    set! zeros (
                      + zeros 1
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
                  equal? current 0.0
                )
                 (
                  begin (
                    set! zeros (
                      + zeros 1
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
                  equal? power 0.0
                )
                 (
                  begin (
                    set! zeros (
                      + zeros 1
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
                  not (
                    equal? zeros 1
                  )
                )
                 (
                  begin (
                    panic "Exactly one argument must be 0"
                  )
                )
                 (
                  if (
                    < power 0.0
                  )
                   (
                    begin (
                      panic "Power cannot be negative in any electrical/electronics system"
                    )
                  )
                   (
                    if (
                      equal? voltage 0.0
                    )
                     (
                      begin (
                        ret6 (
                          alist->hash-table (
                            _list (
                              cons "name" "voltage"
                            )
                             (
                              cons "value" (
                                _div power current
                              )
                            )
                          )
                        )
                      )
                    )
                     (
                      if (
                        equal? current 0.0
                      )
                       (
                        begin (
                          ret6 (
                            alist->hash-table (
                              _list (
                                cons "name" "current"
                              )
                               (
                                cons "value" (
                                  _div power voltage
                                )
                              )
                            )
                          )
                        )
                      )
                       (
                        if (
                          equal? power 0.0
                        )
                         (
                          begin (
                            let (
                              (
                                p (
                                  absf (
                                    * voltage current
                                  )
                                )
                              )
                            )
                             (
                              begin (
                                ret6 (
                                  alist->hash-table (
                                    _list (
                                      cons "name" "power"
                                    )
                                     (
                                      cons "value" (
                                        round_to p 2
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
                            panic "Unhandled case"
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
        str_result r
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            ret7 (
              string-append (
                string-append (
                  string-append (
                    string-append "Result(name='" (
                      hash-table-ref r "name"
                    )
                  )
                   "', value="
                )
                 (
                  to-str-space (
                    hash-table-ref r "value"
                  )
                )
              )
               ")"
            )
          )
        )
      )
    )
     (
      let (
        (
          r1 (
            electric_power 0.0 2.0 5.0
          )
        )
      )
       (
        begin (
          _display (
            if (
              string? (
                str_result r1
              )
            )
             (
              str_result r1
            )
             (
              to-str (
                str_result r1
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
              r2 (
                electric_power 2.0 2.0 0.0
              )
            )
          )
           (
            begin (
              _display (
                if (
                  string? (
                    str_result r2
                  )
                )
                 (
                  str_result r2
                )
                 (
                  to-str (
                    str_result r2
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
                  r3 (
                    electric_power (
                      - 2.0
                    )
                     3.0 0.0
                  )
                )
              )
               (
                begin (
                  _display (
                    if (
                      string? (
                        str_result r3
                      )
                    )
                     (
                      str_result r3
                    )
                     (
                      to-str (
                        str_result r3
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
                      r4 (
                        electric_power 2.2 2.2 0.0
                      )
                    )
                  )
                   (
                    begin (
                      _display (
                        if (
                          string? (
                            str_result r4
                          )
                        )
                         (
                          str_result r4
                        )
                         (
                          to-str (
                            str_result r4
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
                          r5 (
                            electric_power 2.0 0.0 6.0
                          )
                        )
                      )
                       (
                        begin (
                          _display (
                            if (
                              string? (
                                str_result r5
                              )
                            )
                             (
                              str_result r5
                            )
                             (
                              to-str (
                                str_result r5
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
          end9 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur10 (
              quotient (
                * (
                  - end9 start8
                )
                 1000000
              )
               jps11
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur10
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
