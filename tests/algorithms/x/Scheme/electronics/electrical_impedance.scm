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
      start5 (
        current-jiffy
      )
    )
     (
      jps8 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        sqrtApprox x
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            begin (
              if (
                <= x 0.0
              )
               (
                begin (
                  ret1 0.0
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
                  guess (
                    _div x 2.0
                  )
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
                                    < i 20
                                  )
                                   (
                                    begin (
                                      set! guess (
                                        _div (
                                          _add guess (
                                            _div x guess
                                          )
                                        )
                                         2.0
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
                      ret1 guess
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
        electrical_impedance resistance reactance impedance
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            let (
              (
                zero_count 0
              )
            )
             (
              begin (
                if (
                  equal? resistance 0.0
                )
                 (
                  begin (
                    set! zero_count (
                      + zero_count 1
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
                  equal? reactance 0.0
                )
                 (
                  begin (
                    set! zero_count (
                      + zero_count 1
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
                  equal? impedance 0.0
                )
                 (
                  begin (
                    set! zero_count (
                      + zero_count 1
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
                    equal? zero_count 1
                  )
                )
                 (
                  begin (
                    panic "One and only one argument must be 0"
                  )
                )
                 (
                  quote (
                    
                  )
                )
              )
               (
                if (
                  equal? resistance 0.0
                )
                 (
                  begin (
                    let (
                      (
                        value (
                          sqrtApprox (
                            - (
                              * impedance impedance
                            )
                             (
                              * reactance reactance
                            )
                          )
                        )
                      )
                    )
                     (
                      begin (
                        ret4 (
                          alist->hash-table (
                            _list (
                              cons "resistance" value
                            )
                          )
                        )
                      )
                    )
                  )
                )
                 (
                  if (
                    equal? reactance 0.0
                  )
                   (
                    begin (
                      let (
                        (
                          value (
                            sqrtApprox (
                              - (
                                * impedance impedance
                              )
                               (
                                * resistance resistance
                              )
                            )
                          )
                        )
                      )
                       (
                        begin (
                          ret4 (
                            alist->hash-table (
                              _list (
                                cons "reactance" value
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                   (
                    if (
                      equal? impedance 0.0
                    )
                     (
                      begin (
                        let (
                          (
                            value (
                              sqrtApprox (
                                _add (
                                  * resistance resistance
                                )
                                 (
                                  * reactance reactance
                                )
                              )
                            )
                          )
                        )
                         (
                          begin (
                            ret4 (
                              alist->hash-table (
                                _list (
                                  cons "impedance" value
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                     (
                      begin (
                        panic "Exactly one argument must be 0"
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
            electrical_impedance 3.0 4.0 0.0
          )
        )
         (
          electrical_impedance 3.0 4.0 0.0
        )
         (
          to-str (
            electrical_impedance 3.0 4.0 0.0
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
            electrical_impedance 0.0 4.0 5.0
          )
        )
         (
          electrical_impedance 0.0 4.0 5.0
        )
         (
          to-str (
            electrical_impedance 0.0 4.0 5.0
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
            electrical_impedance 3.0 0.0 5.0
          )
        )
         (
          electrical_impedance 3.0 0.0 5.0
        )
         (
          to-str (
            electrical_impedance 3.0 0.0 5.0
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
          end6 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur7 (
              quotient (
                * (
                  - end6 start5
                )
                 1000000
              )
               jps8
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur7
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
