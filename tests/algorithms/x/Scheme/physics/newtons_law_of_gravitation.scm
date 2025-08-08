;; Generated on 2025-08-08 16:57 +0700
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
(define _floor floor)
(define (fmod a b) (- a (* (_floor (/ a b)) b)))
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
      start3 (
        current-jiffy
      )
    )
     (
      jps6 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      let (
        (
          GRAVITATIONAL_CONSTANT 6.6743e-11
        )
      )
       (
        begin (
          define (
            sqrtApprox x
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
                    letrec (
                      (
                        loop1 (
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
                                loop1
                              )
                            )
                             '(
                              
                            )
                          )
                        )
                      )
                    )
                     (
                      loop1
                    )
                  )
                   guess
                )
              )
            )
          )
        )
         (
          define (
            gravitational_law force mass_1 mass_2 distance
          )
           (
            call/cc (
              lambda (
                ret2
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
                      equal? force 0.0
                    )
                     (
                      begin (
                        set! zero_count (
                          + zero_count 1
                        )
                      )
                    )
                     '(
                      
                    )
                  )
                   (
                    if (
                      equal? mass_1 0.0
                    )
                     (
                      begin (
                        set! zero_count (
                          + zero_count 1
                        )
                      )
                    )
                     '(
                      
                    )
                  )
                   (
                    if (
                      equal? mass_2 0.0
                    )
                     (
                      begin (
                        set! zero_count (
                          + zero_count 1
                        )
                      )
                    )
                     '(
                      
                    )
                  )
                   (
                    if (
                      equal? distance 0.0
                    )
                     (
                      begin (
                        set! zero_count (
                          + zero_count 1
                        )
                      )
                    )
                     '(
                      
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
                     '(
                      
                    )
                  )
                   (
                    if (
                      < force 0.0
                    )
                     (
                      begin (
                        panic "Gravitational force can not be negative"
                      )
                    )
                     '(
                      
                    )
                  )
                   (
                    if (
                      < distance 0.0
                    )
                     (
                      begin (
                        panic "Distance can not be negative"
                      )
                    )
                     '(
                      
                    )
                  )
                   (
                    if (
                      < mass_1 0.0
                    )
                     (
                      begin (
                        panic "Mass can not be negative"
                      )
                    )
                     '(
                      
                    )
                  )
                   (
                    if (
                      < mass_2 0.0
                    )
                     (
                      begin (
                        panic "Mass can not be negative"
                      )
                    )
                     '(
                      
                    )
                  )
                   (
                    let (
                      (
                        product_of_mass (
                          * mass_1 mass_2
                        )
                      )
                    )
                     (
                      begin (
                        if (
                          equal? force 0.0
                        )
                         (
                          begin (
                            let (
                              (
                                f (
                                  _div (
                                    * GRAVITATIONAL_CONSTANT product_of_mass
                                  )
                                   (
                                    * distance distance
                                  )
                                )
                              )
                            )
                             (
                              begin (
                                ret2 (
                                  alist->hash-table (
                                    _list (
                                      cons "kind" "force"
                                    )
                                     (
                                      cons "value" f
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                         '(
                          
                        )
                      )
                       (
                        if (
                          equal? mass_1 0.0
                        )
                         (
                          begin (
                            let (
                              (
                                m1 (
                                  _div (
                                    * force (
                                      * distance distance
                                    )
                                  )
                                   (
                                    * GRAVITATIONAL_CONSTANT mass_2
                                  )
                                )
                              )
                            )
                             (
                              begin (
                                ret2 (
                                  alist->hash-table (
                                    _list (
                                      cons "kind" "mass_1"
                                    )
                                     (
                                      cons "value" m1
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                         '(
                          
                        )
                      )
                       (
                        if (
                          equal? mass_2 0.0
                        )
                         (
                          begin (
                            let (
                              (
                                m2 (
                                  _div (
                                    * force (
                                      * distance distance
                                    )
                                  )
                                   (
                                    * GRAVITATIONAL_CONSTANT mass_1
                                  )
                                )
                              )
                            )
                             (
                              begin (
                                ret2 (
                                  alist->hash-table (
                                    _list (
                                      cons "kind" "mass_2"
                                    )
                                     (
                                      cons "value" m2
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                         '(
                          
                        )
                      )
                       (
                        let (
                          (
                            d (
                              sqrtApprox (
                                _div (
                                  * GRAVITATIONAL_CONSTANT product_of_mass
                                )
                                 force
                              )
                            )
                          )
                        )
                         (
                          begin (
                            ret2 (
                              alist->hash-table (
                                _list (
                                  cons "kind" "distance"
                                )
                                 (
                                  cons "value" d
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
              r1 (
                gravitational_law 0.0 5.0 10.0 20.0
              )
            )
          )
           (
            begin (
              let (
                (
                  r2 (
                    gravitational_law 7367.382 0.0 74.0 3048.0
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      r3 (
                        gravitational_law 100.0 5.0 0.0 3.0
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          r4 (
                            gravitational_law 100.0 5.0 10.0 0.0
                          )
                        )
                      )
                       (
                        begin (
                          _display (
                            if (
                              string? (
                                string-append (
                                  string-append (
                                    hash-table-ref r1 "kind"
                                  )
                                   " "
                                )
                                 (
                                  to-str-space (
                                    hash-table-ref r1 "value"
                                  )
                                )
                              )
                            )
                             (
                              string-append (
                                string-append (
                                  hash-table-ref r1 "kind"
                                )
                                 " "
                              )
                               (
                                to-str-space (
                                  hash-table-ref r1 "value"
                                )
                              )
                            )
                             (
                              to-str (
                                string-append (
                                  string-append (
                                    hash-table-ref r1 "kind"
                                  )
                                   " "
                                )
                                 (
                                  to-str-space (
                                    hash-table-ref r1 "value"
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
                                string-append (
                                  string-append (
                                    hash-table-ref r2 "kind"
                                  )
                                   " "
                                )
                                 (
                                  to-str-space (
                                    hash-table-ref r2 "value"
                                  )
                                )
                              )
                            )
                             (
                              string-append (
                                string-append (
                                  hash-table-ref r2 "kind"
                                )
                                 " "
                              )
                               (
                                to-str-space (
                                  hash-table-ref r2 "value"
                                )
                              )
                            )
                             (
                              to-str (
                                string-append (
                                  string-append (
                                    hash-table-ref r2 "kind"
                                  )
                                   " "
                                )
                                 (
                                  to-str-space (
                                    hash-table-ref r2 "value"
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
                                string-append (
                                  string-append (
                                    hash-table-ref r3 "kind"
                                  )
                                   " "
                                )
                                 (
                                  to-str-space (
                                    hash-table-ref r3 "value"
                                  )
                                )
                              )
                            )
                             (
                              string-append (
                                string-append (
                                  hash-table-ref r3 "kind"
                                )
                                 " "
                              )
                               (
                                to-str-space (
                                  hash-table-ref r3 "value"
                                )
                              )
                            )
                             (
                              to-str (
                                string-append (
                                  string-append (
                                    hash-table-ref r3 "kind"
                                  )
                                   " "
                                )
                                 (
                                  to-str-space (
                                    hash-table-ref r3 "value"
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
                                string-append (
                                  string-append (
                                    hash-table-ref r4 "kind"
                                  )
                                   " "
                                )
                                 (
                                  to-str-space (
                                    hash-table-ref r4 "value"
                                  )
                                )
                              )
                            )
                             (
                              string-append (
                                string-append (
                                  hash-table-ref r4 "kind"
                                )
                                 " "
                              )
                               (
                                to-str-space (
                                  hash-table-ref r4 "value"
                                )
                              )
                            )
                             (
                              to-str (
                                string-append (
                                  string-append (
                                    hash-table-ref r4 "kind"
                                  )
                                   " "
                                )
                                 (
                                  to-str-space (
                                    hash-table-ref r4 "value"
                                  )
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
              )
            )
          )
        )
      )
    )
     (
      let (
        (
          end4 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur5 (
              quotient (
                * (
                  - end4 start3
                )
                 1000000
              )
               jps6
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur5
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
