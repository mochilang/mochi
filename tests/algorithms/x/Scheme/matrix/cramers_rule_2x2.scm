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
      start4 (
        current-jiffy
      )
    )
     (
      jps7 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        cramers_rule_2x2 eq1 eq2
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            begin (
              if (
                or (
                  not (
                    equal? (
                      _len eq1
                    )
                     3
                  )
                )
                 (
                  not (
                    equal? (
                      _len eq2
                    )
                     3
                  )
                )
              )
               (
                begin (
                  panic "Please enter a valid equation."
                )
              )
               '(
                
              )
            )
             (
              if (
                and (
                  and (
                    and (
                      equal? (
                        list-ref-safe eq1 0
                      )
                       0.0
                    )
                     (
                      equal? (
                        list-ref-safe eq1 1
                      )
                       0.0
                    )
                  )
                   (
                    equal? (
                      list-ref-safe eq2 0
                    )
                     0.0
                  )
                )
                 (
                  equal? (
                    list-ref-safe eq2 1
                  )
                   0.0
                )
              )
               (
                begin (
                  panic "Both a & b of two equations can't be zero."
                )
              )
               '(
                
              )
            )
             (
              let (
                (
                  a1 (
                    list-ref-safe eq1 0
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      b1 (
                        list-ref-safe eq1 1
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          c1 (
                            list-ref-safe eq1 2
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              a2 (
                                list-ref-safe eq2 0
                              )
                            )
                          )
                           (
                            begin (
                              let (
                                (
                                  b2 (
                                    list-ref-safe eq2 1
                                  )
                                )
                              )
                               (
                                begin (
                                  let (
                                    (
                                      c2 (
                                        list-ref-safe eq2 2
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      let (
                                        (
                                          determinant (
                                            - (
                                              * a1 b2
                                            )
                                             (
                                              * a2 b1
                                            )
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          let (
                                            (
                                              determinant_x (
                                                - (
                                                  * c1 b2
                                                )
                                                 (
                                                  * c2 b1
                                                )
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              let (
                                                (
                                                  determinant_y (
                                                    - (
                                                      * a1 c2
                                                    )
                                                     (
                                                      * a2 c1
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                begin (
                                                  if (
                                                    equal? determinant 0.0
                                                  )
                                                   (
                                                    begin (
                                                      if (
                                                        and (
                                                          equal? determinant_x 0.0
                                                        )
                                                         (
                                                          equal? determinant_y 0.0
                                                        )
                                                      )
                                                       (
                                                        begin (
                                                          panic "Infinite solutions. (Consistent system)"
                                                        )
                                                      )
                                                       '(
                                                        
                                                      )
                                                    )
                                                     (
                                                      panic "No solution. (Inconsistent system)"
                                                    )
                                                  )
                                                   '(
                                                    
                                                  )
                                                )
                                                 (
                                                  if (
                                                    and (
                                                      equal? determinant_x 0.0
                                                    )
                                                     (
                                                      equal? determinant_y 0.0
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      ret1 (
                                                        _list 0.0 0.0
                                                      )
                                                    )
                                                  )
                                                   '(
                                                    
                                                  )
                                                )
                                                 (
                                                  let (
                                                    (
                                                      x (
                                                        _div determinant_x determinant
                                                      )
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      let (
                                                        (
                                                          y (
                                                            _div determinant_y determinant
                                                          )
                                                        )
                                                      )
                                                       (
                                                        begin (
                                                          ret1 (
                                                            _list x y
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
        test_cramers_rule_2x2
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            let (
              (
                r1 (
                  cramers_rule_2x2 (
                    _list 2.0 3.0 0.0
                  )
                   (
                    _list 5.0 1.0 0.0
                  )
                )
              )
            )
             (
              begin (
                if (
                  or (
                    not (
                      equal? (
                        cond (
                          (
                            string? r1
                          )
                           (
                            _substring r1 0 (
                              + 0 1
                            )
                          )
                        )
                         (
                          (
                            hash-table? r1
                          )
                           (
                            hash-table-ref r1 0
                          )
                        )
                         (
                          else (
                            list-ref-safe r1 0
                          )
                        )
                      )
                       0.0
                    )
                  )
                   (
                    not (
                      equal? (
                        cond (
                          (
                            string? r1
                          )
                           (
                            _substring r1 1 (
                              + 1 1
                            )
                          )
                        )
                         (
                          (
                            hash-table? r1
                          )
                           (
                            hash-table-ref r1 1
                          )
                        )
                         (
                          else (
                            list-ref-safe r1 1
                          )
                        )
                      )
                       0.0
                    )
                  )
                )
                 (
                  begin (
                    panic "Test1 failed"
                  )
                )
                 '(
                  
                )
              )
               (
                let (
                  (
                    r2 (
                      cramers_rule_2x2 (
                        _list 0.0 4.0 50.0
                      )
                       (
                        _list 2.0 0.0 26.0
                      )
                    )
                  )
                )
                 (
                  begin (
                    if (
                      or (
                        not (
                          equal? (
                            cond (
                              (
                                string? r2
                              )
                               (
                                _substring r2 0 (
                                  + 0 1
                                )
                              )
                            )
                             (
                              (
                                hash-table? r2
                              )
                               (
                                hash-table-ref r2 0
                              )
                            )
                             (
                              else (
                                list-ref-safe r2 0
                              )
                            )
                          )
                           13.0
                        )
                      )
                       (
                        not (
                          equal? (
                            cond (
                              (
                                string? r2
                              )
                               (
                                _substring r2 1 (
                                  + 1 1
                                )
                              )
                            )
                             (
                              (
                                hash-table? r2
                              )
                               (
                                hash-table-ref r2 1
                              )
                            )
                             (
                              else (
                                list-ref-safe r2 1
                              )
                            )
                          )
                           12.5
                        )
                      )
                    )
                     (
                      begin (
                        panic "Test2 failed"
                      )
                    )
                     '(
                      
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
            ret3
          )
           (
            begin (
              test_cramers_rule_2x2
            )
             (
              _display (
                if (
                  string? (
                    cramers_rule_2x2 (
                      _list 11.0 2.0 30.0
                    )
                     (
                      _list 1.0 0.0 4.0
                    )
                  )
                )
                 (
                  cramers_rule_2x2 (
                    _list 11.0 2.0 30.0
                  )
                   (
                    _list 1.0 0.0 4.0
                  )
                )
                 (
                  to-str (
                    cramers_rule_2x2 (
                      _list 11.0 2.0 30.0
                    )
                     (
                      _list 1.0 0.0 4.0
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
          end5 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur6 (
              quotient (
                * (
                  - end5 start4
                )
                 1000000
              )
               jps7
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur6
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
