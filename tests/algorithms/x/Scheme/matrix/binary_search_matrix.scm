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
      start6 (
        current-jiffy
      )
    )
     (
      jps9 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        binary_search arr lower_bound upper_bound value
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                r (
                  _div (
                    + lower_bound upper_bound
                  )
                   2
                )
              )
            )
             (
              begin (
                if (
                  equal? (
                    list-ref-safe arr r
                  )
                   value
                )
                 (
                  begin (
                    ret1 r
                  )
                )
                 '(
                  
                )
              )
               (
                if (
                  >= lower_bound upper_bound
                )
                 (
                  begin (
                    ret1 (
                      - 1
                    )
                  )
                )
                 '(
                  
                )
              )
               (
                if (
                  < (
                    list-ref-safe arr r
                  )
                   value
                )
                 (
                  begin (
                    ret1 (
                      binary_search arr (
                        + r 1
                      )
                       upper_bound value
                    )
                  )
                )
                 '(
                  
                )
              )
               (
                ret1 (
                  binary_search arr lower_bound (
                    - r 1
                  )
                   value
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        mat_bin_search value matrix
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            let (
              (
                index 0
              )
            )
             (
              begin (
                if (
                  equal? (
                    cond (
                      (
                        string? (
                          list-ref-safe matrix index
                        )
                      )
                       (
                        _substring (
                          list-ref-safe matrix index
                        )
                         0 (
                          + 0 1
                        )
                      )
                    )
                     (
                      (
                        hash-table? (
                          list-ref-safe matrix index
                        )
                      )
                       (
                        hash-table-ref (
                          list-ref-safe matrix index
                        )
                         0
                      )
                    )
                     (
                      else (
                        list-ref-safe (
                          list-ref-safe matrix index
                        )
                         0
                      )
                    )
                  )
                   value
                )
                 (
                  begin (
                    ret2 (
                      _list index 0
                    )
                  )
                )
                 '(
                  
                )
              )
               (
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
                              and (
                                < index (
                                  _len matrix
                                )
                              )
                               (
                                < (
                                  cond (
                                    (
                                      string? (
                                        list-ref-safe matrix index
                                      )
                                    )
                                     (
                                      _substring (
                                        list-ref-safe matrix index
                                      )
                                       0 (
                                        + 0 1
                                      )
                                    )
                                  )
                                   (
                                    (
                                      hash-table? (
                                        list-ref-safe matrix index
                                      )
                                    )
                                     (
                                      hash-table-ref (
                                        list-ref-safe matrix index
                                      )
                                       0
                                    )
                                  )
                                   (
                                    else (
                                      list-ref-safe (
                                        list-ref-safe matrix index
                                      )
                                       0
                                    )
                                  )
                                )
                                 value
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    r (
                                      binary_search (
                                        list-ref-safe matrix index
                                      )
                                       0 (
                                        - (
                                          _len (
                                            list-ref-safe matrix index
                                          )
                                        )
                                         1
                                      )
                                       value
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      not (
                                        equal? r (
                                          - 1
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        ret2 (
                                          _list index r
                                        )
                                      )
                                    )
                                     '(
                                      
                                    )
                                  )
                                   (
                                    set! index (
                                      + index 1
                                    )
                                  )
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
                ret2 (
                  _list (
                    - 1
                  )
                   (
                    - 1
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
            ret5
          )
           (
            let (
              (
                row (
                  _list 1 4 7 11 15
                )
              )
            )
             (
              begin (
                _display (
                  if (
                    string? (
                      to-str-space (
                        binary_search row 0 (
                          - (
                            _len row
                          )
                           1
                        )
                         1
                      )
                    )
                  )
                   (
                    to-str-space (
                      binary_search row 0 (
                        - (
                          _len row
                        )
                         1
                      )
                       1
                    )
                  )
                   (
                    to-str (
                      to-str-space (
                        binary_search row 0 (
                          - (
                            _len row
                          )
                           1
                        )
                         1
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
                        binary_search row 0 (
                          - (
                            _len row
                          )
                           1
                        )
                         23
                      )
                    )
                  )
                   (
                    to-str-space (
                      binary_search row 0 (
                        - (
                          _len row
                        )
                         1
                      )
                       23
                    )
                  )
                   (
                    to-str (
                      to-str-space (
                        binary_search row 0 (
                          - (
                            _len row
                          )
                           1
                        )
                         23
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
                    matrix (
                      _list (
                        _list 1 4 7 11 15
                      )
                       (
                        _list 2 5 8 12 19
                      )
                       (
                        _list 3 6 9 16 22
                      )
                       (
                        _list 10 13 14 17 24
                      )
                       (
                        _list 18 21 23 26 30
                      )
                    )
                  )
                )
                 (
                  begin (
                    _display (
                      if (
                        string? (
                          to-str-space (
                            mat_bin_search 1 matrix
                          )
                        )
                      )
                       (
                        to-str-space (
                          mat_bin_search 1 matrix
                        )
                      )
                       (
                        to-str (
                          to-str-space (
                            mat_bin_search 1 matrix
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
                            mat_bin_search 34 matrix
                          )
                        )
                      )
                       (
                        to-str-space (
                          mat_bin_search 34 matrix
                        )
                      )
                       (
                        to-str (
                          to-str-space (
                            mat_bin_search 34 matrix
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
     (
      main
    )
     (
      let (
        (
          end7 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur8 (
              quotient (
                * (
                  - end7 start6
                )
                 1000000
              )
               jps9
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur8
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
