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
      start11 (
        current-jiffy
      )
    )
     (
      jps14 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        add a b
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            ret1 (
              alist->hash-table (
                _list (
                  cons "re" (
                    + (
                      hash-table-ref a "re"
                    )
                     (
                      hash-table-ref b "re"
                    )
                  )
                )
                 (
                  cons "im" (
                    + (
                      hash-table-ref a "im"
                    )
                     (
                      hash-table-ref b "im"
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
        sub a b
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            ret2 (
              alist->hash-table (
                _list (
                  cons "re" (
                    - (
                      hash-table-ref a "re"
                    )
                     (
                      hash-table-ref b "re"
                    )
                  )
                )
                 (
                  cons "im" (
                    - (
                      hash-table-ref a "im"
                    )
                     (
                      hash-table-ref b "im"
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
        div_real a r
      )
       (
        call/cc (
          lambda (
            ret3
          )
           (
            ret3 (
              alist->hash-table (
                _list (
                  cons "re" (
                    _div (
                      hash-table-ref a "re"
                    )
                     r
                  )
                )
                 (
                  cons "im" (
                    _div (
                      hash-table-ref a "im"
                    )
                     r
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
        sqrt_newton x
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            begin (
              if (
                <= x 0.0
              )
               (
                begin (
                  ret4 0.0
                )
              )
               '(
                
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
                          break6
                        )
                         (
                          letrec (
                            (
                              loop5 (
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
                                      loop5
                                    )
                                  )
                                   '(
                                    
                                  )
                                )
                              )
                            )
                          )
                           (
                            loop5
                          )
                        )
                      )
                    )
                     (
                      ret4 guess
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
        sqrt_to_complex d
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            begin (
              if (
                >= d 0.0
              )
               (
                begin (
                  ret7 (
                    alist->hash-table (
                      _list (
                        cons "re" (
                          sqrt_newton d
                        )
                      )
                       (
                        cons "im" 0.0
                      )
                    )
                  )
                )
              )
               '(
                
              )
            )
             (
              ret7 (
                alist->hash-table (
                  _list (
                    cons "re" 0.0
                  )
                   (
                    cons "im" (
                      sqrt_newton (
                        - d
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
        quadratic_roots a b c
      )
       (
        call/cc (
          lambda (
            ret8
          )
           (
            begin (
              if (
                equal? a 0.0
              )
               (
                begin (
                  _display (
                    if (
                      string? "ValueError: coefficient 'a' must not be zero"
                    )
                     "ValueError: coefficient 'a' must not be zero" (
                      to-str "ValueError: coefficient 'a' must not be zero"
                    )
                  )
                )
                 (
                  newline
                )
                 (
                  ret8 (
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
                  delta (
                    - (
                      * b b
                    )
                     (
                      * (
                        * 4.0 a
                      )
                       c
                    )
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      sqrt_d (
                        sqrt_to_complex delta
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          minus_b (
                            alist->hash-table (
                              _list (
                                cons "re" (
                                  - b
                                )
                              )
                               (
                                cons "im" 0.0
                              )
                            )
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              two_a (
                                * 2.0 a
                              )
                            )
                          )
                           (
                            begin (
                              let (
                                (
                                  root1 (
                                    div_real (
                                      add minus_b sqrt_d
                                    )
                                     two_a
                                  )
                                )
                              )
                               (
                                begin (
                                  let (
                                    (
                                      root2 (
                                        div_real (
                                          sub minus_b sqrt_d
                                        )
                                         two_a
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      ret8 (
                                        _list root1 root2
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
        root_str r
      )
       (
        call/cc (
          lambda (
            ret9
          )
           (
            begin (
              if (
                equal? (
                  hash-table-ref r "im"
                )
                 0.0
              )
               (
                begin (
                  ret9 (
                    to-str-space (
                      hash-table-ref r "re"
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
                  s (
                    to-str-space (
                      hash-table-ref r "re"
                    )
                  )
                )
              )
               (
                begin (
                  if (
                    >= (
                      hash-table-ref r "im"
                    )
                     0.0
                  )
                   (
                    begin (
                      set! s (
                        string-append (
                          string-append (
                            string-append s "+"
                          )
                           (
                            to-str-space (
                              hash-table-ref r "im"
                            )
                          )
                        )
                         "i"
                      )
                    )
                  )
                   (
                    begin (
                      set! s (
                        string-append (
                          string-append s (
                            to-str-space (
                              hash-table-ref r "im"
                            )
                          )
                        )
                         "i"
                      )
                    )
                  )
                )
                 (
                  ret9 s
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
            ret10
          )
           (
            let (
              (
                roots (
                  quadratic_roots 5.0 6.0 1.0
                )
              )
            )
             (
              begin (
                if (
                  equal? (
                    _len roots
                  )
                   2
                )
                 (
                  begin (
                    _display (
                      if (
                        string? (
                          string-append (
                            string-append (
                              string-append "The solutions are: " (
                                root_str (
                                  cond (
                                    (
                                      string? roots
                                    )
                                     (
                                      _substring roots 0 (
                                        + 0 1
                                      )
                                    )
                                  )
                                   (
                                    (
                                      hash-table? roots
                                    )
                                     (
                                      hash-table-ref roots 0
                                    )
                                  )
                                   (
                                    else (
                                      list-ref-safe roots 0
                                    )
                                  )
                                )
                              )
                            )
                             " and "
                          )
                           (
                            root_str (
                              cond (
                                (
                                  string? roots
                                )
                                 (
                                  _substring roots 1 (
                                    + 1 1
                                  )
                                )
                              )
                               (
                                (
                                  hash-table? roots
                                )
                                 (
                                  hash-table-ref roots 1
                                )
                              )
                               (
                                else (
                                  list-ref-safe roots 1
                                )
                              )
                            )
                          )
                        )
                      )
                       (
                        string-append (
                          string-append (
                            string-append "The solutions are: " (
                              root_str (
                                cond (
                                  (
                                    string? roots
                                  )
                                   (
                                    _substring roots 0 (
                                      + 0 1
                                    )
                                  )
                                )
                                 (
                                  (
                                    hash-table? roots
                                  )
                                   (
                                    hash-table-ref roots 0
                                  )
                                )
                                 (
                                  else (
                                    list-ref-safe roots 0
                                  )
                                )
                              )
                            )
                          )
                           " and "
                        )
                         (
                          root_str (
                            cond (
                              (
                                string? roots
                              )
                               (
                                _substring roots 1 (
                                  + 1 1
                                )
                              )
                            )
                             (
                              (
                                hash-table? roots
                              )
                               (
                                hash-table-ref roots 1
                              )
                            )
                             (
                              else (
                                list-ref-safe roots 1
                              )
                            )
                          )
                        )
                      )
                       (
                        to-str (
                          string-append (
                            string-append (
                              string-append "The solutions are: " (
                                root_str (
                                  cond (
                                    (
                                      string? roots
                                    )
                                     (
                                      _substring roots 0 (
                                        + 0 1
                                      )
                                    )
                                  )
                                   (
                                    (
                                      hash-table? roots
                                    )
                                     (
                                      hash-table-ref roots 0
                                    )
                                  )
                                   (
                                    else (
                                      list-ref-safe roots 0
                                    )
                                  )
                                )
                              )
                            )
                             " and "
                          )
                           (
                            root_str (
                              cond (
                                (
                                  string? roots
                                )
                                 (
                                  _substring roots 1 (
                                    + 1 1
                                  )
                                )
                              )
                               (
                                (
                                  hash-table? roots
                                )
                                 (
                                  hash-table-ref roots 1
                                )
                              )
                               (
                                else (
                                  list-ref-safe roots 1
                                )
                              )
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
                 '(
                  
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
          end12 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur13 (
              quotient (
                * (
                  - end12 start11
                )
                 1000000
              )
               jps14
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur13
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
