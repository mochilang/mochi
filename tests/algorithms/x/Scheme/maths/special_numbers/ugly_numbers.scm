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
        ugly_numbers n
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            begin (
              if (
                <= n 0
              )
               (
                begin (
                  ret1 1
                )
              )
               '(
                
              )
            )
             (
              let (
                (
                  ugly_nums (
                    _list
                  )
                )
              )
               (
                begin (
                  set! ugly_nums (
                    append ugly_nums (
                      _list 1
                    )
                  )
                )
                 (
                  let (
                    (
                      i2 0
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          i3 0
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              i5 0
                            )
                          )
                           (
                            begin (
                              let (
                                (
                                  next_2 2
                                )
                              )
                               (
                                begin (
                                  let (
                                    (
                                      next_3 3
                                    )
                                  )
                                   (
                                    begin (
                                      let (
                                        (
                                          next_5 5
                                        )
                                      )
                                       (
                                        begin (
                                          let (
                                            (
                                              count 1
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
                                                            < count n
                                                          )
                                                           (
                                                            begin (
                                                              let (
                                                                (
                                                                  next_num (
                                                                    if (
                                                                      < next_2 next_3
                                                                    )
                                                                     (
                                                                      if (
                                                                        < next_2 next_5
                                                                      )
                                                                       next_2 next_5
                                                                    )
                                                                     (
                                                                      if (
                                                                        < next_3 next_5
                                                                      )
                                                                       next_3 next_5
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                begin (
                                                                  set! ugly_nums (
                                                                    append ugly_nums (
                                                                      _list next_num
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  if (
                                                                    equal? next_num next_2
                                                                  )
                                                                   (
                                                                    begin (
                                                                      set! i2 (
                                                                        + i2 1
                                                                      )
                                                                    )
                                                                     (
                                                                      set! next_2 (
                                                                        * (
                                                                          list-ref-safe ugly_nums i2
                                                                        )
                                                                         2
                                                                      )
                                                                    )
                                                                  )
                                                                   '(
                                                                    
                                                                  )
                                                                )
                                                                 (
                                                                  if (
                                                                    equal? next_num next_3
                                                                  )
                                                                   (
                                                                    begin (
                                                                      set! i3 (
                                                                        + i3 1
                                                                      )
                                                                    )
                                                                     (
                                                                      set! next_3 (
                                                                        * (
                                                                          list-ref-safe ugly_nums i3
                                                                        )
                                                                         3
                                                                      )
                                                                    )
                                                                  )
                                                                   '(
                                                                    
                                                                  )
                                                                )
                                                                 (
                                                                  if (
                                                                    equal? next_num next_5
                                                                  )
                                                                   (
                                                                    begin (
                                                                      set! i5 (
                                                                        + i5 1
                                                                      )
                                                                    )
                                                                     (
                                                                      set! next_5 (
                                                                        * (
                                                                          list-ref-safe ugly_nums i5
                                                                        )
                                                                         5
                                                                      )
                                                                    )
                                                                  )
                                                                   '(
                                                                    
                                                                  )
                                                                )
                                                                 (
                                                                  set! count (
                                                                    + count 1
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             (
                                                              loop2
                                                            )
                                                          )
                                                           '(
                                                            
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
                                              ret1 (
                                                list-ref-safe ugly_nums (
                                                  - (
                                                    _len ugly_nums
                                                  )
                                                   1
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
      _display (
        if (
          string? (
            to-str-space (
              ugly_numbers 100
            )
          )
        )
         (
          to-str-space (
            ugly_numbers 100
          )
        )
         (
          to-str (
            to-str-space (
              ugly_numbers 100
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
              ugly_numbers 0
            )
          )
        )
         (
          to-str-space (
            ugly_numbers 0
          )
        )
         (
          to-str (
            to-str-space (
              ugly_numbers 0
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
              ugly_numbers 20
            )
          )
        )
         (
          to-str-space (
            ugly_numbers 20
          )
        )
         (
          to-str (
            to-str-space (
              ugly_numbers 20
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
              ugly_numbers (
                - 5
              )
            )
          )
        )
         (
          to-str-space (
            ugly_numbers (
              - 5
            )
          )
        )
         (
          to-str (
            to-str-space (
              ugly_numbers (
                - 5
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
              ugly_numbers 200
            )
          )
        )
         (
          to-str-space (
            ugly_numbers 200
          )
        )
         (
          to-str (
            to-str-space (
              ugly_numbers 200
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
