;; Generated on 2025-08-07 10:06 +0700
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
      start14 (
        current-jiffy
      )
    )
     (
      jps17 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        neville_interpolate x_points y_points x0
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                n (
                  _len x_points
                )
              )
            )
             (
              begin (
                let (
                  (
                    q (
                      _list
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
                                      < i n
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            row (
                                              _list
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                j 0
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
                                                              < j n
                                                            )
                                                             (
                                                              begin (
                                                                set! row (
                                                                  append row (
                                                                    _list 0.0
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                set! j (
                                                                  + j 1
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
                                                set! q (
                                                  append q (
                                                    _list row
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
                        set! i 0
                      )
                       (
                        call/cc (
                          lambda (
                            break7
                          )
                           (
                            letrec (
                              (
                                loop6 (
                                  lambda (
                                    
                                  )
                                   (
                                    if (
                                      < i n
                                    )
                                     (
                                      begin (
                                        list-set! (
                                          list-ref q i
                                        )
                                         1 (
                                          list-ref y_points i
                                        )
                                      )
                                       (
                                        set! i (
                                          + i 1
                                        )
                                      )
                                       (
                                        loop6
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
                              loop6
                            )
                          )
                        )
                      )
                       (
                        let (
                          (
                            col 2
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
                                          < col n
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                row_idx col
                                              )
                                            )
                                             (
                                              begin (
                                                call/cc (
                                                  lambda (
                                                    break11
                                                  )
                                                   (
                                                    letrec (
                                                      (
                                                        loop10 (
                                                          lambda (
                                                            
                                                          )
                                                           (
                                                            if (
                                                              < row_idx n
                                                            )
                                                             (
                                                              begin (
                                                                list-set! (
                                                                  list-ref q row_idx
                                                                )
                                                                 col (
                                                                  _div (
                                                                    - (
                                                                      * (
                                                                        - x0 (
                                                                          list-ref x_points (
                                                                            + (
                                                                              - row_idx col
                                                                            )
                                                                             1
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        cond (
                                                                          (
                                                                            string? (
                                                                              list-ref q row_idx
                                                                            )
                                                                          )
                                                                           (
                                                                            _substring (
                                                                              list-ref q row_idx
                                                                            )
                                                                             (
                                                                              - col 1
                                                                            )
                                                                             (
                                                                              + (
                                                                                - col 1
                                                                              )
                                                                               1
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          (
                                                                            hash-table? (
                                                                              list-ref q row_idx
                                                                            )
                                                                          )
                                                                           (
                                                                            hash-table-ref (
                                                                              list-ref q row_idx
                                                                            )
                                                                             (
                                                                              - col 1
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          else (
                                                                            list-ref (
                                                                              list-ref q row_idx
                                                                            )
                                                                             (
                                                                              - col 1
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      * (
                                                                        - x0 (
                                                                          list-ref x_points row_idx
                                                                        )
                                                                      )
                                                                       (
                                                                        cond (
                                                                          (
                                                                            string? (
                                                                              list-ref q (
                                                                                - row_idx 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            _substring (
                                                                              list-ref q (
                                                                                - row_idx 1
                                                                              )
                                                                            )
                                                                             (
                                                                              - col 1
                                                                            )
                                                                             (
                                                                              + (
                                                                                - col 1
                                                                              )
                                                                               1
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          (
                                                                            hash-table? (
                                                                              list-ref q (
                                                                                - row_idx 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            hash-table-ref (
                                                                              list-ref q (
                                                                                - row_idx 1
                                                                              )
                                                                            )
                                                                             (
                                                                              - col 1
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          else (
                                                                            list-ref (
                                                                              list-ref q (
                                                                                - row_idx 1
                                                                              )
                                                                            )
                                                                             (
                                                                              - col 1
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    - (
                                                                      list-ref x_points row_idx
                                                                    )
                                                                     (
                                                                      list-ref x_points (
                                                                        + (
                                                                          - row_idx col
                                                                        )
                                                                         1
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                set! row_idx (
                                                                  + row_idx 1
                                                                )
                                                              )
                                                               (
                                                                loop10
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
                                                      loop10
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                set! col (
                                                  + col 1
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
                            ret1 (
                              alist->hash-table (
                                _list (
                                  cons "value" (
                                    cond (
                                      (
                                        string? (
                                          list-ref q (
                                            - n 1
                                          )
                                        )
                                      )
                                       (
                                        _substring (
                                          list-ref q (
                                            - n 1
                                          )
                                        )
                                         (
                                          - n 1
                                        )
                                         (
                                          + (
                                            - n 1
                                          )
                                           1
                                        )
                                      )
                                    )
                                     (
                                      (
                                        hash-table? (
                                          list-ref q (
                                            - n 1
                                          )
                                        )
                                      )
                                       (
                                        hash-table-ref (
                                          list-ref q (
                                            - n 1
                                          )
                                        )
                                         (
                                          - n 1
                                        )
                                      )
                                    )
                                     (
                                      else (
                                        list-ref (
                                          list-ref q (
                                            - n 1
                                          )
                                        )
                                         (
                                          - n 1
                                        )
                                      )
                                    )
                                  )
                                )
                                 (
                                  cons "table" q
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
        test_neville
      )
       (
        call/cc (
          lambda (
            ret12
          )
           (
            let (
              (
                xs (
                  _list 1.0 2.0 3.0 4.0 6.0
                )
              )
            )
             (
              begin (
                let (
                  (
                    ys (
                      _list 6.0 7.0 8.0 9.0 11.0
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        r1 (
                          neville_interpolate xs ys 5.0
                        )
                      )
                    )
                     (
                      begin (
                        if (
                          not (
                            equal? (
                              hash-table-ref r1 "value"
                            )
                             10.0
                          )
                        )
                         (
                          begin (
                            panic "neville_interpolate at 5 failed"
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
                            r2 (
                              neville_interpolate xs ys 99.0
                            )
                          )
                        )
                         (
                          begin (
                            if (
                              not (
                                equal? (
                                  hash-table-ref r2 "value"
                                )
                                 104.0
                              )
                            )
                             (
                              begin (
                                panic "neville_interpolate at 99 failed"
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
            ret13
          )
           (
            begin (
              test_neville
            )
             (
              let (
                (
                  xs (
                    _list 1.0 2.0 3.0 4.0 6.0
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      ys (
                        _list 6.0 7.0 8.0 9.0 11.0
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          r (
                            neville_interpolate xs ys 5.0
                          )
                        )
                      )
                       (
                        begin (
                          _display (
                            if (
                              string? (
                                hash-table-ref r "value"
                              )
                            )
                             (
                              hash-table-ref r "value"
                            )
                             (
                              to-str (
                                hash-table-ref r "value"
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
      main
    )
     (
      let (
        (
          end15 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur16 (
              quotient (
                * (
                  - end15 start14
                )
                 1000000
              )
               jps17
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur16
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
