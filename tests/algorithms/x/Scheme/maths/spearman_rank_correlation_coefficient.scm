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
        assign_ranks data
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                ranks (
                  _list
                )
              )
            )
             (
              begin (
                let (
                  (
                    n (
                      _len data
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
                                            rank 1
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
                                                                if (
                                                                  or (
                                                                    < (
                                                                      list-ref-safe data j
                                                                    )
                                                                     (
                                                                      list-ref-safe data i
                                                                    )
                                                                  )
                                                                   (
                                                                    and (
                                                                      equal? (
                                                                        list-ref-safe data j
                                                                      )
                                                                       (
                                                                        list-ref-safe data i
                                                                      )
                                                                    )
                                                                     (
                                                                      < j i
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! rank (
                                                                      + rank 1
                                                                    )
                                                                  )
                                                                )
                                                                 '(
                                                                  
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
                                                             '(
                                                              
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
                                                set! ranks (
                                                  append ranks (
                                                    _list rank
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
                        ret1 ranks
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
        calculate_spearman_rank_correlation var1 var2
      )
       (
        call/cc (
          lambda (
            ret6
          )
           (
            begin (
              if (
                not (
                  equal? (
                    _len var1
                  )
                   (
                    _len var2
                  )
                )
              )
               (
                begin (
                  panic "Lists must have equal length"
                )
              )
               '(
                
              )
            )
             (
              let (
                (
                  n (
                    _len var1
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      rank1 (
                        assign_ranks var1
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          rank2 (
                            assign_ranks var2
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
                              let (
                                (
                                  d_sq 0.0
                                )
                              )
                               (
                                begin (
                                  call/cc (
                                    lambda (
                                      break8
                                    )
                                     (
                                      letrec (
                                        (
                                          loop7 (
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
                                                      diff (
                                                        + 0.0 (
                                                          - (
                                                            cond (
                                                              (
                                                                string? rank1
                                                              )
                                                               (
                                                                _substring rank1 i (
                                                                  + i 1
                                                                )
                                                              )
                                                            )
                                                             (
                                                              (
                                                                hash-table? rank1
                                                              )
                                                               (
                                                                hash-table-ref rank1 i
                                                              )
                                                            )
                                                             (
                                                              else (
                                                                list-ref-safe rank1 i
                                                              )
                                                            )
                                                          )
                                                           (
                                                            cond (
                                                              (
                                                                string? rank2
                                                              )
                                                               (
                                                                _substring rank2 i (
                                                                  + i 1
                                                                )
                                                              )
                                                            )
                                                             (
                                                              (
                                                                hash-table? rank2
                                                              )
                                                               (
                                                                hash-table-ref rank2 i
                                                              )
                                                            )
                                                             (
                                                              else (
                                                                list-ref-safe rank2 i
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      set! d_sq (
                                                        _add d_sq (
                                                          * diff diff
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
                                                  loop7
                                                )
                                              )
                                               '(
                                                
                                              )
                                            )
                                          )
                                        )
                                      )
                                       (
                                        loop7
                                      )
                                    )
                                  )
                                )
                                 (
                                  let (
                                    (
                                      n_f (
                                        + 0.0 n
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      ret6 (
                                        - 1.0 (
                                          _div (
                                            * 6.0 d_sq
                                          )
                                           (
                                            * n_f (
                                              - (
                                                * n_f n_f
                                              )
                                               1.0
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
        test_spearman
      )
       (
        call/cc (
          lambda (
            ret9
          )
           (
            let (
              (
                x (
                  _list 1.0 2.0 3.0 4.0 5.0
                )
              )
            )
             (
              begin (
                let (
                  (
                    y_inc (
                      _list 2.0 4.0 6.0 8.0 10.0
                    )
                  )
                )
                 (
                  begin (
                    if (
                      not (
                        equal? (
                          calculate_spearman_rank_correlation x y_inc
                        )
                         1.0
                      )
                    )
                     (
                      begin (
                        panic "case1"
                      )
                    )
                     '(
                      
                    )
                  )
                   (
                    let (
                      (
                        y_dec (
                          _list 5.0 4.0 3.0 2.0 1.0
                        )
                      )
                    )
                     (
                      begin (
                        if (
                          not (
                            equal? (
                              calculate_spearman_rank_correlation x y_dec
                            )
                             (
                              - 1.0
                            )
                          )
                        )
                         (
                          begin (
                            panic "case2"
                          )
                        )
                         '(
                          
                        )
                      )
                       (
                        let (
                          (
                            y_mix (
                              _list 5.0 1.0 2.0 9.0 5.0
                            )
                          )
                        )
                         (
                          begin (
                            if (
                              not (
                                equal? (
                                  calculate_spearman_rank_correlation x y_mix
                                )
                                 0.6
                              )
                            )
                             (
                              begin (
                                panic "case3"
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
            begin (
              test_spearman
            )
             (
              _display (
                if (
                  string? (
                    to-str-space (
                      calculate_spearman_rank_correlation (
                        _list 1.0 2.0 3.0 4.0 5.0
                      )
                       (
                        _list 2.0 4.0 6.0 8.0 10.0
                      )
                    )
                  )
                )
                 (
                  to-str-space (
                    calculate_spearman_rank_correlation (
                      _list 1.0 2.0 3.0 4.0 5.0
                    )
                     (
                      _list 2.0 4.0 6.0 8.0 10.0
                    )
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      calculate_spearman_rank_correlation (
                        _list 1.0 2.0 3.0 4.0 5.0
                      )
                       (
                        _list 2.0 4.0 6.0 8.0 10.0
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
                      calculate_spearman_rank_correlation (
                        _list 1.0 2.0 3.0 4.0 5.0
                      )
                       (
                        _list 5.0 4.0 3.0 2.0 1.0
                      )
                    )
                  )
                )
                 (
                  to-str-space (
                    calculate_spearman_rank_correlation (
                      _list 1.0 2.0 3.0 4.0 5.0
                    )
                     (
                      _list 5.0 4.0 3.0 2.0 1.0
                    )
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      calculate_spearman_rank_correlation (
                        _list 1.0 2.0 3.0 4.0 5.0
                      )
                       (
                        _list 5.0 4.0 3.0 2.0 1.0
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
                      calculate_spearman_rank_correlation (
                        _list 1.0 2.0 3.0 4.0 5.0
                      )
                       (
                        _list 5.0 1.0 2.0 9.0 5.0
                      )
                    )
                  )
                )
                 (
                  to-str-space (
                    calculate_spearman_rank_correlation (
                      _list 1.0 2.0 3.0 4.0 5.0
                    )
                     (
                      _list 5.0 1.0 2.0 9.0 5.0
                    )
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      calculate_spearman_rank_correlation (
                        _list 1.0 2.0 3.0 4.0 5.0
                      )
                       (
                        _list 5.0 1.0 2.0 9.0 5.0
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
