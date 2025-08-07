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
      start35 (
        current-jiffy
      )
    )
     (
      jps38 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        check_matrix mat
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            if (
              or (
                < (
                  _len mat
                )
                 2
              )
               (
                < (
                  _len (
                    list-ref-safe mat 0
                  )
                )
                 2
              )
            )
             (
              begin (
                panic "Expected a matrix with at least 2x2 dimensions"
              )
            )
             '(
              
            )
          )
        )
      )
    )
     (
      define (
        add a b
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            begin (
              check_matrix a
            )
             (
              check_matrix b
            )
             (
              if (
                or (
                  not (
                    equal? (
                      _len a
                    )
                     (
                      _len b
                    )
                  )
                )
                 (
                  not (
                    equal? (
                      _len (
                        list-ref-safe a 0
                      )
                    )
                     (
                      _len (
                        list-ref-safe b 0
                      )
                    )
                  )
                )
              )
               (
                begin (
                  panic "Matrices must have the same dimensions"
                )
              )
               '(
                
              )
            )
             (
              let (
                (
                  rows (
                    _len a
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      cols (
                        _len (
                          list-ref-safe a 0
                        )
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          result (
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
                                            < i rows
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
                                                                    < j cols
                                                                  )
                                                                   (
                                                                    begin (
                                                                      set! row (
                                                                        append row (
                                                                          _list (
                                                                            + (
                                                                              cond (
                                                                                (
                                                                                  string? (
                                                                                    list-ref-safe a i
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  _substring (
                                                                                    list-ref-safe a i
                                                                                  )
                                                                                   j (
                                                                                    + j 1
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                (
                                                                                  hash-table? (
                                                                                    list-ref-safe a i
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  hash-table-ref (
                                                                                    list-ref-safe a i
                                                                                  )
                                                                                   j
                                                                                )
                                                                              )
                                                                               (
                                                                                else (
                                                                                  list-ref-safe (
                                                                                    list-ref-safe a i
                                                                                  )
                                                                                   j
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              cond (
                                                                                (
                                                                                  string? (
                                                                                    list-ref-safe b i
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  _substring (
                                                                                    list-ref-safe b i
                                                                                  )
                                                                                   j (
                                                                                    + j 1
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                (
                                                                                  hash-table? (
                                                                                    list-ref-safe b i
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  hash-table-ref (
                                                                                    list-ref-safe b i
                                                                                  )
                                                                                   j
                                                                                )
                                                                              )
                                                                               (
                                                                                else (
                                                                                  list-ref-safe (
                                                                                    list-ref-safe b i
                                                                                  )
                                                                                   j
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      set! j (
                                                                        + j 1
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
                                                      set! result (
                                                        append result (
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
                              ret2 result
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
        subtract a b
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            begin (
              check_matrix a
            )
             (
              check_matrix b
            )
             (
              if (
                or (
                  not (
                    equal? (
                      _len a
                    )
                     (
                      _len b
                    )
                  )
                )
                 (
                  not (
                    equal? (
                      _len (
                        list-ref-safe a 0
                      )
                    )
                     (
                      _len (
                        list-ref-safe b 0
                      )
                    )
                  )
                )
              )
               (
                begin (
                  panic "Matrices must have the same dimensions"
                )
              )
               '(
                
              )
            )
             (
              let (
                (
                  rows (
                    _len a
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      cols (
                        _len (
                          list-ref-safe a 0
                        )
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          result (
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
                                            < i rows
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
                                                                    < j cols
                                                                  )
                                                                   (
                                                                    begin (
                                                                      set! row (
                                                                        append row (
                                                                          _list (
                                                                            - (
                                                                              cond (
                                                                                (
                                                                                  string? (
                                                                                    list-ref-safe a i
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  _substring (
                                                                                    list-ref-safe a i
                                                                                  )
                                                                                   j (
                                                                                    + j 1
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                (
                                                                                  hash-table? (
                                                                                    list-ref-safe a i
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  hash-table-ref (
                                                                                    list-ref-safe a i
                                                                                  )
                                                                                   j
                                                                                )
                                                                              )
                                                                               (
                                                                                else (
                                                                                  list-ref-safe (
                                                                                    list-ref-safe a i
                                                                                  )
                                                                                   j
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              cond (
                                                                                (
                                                                                  string? (
                                                                                    list-ref-safe b i
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  _substring (
                                                                                    list-ref-safe b i
                                                                                  )
                                                                                   j (
                                                                                    + j 1
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                (
                                                                                  hash-table? (
                                                                                    list-ref-safe b i
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  hash-table-ref (
                                                                                    list-ref-safe b i
                                                                                  )
                                                                                   j
                                                                                )
                                                                              )
                                                                               (
                                                                                else (
                                                                                  list-ref-safe (
                                                                                    list-ref-safe b i
                                                                                  )
                                                                                   j
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      set! j (
                                                                        + j 1
                                                                      )
                                                                    )
                                                                     (
                                                                      loop10
                                                                    )
                                                                  )
                                                                   '(
                                                                    
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
                                                      set! result (
                                                        append result (
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
                                              loop8
                                            )
                                          )
                                           '(
                                            
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
                              ret7 result
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
        scalar_multiply a s
      )
       (
        call/cc (
          lambda (
            ret12
          )
           (
            begin (
              check_matrix a
            )
             (
              let (
                (
                  rows (
                    _len a
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      cols (
                        _len (
                          list-ref-safe a 0
                        )
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          result (
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
                                  break14
                                )
                                 (
                                  letrec (
                                    (
                                      loop13 (
                                        lambda (
                                          
                                        )
                                         (
                                          if (
                                            < i rows
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
                                                          break16
                                                        )
                                                         (
                                                          letrec (
                                                            (
                                                              loop15 (
                                                                lambda (
                                                                  
                                                                )
                                                                 (
                                                                  if (
                                                                    < j cols
                                                                  )
                                                                   (
                                                                    begin (
                                                                      set! row (
                                                                        append row (
                                                                          _list (
                                                                            * (
                                                                              cond (
                                                                                (
                                                                                  string? (
                                                                                    list-ref-safe a i
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  _substring (
                                                                                    list-ref-safe a i
                                                                                  )
                                                                                   j (
                                                                                    + j 1
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                (
                                                                                  hash-table? (
                                                                                    list-ref-safe a i
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  hash-table-ref (
                                                                                    list-ref-safe a i
                                                                                  )
                                                                                   j
                                                                                )
                                                                              )
                                                                               (
                                                                                else (
                                                                                  list-ref-safe (
                                                                                    list-ref-safe a i
                                                                                  )
                                                                                   j
                                                                                )
                                                                              )
                                                                            )
                                                                             s
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      set! j (
                                                                        + j 1
                                                                      )
                                                                    )
                                                                     (
                                                                      loop15
                                                                    )
                                                                  )
                                                                   '(
                                                                    
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                           (
                                                            loop15
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      set! result (
                                                        append result (
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
                                              loop13
                                            )
                                          )
                                           '(
                                            
                                          )
                                        )
                                      )
                                    )
                                  )
                                   (
                                    loop13
                                  )
                                )
                              )
                            )
                             (
                              ret12 result
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
        multiply a b
      )
       (
        call/cc (
          lambda (
            ret17
          )
           (
            begin (
              check_matrix a
            )
             (
              check_matrix b
            )
             (
              if (
                not (
                  equal? (
                    _len (
                      list-ref-safe a 0
                    )
                  )
                   (
                    _len b
                  )
                )
              )
               (
                begin (
                  panic "Invalid dimensions for matrix multiplication"
                )
              )
               '(
                
              )
            )
             (
              let (
                (
                  rows (
                    _len a
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      cols (
                        _len (
                          list-ref-safe b 0
                        )
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          result (
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
                                  break19
                                )
                                 (
                                  letrec (
                                    (
                                      loop18 (
                                        lambda (
                                          
                                        )
                                         (
                                          if (
                                            < i rows
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
                                                          break21
                                                        )
                                                         (
                                                          letrec (
                                                            (
                                                              loop20 (
                                                                lambda (
                                                                  
                                                                )
                                                                 (
                                                                  if (
                                                                    < j cols
                                                                  )
                                                                   (
                                                                    begin (
                                                                      let (
                                                                        (
                                                                          sum 0.0
                                                                        )
                                                                      )
                                                                       (
                                                                        begin (
                                                                          let (
                                                                            (
                                                                              k 0
                                                                            )
                                                                          )
                                                                           (
                                                                            begin (
                                                                              call/cc (
                                                                                lambda (
                                                                                  break23
                                                                                )
                                                                                 (
                                                                                  letrec (
                                                                                    (
                                                                                      loop22 (
                                                                                        lambda (
                                                                                          
                                                                                        )
                                                                                         (
                                                                                          if (
                                                                                            < k (
                                                                                              _len b
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            begin (
                                                                                              set! sum (
                                                                                                _add sum (
                                                                                                  * (
                                                                                                    cond (
                                                                                                      (
                                                                                                        string? (
                                                                                                          list-ref-safe a i
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        _substring (
                                                                                                          list-ref-safe a i
                                                                                                        )
                                                                                                         k (
                                                                                                          + k 1
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      (
                                                                                                        hash-table? (
                                                                                                          list-ref-safe a i
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        hash-table-ref (
                                                                                                          list-ref-safe a i
                                                                                                        )
                                                                                                         k
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      else (
                                                                                                        list-ref-safe (
                                                                                                          list-ref-safe a i
                                                                                                        )
                                                                                                         k
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    cond (
                                                                                                      (
                                                                                                        string? (
                                                                                                          list-ref-safe b k
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        _substring (
                                                                                                          list-ref-safe b k
                                                                                                        )
                                                                                                         j (
                                                                                                          + j 1
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      (
                                                                                                        hash-table? (
                                                                                                          list-ref-safe b k
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        hash-table-ref (
                                                                                                          list-ref-safe b k
                                                                                                        )
                                                                                                         j
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      else (
                                                                                                        list-ref-safe (
                                                                                                          list-ref-safe b k
                                                                                                        )
                                                                                                         j
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              set! k (
                                                                                                + k 1
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              loop22
                                                                                            )
                                                                                          )
                                                                                           '(
                                                                                            
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    loop22
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              set! row (
                                                                                append row (
                                                                                  _list sum
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              set! j (
                                                                                + j 1
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      loop20
                                                                    )
                                                                  )
                                                                   '(
                                                                    
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                           (
                                                            loop20
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      set! result (
                                                        append result (
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
                                              loop18
                                            )
                                          )
                                           '(
                                            
                                          )
                                        )
                                      )
                                    )
                                  )
                                   (
                                    loop18
                                  )
                                )
                              )
                            )
                             (
                              ret17 result
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
        identity n
      )
       (
        call/cc (
          lambda (
            ret24
          )
           (
            let (
              (
                result (
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
                        break26
                      )
                       (
                        letrec (
                          (
                            loop25 (
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
                                                break28
                                              )
                                               (
                                                letrec (
                                                  (
                                                    loop27 (
                                                      lambda (
                                                        
                                                      )
                                                       (
                                                        if (
                                                          < j n
                                                        )
                                                         (
                                                          begin (
                                                            if (
                                                              equal? i j
                                                            )
                                                             (
                                                              begin (
                                                                set! row (
                                                                  append row (
                                                                    _list 1.0
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                set! row (
                                                                  append row (
                                                                    _list 0.0
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                           (
                                                            set! j (
                                                              + j 1
                                                            )
                                                          )
                                                           (
                                                            loop27
                                                          )
                                                        )
                                                         '(
                                                          
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  loop27
                                                )
                                              )
                                            )
                                          )
                                           (
                                            set! result (
                                              append result (
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
                                    loop25
                                  )
                                )
                                 '(
                                  
                                )
                              )
                            )
                          )
                        )
                         (
                          loop25
                        )
                      )
                    )
                  )
                   (
                    ret24 result
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
        transpose a
      )
       (
        call/cc (
          lambda (
            ret29
          )
           (
            begin (
              check_matrix a
            )
             (
              let (
                (
                  rows (
                    _len a
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      cols (
                        _len (
                          list-ref-safe a 0
                        )
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          result (
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
                                  break31
                                )
                                 (
                                  letrec (
                                    (
                                      loop30 (
                                        lambda (
                                          
                                        )
                                         (
                                          if (
                                            < j cols
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
                                                      i 0
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      call/cc (
                                                        lambda (
                                                          break33
                                                        )
                                                         (
                                                          letrec (
                                                            (
                                                              loop32 (
                                                                lambda (
                                                                  
                                                                )
                                                                 (
                                                                  if (
                                                                    < i rows
                                                                  )
                                                                   (
                                                                    begin (
                                                                      set! row (
                                                                        append row (
                                                                          _list (
                                                                            cond (
                                                                              (
                                                                                string? (
                                                                                  list-ref-safe a i
                                                                                )
                                                                              )
                                                                               (
                                                                                _substring (
                                                                                  list-ref-safe a i
                                                                                )
                                                                                 j (
                                                                                  + j 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? (
                                                                                  list-ref-safe a i
                                                                                )
                                                                              )
                                                                               (
                                                                                hash-table-ref (
                                                                                  list-ref-safe a i
                                                                                )
                                                                                 j
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref-safe (
                                                                                  list-ref-safe a i
                                                                                )
                                                                                 j
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      set! i (
                                                                        + i 1
                                                                      )
                                                                    )
                                                                     (
                                                                      loop32
                                                                    )
                                                                  )
                                                                   '(
                                                                    
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                           (
                                                            loop32
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      set! result (
                                                        append result (
                                                          _list row
                                                        )
                                                      )
                                                    )
                                                     (
                                                      set! j (
                                                        + j 1
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              loop30
                                            )
                                          )
                                           '(
                                            
                                          )
                                        )
                                      )
                                    )
                                  )
                                   (
                                    loop30
                                  )
                                )
                              )
                            )
                             (
                              ret29 result
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
            ret34
          )
           (
            let (
              (
                mat_a (
                  _list (
                    _list 12.0 10.0
                  )
                   (
                    _list 3.0 9.0
                  )
                )
              )
            )
             (
              begin (
                let (
                  (
                    mat_b (
                      _list (
                        _list 3.0 4.0
                      )
                       (
                        _list 7.0 4.0
                      )
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        mat_c (
                          _list (
                            _list 3.0 0.0 2.0
                          )
                           (
                            _list 2.0 0.0 (
                              - 2.0
                            )
                          )
                           (
                            _list 0.0 1.0 1.0
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
                                add mat_a mat_b
                              )
                            )
                          )
                           (
                            to-str-space (
                              add mat_a mat_b
                            )
                          )
                           (
                            to-str (
                              to-str-space (
                                add mat_a mat_b
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
                                subtract mat_a mat_b
                              )
                            )
                          )
                           (
                            to-str-space (
                              subtract mat_a mat_b
                            )
                          )
                           (
                            to-str (
                              to-str-space (
                                subtract mat_a mat_b
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
                                multiply mat_a mat_b
                              )
                            )
                          )
                           (
                            to-str-space (
                              multiply mat_a mat_b
                            )
                          )
                           (
                            to-str (
                              to-str-space (
                                multiply mat_a mat_b
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
                                scalar_multiply mat_a 3.5
                              )
                            )
                          )
                           (
                            to-str-space (
                              scalar_multiply mat_a 3.5
                            )
                          )
                           (
                            to-str (
                              to-str-space (
                                scalar_multiply mat_a 3.5
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
                                identity 5
                              )
                            )
                          )
                           (
                            to-str-space (
                              identity 5
                            )
                          )
                           (
                            to-str (
                              to-str-space (
                                identity 5
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
                                transpose mat_c
                              )
                            )
                          )
                           (
                            to-str-space (
                              transpose mat_c
                            )
                          )
                           (
                            to-str (
                              to-str-space (
                                transpose mat_c
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
     (
      main
    )
     (
      let (
        (
          end36 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur37 (
              quotient (
                * (
                  - end36 start35
                )
                 1000000
              )
               jps38
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur37
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
