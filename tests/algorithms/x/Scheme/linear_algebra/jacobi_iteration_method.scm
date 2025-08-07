;; Generated on 2025-08-07 14:57 +0700
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
      start16 (
        current-jiffy
      )
    )
     (
      jps19 (
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
            ret1 (
              if (
                < x 0.0
              )
               (
                - x
              )
               x
            )
          )
        )
      )
    )
     (
      define (
        strictly_diagonally_dominant matrix
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            let (
              (
                n (
                  _len matrix
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
                                  < i n
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
                                                          < j n
                                                        )
                                                         (
                                                          begin (
                                                            if (
                                                              not (
                                                                equal? i j
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                set! sum (
                                                                  _add sum (
                                                                    absf (
                                                                      cond (
                                                                        (
                                                                          string? (
                                                                            list-ref-safe matrix i
                                                                          )
                                                                        )
                                                                         (
                                                                          _substring (
                                                                            list-ref-safe matrix i
                                                                          )
                                                                           j (
                                                                            + j 1
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        (
                                                                          hash-table? (
                                                                            list-ref-safe matrix i
                                                                          )
                                                                        )
                                                                         (
                                                                          hash-table-ref (
                                                                            list-ref-safe matrix i
                                                                          )
                                                                           j
                                                                        )
                                                                      )
                                                                       (
                                                                        else (
                                                                          list-ref-safe (
                                                                            list-ref-safe matrix i
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
                                                              quote (
                                                                
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
                                                         (
                                                          quote (
                                                            
                                                          )
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
                                            if (
                                              _le (
                                                absf (
                                                  cond (
                                                    (
                                                      string? (
                                                        list-ref-safe matrix i
                                                      )
                                                    )
                                                     (
                                                      _substring (
                                                        list-ref-safe matrix i
                                                      )
                                                       i (
                                                        + i 1
                                                      )
                                                    )
                                                  )
                                                   (
                                                    (
                                                      hash-table? (
                                                        list-ref-safe matrix i
                                                      )
                                                    )
                                                     (
                                                      hash-table-ref (
                                                        list-ref-safe matrix i
                                                      )
                                                       i
                                                    )
                                                  )
                                                   (
                                                    else (
                                                      list-ref-safe (
                                                        list-ref-safe matrix i
                                                      )
                                                       i
                                                    )
                                                  )
                                                )
                                              )
                                               sum
                                            )
                                             (
                                              begin (
                                                panic "Coefficient matrix is not strictly diagonally dominant"
                                              )
                                            )
                                             (
                                              quote (
                                                
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
                    ret2 #t
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
        jacobi_iteration_method coefficient constant init_val iterations
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            let (
              (
                n (
                  _len coefficient
                )
              )
            )
             (
              begin (
                if (
                  equal? n 0
                )
                 (
                  begin (
                    panic "Coefficient matrix cannot be empty"
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
                    equal? (
                      _len constant
                    )
                     n
                  )
                )
                 (
                  begin (
                    panic "Constant vector length must equal number of rows in coefficient matrix"
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
                    equal? (
                      _len init_val
                    )
                     n
                  )
                )
                 (
                  begin (
                    panic "Initial values count must match matrix size"
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
                    r 0
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
                                  < r n
                                )
                                 (
                                  begin (
                                    if (
                                      not (
                                        equal? (
                                          _len (
                                            list-ref-safe coefficient r
                                          )
                                        )
                                         n
                                      )
                                    )
                                     (
                                      begin (
                                        panic "Coefficient matrix must be square"
                                      )
                                    )
                                     (
                                      quote (
                                        
                                      )
                                    )
                                  )
                                   (
                                    set! r (
                                      + r 1
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
                    if (
                      <= iterations 0
                    )
                     (
                      begin (
                        panic "Iterations must be at least 1"
                      )
                    )
                     (
                      quote (
                        
                      )
                    )
                  )
                   (
                    strictly_diagonally_dominant coefficient
                  )
                   (
                    let (
                      (
                        x init_val
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
                                          < k iterations
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                new_x (
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
                                                        break13
                                                      )
                                                       (
                                                        letrec (
                                                          (
                                                            loop12 (
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
                                                                        sum 0.0
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
                                                                                break15
                                                                              )
                                                                               (
                                                                                letrec (
                                                                                  (
                                                                                    loop14 (
                                                                                      lambda (
                                                                                        
                                                                                      )
                                                                                       (
                                                                                        if (
                                                                                          < j n
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            if (
                                                                                              not (
                                                                                                equal? i j
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
                                                                                                            list-ref-safe coefficient i
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          _substring (
                                                                                                            list-ref-safe coefficient i
                                                                                                          )
                                                                                                           j (
                                                                                                            + j 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? (
                                                                                                            list-ref-safe coefficient i
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref (
                                                                                                            list-ref-safe coefficient i
                                                                                                          )
                                                                                                           j
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref-safe (
                                                                                                            list-ref-safe coefficient i
                                                                                                          )
                                                                                                           j
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      list-ref-safe x j
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              quote (
                                                                                                
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            set! j (
                                                                                              + j 1
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            loop14
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
                                                                                  loop14
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            let (
                                                                              (
                                                                                value (
                                                                                  _div (
                                                                                    - (
                                                                                      list-ref-safe constant i
                                                                                    )
                                                                                     sum
                                                                                  )
                                                                                   (
                                                                                    cond (
                                                                                      (
                                                                                        string? (
                                                                                          list-ref-safe coefficient i
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        _substring (
                                                                                          list-ref-safe coefficient i
                                                                                        )
                                                                                         i (
                                                                                          + i 1
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      (
                                                                                        hash-table? (
                                                                                          list-ref-safe coefficient i
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        hash-table-ref (
                                                                                          list-ref-safe coefficient i
                                                                                        )
                                                                                         i
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      else (
                                                                                        list-ref-safe (
                                                                                          list-ref-safe coefficient i
                                                                                        )
                                                                                         i
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                set! new_x (
                                                                                  append new_x (
                                                                                    _list value
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
                                                                    )
                                                                  )
                                                                   (
                                                                    loop12
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
                                                          loop12
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    set! x new_x
                                                  )
                                                   (
                                                    set! k (
                                                      + k 1
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
                            ret7 x
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
          coefficient (
            _list (
              _list 4.0 1.0 1.0
            )
             (
              _list 1.0 5.0 2.0
            )
             (
              _list 1.0 2.0 4.0
            )
          )
        )
      )
       (
        begin (
          let (
            (
              constant (
                _list 2.0 (
                  - 6.0
                )
                 (
                  - 4.0
                )
              )
            )
          )
           (
            begin (
              let (
                (
                  init_val (
                    _list 0.5 (
                      - 0.5
                    )
                     (
                      - 0.5
                    )
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      iterations 3
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          result (
                            jacobi_iteration_method coefficient constant init_val iterations
                          )
                        )
                      )
                       (
                        begin (
                          _display (
                            if (
                              string? result
                            )
                             result (
                              to-str result
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
          end17 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur18 (
              quotient (
                * (
                  - end17 start16
                )
                 1000000
              )
               jps19
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur18
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
