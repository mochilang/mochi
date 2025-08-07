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
      start23 (
        current-jiffy
      )
    )
     (
      jps26 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        pivot t row col
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                pivotRow (
                  _list
                )
              )
            )
             (
              begin (
                let (
                  (
                    pivotVal (
                      cond (
                        (
                          string? (
                            list-ref-safe t row
                          )
                        )
                         (
                          _substring (
                            list-ref-safe t row
                          )
                           col (
                            + col 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? (
                            list-ref-safe t row
                          )
                        )
                         (
                          hash-table-ref (
                            list-ref-safe t row
                          )
                           col
                        )
                      )
                       (
                        else (
                          list-ref-safe (
                            list-ref-safe t row
                          )
                           col
                        )
                      )
                    )
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
                                j
                              )
                               (
                                if (
                                  < j (
                                    _len (
                                      list-ref-safe t row
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    begin (
                                      set! pivotRow (
                                        append pivotRow (
                                          _list (
                                            _div (
                                              cond (
                                                (
                                                  string? (
                                                    list-ref-safe t row
                                                  )
                                                )
                                                 (
                                                  _substring (
                                                    list-ref-safe t row
                                                  )
                                                   j (
                                                    + j 1
                                                  )
                                                )
                                              )
                                               (
                                                (
                                                  hash-table? (
                                                    list-ref-safe t row
                                                  )
                                                )
                                                 (
                                                  hash-table-ref (
                                                    list-ref-safe t row
                                                  )
                                                   j
                                                )
                                              )
                                               (
                                                else (
                                                  list-ref-safe (
                                                    list-ref-safe t row
                                                  )
                                                   j
                                                )
                                              )
                                            )
                                             pivotVal
                                          )
                                        )
                                      )
                                    )
                                  )
                                   (
                                    loop2 (
                                      + j 1
                                    )
                                  )
                                )
                                 '(
                                  
                                )
                              )
                            )
                          )
                        )
                         (
                          loop2 0
                        )
                      )
                    )
                  )
                   (
                    list-set! t row pivotRow
                  )
                   (
                    call/cc (
                      lambda (
                        break5
                      )
                       (
                        letrec (
                          (
                            loop4 (
                              lambda (
                                i
                              )
                               (
                                if (
                                  < i (
                                    _len t
                                  )
                                )
                                 (
                                  begin (
                                    begin (
                                      if (
                                        not (
                                          equal? i row
                                        )
                                      )
                                       (
                                        begin (
                                          let (
                                            (
                                              factor (
                                                cond (
                                                  (
                                                    string? (
                                                      list-ref-safe t i
                                                    )
                                                  )
                                                   (
                                                    _substring (
                                                      list-ref-safe t i
                                                    )
                                                     col (
                                                      + col 1
                                                    )
                                                  )
                                                )
                                                 (
                                                  (
                                                    hash-table? (
                                                      list-ref-safe t i
                                                    )
                                                  )
                                                   (
                                                    hash-table-ref (
                                                      list-ref-safe t i
                                                    )
                                                     col
                                                  )
                                                )
                                                 (
                                                  else (
                                                    list-ref-safe (
                                                      list-ref-safe t i
                                                    )
                                                     col
                                                  )
                                                )
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              let (
                                                (
                                                  newRow (
                                                    _list
                                                  )
                                                )
                                              )
                                               (
                                                begin (
                                                  call/cc (
                                                    lambda (
                                                      break7
                                                    )
                                                     (
                                                      letrec (
                                                        (
                                                          loop6 (
                                                            lambda (
                                                              j
                                                            )
                                                             (
                                                              if (
                                                                < j (
                                                                  _len (
                                                                    list-ref-safe t i
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                begin (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        value (
                                                                          - (
                                                                            cond (
                                                                              (
                                                                                string? (
                                                                                  list-ref-safe t i
                                                                                )
                                                                              )
                                                                               (
                                                                                _substring (
                                                                                  list-ref-safe t i
                                                                                )
                                                                                 j (
                                                                                  + j 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? (
                                                                                  list-ref-safe t i
                                                                                )
                                                                              )
                                                                               (
                                                                                hash-table-ref (
                                                                                  list-ref-safe t i
                                                                                )
                                                                                 j
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref-safe (
                                                                                  list-ref-safe t i
                                                                                )
                                                                                 j
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            * factor (
                                                                              list-ref-safe pivotRow j
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        set! newRow (
                                                                          append newRow (
                                                                            _list value
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  loop6 (
                                                                    + j 1
                                                                  )
                                                                )
                                                              )
                                                               '(
                                                                
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        loop6 0
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  list-set! t i newRow
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                       '(
                                        
                                      )
                                    )
                                  )
                                   (
                                    loop4 (
                                      + i 1
                                    )
                                  )
                                )
                                 '(
                                  
                                )
                              )
                            )
                          )
                        )
                         (
                          loop4 0
                        )
                      )
                    )
                  )
                   (
                    ret1 t
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
        findPivot t
      )
       (
        call/cc (
          lambda (
            ret8
          )
           (
            let (
              (
                col 0
              )
            )
             (
              begin (
                let (
                  (
                    minVal 0.0
                  )
                )
                 (
                  begin (
                    call/cc (
                      lambda (
                        break10
                      )
                       (
                        letrec (
                          (
                            loop9 (
                              lambda (
                                j
                              )
                               (
                                if (
                                  < j (
                                    - (
                                      _len (
                                        list-ref-safe t 0
                                      )
                                    )
                                     1
                                  )
                                )
                                 (
                                  begin (
                                    begin (
                                      let (
                                        (
                                          v (
                                            cond (
                                              (
                                                string? (
                                                  list-ref-safe t 0
                                                )
                                              )
                                               (
                                                _substring (
                                                  list-ref-safe t 0
                                                )
                                                 j (
                                                  + j 1
                                                )
                                              )
                                            )
                                             (
                                              (
                                                hash-table? (
                                                  list-ref-safe t 0
                                                )
                                              )
                                               (
                                                hash-table-ref (
                                                  list-ref-safe t 0
                                                )
                                                 j
                                              )
                                            )
                                             (
                                              else (
                                                list-ref-safe (
                                                  list-ref-safe t 0
                                                )
                                                 j
                                              )
                                            )
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          if (
                                            < v minVal
                                          )
                                           (
                                            begin (
                                              set! minVal v
                                            )
                                             (
                                              set! col j
                                            )
                                          )
                                           '(
                                            
                                          )
                                        )
                                      )
                                    )
                                  )
                                   (
                                    loop9 (
                                      + j 1
                                    )
                                  )
                                )
                                 '(
                                  
                                )
                              )
                            )
                          )
                        )
                         (
                          loop9 0
                        )
                      )
                    )
                  )
                   (
                    if (
                      >= minVal 0.0
                    )
                     (
                      begin (
                        ret8 (
                          _list (
                            - 1
                          )
                           (
                            - 1
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
                        row (
                          - 1
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            minRatio 0.0
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                first #t
                              )
                            )
                             (
                              begin (
                                call/cc (
                                  lambda (
                                    break12
                                  )
                                   (
                                    letrec (
                                      (
                                        loop11 (
                                          lambda (
                                            i
                                          )
                                           (
                                            if (
                                              < i (
                                                _len t
                                              )
                                            )
                                             (
                                              begin (
                                                begin (
                                                  let (
                                                    (
                                                      coeff (
                                                        cond (
                                                          (
                                                            string? (
                                                              list-ref-safe t i
                                                            )
                                                          )
                                                           (
                                                            _substring (
                                                              list-ref-safe t i
                                                            )
                                                             col (
                                                              + col 1
                                                            )
                                                          )
                                                        )
                                                         (
                                                          (
                                                            hash-table? (
                                                              list-ref-safe t i
                                                            )
                                                          )
                                                           (
                                                            hash-table-ref (
                                                              list-ref-safe t i
                                                            )
                                                             col
                                                          )
                                                        )
                                                         (
                                                          else (
                                                            list-ref-safe (
                                                              list-ref-safe t i
                                                            )
                                                             col
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      if (
                                                        > coeff 0.0
                                                      )
                                                       (
                                                        begin (
                                                          let (
                                                            (
                                                              rhs (
                                                                cond (
                                                                  (
                                                                    string? (
                                                                      list-ref-safe t i
                                                                    )
                                                                  )
                                                                   (
                                                                    _substring (
                                                                      list-ref-safe t i
                                                                    )
                                                                     (
                                                                      - (
                                                                        _len (
                                                                          list-ref-safe t i
                                                                        )
                                                                      )
                                                                       1
                                                                    )
                                                                     (
                                                                      + (
                                                                        - (
                                                                          _len (
                                                                            list-ref-safe t i
                                                                          )
                                                                        )
                                                                         1
                                                                      )
                                                                       1
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  (
                                                                    hash-table? (
                                                                      list-ref-safe t i
                                                                    )
                                                                  )
                                                                   (
                                                                    hash-table-ref (
                                                                      list-ref-safe t i
                                                                    )
                                                                     (
                                                                      - (
                                                                        _len (
                                                                          list-ref-safe t i
                                                                        )
                                                                      )
                                                                       1
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  else (
                                                                    list-ref-safe (
                                                                      list-ref-safe t i
                                                                    )
                                                                     (
                                                                      - (
                                                                        _len (
                                                                          list-ref-safe t i
                                                                        )
                                                                      )
                                                                       1
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                           (
                                                            begin (
                                                              let (
                                                                (
                                                                  ratio (
                                                                    _div rhs coeff
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                begin (
                                                                  if (
                                                                    or first (
                                                                      < ratio minRatio
                                                                    )
                                                                  )
                                                                   (
                                                                    begin (
                                                                      set! minRatio ratio
                                                                    )
                                                                     (
                                                                      set! row i
                                                                    )
                                                                     (
                                                                      set! first #f
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
                                                       '(
                                                        
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                loop11 (
                                                  + i 1
                                                )
                                              )
                                            )
                                             '(
                                              
                                            )
                                          )
                                        )
                                      )
                                    )
                                     (
                                      loop11 1
                                    )
                                  )
                                )
                              )
                               (
                                ret8 (
                                  _list row col
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
        interpret t nVars
      )
       (
        call/cc (
          lambda (
            ret13
          )
           (
            let (
              (
                lastCol (
                  - (
                    _len (
                      list-ref-safe t 0
                    )
                  )
                   1
                )
              )
            )
             (
              begin (
                let (
                  (
                    p (
                      cond (
                        (
                          string? (
                            list-ref-safe t 0
                          )
                        )
                         (
                          _substring (
                            list-ref-safe t 0
                          )
                           lastCol (
                            + lastCol 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? (
                            list-ref-safe t 0
                          )
                        )
                         (
                          hash-table-ref (
                            list-ref-safe t 0
                          )
                           lastCol
                        )
                      )
                       (
                        else (
                          list-ref-safe (
                            list-ref-safe t 0
                          )
                           lastCol
                        )
                      )
                    )
                  )
                )
                 (
                  begin (
                    if (
                      < p 0.0
                    )
                     (
                      begin (
                        set! p (
                          - p
                        )
                      )
                    )
                     '(
                      
                    )
                  )
                   (
                    let (
                      (
                        result (
                          alist->hash-table (
                            _list
                          )
                        )
                      )
                    )
                     (
                      begin (
                        hash-table-set! result "P" p
                      )
                       (
                        call/cc (
                          lambda (
                            break15
                          )
                           (
                            letrec (
                              (
                                loop14 (
                                  lambda (
                                    i
                                  )
                                   (
                                    if (
                                      < i nVars
                                    )
                                     (
                                      begin (
                                        begin (
                                          let (
                                            (
                                              nzRow (
                                                - 1
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              let (
                                                (
                                                  nzCount 0
                                                )
                                              )
                                               (
                                                begin (
                                                  call/cc (
                                                    lambda (
                                                      break17
                                                    )
                                                     (
                                                      letrec (
                                                        (
                                                          loop16 (
                                                            lambda (
                                                              r
                                                            )
                                                             (
                                                              if (
                                                                < r (
                                                                  _len t
                                                                )
                                                              )
                                                               (
                                                                begin (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        val (
                                                                          cond (
                                                                            (
                                                                              string? (
                                                                                list-ref-safe t r
                                                                              )
                                                                            )
                                                                             (
                                                                              _substring (
                                                                                list-ref-safe t r
                                                                              )
                                                                               i (
                                                                                + i 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            (
                                                                              hash-table? (
                                                                                list-ref-safe t r
                                                                              )
                                                                            )
                                                                             (
                                                                              hash-table-ref (
                                                                                list-ref-safe t r
                                                                              )
                                                                               i
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref-safe (
                                                                                list-ref-safe t r
                                                                              )
                                                                               i
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        if (
                                                                          not (
                                                                            equal? val 0.0
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            set! nzCount (
                                                                              + nzCount 1
                                                                            )
                                                                          )
                                                                           (
                                                                            set! nzRow r
                                                                          )
                                                                        )
                                                                         '(
                                                                          
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  loop16 (
                                                                    + r 1
                                                                  )
                                                                )
                                                              )
                                                               '(
                                                                
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        loop16 0
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  if (
                                                    and (
                                                      equal? nzCount 1
                                                    )
                                                     (
                                                      equal? (
                                                        cond (
                                                          (
                                                            string? (
                                                              list-ref-safe t nzRow
                                                            )
                                                          )
                                                           (
                                                            _substring (
                                                              list-ref-safe t nzRow
                                                            )
                                                             i (
                                                              + i 1
                                                            )
                                                          )
                                                        )
                                                         (
                                                          (
                                                            hash-table? (
                                                              list-ref-safe t nzRow
                                                            )
                                                          )
                                                           (
                                                            hash-table-ref (
                                                              list-ref-safe t nzRow
                                                            )
                                                             i
                                                          )
                                                        )
                                                         (
                                                          else (
                                                            list-ref-safe (
                                                              list-ref-safe t nzRow
                                                            )
                                                             i
                                                          )
                                                        )
                                                      )
                                                       1.0
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      hash-table-set! result (
                                                        string-append "x" (
                                                          to-str-space (
                                                            _add i 1
                                                          )
                                                        )
                                                      )
                                                       (
                                                        cond (
                                                          (
                                                            string? (
                                                              list-ref-safe t nzRow
                                                            )
                                                          )
                                                           (
                                                            _substring (
                                                              list-ref-safe t nzRow
                                                            )
                                                             lastCol (
                                                              + lastCol 1
                                                            )
                                                          )
                                                        )
                                                         (
                                                          (
                                                            hash-table? (
                                                              list-ref-safe t nzRow
                                                            )
                                                          )
                                                           (
                                                            hash-table-ref (
                                                              list-ref-safe t nzRow
                                                            )
                                                             lastCol
                                                          )
                                                        )
                                                         (
                                                          else (
                                                            list-ref-safe (
                                                              list-ref-safe t nzRow
                                                            )
                                                             lastCol
                                                          )
                                                        )
                                                      )
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
                                        loop14 (
                                          + i 1
                                        )
                                      )
                                    )
                                     '(
                                      
                                    )
                                  )
                                )
                              )
                            )
                             (
                              loop14 0
                            )
                          )
                        )
                      )
                       (
                        ret13 result
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
        simplex tab
      )
       (
        call/cc (
          lambda (
            ret18
          )
           (
            let (
              (
                t tab
              )
            )
             (
              begin (
                call/cc (
                  lambda (
                    break20
                  )
                   (
                    letrec (
                      (
                        loop19 (
                          lambda (
                            
                          )
                           (
                            if #t (
                              begin (
                                let (
                                  (
                                    p (
                                      findPivot t
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        row (
                                          cond (
                                            (
                                              string? p
                                            )
                                             (
                                              _substring p 0 (
                                                + 0 1
                                              )
                                            )
                                          )
                                           (
                                            (
                                              hash-table? p
                                            )
                                             (
                                              hash-table-ref p 0
                                            )
                                          )
                                           (
                                            else (
                                              list-ref-safe p 0
                                            )
                                          )
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            col (
                                              cond (
                                                (
                                                  string? p
                                                )
                                                 (
                                                  _substring p 1 (
                                                    + 1 1
                                                  )
                                                )
                                              )
                                               (
                                                (
                                                  hash-table? p
                                                )
                                                 (
                                                  hash-table-ref p 1
                                                )
                                              )
                                               (
                                                else (
                                                  list-ref-safe p 1
                                                )
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              _lt row 0
                                            )
                                             (
                                              begin (
                                                break20 '(
                                                  
                                                )
                                              )
                                            )
                                             '(
                                              
                                            )
                                          )
                                           (
                                            set! t (
                                              pivot t row col
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                               (
                                loop19
                              )
                            )
                             '(
                              
                            )
                          )
                        )
                      )
                    )
                     (
                      loop19
                    )
                  )
                )
              )
               (
                ret18 t
              )
            )
          )
        )
      )
    )
     (
      let (
        (
          tableau (
            _list (
              _list (
                - 1.0
              )
               (
                - 1.0
              )
               0.0 0.0 0.0
            )
             (
              _list 1.0 3.0 1.0 0.0 4.0
            )
             (
              _list 3.0 1.0 0.0 1.0 4.0
            )
          )
        )
      )
       (
        begin (
          let (
            (
              finalTab (
                simplex tableau
              )
            )
          )
           (
            begin (
              let (
                (
                  res (
                    interpret finalTab 2
                  )
                )
              )
               (
                begin (
                  _display (
                    if (
                      string? (
                        string-append "P: " (
                          to-str-space (
                            cond (
                              (
                                string? res
                              )
                               (
                                _substring res "P" (
                                  + "P" 1
                                )
                              )
                            )
                             (
                              (
                                hash-table? res
                              )
                               (
                                hash-table-ref res "P"
                              )
                            )
                             (
                              else (
                                list-ref-safe res "P"
                              )
                            )
                          )
                        )
                      )
                    )
                     (
                      string-append "P: " (
                        to-str-space (
                          cond (
                            (
                              string? res
                            )
                             (
                              _substring res "P" (
                                + "P" 1
                              )
                            )
                          )
                           (
                            (
                              hash-table? res
                            )
                             (
                              hash-table-ref res "P"
                            )
                          )
                           (
                            else (
                              list-ref-safe res "P"
                            )
                          )
                        )
                      )
                    )
                     (
                      to-str (
                        string-append "P: " (
                          to-str-space (
                            cond (
                              (
                                string? res
                              )
                               (
                                _substring res "P" (
                                  + "P" 1
                                )
                              )
                            )
                             (
                              (
                                hash-table? res
                              )
                               (
                                hash-table-ref res "P"
                              )
                            )
                             (
                              else (
                                list-ref-safe res "P"
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
                 (
                  call/cc (
                    lambda (
                      break22
                    )
                     (
                      letrec (
                        (
                          loop21 (
                            lambda (
                              i
                            )
                             (
                              if (
                                < i 2
                              )
                               (
                                begin (
                                  begin (
                                    let (
                                      (
                                        key (
                                          string-append "x" (
                                            to-str-space (
                                              _add i 1
                                            )
                                          )
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        if (
                                          cond (
                                            (
                                              string? res
                                            )
                                             (
                                              if (
                                                string-contains res key
                                              )
                                               #t #f
                                            )
                                          )
                                           (
                                            (
                                              hash-table? res
                                            )
                                             (
                                              if (
                                                hash-table-exists? res key
                                              )
                                               #t #f
                                            )
                                          )
                                           (
                                            else (
                                              if (
                                                member key res
                                              )
                                               #t #f
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            _display (
                                              if (
                                                string? (
                                                  string-append (
                                                    string-append key ": "
                                                  )
                                                   (
                                                    to-str-space (
                                                      cond (
                                                        (
                                                          string? res
                                                        )
                                                         (
                                                          _substring res key (
                                                            + key 1
                                                          )
                                                        )
                                                      )
                                                       (
                                                        (
                                                          hash-table? res
                                                        )
                                                         (
                                                          hash-table-ref res key
                                                        )
                                                      )
                                                       (
                                                        else (
                                                          list-ref-safe res key
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                string-append (
                                                  string-append key ": "
                                                )
                                                 (
                                                  to-str-space (
                                                    cond (
                                                      (
                                                        string? res
                                                      )
                                                       (
                                                        _substring res key (
                                                          + key 1
                                                        )
                                                      )
                                                    )
                                                     (
                                                      (
                                                        hash-table? res
                                                      )
                                                       (
                                                        hash-table-ref res key
                                                      )
                                                    )
                                                     (
                                                      else (
                                                        list-ref-safe res key
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                to-str (
                                                  string-append (
                                                    string-append key ": "
                                                  )
                                                   (
                                                    to-str-space (
                                                      cond (
                                                        (
                                                          string? res
                                                        )
                                                         (
                                                          _substring res key (
                                                            + key 1
                                                          )
                                                        )
                                                      )
                                                       (
                                                        (
                                                          hash-table? res
                                                        )
                                                         (
                                                          hash-table-ref res key
                                                        )
                                                      )
                                                       (
                                                        else (
                                                          list-ref-safe res key
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
                                 (
                                  loop21 (
                                    + i 1
                                  )
                                )
                              )
                               '(
                                
                              )
                            )
                          )
                        )
                      )
                       (
                        loop21 0
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
          end24 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur25 (
              quotient (
                * (
                  - end24 start23
                )
                 1000000
              )
               jps26
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur25
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
