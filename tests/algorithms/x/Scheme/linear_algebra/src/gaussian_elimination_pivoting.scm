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
        panic msg
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            begin (
              _display (
                if (
                  string? msg
                )
                 msg (
                  to-str msg
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
      define (
        abs_float x
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            begin (
              if (
                < x 0.0
              )
               (
                begin (
                  ret2 (
                    - x
                  )
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              ret2 x
            )
          )
        )
      )
    )
     (
      define (
        copy_matrix src
      )
       (
        call/cc (
          lambda (
            ret3
          )
           (
            let (
              (
                res (
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
                                  < i (
                                    _len src
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        row_src (
                                          list-ref-safe src i
                                        )
                                      )
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
                                                              < j (
                                                                _len row_src
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                set! row (
                                                                  append row (
                                                                    _list (
                                                                      list-ref-safe row_src j
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
                                                set! res (
                                                  append res (
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
                    ret3 res
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
        solve_linear_system matrix
      )
       (
        call/cc (
          lambda (
            ret8
          )
           (
            let (
              (
                ab (
                  copy_matrix matrix
                )
              )
            )
             (
              begin (
                let (
                  (
                    num_rows (
                      _len ab
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        num_cols (
                          - (
                            _len (
                              cond (
                                (
                                  string? ab
                                )
                                 (
                                  _substring ab 0 (
                                    + 0 1
                                  )
                                )
                              )
                               (
                                (
                                  hash-table? ab
                                )
                                 (
                                  hash-table-ref ab 0
                                )
                              )
                               (
                                else (
                                  list-ref-safe ab 0
                                )
                              )
                            )
                          )
                           1
                        )
                      )
                    )
                     (
                      begin (
                        if (
                          not (
                            equal? num_rows num_cols
                          )
                        )
                         (
                          begin (
                            panic "Matrix is not square"
                          )
                           (
                            ret8 (
                              _list
                            )
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
                            column_num 0
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
                                        
                                      )
                                       (
                                        if (
                                          < column_num num_rows
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                i column_num
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
                                                            
                                                          )
                                                           (
                                                            if (
                                                              < i num_cols
                                                            )
                                                             (
                                                              begin (
                                                                if (
                                                                  _gt (
                                                                    abs_float (
                                                                      cond (
                                                                        (
                                                                          string? (
                                                                            cond (
                                                                              (
                                                                                string? ab
                                                                              )
                                                                               (
                                                                                _substring ab i (
                                                                                  + i 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? ab
                                                                              )
                                                                               (
                                                                                hash-table-ref ab i
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref-safe ab i
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          _substring (
                                                                            cond (
                                                                              (
                                                                                string? ab
                                                                              )
                                                                               (
                                                                                _substring ab i (
                                                                                  + i 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? ab
                                                                              )
                                                                               (
                                                                                hash-table-ref ab i
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref-safe ab i
                                                                              )
                                                                            )
                                                                          )
                                                                           column_num (
                                                                            + column_num 1
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        (
                                                                          hash-table? (
                                                                            cond (
                                                                              (
                                                                                string? ab
                                                                              )
                                                                               (
                                                                                _substring ab i (
                                                                                  + i 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? ab
                                                                              )
                                                                               (
                                                                                hash-table-ref ab i
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref-safe ab i
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          hash-table-ref (
                                                                            cond (
                                                                              (
                                                                                string? ab
                                                                              )
                                                                               (
                                                                                _substring ab i (
                                                                                  + i 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? ab
                                                                              )
                                                                               (
                                                                                hash-table-ref ab i
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref-safe ab i
                                                                              )
                                                                            )
                                                                          )
                                                                           column_num
                                                                        )
                                                                      )
                                                                       (
                                                                        else (
                                                                          list-ref-safe (
                                                                            cond (
                                                                              (
                                                                                string? ab
                                                                              )
                                                                               (
                                                                                _substring ab i (
                                                                                  + i 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? ab
                                                                              )
                                                                               (
                                                                                hash-table-ref ab i
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref-safe ab i
                                                                              )
                                                                            )
                                                                          )
                                                                           column_num
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    abs_float (
                                                                      cond (
                                                                        (
                                                                          string? (
                                                                            cond (
                                                                              (
                                                                                string? ab
                                                                              )
                                                                               (
                                                                                _substring ab column_num (
                                                                                  + column_num 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? ab
                                                                              )
                                                                               (
                                                                                hash-table-ref ab column_num
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref-safe ab column_num
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          _substring (
                                                                            cond (
                                                                              (
                                                                                string? ab
                                                                              )
                                                                               (
                                                                                _substring ab column_num (
                                                                                  + column_num 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? ab
                                                                              )
                                                                               (
                                                                                hash-table-ref ab column_num
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref-safe ab column_num
                                                                              )
                                                                            )
                                                                          )
                                                                           column_num (
                                                                            + column_num 1
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        (
                                                                          hash-table? (
                                                                            cond (
                                                                              (
                                                                                string? ab
                                                                              )
                                                                               (
                                                                                _substring ab column_num (
                                                                                  + column_num 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? ab
                                                                              )
                                                                               (
                                                                                hash-table-ref ab column_num
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref-safe ab column_num
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          hash-table-ref (
                                                                            cond (
                                                                              (
                                                                                string? ab
                                                                              )
                                                                               (
                                                                                _substring ab column_num (
                                                                                  + column_num 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? ab
                                                                              )
                                                                               (
                                                                                hash-table-ref ab column_num
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref-safe ab column_num
                                                                              )
                                                                            )
                                                                          )
                                                                           column_num
                                                                        )
                                                                      )
                                                                       (
                                                                        else (
                                                                          list-ref-safe (
                                                                            cond (
                                                                              (
                                                                                string? ab
                                                                              )
                                                                               (
                                                                                _substring ab column_num (
                                                                                  + column_num 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? ab
                                                                              )
                                                                               (
                                                                                hash-table-ref ab column_num
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref-safe ab column_num
                                                                              )
                                                                            )
                                                                          )
                                                                           column_num
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        temp (
                                                                          cond (
                                                                            (
                                                                              string? ab
                                                                            )
                                                                             (
                                                                              _substring ab column_num (
                                                                                + column_num 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            (
                                                                              hash-table? ab
                                                                            )
                                                                             (
                                                                              hash-table-ref ab column_num
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref-safe ab column_num
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        list-set! ab column_num (
                                                                          cond (
                                                                            (
                                                                              string? ab
                                                                            )
                                                                             (
                                                                              _substring ab i (
                                                                                + i 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            (
                                                                              hash-table? ab
                                                                            )
                                                                             (
                                                                              hash-table-ref ab i
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref-safe ab i
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        list-set! ab i temp
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
                                                                set! i (
                                                                  + i 1
                                                                )
                                                              )
                                                               (
                                                                loop11
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
                                                      loop11
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                if (
                                                  _lt (
                                                    abs_float (
                                                      cond (
                                                        (
                                                          string? (
                                                            cond (
                                                              (
                                                                string? ab
                                                              )
                                                               (
                                                                _substring ab column_num (
                                                                  + column_num 1
                                                                )
                                                              )
                                                            )
                                                             (
                                                              (
                                                                hash-table? ab
                                                              )
                                                               (
                                                                hash-table-ref ab column_num
                                                              )
                                                            )
                                                             (
                                                              else (
                                                                list-ref-safe ab column_num
                                                              )
                                                            )
                                                          )
                                                        )
                                                         (
                                                          _substring (
                                                            cond (
                                                              (
                                                                string? ab
                                                              )
                                                               (
                                                                _substring ab column_num (
                                                                  + column_num 1
                                                                )
                                                              )
                                                            )
                                                             (
                                                              (
                                                                hash-table? ab
                                                              )
                                                               (
                                                                hash-table-ref ab column_num
                                                              )
                                                            )
                                                             (
                                                              else (
                                                                list-ref-safe ab column_num
                                                              )
                                                            )
                                                          )
                                                           column_num (
                                                            + column_num 1
                                                          )
                                                        )
                                                      )
                                                       (
                                                        (
                                                          hash-table? (
                                                            cond (
                                                              (
                                                                string? ab
                                                              )
                                                               (
                                                                _substring ab column_num (
                                                                  + column_num 1
                                                                )
                                                              )
                                                            )
                                                             (
                                                              (
                                                                hash-table? ab
                                                              )
                                                               (
                                                                hash-table-ref ab column_num
                                                              )
                                                            )
                                                             (
                                                              else (
                                                                list-ref-safe ab column_num
                                                              )
                                                            )
                                                          )
                                                        )
                                                         (
                                                          hash-table-ref (
                                                            cond (
                                                              (
                                                                string? ab
                                                              )
                                                               (
                                                                _substring ab column_num (
                                                                  + column_num 1
                                                                )
                                                              )
                                                            )
                                                             (
                                                              (
                                                                hash-table? ab
                                                              )
                                                               (
                                                                hash-table-ref ab column_num
                                                              )
                                                            )
                                                             (
                                                              else (
                                                                list-ref-safe ab column_num
                                                              )
                                                            )
                                                          )
                                                           column_num
                                                        )
                                                      )
                                                       (
                                                        else (
                                                          list-ref-safe (
                                                            cond (
                                                              (
                                                                string? ab
                                                              )
                                                               (
                                                                _substring ab column_num (
                                                                  + column_num 1
                                                                )
                                                              )
                                                            )
                                                             (
                                                              (
                                                                hash-table? ab
                                                              )
                                                               (
                                                                hash-table-ref ab column_num
                                                              )
                                                            )
                                                             (
                                                              else (
                                                                list-ref-safe ab column_num
                                                              )
                                                            )
                                                          )
                                                           column_num
                                                        )
                                                      )
                                                    )
                                                  )
                                                   1e-08
                                                )
                                                 (
                                                  begin (
                                                    panic "Matrix is singular"
                                                  )
                                                   (
                                                    ret8 (
                                                      _list
                                                    )
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
                                                    equal? column_num 0
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    set! i column_num
                                                  )
                                                   (
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
                                                                  < i num_rows
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        factor (
                                                                          _div (
                                                                            cond (
                                                                              (
                                                                                string? (
                                                                                  cond (
                                                                                    (
                                                                                      string? ab
                                                                                    )
                                                                                     (
                                                                                      _substring ab i (
                                                                                        + i 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? ab
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref ab i
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref-safe ab i
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                _substring (
                                                                                  cond (
                                                                                    (
                                                                                      string? ab
                                                                                    )
                                                                                     (
                                                                                      _substring ab i (
                                                                                        + i 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? ab
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref ab i
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref-safe ab i
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  - column_num 1
                                                                                )
                                                                                 (
                                                                                  + (
                                                                                    - column_num 1
                                                                                  )
                                                                                   1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? (
                                                                                  cond (
                                                                                    (
                                                                                      string? ab
                                                                                    )
                                                                                     (
                                                                                      _substring ab i (
                                                                                        + i 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? ab
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref ab i
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref-safe ab i
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                hash-table-ref (
                                                                                  cond (
                                                                                    (
                                                                                      string? ab
                                                                                    )
                                                                                     (
                                                                                      _substring ab i (
                                                                                        + i 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? ab
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref ab i
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref-safe ab i
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  - column_num 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref-safe (
                                                                                  cond (
                                                                                    (
                                                                                      string? ab
                                                                                    )
                                                                                     (
                                                                                      _substring ab i (
                                                                                        + i 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? ab
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref ab i
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref-safe ab i
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  - column_num 1
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            cond (
                                                                              (
                                                                                string? (
                                                                                  cond (
                                                                                    (
                                                                                      string? ab
                                                                                    )
                                                                                     (
                                                                                      _substring ab (
                                                                                        - column_num 1
                                                                                      )
                                                                                       (
                                                                                        + (
                                                                                          - column_num 1
                                                                                        )
                                                                                         1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? ab
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref ab (
                                                                                        - column_num 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref-safe ab (
                                                                                        - column_num 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                _substring (
                                                                                  cond (
                                                                                    (
                                                                                      string? ab
                                                                                    )
                                                                                     (
                                                                                      _substring ab (
                                                                                        - column_num 1
                                                                                      )
                                                                                       (
                                                                                        + (
                                                                                          - column_num 1
                                                                                        )
                                                                                         1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? ab
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref ab (
                                                                                        - column_num 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref-safe ab (
                                                                                        - column_num 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  - column_num 1
                                                                                )
                                                                                 (
                                                                                  + (
                                                                                    - column_num 1
                                                                                  )
                                                                                   1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? (
                                                                                  cond (
                                                                                    (
                                                                                      string? ab
                                                                                    )
                                                                                     (
                                                                                      _substring ab (
                                                                                        - column_num 1
                                                                                      )
                                                                                       (
                                                                                        + (
                                                                                          - column_num 1
                                                                                        )
                                                                                         1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? ab
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref ab (
                                                                                        - column_num 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref-safe ab (
                                                                                        - column_num 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                hash-table-ref (
                                                                                  cond (
                                                                                    (
                                                                                      string? ab
                                                                                    )
                                                                                     (
                                                                                      _substring ab (
                                                                                        - column_num 1
                                                                                      )
                                                                                       (
                                                                                        + (
                                                                                          - column_num 1
                                                                                        )
                                                                                         1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? ab
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref ab (
                                                                                        - column_num 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref-safe ab (
                                                                                        - column_num 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  - column_num 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref-safe (
                                                                                  cond (
                                                                                    (
                                                                                      string? ab
                                                                                    )
                                                                                     (
                                                                                      _substring ab (
                                                                                        - column_num 1
                                                                                      )
                                                                                       (
                                                                                        + (
                                                                                          - column_num 1
                                                                                        )
                                                                                         1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? ab
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref ab (
                                                                                        - column_num 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref-safe ab (
                                                                                        - column_num 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  - column_num 1
                                                                                )
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
                                                                                          < j (
                                                                                            _len (
                                                                                              cond (
                                                                                                (
                                                                                                  string? ab
                                                                                                )
                                                                                                 (
                                                                                                  _substring ab i (
                                                                                                    + i 1
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                (
                                                                                                  hash-table? ab
                                                                                                )
                                                                                                 (
                                                                                                  hash-table-ref ab i
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                else (
                                                                                                  list-ref-safe ab i
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            list-set! (
                                                                                              list-ref-safe ab i
                                                                                            )
                                                                                             j (
                                                                                              - (
                                                                                                cond (
                                                                                                  (
                                                                                                    string? (
                                                                                                      cond (
                                                                                                        (
                                                                                                          string? ab
                                                                                                        )
                                                                                                         (
                                                                                                          _substring ab i (
                                                                                                            + i 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? ab
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref ab i
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref-safe ab i
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    _substring (
                                                                                                      cond (
                                                                                                        (
                                                                                                          string? ab
                                                                                                        )
                                                                                                         (
                                                                                                          _substring ab i (
                                                                                                            + i 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? ab
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref ab i
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref-safe ab i
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     j (
                                                                                                      + j 1
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  (
                                                                                                    hash-table? (
                                                                                                      cond (
                                                                                                        (
                                                                                                          string? ab
                                                                                                        )
                                                                                                         (
                                                                                                          _substring ab i (
                                                                                                            + i 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? ab
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref ab i
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref-safe ab i
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    hash-table-ref (
                                                                                                      cond (
                                                                                                        (
                                                                                                          string? ab
                                                                                                        )
                                                                                                         (
                                                                                                          _substring ab i (
                                                                                                            + i 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? ab
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref ab i
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref-safe ab i
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     j
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  else (
                                                                                                    list-ref-safe (
                                                                                                      cond (
                                                                                                        (
                                                                                                          string? ab
                                                                                                        )
                                                                                                         (
                                                                                                          _substring ab i (
                                                                                                            + i 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? ab
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref ab i
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref-safe ab i
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     j
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                * factor (
                                                                                                  cond (
                                                                                                    (
                                                                                                      string? (
                                                                                                        cond (
                                                                                                          (
                                                                                                            string? ab
                                                                                                          )
                                                                                                           (
                                                                                                            _substring ab (
                                                                                                              - column_num 1
                                                                                                            )
                                                                                                             (
                                                                                                              + (
                                                                                                                - column_num 1
                                                                                                              )
                                                                                                               1
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          (
                                                                                                            hash-table? ab
                                                                                                          )
                                                                                                           (
                                                                                                            hash-table-ref ab (
                                                                                                              - column_num 1
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          else (
                                                                                                            list-ref-safe ab (
                                                                                                              - column_num 1
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      _substring (
                                                                                                        cond (
                                                                                                          (
                                                                                                            string? ab
                                                                                                          )
                                                                                                           (
                                                                                                            _substring ab (
                                                                                                              - column_num 1
                                                                                                            )
                                                                                                             (
                                                                                                              + (
                                                                                                                - column_num 1
                                                                                                              )
                                                                                                               1
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          (
                                                                                                            hash-table? ab
                                                                                                          )
                                                                                                           (
                                                                                                            hash-table-ref ab (
                                                                                                              - column_num 1
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          else (
                                                                                                            list-ref-safe ab (
                                                                                                              - column_num 1
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       j (
                                                                                                        + j 1
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    (
                                                                                                      hash-table? (
                                                                                                        cond (
                                                                                                          (
                                                                                                            string? ab
                                                                                                          )
                                                                                                           (
                                                                                                            _substring ab (
                                                                                                              - column_num 1
                                                                                                            )
                                                                                                             (
                                                                                                              + (
                                                                                                                - column_num 1
                                                                                                              )
                                                                                                               1
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          (
                                                                                                            hash-table? ab
                                                                                                          )
                                                                                                           (
                                                                                                            hash-table-ref ab (
                                                                                                              - column_num 1
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          else (
                                                                                                            list-ref-safe ab (
                                                                                                              - column_num 1
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      hash-table-ref (
                                                                                                        cond (
                                                                                                          (
                                                                                                            string? ab
                                                                                                          )
                                                                                                           (
                                                                                                            _substring ab (
                                                                                                              - column_num 1
                                                                                                            )
                                                                                                             (
                                                                                                              + (
                                                                                                                - column_num 1
                                                                                                              )
                                                                                                               1
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          (
                                                                                                            hash-table? ab
                                                                                                          )
                                                                                                           (
                                                                                                            hash-table-ref ab (
                                                                                                              - column_num 1
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          else (
                                                                                                            list-ref-safe ab (
                                                                                                              - column_num 1
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       j
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    else (
                                                                                                      list-ref-safe (
                                                                                                        cond (
                                                                                                          (
                                                                                                            string? ab
                                                                                                          )
                                                                                                           (
                                                                                                            _substring ab (
                                                                                                              - column_num 1
                                                                                                            )
                                                                                                             (
                                                                                                              + (
                                                                                                                - column_num 1
                                                                                                              )
                                                                                                               1
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          (
                                                                                                            hash-table? ab
                                                                                                          )
                                                                                                           (
                                                                                                            hash-table-ref ab (
                                                                                                              - column_num 1
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          else (
                                                                                                            list-ref-safe ab (
                                                                                                              - column_num 1
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       j
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
                                                                                            loop15
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
                                                                                  loop15
                                                                                )
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
                                                                 (
                                                                  quote (
                                                                    
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
                                                    )
                                                  )
                                                )
                                                 (
                                                  quote (
                                                    
                                                  )
                                                )
                                              )
                                               (
                                                set! column_num (
                                                  + column_num 1
                                                )
                                              )
                                            )
                                          )
                                           (
                                            loop9
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
                                  loop9
                                )
                              )
                            )
                          )
                           (
                            let (
                              (
                                x_lst (
                                  _list
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    t 0
                                  )
                                )
                                 (
                                  begin (
                                    call/cc (
                                      lambda (
                                        break18
                                      )
                                       (
                                        letrec (
                                          (
                                            loop17 (
                                              lambda (
                                                
                                              )
                                               (
                                                if (
                                                  < t num_rows
                                                )
                                                 (
                                                  begin (
                                                    set! x_lst (
                                                      append x_lst (
                                                        _list 0.0
                                                      )
                                                    )
                                                  )
                                                   (
                                                    set! t (
                                                      + t 1
                                                    )
                                                  )
                                                   (
                                                    loop17
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
                                          loop17
                                        )
                                      )
                                    )
                                  )
                                   (
                                    set! column_num (
                                      - num_rows 1
                                    )
                                  )
                                   (
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
                                                if (
                                                  >= column_num 0
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        x (
                                                          _div (
                                                            cond (
                                                              (
                                                                string? (
                                                                  cond (
                                                                    (
                                                                      string? ab
                                                                    )
                                                                     (
                                                                      _substring ab column_num (
                                                                        + column_num 1
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    (
                                                                      hash-table? ab
                                                                    )
                                                                     (
                                                                      hash-table-ref ab column_num
                                                                    )
                                                                  )
                                                                   (
                                                                    else (
                                                                      list-ref-safe ab column_num
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                _substring (
                                                                  cond (
                                                                    (
                                                                      string? ab
                                                                    )
                                                                     (
                                                                      _substring ab column_num (
                                                                        + column_num 1
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    (
                                                                      hash-table? ab
                                                                    )
                                                                     (
                                                                      hash-table-ref ab column_num
                                                                    )
                                                                  )
                                                                   (
                                                                    else (
                                                                      list-ref-safe ab column_num
                                                                    )
                                                                  )
                                                                )
                                                                 num_cols (
                                                                  + num_cols 1
                                                                )
                                                              )
                                                            )
                                                             (
                                                              (
                                                                hash-table? (
                                                                  cond (
                                                                    (
                                                                      string? ab
                                                                    )
                                                                     (
                                                                      _substring ab column_num (
                                                                        + column_num 1
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    (
                                                                      hash-table? ab
                                                                    )
                                                                     (
                                                                      hash-table-ref ab column_num
                                                                    )
                                                                  )
                                                                   (
                                                                    else (
                                                                      list-ref-safe ab column_num
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                hash-table-ref (
                                                                  cond (
                                                                    (
                                                                      string? ab
                                                                    )
                                                                     (
                                                                      _substring ab column_num (
                                                                        + column_num 1
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    (
                                                                      hash-table? ab
                                                                    )
                                                                     (
                                                                      hash-table-ref ab column_num
                                                                    )
                                                                  )
                                                                   (
                                                                    else (
                                                                      list-ref-safe ab column_num
                                                                    )
                                                                  )
                                                                )
                                                                 num_cols
                                                              )
                                                            )
                                                             (
                                                              else (
                                                                list-ref-safe (
                                                                  cond (
                                                                    (
                                                                      string? ab
                                                                    )
                                                                     (
                                                                      _substring ab column_num (
                                                                        + column_num 1
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    (
                                                                      hash-table? ab
                                                                    )
                                                                     (
                                                                      hash-table-ref ab column_num
                                                                    )
                                                                  )
                                                                   (
                                                                    else (
                                                                      list-ref-safe ab column_num
                                                                    )
                                                                  )
                                                                )
                                                                 num_cols
                                                              )
                                                            )
                                                          )
                                                           (
                                                            cond (
                                                              (
                                                                string? (
                                                                  cond (
                                                                    (
                                                                      string? ab
                                                                    )
                                                                     (
                                                                      _substring ab column_num (
                                                                        + column_num 1
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    (
                                                                      hash-table? ab
                                                                    )
                                                                     (
                                                                      hash-table-ref ab column_num
                                                                    )
                                                                  )
                                                                   (
                                                                    else (
                                                                      list-ref-safe ab column_num
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                _substring (
                                                                  cond (
                                                                    (
                                                                      string? ab
                                                                    )
                                                                     (
                                                                      _substring ab column_num (
                                                                        + column_num 1
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    (
                                                                      hash-table? ab
                                                                    )
                                                                     (
                                                                      hash-table-ref ab column_num
                                                                    )
                                                                  )
                                                                   (
                                                                    else (
                                                                      list-ref-safe ab column_num
                                                                    )
                                                                  )
                                                                )
                                                                 column_num (
                                                                  + column_num 1
                                                                )
                                                              )
                                                            )
                                                             (
                                                              (
                                                                hash-table? (
                                                                  cond (
                                                                    (
                                                                      string? ab
                                                                    )
                                                                     (
                                                                      _substring ab column_num (
                                                                        + column_num 1
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    (
                                                                      hash-table? ab
                                                                    )
                                                                     (
                                                                      hash-table-ref ab column_num
                                                                    )
                                                                  )
                                                                   (
                                                                    else (
                                                                      list-ref-safe ab column_num
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                hash-table-ref (
                                                                  cond (
                                                                    (
                                                                      string? ab
                                                                    )
                                                                     (
                                                                      _substring ab column_num (
                                                                        + column_num 1
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    (
                                                                      hash-table? ab
                                                                    )
                                                                     (
                                                                      hash-table-ref ab column_num
                                                                    )
                                                                  )
                                                                   (
                                                                    else (
                                                                      list-ref-safe ab column_num
                                                                    )
                                                                  )
                                                                )
                                                                 column_num
                                                              )
                                                            )
                                                             (
                                                              else (
                                                                list-ref-safe (
                                                                  cond (
                                                                    (
                                                                      string? ab
                                                                    )
                                                                     (
                                                                      _substring ab column_num (
                                                                        + column_num 1
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    (
                                                                      hash-table? ab
                                                                    )
                                                                     (
                                                                      hash-table-ref ab column_num
                                                                    )
                                                                  )
                                                                   (
                                                                    else (
                                                                      list-ref-safe ab column_num
                                                                    )
                                                                  )
                                                                )
                                                                 column_num
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        list-set! x_lst column_num x
                                                      )
                                                       (
                                                        let (
                                                          (
                                                            i (
                                                              - column_num 1
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            call/cc (
                                                              lambda (
                                                                break22
                                                              )
                                                               (
                                                                letrec (
                                                                  (
                                                                    loop21 (
                                                                      lambda (
                                                                        
                                                                      )
                                                                       (
                                                                        if (
                                                                          >= i 0
                                                                        )
                                                                         (
                                                                          begin (
                                                                            list-set! (
                                                                              list-ref-safe ab i
                                                                            )
                                                                             num_cols (
                                                                              - (
                                                                                cond (
                                                                                  (
                                                                                    string? (
                                                                                      cond (
                                                                                        (
                                                                                          string? ab
                                                                                        )
                                                                                         (
                                                                                          _substring ab i (
                                                                                            + i 1
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        (
                                                                                          hash-table? ab
                                                                                        )
                                                                                         (
                                                                                          hash-table-ref ab i
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        else (
                                                                                          list-ref-safe ab i
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    _substring (
                                                                                      cond (
                                                                                        (
                                                                                          string? ab
                                                                                        )
                                                                                         (
                                                                                          _substring ab i (
                                                                                            + i 1
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        (
                                                                                          hash-table? ab
                                                                                        )
                                                                                         (
                                                                                          hash-table-ref ab i
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        else (
                                                                                          list-ref-safe ab i
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     num_cols (
                                                                                      + num_cols 1
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  (
                                                                                    hash-table? (
                                                                                      cond (
                                                                                        (
                                                                                          string? ab
                                                                                        )
                                                                                         (
                                                                                          _substring ab i (
                                                                                            + i 1
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        (
                                                                                          hash-table? ab
                                                                                        )
                                                                                         (
                                                                                          hash-table-ref ab i
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        else (
                                                                                          list-ref-safe ab i
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    hash-table-ref (
                                                                                      cond (
                                                                                        (
                                                                                          string? ab
                                                                                        )
                                                                                         (
                                                                                          _substring ab i (
                                                                                            + i 1
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        (
                                                                                          hash-table? ab
                                                                                        )
                                                                                         (
                                                                                          hash-table-ref ab i
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        else (
                                                                                          list-ref-safe ab i
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     num_cols
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  else (
                                                                                    list-ref-safe (
                                                                                      cond (
                                                                                        (
                                                                                          string? ab
                                                                                        )
                                                                                         (
                                                                                          _substring ab i (
                                                                                            + i 1
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        (
                                                                                          hash-table? ab
                                                                                        )
                                                                                         (
                                                                                          hash-table-ref ab i
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        else (
                                                                                          list-ref-safe ab i
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     num_cols
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                * (
                                                                                  cond (
                                                                                    (
                                                                                      string? (
                                                                                        cond (
                                                                                          (
                                                                                            string? ab
                                                                                          )
                                                                                           (
                                                                                            _substring ab i (
                                                                                              + i 1
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          (
                                                                                            hash-table? ab
                                                                                          )
                                                                                           (
                                                                                            hash-table-ref ab i
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          else (
                                                                                            list-ref-safe ab i
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        cond (
                                                                                          (
                                                                                            string? ab
                                                                                          )
                                                                                           (
                                                                                            _substring ab i (
                                                                                              + i 1
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          (
                                                                                            hash-table? ab
                                                                                          )
                                                                                           (
                                                                                            hash-table-ref ab i
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          else (
                                                                                            list-ref-safe ab i
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       column_num (
                                                                                        + column_num 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        cond (
                                                                                          (
                                                                                            string? ab
                                                                                          )
                                                                                           (
                                                                                            _substring ab i (
                                                                                              + i 1
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          (
                                                                                            hash-table? ab
                                                                                          )
                                                                                           (
                                                                                            hash-table-ref ab i
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          else (
                                                                                            list-ref-safe ab i
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        cond (
                                                                                          (
                                                                                            string? ab
                                                                                          )
                                                                                           (
                                                                                            _substring ab i (
                                                                                              + i 1
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          (
                                                                                            hash-table? ab
                                                                                          )
                                                                                           (
                                                                                            hash-table-ref ab i
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          else (
                                                                                            list-ref-safe ab i
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       column_num
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref-safe (
                                                                                        cond (
                                                                                          (
                                                                                            string? ab
                                                                                          )
                                                                                           (
                                                                                            _substring ab i (
                                                                                              + i 1
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          (
                                                                                            hash-table? ab
                                                                                          )
                                                                                           (
                                                                                            hash-table-ref ab i
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          else (
                                                                                            list-ref-safe ab i
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       column_num
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 x
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            set! i (
                                                                              - i 1
                                                                            )
                                                                          )
                                                                           (
                                                                            loop21
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
                                                                  loop21
                                                                )
                                                              )
                                                            )
                                                          )
                                                           (
                                                            set! column_num (
                                                              - column_num 1
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
                                                 (
                                                  quote (
                                                    
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
                                    )
                                  )
                                   (
                                    ret8 x_lst
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
      let (
        (
          example_matrix (
            _list (
              _list 5.0 (
                - 5.0
              )
               (
                - 3.0
              )
               4.0 (
                - 11.0
              )
            )
             (
              _list 1.0 (
                - 4.0
              )
               6.0 (
                - 4.0
              )
               (
                - 10.0
              )
            )
             (
              _list (
                - 2.0
              )
               (
                - 5.0
              )
               4.0 (
                - 5.0
              )
               (
                - 12.0
              )
            )
             (
              _list (
                - 3.0
              )
               (
                - 3.0
              )
               5.0 (
                - 5.0
              )
               8.0
            )
          )
        )
      )
       (
        begin (
          _display (
            if (
              string? "Matrix:"
            )
             "Matrix:" (
              to-str "Matrix:"
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
                to-str-space example_matrix
              )
            )
             (
              to-str-space example_matrix
            )
             (
              to-str (
                to-str-space example_matrix
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
              solution (
                solve_linear_system example_matrix
              )
            )
          )
           (
            begin (
              _display (
                if (
                  string? (
                    to-str-space solution
                  )
                )
                 (
                  to-str-space solution
                )
                 (
                  to-str (
                    to-str-space solution
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
