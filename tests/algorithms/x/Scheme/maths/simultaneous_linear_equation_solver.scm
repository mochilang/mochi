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
      start29 (
        current-jiffy
      )
    )
     (
      jps32 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        floor x
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                i (
                  let (
                    (
                      v2 x
                    )
                  )
                   (
                    cond (
                      (
                        string? v2
                      )
                       (
                        inexact->exact (
                          floor (
                            string->number v2
                          )
                        )
                      )
                    )
                     (
                      (
                        boolean? v2
                      )
                       (
                        if v2 1 0
                      )
                    )
                     (
                      else (
                        inexact->exact (
                          floor v2
                        )
                      )
                    )
                  )
                )
              )
            )
             (
              begin (
                if (
                  > (
                    + 0.0 i
                  )
                   x
                )
                 (
                  begin (
                    set! i (
                      - i 1
                    )
                  )
                )
                 '(
                  
                )
              )
               (
                ret1 (
                  + 0.0 i
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        pow10 n
      )
       (
        call/cc (
          lambda (
            ret3
          )
           (
            let (
              (
                p 1.0
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
                                  < i n
                                )
                                 (
                                  begin (
                                    set! p (
                                      * p 10.0
                                    )
                                  )
                                   (
                                    set! i (
                                      + i 1
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
                    ret3 p
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
        round x n
      )
       (
        call/cc (
          lambda (
            ret6
          )
           (
            let (
              (
                m (
                  pow10 n
                )
              )
            )
             (
              begin (
                ret6 (
                  _div (
                    floor (
                      _add (
                        * x m
                      )
                       0.5
                    )
                  )
                   m
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        clone_matrix mat
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            let (
              (
                new_mat (
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
                                  < i (
                                    _len mat
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
                                                          < j (
                                                            _len (
                                                              list-ref-safe mat i
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            set! row (
                                                              append row (
                                                                _list (
                                                                  cond (
                                                                    (
                                                                      string? (
                                                                        list-ref-safe mat i
                                                                      )
                                                                    )
                                                                     (
                                                                      _substring (
                                                                        list-ref-safe mat i
                                                                      )
                                                                       j (
                                                                        + j 1
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    (
                                                                      hash-table? (
                                                                        list-ref-safe mat i
                                                                      )
                                                                    )
                                                                     (
                                                                      hash-table-ref (
                                                                        list-ref-safe mat i
                                                                      )
                                                                       j
                                                                    )
                                                                  )
                                                                   (
                                                                    else (
                                                                      list-ref-safe (
                                                                        list-ref-safe mat i
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
                                            set! new_mat (
                                              append new_mat (
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
                    ret7 new_mat
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
        solve_simultaneous equations
      )
       (
        call/cc (
          lambda (
            ret12
          )
           (
            let (
              (
                n (
                  _len equations
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
                    panic "solve_simultaneous() requires n lists of length n+1"
                  )
                )
                 '(
                  
                )
              )
               (
                let (
                  (
                    m (
                      + n 1
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
                                      < i n
                                    )
                                     (
                                      begin (
                                        if (
                                          not (
                                            equal? (
                                              _len (
                                                list-ref-safe equations i
                                              )
                                            )
                                             m
                                          )
                                        )
                                         (
                                          begin (
                                            panic "solve_simultaneous() requires n lists of length n+1"
                                          )
                                        )
                                         '(
                                          
                                        )
                                      )
                                       (
                                        set! i (
                                          + i 1
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
                        let (
                          (
                            a (
                              clone_matrix equations
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                row 0
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
                                              < row n
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    pivot row
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
                                                                  and (
                                                                    < pivot n
                                                                  )
                                                                   (
                                                                    equal? (
                                                                      cond (
                                                                        (
                                                                          string? (
                                                                            cond (
                                                                              (
                                                                                string? a
                                                                              )
                                                                               (
                                                                                _substring a pivot (
                                                                                  + pivot 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? a
                                                                              )
                                                                               (
                                                                                hash-table-ref a pivot
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref-safe a pivot
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          _substring (
                                                                            cond (
                                                                              (
                                                                                string? a
                                                                              )
                                                                               (
                                                                                _substring a pivot (
                                                                                  + pivot 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? a
                                                                              )
                                                                               (
                                                                                hash-table-ref a pivot
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref-safe a pivot
                                                                              )
                                                                            )
                                                                          )
                                                                           row (
                                                                            + row 1
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        (
                                                                          hash-table? (
                                                                            cond (
                                                                              (
                                                                                string? a
                                                                              )
                                                                               (
                                                                                _substring a pivot (
                                                                                  + pivot 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? a
                                                                              )
                                                                               (
                                                                                hash-table-ref a pivot
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref-safe a pivot
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          hash-table-ref (
                                                                            cond (
                                                                              (
                                                                                string? a
                                                                              )
                                                                               (
                                                                                _substring a pivot (
                                                                                  + pivot 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? a
                                                                              )
                                                                               (
                                                                                hash-table-ref a pivot
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref-safe a pivot
                                                                              )
                                                                            )
                                                                          )
                                                                           row
                                                                        )
                                                                      )
                                                                       (
                                                                        else (
                                                                          list-ref-safe (
                                                                            cond (
                                                                              (
                                                                                string? a
                                                                              )
                                                                               (
                                                                                _substring a pivot (
                                                                                  + pivot 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? a
                                                                              )
                                                                               (
                                                                                hash-table-ref a pivot
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref-safe a pivot
                                                                              )
                                                                            )
                                                                          )
                                                                           row
                                                                        )
                                                                      )
                                                                    )
                                                                     0.0
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! pivot (
                                                                      + pivot 1
                                                                    )
                                                                  )
                                                                   (
                                                                    loop17
                                                                  )
                                                                )
                                                                 '(
                                                                  
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
                                                    if (
                                                      equal? pivot n
                                                    )
                                                     (
                                                      begin (
                                                        panic "solve_simultaneous() requires at least 1 full equation"
                                                      )
                                                    )
                                                     '(
                                                      
                                                    )
                                                  )
                                                   (
                                                    if (
                                                      not (
                                                        equal? pivot row
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            temp (
                                                              cond (
                                                                (
                                                                  string? a
                                                                )
                                                                 (
                                                                  _substring a row (
                                                                    + row 1
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                (
                                                                  hash-table? a
                                                                )
                                                                 (
                                                                  hash-table-ref a row
                                                                )
                                                              )
                                                               (
                                                                else (
                                                                  list-ref-safe a row
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            list-set! a row (
                                                              cond (
                                                                (
                                                                  string? a
                                                                )
                                                                 (
                                                                  _substring a pivot (
                                                                    + pivot 1
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                (
                                                                  hash-table? a
                                                                )
                                                                 (
                                                                  hash-table-ref a pivot
                                                                )
                                                              )
                                                               (
                                                                else (
                                                                  list-ref-safe a pivot
                                                                )
                                                              )
                                                            )
                                                          )
                                                           (
                                                            list-set! a pivot temp
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
                                                        pivot_val (
                                                          cond (
                                                            (
                                                              string? (
                                                                cond (
                                                                  (
                                                                    string? a
                                                                  )
                                                                   (
                                                                    _substring a row (
                                                                      + row 1
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  (
                                                                    hash-table? a
                                                                  )
                                                                   (
                                                                    hash-table-ref a row
                                                                  )
                                                                )
                                                                 (
                                                                  else (
                                                                    list-ref-safe a row
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             (
                                                              _substring (
                                                                cond (
                                                                  (
                                                                    string? a
                                                                  )
                                                                   (
                                                                    _substring a row (
                                                                      + row 1
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  (
                                                                    hash-table? a
                                                                  )
                                                                   (
                                                                    hash-table-ref a row
                                                                  )
                                                                )
                                                                 (
                                                                  else (
                                                                    list-ref-safe a row
                                                                  )
                                                                )
                                                              )
                                                               row (
                                                                + row 1
                                                              )
                                                            )
                                                          )
                                                           (
                                                            (
                                                              hash-table? (
                                                                cond (
                                                                  (
                                                                    string? a
                                                                  )
                                                                   (
                                                                    _substring a row (
                                                                      + row 1
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  (
                                                                    hash-table? a
                                                                  )
                                                                   (
                                                                    hash-table-ref a row
                                                                  )
                                                                )
                                                                 (
                                                                  else (
                                                                    list-ref-safe a row
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             (
                                                              hash-table-ref (
                                                                cond (
                                                                  (
                                                                    string? a
                                                                  )
                                                                   (
                                                                    _substring a row (
                                                                      + row 1
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  (
                                                                    hash-table? a
                                                                  )
                                                                   (
                                                                    hash-table-ref a row
                                                                  )
                                                                )
                                                                 (
                                                                  else (
                                                                    list-ref-safe a row
                                                                  )
                                                                )
                                                              )
                                                               row
                                                            )
                                                          )
                                                           (
                                                            else (
                                                              list-ref-safe (
                                                                cond (
                                                                  (
                                                                    string? a
                                                                  )
                                                                   (
                                                                    _substring a row (
                                                                      + row 1
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  (
                                                                    hash-table? a
                                                                  )
                                                                   (
                                                                    hash-table-ref a row
                                                                  )
                                                                )
                                                                 (
                                                                  else (
                                                                    list-ref-safe a row
                                                                  )
                                                                )
                                                              )
                                                               row
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            col 0
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
                                                                        if (
                                                                          < col m
                                                                        )
                                                                         (
                                                                          begin (
                                                                            list-set! (
                                                                              list-ref-safe a row
                                                                            )
                                                                             col (
                                                                              _div (
                                                                                cond (
                                                                                  (
                                                                                    string? (
                                                                                      cond (
                                                                                        (
                                                                                          string? a
                                                                                        )
                                                                                         (
                                                                                          _substring a row (
                                                                                            + row 1
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        (
                                                                                          hash-table? a
                                                                                        )
                                                                                         (
                                                                                          hash-table-ref a row
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        else (
                                                                                          list-ref-safe a row
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    _substring (
                                                                                      cond (
                                                                                        (
                                                                                          string? a
                                                                                        )
                                                                                         (
                                                                                          _substring a row (
                                                                                            + row 1
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        (
                                                                                          hash-table? a
                                                                                        )
                                                                                         (
                                                                                          hash-table-ref a row
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        else (
                                                                                          list-ref-safe a row
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     col (
                                                                                      + col 1
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  (
                                                                                    hash-table? (
                                                                                      cond (
                                                                                        (
                                                                                          string? a
                                                                                        )
                                                                                         (
                                                                                          _substring a row (
                                                                                            + row 1
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        (
                                                                                          hash-table? a
                                                                                        )
                                                                                         (
                                                                                          hash-table-ref a row
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        else (
                                                                                          list-ref-safe a row
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    hash-table-ref (
                                                                                      cond (
                                                                                        (
                                                                                          string? a
                                                                                        )
                                                                                         (
                                                                                          _substring a row (
                                                                                            + row 1
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        (
                                                                                          hash-table? a
                                                                                        )
                                                                                         (
                                                                                          hash-table-ref a row
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        else (
                                                                                          list-ref-safe a row
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     col
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  else (
                                                                                    list-ref-safe (
                                                                                      cond (
                                                                                        (
                                                                                          string? a
                                                                                        )
                                                                                         (
                                                                                          _substring a row (
                                                                                            + row 1
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        (
                                                                                          hash-table? a
                                                                                        )
                                                                                         (
                                                                                          hash-table-ref a row
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        else (
                                                                                          list-ref-safe a row
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     col
                                                                                  )
                                                                                )
                                                                              )
                                                                               pivot_val
                                                                            )
                                                                          )
                                                                           (
                                                                            set! col (
                                                                              + col 1
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
                                                            let (
                                                              (
                                                                r 0
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
                                                                              < r n
                                                                            )
                                                                             (
                                                                              begin (
                                                                                if (
                                                                                  not (
                                                                                    equal? r row
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
                                                                                                cond (
                                                                                                  (
                                                                                                    string? a
                                                                                                  )
                                                                                                   (
                                                                                                    _substring a r (
                                                                                                      + r 1
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  (
                                                                                                    hash-table? a
                                                                                                  )
                                                                                                   (
                                                                                                    hash-table-ref a r
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  else (
                                                                                                    list-ref-safe a r
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              _substring (
                                                                                                cond (
                                                                                                  (
                                                                                                    string? a
                                                                                                  )
                                                                                                   (
                                                                                                    _substring a r (
                                                                                                      + r 1
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  (
                                                                                                    hash-table? a
                                                                                                  )
                                                                                                   (
                                                                                                    hash-table-ref a r
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  else (
                                                                                                    list-ref-safe a r
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               row (
                                                                                                + row 1
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            (
                                                                                              hash-table? (
                                                                                                cond (
                                                                                                  (
                                                                                                    string? a
                                                                                                  )
                                                                                                   (
                                                                                                    _substring a r (
                                                                                                      + r 1
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  (
                                                                                                    hash-table? a
                                                                                                  )
                                                                                                   (
                                                                                                    hash-table-ref a r
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  else (
                                                                                                    list-ref-safe a r
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              hash-table-ref (
                                                                                                cond (
                                                                                                  (
                                                                                                    string? a
                                                                                                  )
                                                                                                   (
                                                                                                    _substring a r (
                                                                                                      + r 1
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  (
                                                                                                    hash-table? a
                                                                                                  )
                                                                                                   (
                                                                                                    hash-table-ref a r
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  else (
                                                                                                    list-ref-safe a r
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               row
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            else (
                                                                                              list-ref-safe (
                                                                                                cond (
                                                                                                  (
                                                                                                    string? a
                                                                                                  )
                                                                                                   (
                                                                                                    _substring a r (
                                                                                                      + r 1
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  (
                                                                                                    hash-table? a
                                                                                                  )
                                                                                                   (
                                                                                                    hash-table-ref a r
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  else (
                                                                                                    list-ref-safe a r
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               row
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        let (
                                                                                          (
                                                                                            c 0
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            call/cc (
                                                                                              lambda (
                                                                                                break24
                                                                                              )
                                                                                               (
                                                                                                letrec (
                                                                                                  (
                                                                                                    loop23 (
                                                                                                      lambda (
                                                                                                        
                                                                                                      )
                                                                                                       (
                                                                                                        if (
                                                                                                          < c m
                                                                                                        )
                                                                                                         (
                                                                                                          begin (
                                                                                                            list-set! (
                                                                                                              list-ref-safe a r
                                                                                                            )
                                                                                                             c (
                                                                                                              - (
                                                                                                                cond (
                                                                                                                  (
                                                                                                                    string? (
                                                                                                                      cond (
                                                                                                                        (
                                                                                                                          string? a
                                                                                                                        )
                                                                                                                         (
                                                                                                                          _substring a r (
                                                                                                                            + r 1
                                                                                                                          )
                                                                                                                        )
                                                                                                                      )
                                                                                                                       (
                                                                                                                        (
                                                                                                                          hash-table? a
                                                                                                                        )
                                                                                                                         (
                                                                                                                          hash-table-ref a r
                                                                                                                        )
                                                                                                                      )
                                                                                                                       (
                                                                                                                        else (
                                                                                                                          list-ref-safe a r
                                                                                                                        )
                                                                                                                      )
                                                                                                                    )
                                                                                                                  )
                                                                                                                   (
                                                                                                                    _substring (
                                                                                                                      cond (
                                                                                                                        (
                                                                                                                          string? a
                                                                                                                        )
                                                                                                                         (
                                                                                                                          _substring a r (
                                                                                                                            + r 1
                                                                                                                          )
                                                                                                                        )
                                                                                                                      )
                                                                                                                       (
                                                                                                                        (
                                                                                                                          hash-table? a
                                                                                                                        )
                                                                                                                         (
                                                                                                                          hash-table-ref a r
                                                                                                                        )
                                                                                                                      )
                                                                                                                       (
                                                                                                                        else (
                                                                                                                          list-ref-safe a r
                                                                                                                        )
                                                                                                                      )
                                                                                                                    )
                                                                                                                     c (
                                                                                                                      + c 1
                                                                                                                    )
                                                                                                                  )
                                                                                                                )
                                                                                                                 (
                                                                                                                  (
                                                                                                                    hash-table? (
                                                                                                                      cond (
                                                                                                                        (
                                                                                                                          string? a
                                                                                                                        )
                                                                                                                         (
                                                                                                                          _substring a r (
                                                                                                                            + r 1
                                                                                                                          )
                                                                                                                        )
                                                                                                                      )
                                                                                                                       (
                                                                                                                        (
                                                                                                                          hash-table? a
                                                                                                                        )
                                                                                                                         (
                                                                                                                          hash-table-ref a r
                                                                                                                        )
                                                                                                                      )
                                                                                                                       (
                                                                                                                        else (
                                                                                                                          list-ref-safe a r
                                                                                                                        )
                                                                                                                      )
                                                                                                                    )
                                                                                                                  )
                                                                                                                   (
                                                                                                                    hash-table-ref (
                                                                                                                      cond (
                                                                                                                        (
                                                                                                                          string? a
                                                                                                                        )
                                                                                                                         (
                                                                                                                          _substring a r (
                                                                                                                            + r 1
                                                                                                                          )
                                                                                                                        )
                                                                                                                      )
                                                                                                                       (
                                                                                                                        (
                                                                                                                          hash-table? a
                                                                                                                        )
                                                                                                                         (
                                                                                                                          hash-table-ref a r
                                                                                                                        )
                                                                                                                      )
                                                                                                                       (
                                                                                                                        else (
                                                                                                                          list-ref-safe a r
                                                                                                                        )
                                                                                                                      )
                                                                                                                    )
                                                                                                                     c
                                                                                                                  )
                                                                                                                )
                                                                                                                 (
                                                                                                                  else (
                                                                                                                    list-ref-safe (
                                                                                                                      cond (
                                                                                                                        (
                                                                                                                          string? a
                                                                                                                        )
                                                                                                                         (
                                                                                                                          _substring a r (
                                                                                                                            + r 1
                                                                                                                          )
                                                                                                                        )
                                                                                                                      )
                                                                                                                       (
                                                                                                                        (
                                                                                                                          hash-table? a
                                                                                                                        )
                                                                                                                         (
                                                                                                                          hash-table-ref a r
                                                                                                                        )
                                                                                                                      )
                                                                                                                       (
                                                                                                                        else (
                                                                                                                          list-ref-safe a r
                                                                                                                        )
                                                                                                                      )
                                                                                                                    )
                                                                                                                     c
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
                                                                                                                            string? a
                                                                                                                          )
                                                                                                                           (
                                                                                                                            _substring a row (
                                                                                                                              + row 1
                                                                                                                            )
                                                                                                                          )
                                                                                                                        )
                                                                                                                         (
                                                                                                                          (
                                                                                                                            hash-table? a
                                                                                                                          )
                                                                                                                           (
                                                                                                                            hash-table-ref a row
                                                                                                                          )
                                                                                                                        )
                                                                                                                         (
                                                                                                                          else (
                                                                                                                            list-ref-safe a row
                                                                                                                          )
                                                                                                                        )
                                                                                                                      )
                                                                                                                    )
                                                                                                                     (
                                                                                                                      _substring (
                                                                                                                        cond (
                                                                                                                          (
                                                                                                                            string? a
                                                                                                                          )
                                                                                                                           (
                                                                                                                            _substring a row (
                                                                                                                              + row 1
                                                                                                                            )
                                                                                                                          )
                                                                                                                        )
                                                                                                                         (
                                                                                                                          (
                                                                                                                            hash-table? a
                                                                                                                          )
                                                                                                                           (
                                                                                                                            hash-table-ref a row
                                                                                                                          )
                                                                                                                        )
                                                                                                                         (
                                                                                                                          else (
                                                                                                                            list-ref-safe a row
                                                                                                                          )
                                                                                                                        )
                                                                                                                      )
                                                                                                                       c (
                                                                                                                        + c 1
                                                                                                                      )
                                                                                                                    )
                                                                                                                  )
                                                                                                                   (
                                                                                                                    (
                                                                                                                      hash-table? (
                                                                                                                        cond (
                                                                                                                          (
                                                                                                                            string? a
                                                                                                                          )
                                                                                                                           (
                                                                                                                            _substring a row (
                                                                                                                              + row 1
                                                                                                                            )
                                                                                                                          )
                                                                                                                        )
                                                                                                                         (
                                                                                                                          (
                                                                                                                            hash-table? a
                                                                                                                          )
                                                                                                                           (
                                                                                                                            hash-table-ref a row
                                                                                                                          )
                                                                                                                        )
                                                                                                                         (
                                                                                                                          else (
                                                                                                                            list-ref-safe a row
                                                                                                                          )
                                                                                                                        )
                                                                                                                      )
                                                                                                                    )
                                                                                                                     (
                                                                                                                      hash-table-ref (
                                                                                                                        cond (
                                                                                                                          (
                                                                                                                            string? a
                                                                                                                          )
                                                                                                                           (
                                                                                                                            _substring a row (
                                                                                                                              + row 1
                                                                                                                            )
                                                                                                                          )
                                                                                                                        )
                                                                                                                         (
                                                                                                                          (
                                                                                                                            hash-table? a
                                                                                                                          )
                                                                                                                           (
                                                                                                                            hash-table-ref a row
                                                                                                                          )
                                                                                                                        )
                                                                                                                         (
                                                                                                                          else (
                                                                                                                            list-ref-safe a row
                                                                                                                          )
                                                                                                                        )
                                                                                                                      )
                                                                                                                       c
                                                                                                                    )
                                                                                                                  )
                                                                                                                   (
                                                                                                                    else (
                                                                                                                      list-ref-safe (
                                                                                                                        cond (
                                                                                                                          (
                                                                                                                            string? a
                                                                                                                          )
                                                                                                                           (
                                                                                                                            _substring a row (
                                                                                                                              + row 1
                                                                                                                            )
                                                                                                                          )
                                                                                                                        )
                                                                                                                         (
                                                                                                                          (
                                                                                                                            hash-table? a
                                                                                                                          )
                                                                                                                           (
                                                                                                                            hash-table-ref a row
                                                                                                                          )
                                                                                                                        )
                                                                                                                         (
                                                                                                                          else (
                                                                                                                            list-ref-safe a row
                                                                                                                          )
                                                                                                                        )
                                                                                                                      )
                                                                                                                       c
                                                                                                                    )
                                                                                                                  )
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                           (
                                                                                                            set! c (
                                                                                                              + c 1
                                                                                                            )
                                                                                                          )
                                                                                                           (
                                                                                                            loop23
                                                                                                          )
                                                                                                        )
                                                                                                         '(
                                                                                                          
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  loop23
                                                                                                )
                                                                                              )
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
                                                                               (
                                                                                set! r (
                                                                                  + r 1
                                                                                )
                                                                              )
                                                                               (
                                                                                loop21
                                                                              )
                                                                            )
                                                                             '(
                                                                              
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
                                                                set! row (
                                                                  + row 1
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
                                        k 0
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
                                                      < k n
                                                    )
                                                     (
                                                      begin (
                                                        set! res (
                                                          append res (
                                                            _list (
                                                              round (
                                                                cond (
                                                                  (
                                                                    string? (
                                                                      cond (
                                                                        (
                                                                          string? a
                                                                        )
                                                                         (
                                                                          _substring a k (
                                                                            + k 1
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        (
                                                                          hash-table? a
                                                                        )
                                                                         (
                                                                          hash-table-ref a k
                                                                        )
                                                                      )
                                                                       (
                                                                        else (
                                                                          list-ref-safe a k
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    _substring (
                                                                      cond (
                                                                        (
                                                                          string? a
                                                                        )
                                                                         (
                                                                          _substring a k (
                                                                            + k 1
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        (
                                                                          hash-table? a
                                                                        )
                                                                         (
                                                                          hash-table-ref a k
                                                                        )
                                                                      )
                                                                       (
                                                                        else (
                                                                          list-ref-safe a k
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      - m 1
                                                                    )
                                                                     (
                                                                      + (
                                                                        - m 1
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
                                                                          string? a
                                                                        )
                                                                         (
                                                                          _substring a k (
                                                                            + k 1
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        (
                                                                          hash-table? a
                                                                        )
                                                                         (
                                                                          hash-table-ref a k
                                                                        )
                                                                      )
                                                                       (
                                                                        else (
                                                                          list-ref-safe a k
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    hash-table-ref (
                                                                      cond (
                                                                        (
                                                                          string? a
                                                                        )
                                                                         (
                                                                          _substring a k (
                                                                            + k 1
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        (
                                                                          hash-table? a
                                                                        )
                                                                         (
                                                                          hash-table-ref a k
                                                                        )
                                                                      )
                                                                       (
                                                                        else (
                                                                          list-ref-safe a k
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      - m 1
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  else (
                                                                    list-ref-safe (
                                                                      cond (
                                                                        (
                                                                          string? a
                                                                        )
                                                                         (
                                                                          _substring a k (
                                                                            + k 1
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        (
                                                                          hash-table? a
                                                                        )
                                                                         (
                                                                          hash-table-ref a k
                                                                        )
                                                                      )
                                                                       (
                                                                        else (
                                                                          list-ref-safe a k
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      - m 1
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                               5
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
                                        ret12 res
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
        test_solver
      )
       (
        call/cc (
          lambda (
            ret27
          )
           (
            let (
              (
                a (
                  _list (
                    _list 1.0 2.0 3.0
                  )
                   (
                    _list 4.0 5.0 6.0
                  )
                )
              )
            )
             (
              begin (
                let (
                  (
                    r1 (
                      solve_simultaneous a
                    )
                  )
                )
                 (
                  begin (
                    if (
                      not (
                        and (
                          and (
                            equal? (
                              _len r1
                            )
                             2
                          )
                           (
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
                             (
                              - 0.0 1.0
                            )
                          )
                        )
                         (
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
                           2.0
                        )
                      )
                    )
                     (
                      begin (
                        panic "test1 failed"
                      )
                    )
                     '(
                      
                    )
                  )
                   (
                    let (
                      (
                        b (
                          _list (
                            _list 0.0 (
                              - 0.0 3.0
                            )
                             1.0 7.0
                          )
                           (
                            _list 3.0 2.0 (
                              - 0.0 1.0
                            )
                             11.0
                          )
                           (
                            _list 5.0 1.0 (
                              - 0.0 2.0
                            )
                             12.0
                          )
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            r2 (
                              solve_simultaneous b
                            )
                          )
                        )
                         (
                          begin (
                            if (
                              not (
                                and (
                                  and (
                                    and (
                                      equal? (
                                        _len r2
                                      )
                                       3
                                    )
                                     (
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
                                       6.4
                                    )
                                  )
                                   (
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
                                     1.2
                                  )
                                )
                                 (
                                  equal? (
                                    cond (
                                      (
                                        string? r2
                                      )
                                       (
                                        _substring r2 2 (
                                          + 2 1
                                        )
                                      )
                                    )
                                     (
                                      (
                                        hash-table? r2
                                      )
                                       (
                                        hash-table-ref r2 2
                                      )
                                    )
                                     (
                                      else (
                                        list-ref-safe r2 2
                                      )
                                    )
                                  )
                                   10.6
                                )
                              )
                            )
                             (
                              begin (
                                panic "test2 failed"
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
            ret28
          )
           (
            begin (
              test_solver
            )
             (
              let (
                (
                  eq (
                    _list (
                      _list 2.0 1.0 1.0 1.0 1.0 4.0
                    )
                     (
                      _list 1.0 2.0 1.0 1.0 1.0 5.0
                    )
                     (
                      _list 1.0 1.0 2.0 1.0 1.0 6.0
                    )
                     (
                      _list 1.0 1.0 1.0 2.0 1.0 7.0
                    )
                     (
                      _list 1.0 1.0 1.0 1.0 2.0 8.0
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
                          solve_simultaneous eq
                        )
                      )
                    )
                     (
                      to-str-space (
                        solve_simultaneous eq
                      )
                    )
                     (
                      to-str (
                        to-str-space (
                          solve_simultaneous eq
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
                          solve_simultaneous (
                            _list (
                              _list 4.0 2.0
                            )
                          )
                        )
                      )
                    )
                     (
                      to-str-space (
                        solve_simultaneous (
                          _list (
                            _list 4.0 2.0
                          )
                        )
                      )
                    )
                     (
                      to-str (
                        to-str-space (
                          solve_simultaneous (
                            _list (
                              _list 4.0 2.0
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
          end30 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur31 (
              quotient (
                * (
                  - end30 start29
                )
                 1000000
              )
               jps32
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur31
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
