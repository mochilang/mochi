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
      start54 (
        current-jiffy
      )
    )
     (
      jps57 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        expApprox x
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            begin (
              if (
                < x 0.0
              )
               (
                begin (
                  ret1 (
                    _div 1.0 (
                      expApprox (
                        - x
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
              if (
                > x 1.0
              )
               (
                begin (
                  let (
                    (
                      half (
                        expApprox (
                          _div x 2.0
                        )
                      )
                    )
                  )
                   (
                    begin (
                      ret1 (
                        * half half
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
              let (
                (
                  sum 1.0
                )
              )
               (
                begin (
                  let (
                    (
                      term 1.0
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          n 1
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
                                        < n 20
                                      )
                                       (
                                        begin (
                                          set! term (
                                            _div (
                                              * term x
                                            )
                                             (
                                              + 0.0 n
                                            )
                                          )
                                        )
                                         (
                                          set! sum (
                                            + sum term
                                          )
                                        )
                                         (
                                          set! n (
                                            + n 1
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
                          ret1 sum
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
        transpose mat
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            let (
              (
                rows (
                  _len mat
                )
              )
            )
             (
              begin (
                let (
                  (
                    cols (
                      _len (
                        list-ref mat 0
                      )
                    )
                  )
                )
                 (
                  begin (
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
                                          < i cols
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
                                                                  < j rows
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! row (
                                                                      append row (
                                                                        _list (
                                                                          cond (
                                                                            (
                                                                              string? (
                                                                                list-ref mat j
                                                                              )
                                                                            )
                                                                             (
                                                                              _substring (
                                                                                list-ref mat j
                                                                              )
                                                                               i (
                                                                                + i 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            (
                                                                              hash-table? (
                                                                                list-ref mat j
                                                                              )
                                                                            )
                                                                             (
                                                                              hash-table-ref (
                                                                                list-ref mat j
                                                                              )
                                                                               i
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref (
                                                                                list-ref mat j
                                                                              )
                                                                               i
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
                                                                    loop7
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
                                                          loop7
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
                            ret4 res
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
        matMul a b
      )
       (
        call/cc (
          lambda (
            ret9
          )
           (
            let (
              (
                a_rows (
                  _len a
                )
              )
            )
             (
              begin (
                let (
                  (
                    a_cols (
                      _len (
                        list-ref a 0
                      )
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        b_cols (
                          _len (
                            list-ref b 0
                          )
                        )
                      )
                    )
                     (
                      begin (
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
                                              < i a_rows
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
                                                                      < j b_cols
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
                                                                                              < k a_cols
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                set! sum (
                                                                                                  _add sum (
                                                                                                    * (
                                                                                                      cond (
                                                                                                        (
                                                                                                          string? (
                                                                                                            list-ref a i
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          _substring (
                                                                                                            list-ref a i
                                                                                                          )
                                                                                                           k (
                                                                                                            + k 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? (
                                                                                                            list-ref a i
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref (
                                                                                                            list-ref a i
                                                                                                          )
                                                                                                           k
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref (
                                                                                                            list-ref a i
                                                                                                          )
                                                                                                           k
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      cond (
                                                                                                        (
                                                                                                          string? (
                                                                                                            list-ref b k
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          _substring (
                                                                                                            list-ref b k
                                                                                                          )
                                                                                                           j (
                                                                                                            + j 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? (
                                                                                                            list-ref b k
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref (
                                                                                                            list-ref b k
                                                                                                          )
                                                                                                           j
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref (
                                                                                                            list-ref b k
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
                                ret9 res
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
        matInv mat
      )
       (
        call/cc (
          lambda (
            ret16
          )
           (
            let (
              (
                n (
                  _len mat
                )
              )
            )
             (
              begin (
                let (
                  (
                    aug (
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
                                                              < j n
                                                            )
                                                             (
                                                              begin (
                                                                set! row (
                                                                  append row (
                                                                    _list (
                                                                      cond (
                                                                        (
                                                                          string? (
                                                                            list-ref mat i
                                                                          )
                                                                        )
                                                                         (
                                                                          _substring (
                                                                            list-ref mat i
                                                                          )
                                                                           j (
                                                                            + j 1
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        (
                                                                          hash-table? (
                                                                            list-ref mat i
                                                                          )
                                                                        )
                                                                         (
                                                                          hash-table-ref (
                                                                            list-ref mat i
                                                                          )
                                                                           j
                                                                        )
                                                                      )
                                                                       (
                                                                        else (
                                                                          list-ref (
                                                                            list-ref mat i
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
                                                set! j 0
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
                                                set! aug (
                                                  append aug (
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
                        let (
                          (
                            col 0
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
                                          < col n
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                pivot (
                                                  cond (
                                                    (
                                                      string? (
                                                        list-ref aug col
                                                      )
                                                    )
                                                     (
                                                      _substring (
                                                        list-ref aug col
                                                      )
                                                       col (
                                                        + col 1
                                                      )
                                                    )
                                                  )
                                                   (
                                                    (
                                                      hash-table? (
                                                        list-ref aug col
                                                      )
                                                    )
                                                     (
                                                      hash-table-ref (
                                                        list-ref aug col
                                                      )
                                                       col
                                                    )
                                                  )
                                                   (
                                                    else (
                                                      list-ref (
                                                        list-ref aug col
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
                                                  equal? pivot 0.0
                                                )
                                                 (
                                                  begin (
                                                    panic "Matrix is singular"
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
                                                    j 0
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
                                                                  < j (
                                                                    * 2 n
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    list-set! (
                                                                      list-ref aug col
                                                                    )
                                                                     j (
                                                                      _div (
                                                                        cond (
                                                                          (
                                                                            string? (
                                                                              list-ref aug col
                                                                            )
                                                                          )
                                                                           (
                                                                            _substring (
                                                                              list-ref aug col
                                                                            )
                                                                             j (
                                                                              + j 1
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          (
                                                                            hash-table? (
                                                                              list-ref aug col
                                                                            )
                                                                          )
                                                                           (
                                                                            hash-table-ref (
                                                                              list-ref aug col
                                                                            )
                                                                             j
                                                                          )
                                                                        )
                                                                         (
                                                                          else (
                                                                            list-ref (
                                                                              list-ref aug col
                                                                            )
                                                                             j
                                                                          )
                                                                        )
                                                                      )
                                                                       pivot
                                                                    )
                                                                  )
                                                                   (
                                                                    set! j (
                                                                      + j 1
                                                                    )
                                                                  )
                                                                   (
                                                                    loop25
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
                                                          loop25
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
                                                                      < r n
                                                                    )
                                                                     (
                                                                      begin (
                                                                        if (
                                                                          not (
                                                                            equal? r col
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
                                                                                        list-ref aug r
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref aug r
                                                                                      )
                                                                                       col (
                                                                                        + col 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref aug r
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref aug r
                                                                                      )
                                                                                       col
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref aug r
                                                                                      )
                                                                                       col
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                set! j 0
                                                                              )
                                                                               (
                                                                                call/cc (
                                                                                  lambda (
                                                                                    break30
                                                                                  )
                                                                                   (
                                                                                    letrec (
                                                                                      (
                                                                                        loop29 (
                                                                                          lambda (
                                                                                            
                                                                                          )
                                                                                           (
                                                                                            if (
                                                                                              < j (
                                                                                                * 2 n
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                list-set! (
                                                                                                  list-ref aug r
                                                                                                )
                                                                                                 j (
                                                                                                  - (
                                                                                                    cond (
                                                                                                      (
                                                                                                        string? (
                                                                                                          list-ref aug r
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        _substring (
                                                                                                          list-ref aug r
                                                                                                        )
                                                                                                         j (
                                                                                                          + j 1
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      (
                                                                                                        hash-table? (
                                                                                                          list-ref aug r
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        hash-table-ref (
                                                                                                          list-ref aug r
                                                                                                        )
                                                                                                         j
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      else (
                                                                                                        list-ref (
                                                                                                          list-ref aug r
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
                                                                                                            list-ref aug col
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          _substring (
                                                                                                            list-ref aug col
                                                                                                          )
                                                                                                           j (
                                                                                                            + j 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? (
                                                                                                            list-ref aug col
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref (
                                                                                                            list-ref aug col
                                                                                                          )
                                                                                                           j
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref (
                                                                                                            list-ref aug col
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
                                                                                                loop29
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
                                                                                      loop29
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
                                                                        set! r (
                                                                          + r 1
                                                                        )
                                                                      )
                                                                       (
                                                                        loop27
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
                                                              loop27
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
                                                )
                                              )
                                            )
                                          )
                                           (
                                            loop23
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
                                  loop23
                                )
                              )
                            )
                          )
                           (
                            let (
                              (
                                inv (
                                  _list
                                )
                              )
                            )
                             (
                              begin (
                                set! i 0
                              )
                               (
                                call/cc (
                                  lambda (
                                    break32
                                  )
                                   (
                                    letrec (
                                      (
                                        loop31 (
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
                                                            break34
                                                          )
                                                           (
                                                            letrec (
                                                              (
                                                                loop33 (
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
                                                                            _list (
                                                                              cond (
                                                                                (
                                                                                  string? (
                                                                                    list-ref aug i
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  _substring (
                                                                                    list-ref aug i
                                                                                  )
                                                                                   (
                                                                                    + j n
                                                                                  )
                                                                                   (
                                                                                    + (
                                                                                      + j n
                                                                                    )
                                                                                     1
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                (
                                                                                  hash-table? (
                                                                                    list-ref aug i
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  hash-table-ref (
                                                                                    list-ref aug i
                                                                                  )
                                                                                   (
                                                                                    + j n
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                else (
                                                                                  list-ref (
                                                                                    list-ref aug i
                                                                                  )
                                                                                   (
                                                                                    + j n
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
                                                                        loop33
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
                                                              loop33
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        set! inv (
                                                          append inv (
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
                                                loop31
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
                                      loop31
                                    )
                                  )
                                )
                              )
                               (
                                ret16 inv
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
        weight_matrix point x_train tau
      )
       (
        call/cc (
          lambda (
            ret35
          )
           (
            let (
              (
                m (
                  _len x_train
                )
              )
            )
             (
              begin (
                let (
                  (
                    weights (
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
                            break37
                          )
                           (
                            letrec (
                              (
                                loop36 (
                                  lambda (
                                    
                                  )
                                   (
                                    if (
                                      < i m
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
                                                    break39
                                                  )
                                                   (
                                                    letrec (
                                                      (
                                                        loop38 (
                                                          lambda (
                                                            
                                                          )
                                                           (
                                                            if (
                                                              < j m
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
                                                                loop38
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
                                                      loop38
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                set! weights (
                                                  append weights (
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
                                        loop36
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
                              loop36
                            )
                          )
                        )
                      )
                       (
                        let (
                          (
                            j 0
                          )
                        )
                         (
                          begin (
                            call/cc (
                              lambda (
                                break41
                              )
                               (
                                letrec (
                                  (
                                    loop40 (
                                      lambda (
                                        
                                      )
                                       (
                                        if (
                                          < j m
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                diff_sq 0.0
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
                                                        break43
                                                      )
                                                       (
                                                        letrec (
                                                          (
                                                            loop42 (
                                                              lambda (
                                                                
                                                              )
                                                               (
                                                                if (
                                                                  < k (
                                                                    _len point
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        diff (
                                                                          - (
                                                                            list-ref point k
                                                                          )
                                                                           (
                                                                            cond (
                                                                              (
                                                                                string? (
                                                                                  list-ref x_train j
                                                                                )
                                                                              )
                                                                               (
                                                                                _substring (
                                                                                  list-ref x_train j
                                                                                )
                                                                                 k (
                                                                                  + k 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? (
                                                                                  list-ref x_train j
                                                                                )
                                                                              )
                                                                               (
                                                                                hash-table-ref (
                                                                                  list-ref x_train j
                                                                                )
                                                                                 k
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref (
                                                                                  list-ref x_train j
                                                                                )
                                                                                 k
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        set! diff_sq (
                                                                          _add diff_sq (
                                                                            * diff diff
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        set! k (
                                                                          + k 1
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    loop42
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
                                                          loop42
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    list-set! (
                                                      list-ref weights j
                                                    )
                                                     j (
                                                      expApprox (
                                                        _div (
                                                          - diff_sq
                                                        )
                                                         (
                                                          * (
                                                            * 2.0 tau
                                                          )
                                                           tau
                                                        )
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
                                            loop40
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
                                  loop40
                                )
                              )
                            )
                          )
                           (
                            ret35 weights
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
        local_weight point x_train y_train tau
      )
       (
        call/cc (
          lambda (
            ret44
          )
           (
            let (
              (
                w (
                  weight_matrix point x_train tau
                )
              )
            )
             (
              begin (
                let (
                  (
                    x_t (
                      transpose x_train
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        x_t_w (
                          matMul x_t w
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            x_t_w_x (
                              matMul x_t_w x_train
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                inv_part (
                                  matInv x_t_w_x
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    y_col (
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
                                            break46
                                          )
                                           (
                                            letrec (
                                              (
                                                loop45 (
                                                  lambda (
                                                    
                                                  )
                                                   (
                                                    if (
                                                      < i (
                                                        _len y_train
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        set! y_col (
                                                          append y_col (
                                                            _list (
                                                              _list (
                                                                list-ref y_train i
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
                                                        loop45
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
                                              loop45
                                            )
                                          )
                                        )
                                      )
                                       (
                                        let (
                                          (
                                            x_t_w_y (
                                              matMul x_t_w y_col
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            ret44 (
                                              matMul inv_part x_t_w_y
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
        local_weight_regression x_train y_train tau
      )
       (
        call/cc (
          lambda (
            ret47
          )
           (
            let (
              (
                m (
                  _len x_train
                )
              )
            )
             (
              begin (
                let (
                  (
                    preds (
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
                            break49
                          )
                           (
                            letrec (
                              (
                                loop48 (
                                  lambda (
                                    
                                  )
                                   (
                                    if (
                                      < i m
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            theta (
                                              local_weight (
                                                list-ref x_train i
                                              )
                                               x_train y_train tau
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                weights_vec (
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
                                                        break51
                                                      )
                                                       (
                                                        letrec (
                                                          (
                                                            loop50 (
                                                              lambda (
                                                                
                                                              )
                                                               (
                                                                if (
                                                                  < k (
                                                                    _len theta
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! weights_vec (
                                                                      append weights_vec (
                                                                        _list (
                                                                          cond (
                                                                            (
                                                                              string? (
                                                                                cond (
                                                                                  (
                                                                                    string? theta
                                                                                  )
                                                                                   (
                                                                                    _substring theta k (
                                                                                      + k 1
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  (
                                                                                    hash-table? theta
                                                                                  )
                                                                                   (
                                                                                    hash-table-ref theta k
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  else (
                                                                                    list-ref theta k
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              _substring (
                                                                                cond (
                                                                                  (
                                                                                    string? theta
                                                                                  )
                                                                                   (
                                                                                    _substring theta k (
                                                                                      + k 1
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  (
                                                                                    hash-table? theta
                                                                                  )
                                                                                   (
                                                                                    hash-table-ref theta k
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  else (
                                                                                    list-ref theta k
                                                                                  )
                                                                                )
                                                                              )
                                                                               0 (
                                                                                + 0 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            (
                                                                              hash-table? (
                                                                                cond (
                                                                                  (
                                                                                    string? theta
                                                                                  )
                                                                                   (
                                                                                    _substring theta k (
                                                                                      + k 1
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  (
                                                                                    hash-table? theta
                                                                                  )
                                                                                   (
                                                                                    hash-table-ref theta k
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  else (
                                                                                    list-ref theta k
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              hash-table-ref (
                                                                                cond (
                                                                                  (
                                                                                    string? theta
                                                                                  )
                                                                                   (
                                                                                    _substring theta k (
                                                                                      + k 1
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  (
                                                                                    hash-table? theta
                                                                                  )
                                                                                   (
                                                                                    hash-table-ref theta k
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  else (
                                                                                    list-ref theta k
                                                                                  )
                                                                                )
                                                                              )
                                                                               0
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref (
                                                                                cond (
                                                                                  (
                                                                                    string? theta
                                                                                  )
                                                                                   (
                                                                                    _substring theta k (
                                                                                      + k 1
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  (
                                                                                    hash-table? theta
                                                                                  )
                                                                                   (
                                                                                    hash-table-ref theta k
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  else (
                                                                                    list-ref theta k
                                                                                  )
                                                                                )
                                                                              )
                                                                               0
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
                                                                    loop50
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
                                                          loop50
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    let (
                                                      (
                                                        pred 0.0
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
                                                                break53
                                                              )
                                                               (
                                                                letrec (
                                                                  (
                                                                    loop52 (
                                                                      lambda (
                                                                        
                                                                      )
                                                                       (
                                                                        if (
                                                                          < j (
                                                                            _len (
                                                                              list-ref x_train i
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            set! pred (
                                                                              _add pred (
                                                                                * (
                                                                                  cond (
                                                                                    (
                                                                                      string? (
                                                                                        list-ref x_train i
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref x_train i
                                                                                      )
                                                                                       j (
                                                                                        + j 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref x_train i
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref x_train i
                                                                                      )
                                                                                       j
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref x_train i
                                                                                      )
                                                                                       j
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  list-ref weights_vec j
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
                                                                            loop52
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
                                                                  loop52
                                                                )
                                                              )
                                                            )
                                                          )
                                                           (
                                                            set! preds (
                                                              append preds (
                                                                _list pred
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
                                            )
                                          )
                                        )
                                      )
                                       (
                                        loop48
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
                              loop48
                            )
                          )
                        )
                      )
                       (
                        ret47 preds
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
          x_train (
            _list (
              _list 16.99 10.34
            )
             (
              _list 21.01 23.68
            )
             (
              _list 24.59 25.69
            )
          )
        )
      )
       (
        begin (
          let (
            (
              y_train (
                _list 1.01 1.66 3.5
              )
            )
          )
           (
            begin (
              let (
                (
                  preds (
                    local_weight_regression x_train y_train 0.6
                  )
                )
              )
               (
                begin (
                  _display preds
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
      let (
        (
          end55 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur56 (
              quotient (
                * (
                  - end55 start54
                )
                 1000000
              )
               jps57
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur56
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
