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
      start86 (
        current-jiffy
      )
    )
     (
      jps89 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      let (
        (
          seed 1
        )
      )
       (
        begin (
          define (
            random
          )
           (
            call/cc (
              lambda (
                ret1
              )
               (
                begin (
                  set! seed (
                    _mod (
                      + (
                        * seed 13
                      )
                       7
                    )
                     100
                  )
                )
                 (
                  ret1 (
                    _div (
                      + 0.0 seed
                    )
                     100.0
                  )
                )
              )
            )
          )
        )
         (
          define (
            sigmoid x
          )
           (
            call/cc (
              lambda (
                ret2
              )
               (
                ret2 (
                  _div 1.0 (
                    + 1.0 (
                      exp (
                        - x
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
            to_float x
          )
           (
            call/cc (
              lambda (
                ret3
              )
               (
                ret3 (
                  * x 1.0
                )
              )
            )
          )
        )
         (
          define (
            exp x
          )
           (
            call/cc (
              lambda (
                ret4
              )
               (
                let (
                  (
                    term 1.0
                  )
                )
                 (
                  begin (
                    let (
                      (
                        sum 1.0
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
                                          < n 20
                                        )
                                         (
                                          begin (
                                            set! term (
                                              _div (
                                                * term x
                                              )
                                               (
                                                to_float n
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
                            ret4 sum
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
            convolve data kernel step bias
          )
           (
            call/cc (
              lambda (
                ret7
              )
               (
                let (
                  (
                    size_data (
                      _len data
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        size_kernel (
                          _len kernel
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            out (
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
                                              <= i (
                                                - size_data size_kernel
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
                                                                      <= j (
                                                                        - size_data size_kernel
                                                                      )
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
                                                                                a 0
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
                                                                                              < a size_kernel
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                let (
                                                                                                  (
                                                                                                    b 0
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
                                                                                                                  < b size_kernel
                                                                                                                )
                                                                                                                 (
                                                                                                                  begin (
                                                                                                                    set! sum (
                                                                                                                      _add sum (
                                                                                                                        * (
                                                                                                                          cond (
                                                                                                                            (
                                                                                                                              string? (
                                                                                                                                list-ref-safe data (
                                                                                                                                  + i a
                                                                                                                                )
                                                                                                                              )
                                                                                                                            )
                                                                                                                             (
                                                                                                                              _substring (
                                                                                                                                list-ref-safe data (
                                                                                                                                  + i a
                                                                                                                                )
                                                                                                                              )
                                                                                                                               (
                                                                                                                                + j b
                                                                                                                              )
                                                                                                                               (
                                                                                                                                + (
                                                                                                                                  + j b
                                                                                                                                )
                                                                                                                                 1
                                                                                                                              )
                                                                                                                            )
                                                                                                                          )
                                                                                                                           (
                                                                                                                            (
                                                                                                                              hash-table? (
                                                                                                                                list-ref-safe data (
                                                                                                                                  + i a
                                                                                                                                )
                                                                                                                              )
                                                                                                                            )
                                                                                                                             (
                                                                                                                              hash-table-ref (
                                                                                                                                list-ref-safe data (
                                                                                                                                  + i a
                                                                                                                                )
                                                                                                                              )
                                                                                                                               (
                                                                                                                                + j b
                                                                                                                              )
                                                                                                                            )
                                                                                                                          )
                                                                                                                           (
                                                                                                                            else (
                                                                                                                              list-ref-safe (
                                                                                                                                list-ref-safe data (
                                                                                                                                  + i a
                                                                                                                                )
                                                                                                                              )
                                                                                                                               (
                                                                                                                                + j b
                                                                                                                              )
                                                                                                                            )
                                                                                                                          )
                                                                                                                        )
                                                                                                                         (
                                                                                                                          cond (
                                                                                                                            (
                                                                                                                              string? (
                                                                                                                                list-ref-safe kernel a
                                                                                                                              )
                                                                                                                            )
                                                                                                                             (
                                                                                                                              _substring (
                                                                                                                                list-ref-safe kernel a
                                                                                                                              )
                                                                                                                               b (
                                                                                                                                + b 1
                                                                                                                              )
                                                                                                                            )
                                                                                                                          )
                                                                                                                           (
                                                                                                                            (
                                                                                                                              hash-table? (
                                                                                                                                list-ref-safe kernel a
                                                                                                                              )
                                                                                                                            )
                                                                                                                             (
                                                                                                                              hash-table-ref (
                                                                                                                                list-ref-safe kernel a
                                                                                                                              )
                                                                                                                               b
                                                                                                                            )
                                                                                                                          )
                                                                                                                           (
                                                                                                                            else (
                                                                                                                              list-ref-safe (
                                                                                                                                list-ref-safe kernel a
                                                                                                                              )
                                                                                                                               b
                                                                                                                            )
                                                                                                                          )
                                                                                                                        )
                                                                                                                      )
                                                                                                                    )
                                                                                                                  )
                                                                                                                   (
                                                                                                                    set! b (
                                                                                                                      + b 1
                                                                                                                    )
                                                                                                                  )
                                                                                                                   (
                                                                                                                    loop14
                                                                                                                  )
                                                                                                                )
                                                                                                                 '(
                                                                                                                  
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
                                                                                                    set! a (
                                                                                                      + a 1
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                loop12
                                                                                              )
                                                                                            )
                                                                                             '(
                                                                                              
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
                                                                                set! row (
                                                                                  append row (
                                                                                    _list (
                                                                                      sigmoid (
                                                                                        - sum bias
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                set! j (
                                                                                  + j step
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
                                                        set! out (
                                                          append out (
                                                            _list row
                                                          )
                                                        )
                                                      )
                                                       (
                                                        set! i (
                                                          + i step
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
                                ret7 out
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
            average_pool map size
          )
           (
            call/cc (
              lambda (
                ret16
              )
               (
                let (
                  (
                    out (
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
                                      < i (
                                        _len map
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
                                                              < j (
                                                                _len (
                                                                  list-ref-safe map i
                                                                )
                                                              )
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
                                                                        a 0
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
                                                                                      < a size
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        let (
                                                                                          (
                                                                                            b 0
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
                                                                                                          < b size
                                                                                                        )
                                                                                                         (
                                                                                                          begin (
                                                                                                            set! sum (
                                                                                                              + sum (
                                                                                                                cond (
                                                                                                                  (
                                                                                                                    string? (
                                                                                                                      list-ref-safe map (
                                                                                                                        + i a
                                                                                                                      )
                                                                                                                    )
                                                                                                                  )
                                                                                                                   (
                                                                                                                    _substring (
                                                                                                                      list-ref-safe map (
                                                                                                                        + i a
                                                                                                                      )
                                                                                                                    )
                                                                                                                     (
                                                                                                                      + j b
                                                                                                                    )
                                                                                                                     (
                                                                                                                      + (
                                                                                                                        + j b
                                                                                                                      )
                                                                                                                       1
                                                                                                                    )
                                                                                                                  )
                                                                                                                )
                                                                                                                 (
                                                                                                                  (
                                                                                                                    hash-table? (
                                                                                                                      list-ref-safe map (
                                                                                                                        + i a
                                                                                                                      )
                                                                                                                    )
                                                                                                                  )
                                                                                                                   (
                                                                                                                    hash-table-ref (
                                                                                                                      list-ref-safe map (
                                                                                                                        + i a
                                                                                                                      )
                                                                                                                    )
                                                                                                                     (
                                                                                                                      + j b
                                                                                                                    )
                                                                                                                  )
                                                                                                                )
                                                                                                                 (
                                                                                                                  else (
                                                                                                                    list-ref-safe (
                                                                                                                      list-ref-safe map (
                                                                                                                        + i a
                                                                                                                      )
                                                                                                                    )
                                                                                                                     (
                                                                                                                      + j b
                                                                                                                    )
                                                                                                                  )
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                           (
                                                                                                            set! b (
                                                                                                              + b 1
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
                                                                                           (
                                                                                            set! a (
                                                                                              + a 1
                                                                                            )
                                                                                          )
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
                                                                          append row (
                                                                            _list (
                                                                              _div sum (
                                                                                + 0.0 (
                                                                                  * size size
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        set! j (
                                                                          + j size
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
                                                set! out (
                                                  append out (
                                                    _list row
                                                  )
                                                )
                                              )
                                               (
                                                set! i (
                                                  + i size
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
                        ret16 out
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
            flatten maps
          )
           (
            call/cc (
              lambda (
                ret25
              )
               (
                let (
                  (
                    out (
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
                            break27
                          )
                           (
                            letrec (
                              (
                                loop26 (
                                  lambda (
                                    
                                  )
                                   (
                                    if (
                                      < i (
                                        _len maps
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
                                                break29
                                              )
                                               (
                                                letrec (
                                                  (
                                                    loop28 (
                                                      lambda (
                                                        
                                                      )
                                                       (
                                                        if (
                                                          < j (
                                                            _len (
                                                              list-ref-safe maps i
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
                                                                              < k (
                                                                                _len (
                                                                                  cond (
                                                                                    (
                                                                                      string? (
                                                                                        list-ref-safe maps i
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref-safe maps i
                                                                                      )
                                                                                       j (
                                                                                        + j 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref-safe maps i
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref-safe maps i
                                                                                      )
                                                                                       j
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref-safe (
                                                                                        list-ref-safe maps i
                                                                                      )
                                                                                       j
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                set! out (
                                                                                  append out (
                                                                                    _list (
                                                                                      cond (
                                                                                        (
                                                                                          string? (
                                                                                            cond (
                                                                                              (
                                                                                                string? (
                                                                                                  list-ref-safe maps i
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                _substring (
                                                                                                  list-ref-safe maps i
                                                                                                )
                                                                                                 j (
                                                                                                  + j 1
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              (
                                                                                                hash-table? (
                                                                                                  list-ref-safe maps i
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                hash-table-ref (
                                                                                                  list-ref-safe maps i
                                                                                                )
                                                                                                 j
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              else (
                                                                                                list-ref-safe (
                                                                                                  list-ref-safe maps i
                                                                                                )
                                                                                                 j
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          _substring (
                                                                                            cond (
                                                                                              (
                                                                                                string? (
                                                                                                  list-ref-safe maps i
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                _substring (
                                                                                                  list-ref-safe maps i
                                                                                                )
                                                                                                 j (
                                                                                                  + j 1
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              (
                                                                                                hash-table? (
                                                                                                  list-ref-safe maps i
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                hash-table-ref (
                                                                                                  list-ref-safe maps i
                                                                                                )
                                                                                                 j
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              else (
                                                                                                list-ref-safe (
                                                                                                  list-ref-safe maps i
                                                                                                )
                                                                                                 j
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           k (
                                                                                            + k 1
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        (
                                                                                          hash-table? (
                                                                                            cond (
                                                                                              (
                                                                                                string? (
                                                                                                  list-ref-safe maps i
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                _substring (
                                                                                                  list-ref-safe maps i
                                                                                                )
                                                                                                 j (
                                                                                                  + j 1
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              (
                                                                                                hash-table? (
                                                                                                  list-ref-safe maps i
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                hash-table-ref (
                                                                                                  list-ref-safe maps i
                                                                                                )
                                                                                                 j
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              else (
                                                                                                list-ref-safe (
                                                                                                  list-ref-safe maps i
                                                                                                )
                                                                                                 j
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          hash-table-ref (
                                                                                            cond (
                                                                                              (
                                                                                                string? (
                                                                                                  list-ref-safe maps i
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                _substring (
                                                                                                  list-ref-safe maps i
                                                                                                )
                                                                                                 j (
                                                                                                  + j 1
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              (
                                                                                                hash-table? (
                                                                                                  list-ref-safe maps i
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                hash-table-ref (
                                                                                                  list-ref-safe maps i
                                                                                                )
                                                                                                 j
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              else (
                                                                                                list-ref-safe (
                                                                                                  list-ref-safe maps i
                                                                                                )
                                                                                                 j
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           k
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        else (
                                                                                          list-ref-safe (
                                                                                            cond (
                                                                                              (
                                                                                                string? (
                                                                                                  list-ref-safe maps i
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                _substring (
                                                                                                  list-ref-safe maps i
                                                                                                )
                                                                                                 j (
                                                                                                  + j 1
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              (
                                                                                                hash-table? (
                                                                                                  list-ref-safe maps i
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                hash-table-ref (
                                                                                                  list-ref-safe maps i
                                                                                                )
                                                                                                 j
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              else (
                                                                                                list-ref-safe (
                                                                                                  list-ref-safe maps i
                                                                                                )
                                                                                                 j
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           k
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
                                                                set! j (
                                                                  + j 1
                                                                )
                                                              )
                                                            )
                                                          )
                                                           (
                                                            loop28
                                                          )
                                                        )
                                                         '(
                                                          
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  loop28
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
                                       (
                                        loop26
                                      )
                                    )
                                     '(
                                      
                                    )
                                  )
                                )
                              )
                            )
                             (
                              loop26
                            )
                          )
                        )
                      )
                       (
                        ret25 out
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
            vec_mul_mat v m
          )
           (
            call/cc (
              lambda (
                ret32
              )
               (
                let (
                  (
                    cols (
                      _len (
                        list-ref-safe m 0
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
                                                    i 0
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    call/cc (
                                                      lambda (
                                                        break36
                                                      )
                                                       (
                                                        letrec (
                                                          (
                                                            loop35 (
                                                              lambda (
                                                                
                                                              )
                                                               (
                                                                if (
                                                                  < i (
                                                                    _len v
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! sum (
                                                                      _add sum (
                                                                        * (
                                                                          list-ref-safe v i
                                                                        )
                                                                         (
                                                                          cond (
                                                                            (
                                                                              string? (
                                                                                list-ref-safe m i
                                                                              )
                                                                            )
                                                                             (
                                                                              _substring (
                                                                                list-ref-safe m i
                                                                              )
                                                                               j (
                                                                                + j 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            (
                                                                              hash-table? (
                                                                                list-ref-safe m i
                                                                              )
                                                                            )
                                                                             (
                                                                              hash-table-ref (
                                                                                list-ref-safe m i
                                                                              )
                                                                               j
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref-safe (
                                                                                list-ref-safe m i
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
                                                                    loop35
                                                                  )
                                                                )
                                                                 '(
                                                                  
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                         (
                                                          loop35
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    set! res (
                                                      append res (
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
                                            loop33
                                          )
                                        )
                                         '(
                                          
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
                            ret32 res
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
            matT_vec_mul m v
          )
           (
            call/cc (
              lambda (
                ret37
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
                                      < i (
                                        _len m
                                      )
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
                                                              < j (
                                                                _len (
                                                                  list-ref-safe m i
                                                                )
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
                                                                            list-ref-safe m i
                                                                          )
                                                                        )
                                                                         (
                                                                          _substring (
                                                                            list-ref-safe m i
                                                                          )
                                                                           j (
                                                                            + j 1
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        (
                                                                          hash-table? (
                                                                            list-ref-safe m i
                                                                          )
                                                                        )
                                                                         (
                                                                          hash-table-ref (
                                                                            list-ref-safe m i
                                                                          )
                                                                           j
                                                                        )
                                                                      )
                                                                       (
                                                                        else (
                                                                          list-ref-safe (
                                                                            list-ref-safe m i
                                                                          )
                                                                           j
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      list-ref-safe v j
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
                                                                loop40
                                                              )
                                                            )
                                                             '(
                                                              
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
                                                set! res (
                                                  append res (
                                                    _list sum
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
                                        loop38
                                      )
                                    )
                                     '(
                                      
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
                        ret37 res
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
            vec_add a b
          )
           (
            call/cc (
              lambda (
                ret42
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
                            break44
                          )
                           (
                            letrec (
                              (
                                loop43 (
                                  lambda (
                                    
                                  )
                                   (
                                    if (
                                      < i (
                                        _len a
                                      )
                                    )
                                     (
                                      begin (
                                        set! res (
                                          append res (
                                            _list (
                                              + (
                                                list-ref-safe a i
                                              )
                                               (
                                                list-ref-safe b i
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
                                        loop43
                                      )
                                    )
                                     '(
                                      
                                    )
                                  )
                                )
                              )
                            )
                             (
                              loop43
                            )
                          )
                        )
                      )
                       (
                        ret42 res
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
            vec_sub a b
          )
           (
            call/cc (
              lambda (
                ret45
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
                            break47
                          )
                           (
                            letrec (
                              (
                                loop46 (
                                  lambda (
                                    
                                  )
                                   (
                                    if (
                                      < i (
                                        _len a
                                      )
                                    )
                                     (
                                      begin (
                                        set! res (
                                          append res (
                                            _list (
                                              - (
                                                list-ref-safe a i
                                              )
                                               (
                                                list-ref-safe b i
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
                                        loop46
                                      )
                                    )
                                     '(
                                      
                                    )
                                  )
                                )
                              )
                            )
                             (
                              loop46
                            )
                          )
                        )
                      )
                       (
                        ret45 res
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
            vec_mul a b
          )
           (
            call/cc (
              lambda (
                ret48
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
                            break50
                          )
                           (
                            letrec (
                              (
                                loop49 (
                                  lambda (
                                    
                                  )
                                   (
                                    if (
                                      < i (
                                        _len a
                                      )
                                    )
                                     (
                                      begin (
                                        set! res (
                                          append res (
                                            _list (
                                              * (
                                                list-ref-safe a i
                                              )
                                               (
                                                list-ref-safe b i
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
                                        loop49
                                      )
                                    )
                                     '(
                                      
                                    )
                                  )
                                )
                              )
                            )
                             (
                              loop49
                            )
                          )
                        )
                      )
                       (
                        ret48 res
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
            vec_map_sig v
          )
           (
            call/cc (
              lambda (
                ret51
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
                                      < i (
                                        _len v
                                      )
                                    )
                                     (
                                      begin (
                                        set! res (
                                          append res (
                                            _list (
                                              sigmoid (
                                                list-ref-safe v i
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
                                        loop52
                                      )
                                    )
                                     '(
                                      
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
                        ret51 res
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
            new_cnn
          )
           (
            call/cc (
              lambda (
                ret54
              )
               (
                let (
                  (
                    k1 (
                      _list (
                        _list 1.0 0.0
                      )
                       (
                        _list 0.0 1.0
                      )
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        k2 (
                          _list (
                            _list 0.0 1.0
                          )
                           (
                            _list 1.0 0.0
                          )
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            conv_kernels (
                              _list k1 k2
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                conv_bias (
                                  _list 0.0 0.0
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    conv_step 2
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        pool_size 2
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            input_size 2
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                hidden_size 2
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    output_size 2
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        w_hidden (
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
                                                                break56
                                                              )
                                                               (
                                                                letrec (
                                                                  (
                                                                    loop55 (
                                                                      lambda (
                                                                        
                                                                      )
                                                                       (
                                                                        if (
                                                                          < i input_size
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
                                                                                        break58
                                                                                      )
                                                                                       (
                                                                                        letrec (
                                                                                          (
                                                                                            loop57 (
                                                                                              lambda (
                                                                                                
                                                                                              )
                                                                                               (
                                                                                                if (
                                                                                                  < j hidden_size
                                                                                                )
                                                                                                 (
                                                                                                  begin (
                                                                                                    set! row (
                                                                                                      append row (
                                                                                                        _list (
                                                                                                          - (
                                                                                                            random
                                                                                                          )
                                                                                                           0.5
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
                                                                                                    loop57
                                                                                                  )
                                                                                                )
                                                                                                 '(
                                                                                                  
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          loop57
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    set! w_hidden (
                                                                                      append w_hidden (
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
                                                                            loop55
                                                                          )
                                                                        )
                                                                         '(
                                                                          
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  loop55
                                                                )
                                                              )
                                                            )
                                                          )
                                                           (
                                                            let (
                                                              (
                                                                w_out (
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
                                                                    break60
                                                                  )
                                                                   (
                                                                    letrec (
                                                                      (
                                                                        loop59 (
                                                                          lambda (
                                                                            
                                                                          )
                                                                           (
                                                                            if (
                                                                              < i hidden_size
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
                                                                                            break62
                                                                                          )
                                                                                           (
                                                                                            letrec (
                                                                                              (
                                                                                                loop61 (
                                                                                                  lambda (
                                                                                                    
                                                                                                  )
                                                                                                   (
                                                                                                    if (
                                                                                                      < j output_size
                                                                                                    )
                                                                                                     (
                                                                                                      begin (
                                                                                                        set! row (
                                                                                                          append row (
                                                                                                            _list (
                                                                                                              - (
                                                                                                                random
                                                                                                              )
                                                                                                               0.5
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
                                                                                                        loop61
                                                                                                      )
                                                                                                    )
                                                                                                     '(
                                                                                                      
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              loop61
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        set! w_out (
                                                                                          append w_out (
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
                                                                                loop59
                                                                              )
                                                                            )
                                                                             '(
                                                                              
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      loop59
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                let (
                                                                  (
                                                                    b_hidden (
                                                                      _list 0.0 0.0
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        b_out (
                                                                          _list 0.0 0.0
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        ret54 (
                                                                          alist->hash-table (
                                                                            _list (
                                                                              cons "conv_kernels" conv_kernels
                                                                            )
                                                                             (
                                                                              cons "conv_bias" conv_bias
                                                                            )
                                                                             (
                                                                              cons "conv_step" conv_step
                                                                            )
                                                                             (
                                                                              cons "pool_size" pool_size
                                                                            )
                                                                             (
                                                                              cons "w_hidden" w_hidden
                                                                            )
                                                                             (
                                                                              cons "w_out" w_out
                                                                            )
                                                                             (
                                                                              cons "b_hidden" b_hidden
                                                                            )
                                                                             (
                                                                              cons "b_out" b_out
                                                                            )
                                                                             (
                                                                              cons "rate_weight" 0.2
                                                                            )
                                                                             (
                                                                              cons "rate_bias" 0.2
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
            forward cnn data
          )
           (
            call/cc (
              lambda (
                ret63
              )
               (
                let (
                  (
                    maps (
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
                            break65
                          )
                           (
                            letrec (
                              (
                                loop64 (
                                  lambda (
                                    
                                  )
                                   (
                                    if (
                                      < i (
                                        _len (
                                          hash-table-ref cnn "conv_kernels"
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            conv_map (
                                              convolve data (
                                                list-ref-safe (
                                                  hash-table-ref cnn "conv_kernels"
                                                )
                                                 i
                                              )
                                               (
                                                hash-table-ref cnn "conv_step"
                                              )
                                               (
                                                list-ref-safe (
                                                  hash-table-ref cnn "conv_bias"
                                                )
                                                 i
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                pooled (
                                                  average_pool conv_map (
                                                    hash-table-ref cnn "pool_size"
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                set! maps (
                                                  append maps (
                                                    _list pooled
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
                                        loop64
                                      )
                                    )
                                     '(
                                      
                                    )
                                  )
                                )
                              )
                            )
                             (
                              loop64
                            )
                          )
                        )
                      )
                       (
                        let (
                          (
                            flat (
                              flatten maps
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                hidden_net (
                                  vec_add (
                                    vec_mul_mat flat (
                                      hash-table-ref cnn "w_hidden"
                                    )
                                  )
                                   (
                                    hash-table-ref cnn "b_hidden"
                                  )
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    hidden_out (
                                      vec_map_sig hidden_net
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        out_net (
                                          vec_add (
                                            vec_mul_mat hidden_out (
                                              hash-table-ref cnn "w_out"
                                            )
                                          )
                                           (
                                            hash-table-ref cnn "b_out"
                                          )
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            out (
                                              vec_map_sig out_net
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            ret63 out
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
            train cnn samples epochs
          )
           (
            call/cc (
              lambda (
                ret66
              )
               (
                let (
                  (
                    w_out (
                      hash-table-ref cnn "w_out"
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        b_out (
                          hash-table-ref cnn "b_out"
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            w_hidden (
                              hash-table-ref cnn "w_hidden"
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                b_hidden (
                                  hash-table-ref cnn "b_hidden"
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    e 0
                                  )
                                )
                                 (
                                  begin (
                                    call/cc (
                                      lambda (
                                        break68
                                      )
                                       (
                                        letrec (
                                          (
                                            loop67 (
                                              lambda (
                                                
                                              )
                                               (
                                                if (
                                                  < e epochs
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        s 0
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        call/cc (
                                                          lambda (
                                                            break70
                                                          )
                                                           (
                                                            letrec (
                                                              (
                                                                loop69 (
                                                                  lambda (
                                                                    
                                                                  )
                                                                   (
                                                                    if (
                                                                      < s (
                                                                        _len samples
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            data (
                                                                              hash-table-ref (
                                                                                list-ref-safe samples s
                                                                              )
                                                                               "image"
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                target (
                                                                                  hash-table-ref (
                                                                                    list-ref-safe samples s
                                                                                  )
                                                                                   "target"
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                let (
                                                                                  (
                                                                                    maps (
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
                                                                                            break72
                                                                                          )
                                                                                           (
                                                                                            letrec (
                                                                                              (
                                                                                                loop71 (
                                                                                                  lambda (
                                                                                                    
                                                                                                  )
                                                                                                   (
                                                                                                    if (
                                                                                                      < i (
                                                                                                        _len (
                                                                                                          hash-table-ref cnn "conv_kernels"
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      begin (
                                                                                                        let (
                                                                                                          (
                                                                                                            conv_map (
                                                                                                              convolve data (
                                                                                                                list-ref-safe (
                                                                                                                  hash-table-ref cnn "conv_kernels"
                                                                                                                )
                                                                                                                 i
                                                                                                              )
                                                                                                               (
                                                                                                                hash-table-ref cnn "conv_step"
                                                                                                              )
                                                                                                               (
                                                                                                                list-ref-safe (
                                                                                                                  hash-table-ref cnn "conv_bias"
                                                                                                                )
                                                                                                                 i
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          begin (
                                                                                                            let (
                                                                                                              (
                                                                                                                pooled (
                                                                                                                  average_pool conv_map (
                                                                                                                    hash-table-ref cnn "pool_size"
                                                                                                                  )
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              begin (
                                                                                                                set! maps (
                                                                                                                  append maps (
                                                                                                                    _list pooled
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
                                                                                                        loop71
                                                                                                      )
                                                                                                    )
                                                                                                     '(
                                                                                                      
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              loop71
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        let (
                                                                                          (
                                                                                            flat (
                                                                                              flatten maps
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            let (
                                                                                              (
                                                                                                hidden_net (
                                                                                                  vec_add (
                                                                                                    vec_mul_mat flat w_hidden
                                                                                                  )
                                                                                                   b_hidden
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                let (
                                                                                                  (
                                                                                                    hidden_out (
                                                                                                      vec_map_sig hidden_net
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  begin (
                                                                                                    let (
                                                                                                      (
                                                                                                        out_net (
                                                                                                          vec_add (
                                                                                                            vec_mul_mat hidden_out w_out
                                                                                                          )
                                                                                                           b_out
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      begin (
                                                                                                        let (
                                                                                                          (
                                                                                                            out (
                                                                                                              vec_map_sig out_net
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          begin (
                                                                                                            let (
                                                                                                              (
                                                                                                                error_out (
                                                                                                                  vec_sub target out
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              begin (
                                                                                                                let (
                                                                                                                  (
                                                                                                                    pd_out (
                                                                                                                      vec_mul error_out (
                                                                                                                        vec_mul out (
                                                                                                                          vec_sub (
                                                                                                                            _list 1.0 1.0
                                                                                                                          )
                                                                                                                           out
                                                                                                                        )
                                                                                                                      )
                                                                                                                    )
                                                                                                                  )
                                                                                                                )
                                                                                                                 (
                                                                                                                  begin (
                                                                                                                    let (
                                                                                                                      (
                                                                                                                        error_hidden (
                                                                                                                          matT_vec_mul w_out pd_out
                                                                                                                        )
                                                                                                                      )
                                                                                                                    )
                                                                                                                     (
                                                                                                                      begin (
                                                                                                                        let (
                                                                                                                          (
                                                                                                                            pd_hidden (
                                                                                                                              vec_mul error_hidden (
                                                                                                                                vec_mul hidden_out (
                                                                                                                                  vec_sub (
                                                                                                                                    _list 1.0 1.0
                                                                                                                                  )
                                                                                                                                   hidden_out
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
                                                                                                                                    break74
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    letrec (
                                                                                                                                      (
                                                                                                                                        loop73 (
                                                                                                                                          lambda (
                                                                                                                                            
                                                                                                                                          )
                                                                                                                                           (
                                                                                                                                            if (
                                                                                                                                              < j (
                                                                                                                                                _len w_out
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
                                                                                                                                                        break76
                                                                                                                                                      )
                                                                                                                                                       (
                                                                                                                                                        letrec (
                                                                                                                                                          (
                                                                                                                                                            loop75 (
                                                                                                                                                              lambda (
                                                                                                                                                                
                                                                                                                                                              )
                                                                                                                                                               (
                                                                                                                                                                if (
                                                                                                                                                                  < k (
                                                                                                                                                                    _len (
                                                                                                                                                                      list-ref-safe w_out j
                                                                                                                                                                    )
                                                                                                                                                                  )
                                                                                                                                                                )
                                                                                                                                                                 (
                                                                                                                                                                  begin (
                                                                                                                                                                    list-set! (
                                                                                                                                                                      list-ref-safe w_out j
                                                                                                                                                                    )
                                                                                                                                                                     k (
                                                                                                                                                                      _add (
                                                                                                                                                                        cond (
                                                                                                                                                                          (
                                                                                                                                                                            string? (
                                                                                                                                                                              list-ref-safe w_out j
                                                                                                                                                                            )
                                                                                                                                                                          )
                                                                                                                                                                           (
                                                                                                                                                                            _substring (
                                                                                                                                                                              list-ref-safe w_out j
                                                                                                                                                                            )
                                                                                                                                                                             k (
                                                                                                                                                                              + k 1
                                                                                                                                                                            )
                                                                                                                                                                          )
                                                                                                                                                                        )
                                                                                                                                                                         (
                                                                                                                                                                          (
                                                                                                                                                                            hash-table? (
                                                                                                                                                                              list-ref-safe w_out j
                                                                                                                                                                            )
                                                                                                                                                                          )
                                                                                                                                                                           (
                                                                                                                                                                            hash-table-ref (
                                                                                                                                                                              list-ref-safe w_out j
                                                                                                                                                                            )
                                                                                                                                                                             k
                                                                                                                                                                          )
                                                                                                                                                                        )
                                                                                                                                                                         (
                                                                                                                                                                          else (
                                                                                                                                                                            list-ref-safe (
                                                                                                                                                                              list-ref-safe w_out j
                                                                                                                                                                            )
                                                                                                                                                                             k
                                                                                                                                                                          )
                                                                                                                                                                        )
                                                                                                                                                                      )
                                                                                                                                                                       (
                                                                                                                                                                        * (
                                                                                                                                                                          * (
                                                                                                                                                                            hash-table-ref cnn "rate_weight"
                                                                                                                                                                          )
                                                                                                                                                                           (
                                                                                                                                                                            cond (
                                                                                                                                                                              (
                                                                                                                                                                                string? hidden_out
                                                                                                                                                                              )
                                                                                                                                                                               (
                                                                                                                                                                                _substring hidden_out j (
                                                                                                                                                                                  + j 1
                                                                                                                                                                                )
                                                                                                                                                                              )
                                                                                                                                                                            )
                                                                                                                                                                             (
                                                                                                                                                                              (
                                                                                                                                                                                hash-table? hidden_out
                                                                                                                                                                              )
                                                                                                                                                                               (
                                                                                                                                                                                hash-table-ref hidden_out j
                                                                                                                                                                              )
                                                                                                                                                                            )
                                                                                                                                                                             (
                                                                                                                                                                              else (
                                                                                                                                                                                list-ref-safe hidden_out j
                                                                                                                                                                              )
                                                                                                                                                                            )
                                                                                                                                                                          )
                                                                                                                                                                        )
                                                                                                                                                                         (
                                                                                                                                                                          cond (
                                                                                                                                                                            (
                                                                                                                                                                              string? pd_out
                                                                                                                                                                            )
                                                                                                                                                                             (
                                                                                                                                                                              _substring pd_out k (
                                                                                                                                                                                + k 1
                                                                                                                                                                              )
                                                                                                                                                                            )
                                                                                                                                                                          )
                                                                                                                                                                           (
                                                                                                                                                                            (
                                                                                                                                                                              hash-table? pd_out
                                                                                                                                                                            )
                                                                                                                                                                             (
                                                                                                                                                                              hash-table-ref pd_out k
                                                                                                                                                                            )
                                                                                                                                                                          )
                                                                                                                                                                           (
                                                                                                                                                                            else (
                                                                                                                                                                              list-ref-safe pd_out k
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
                                                                                                                                                                    loop75
                                                                                                                                                                  )
                                                                                                                                                                )
                                                                                                                                                                 '(
                                                                                                                                                                  
                                                                                                                                                                )
                                                                                                                                                              )
                                                                                                                                                            )
                                                                                                                                                          )
                                                                                                                                                        )
                                                                                                                                                         (
                                                                                                                                                          loop75
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
                                                                                                                                               (
                                                                                                                                                loop73
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                             '(
                                                                                                                                              
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      loop73
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
                                                                                                                                    break78
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    letrec (
                                                                                                                                      (
                                                                                                                                        loop77 (
                                                                                                                                          lambda (
                                                                                                                                            
                                                                                                                                          )
                                                                                                                                           (
                                                                                                                                            if (
                                                                                                                                              < j (
                                                                                                                                                _len b_out
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                             (
                                                                                                                                              begin (
                                                                                                                                                list-set! b_out j (
                                                                                                                                                  - (
                                                                                                                                                    list-ref-safe b_out j
                                                                                                                                                  )
                                                                                                                                                   (
                                                                                                                                                    * (
                                                                                                                                                      hash-table-ref cnn "rate_bias"
                                                                                                                                                    )
                                                                                                                                                     (
                                                                                                                                                      cond (
                                                                                                                                                        (
                                                                                                                                                          string? pd_out
                                                                                                                                                        )
                                                                                                                                                         (
                                                                                                                                                          _substring pd_out j (
                                                                                                                                                            + j 1
                                                                                                                                                          )
                                                                                                                                                        )
                                                                                                                                                      )
                                                                                                                                                       (
                                                                                                                                                        (
                                                                                                                                                          hash-table? pd_out
                                                                                                                                                        )
                                                                                                                                                         (
                                                                                                                                                          hash-table-ref pd_out j
                                                                                                                                                        )
                                                                                                                                                      )
                                                                                                                                                       (
                                                                                                                                                        else (
                                                                                                                                                          list-ref-safe pd_out j
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
                                                                                                                                                loop77
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                             '(
                                                                                                                                              
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      loop77
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                )
                                                                                                                              )
                                                                                                                               (
                                                                                                                                let (
                                                                                                                                  (
                                                                                                                                    i_h 0
                                                                                                                                  )
                                                                                                                                )
                                                                                                                                 (
                                                                                                                                  begin (
                                                                                                                                    call/cc (
                                                                                                                                      lambda (
                                                                                                                                        break80
                                                                                                                                      )
                                                                                                                                       (
                                                                                                                                        letrec (
                                                                                                                                          (
                                                                                                                                            loop79 (
                                                                                                                                              lambda (
                                                                                                                                                
                                                                                                                                              )
                                                                                                                                               (
                                                                                                                                                if (
                                                                                                                                                  < i_h (
                                                                                                                                                    _len w_hidden
                                                                                                                                                  )
                                                                                                                                                )
                                                                                                                                                 (
                                                                                                                                                  begin (
                                                                                                                                                    let (
                                                                                                                                                      (
                                                                                                                                                        j_h 0
                                                                                                                                                      )
                                                                                                                                                    )
                                                                                                                                                     (
                                                                                                                                                      begin (
                                                                                                                                                        call/cc (
                                                                                                                                                          lambda (
                                                                                                                                                            break82
                                                                                                                                                          )
                                                                                                                                                           (
                                                                                                                                                            letrec (
                                                                                                                                                              (
                                                                                                                                                                loop81 (
                                                                                                                                                                  lambda (
                                                                                                                                                                    
                                                                                                                                                                  )
                                                                                                                                                                   (
                                                                                                                                                                    if (
                                                                                                                                                                      < j_h (
                                                                                                                                                                        _len (
                                                                                                                                                                          list-ref-safe w_hidden i_h
                                                                                                                                                                        )
                                                                                                                                                                      )
                                                                                                                                                                    )
                                                                                                                                                                     (
                                                                                                                                                                      begin (
                                                                                                                                                                        list-set! (
                                                                                                                                                                          list-ref-safe w_hidden i_h
                                                                                                                                                                        )
                                                                                                                                                                         j_h (
                                                                                                                                                                          _add (
                                                                                                                                                                            cond (
                                                                                                                                                                              (
                                                                                                                                                                                string? (
                                                                                                                                                                                  list-ref-safe w_hidden i_h
                                                                                                                                                                                )
                                                                                                                                                                              )
                                                                                                                                                                               (
                                                                                                                                                                                _substring (
                                                                                                                                                                                  list-ref-safe w_hidden i_h
                                                                                                                                                                                )
                                                                                                                                                                                 j_h (
                                                                                                                                                                                  + j_h 1
                                                                                                                                                                                )
                                                                                                                                                                              )
                                                                                                                                                                            )
                                                                                                                                                                             (
                                                                                                                                                                              (
                                                                                                                                                                                hash-table? (
                                                                                                                                                                                  list-ref-safe w_hidden i_h
                                                                                                                                                                                )
                                                                                                                                                                              )
                                                                                                                                                                               (
                                                                                                                                                                                hash-table-ref (
                                                                                                                                                                                  list-ref-safe w_hidden i_h
                                                                                                                                                                                )
                                                                                                                                                                                 j_h
                                                                                                                                                                              )
                                                                                                                                                                            )
                                                                                                                                                                             (
                                                                                                                                                                              else (
                                                                                                                                                                                list-ref-safe (
                                                                                                                                                                                  list-ref-safe w_hidden i_h
                                                                                                                                                                                )
                                                                                                                                                                                 j_h
                                                                                                                                                                              )
                                                                                                                                                                            )
                                                                                                                                                                          )
                                                                                                                                                                           (
                                                                                                                                                                            * (
                                                                                                                                                                              * (
                                                                                                                                                                                hash-table-ref cnn "rate_weight"
                                                                                                                                                                              )
                                                                                                                                                                               (
                                                                                                                                                                                cond (
                                                                                                                                                                                  (
                                                                                                                                                                                    string? flat
                                                                                                                                                                                  )
                                                                                                                                                                                   (
                                                                                                                                                                                    _substring flat i_h (
                                                                                                                                                                                      + i_h 1
                                                                                                                                                                                    )
                                                                                                                                                                                  )
                                                                                                                                                                                )
                                                                                                                                                                                 (
                                                                                                                                                                                  (
                                                                                                                                                                                    hash-table? flat
                                                                                                                                                                                  )
                                                                                                                                                                                   (
                                                                                                                                                                                    hash-table-ref flat i_h
                                                                                                                                                                                  )
                                                                                                                                                                                )
                                                                                                                                                                                 (
                                                                                                                                                                                  else (
                                                                                                                                                                                    list-ref-safe flat i_h
                                                                                                                                                                                  )
                                                                                                                                                                                )
                                                                                                                                                                              )
                                                                                                                                                                            )
                                                                                                                                                                             (
                                                                                                                                                                              cond (
                                                                                                                                                                                (
                                                                                                                                                                                  string? pd_hidden
                                                                                                                                                                                )
                                                                                                                                                                                 (
                                                                                                                                                                                  _substring pd_hidden j_h (
                                                                                                                                                                                    + j_h 1
                                                                                                                                                                                  )
                                                                                                                                                                                )
                                                                                                                                                                              )
                                                                                                                                                                               (
                                                                                                                                                                                (
                                                                                                                                                                                  hash-table? pd_hidden
                                                                                                                                                                                )
                                                                                                                                                                                 (
                                                                                                                                                                                  hash-table-ref pd_hidden j_h
                                                                                                                                                                                )
                                                                                                                                                                              )
                                                                                                                                                                               (
                                                                                                                                                                                else (
                                                                                                                                                                                  list-ref-safe pd_hidden j_h
                                                                                                                                                                                )
                                                                                                                                                                              )
                                                                                                                                                                            )
                                                                                                                                                                          )
                                                                                                                                                                        )
                                                                                                                                                                      )
                                                                                                                                                                       (
                                                                                                                                                                        set! j_h (
                                                                                                                                                                          + j_h 1
                                                                                                                                                                        )
                                                                                                                                                                      )
                                                                                                                                                                       (
                                                                                                                                                                        loop81
                                                                                                                                                                      )
                                                                                                                                                                    )
                                                                                                                                                                     '(
                                                                                                                                                                      
                                                                                                                                                                    )
                                                                                                                                                                  )
                                                                                                                                                                )
                                                                                                                                                              )
                                                                                                                                                            )
                                                                                                                                                             (
                                                                                                                                                              loop81
                                                                                                                                                            )
                                                                                                                                                          )
                                                                                                                                                        )
                                                                                                                                                      )
                                                                                                                                                       (
                                                                                                                                                        set! i_h (
                                                                                                                                                          + i_h 1
                                                                                                                                                        )
                                                                                                                                                      )
                                                                                                                                                    )
                                                                                                                                                  )
                                                                                                                                                   (
                                                                                                                                                    loop79
                                                                                                                                                  )
                                                                                                                                                )
                                                                                                                                                 '(
                                                                                                                                                  
                                                                                                                                                )
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                         (
                                                                                                                                          loop79
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
                                                                                                                                        break84
                                                                                                                                      )
                                                                                                                                       (
                                                                                                                                        letrec (
                                                                                                                                          (
                                                                                                                                            loop83 (
                                                                                                                                              lambda (
                                                                                                                                                
                                                                                                                                              )
                                                                                                                                               (
                                                                                                                                                if (
                                                                                                                                                  < j (
                                                                                                                                                    _len b_hidden
                                                                                                                                                  )
                                                                                                                                                )
                                                                                                                                                 (
                                                                                                                                                  begin (
                                                                                                                                                    list-set! b_hidden j (
                                                                                                                                                      - (
                                                                                                                                                        list-ref-safe b_hidden j
                                                                                                                                                      )
                                                                                                                                                       (
                                                                                                                                                        * (
                                                                                                                                                          hash-table-ref cnn "rate_bias"
                                                                                                                                                        )
                                                                                                                                                         (
                                                                                                                                                          cond (
                                                                                                                                                            (
                                                                                                                                                              string? pd_hidden
                                                                                                                                                            )
                                                                                                                                                             (
                                                                                                                                                              _substring pd_hidden j (
                                                                                                                                                                + j 1
                                                                                                                                                              )
                                                                                                                                                            )
                                                                                                                                                          )
                                                                                                                                                           (
                                                                                                                                                            (
                                                                                                                                                              hash-table? pd_hidden
                                                                                                                                                            )
                                                                                                                                                             (
                                                                                                                                                              hash-table-ref pd_hidden j
                                                                                                                                                            )
                                                                                                                                                          )
                                                                                                                                                           (
                                                                                                                                                            else (
                                                                                                                                                              list-ref-safe pd_hidden j
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
                                                                                                                                                    loop83
                                                                                                                                                  )
                                                                                                                                                )
                                                                                                                                                 '(
                                                                                                                                                  
                                                                                                                                                )
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                         (
                                                                                                                                          loop83
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    set! s (
                                                                                                                                      + s 1
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
                                                                        loop69
                                                                      )
                                                                    )
                                                                     '(
                                                                      
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             (
                                                              loop69
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        set! e (
                                                          + e 1
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    loop67
                                                  )
                                                )
                                                 '(
                                                  
                                                )
                                              )
                                            )
                                          )
                                        )
                                         (
                                          loop67
                                        )
                                      )
                                    )
                                  )
                                   (
                                    ret66 (
                                      alist->hash-table (
                                        _list (
                                          cons "conv_kernels" (
                                            hash-table-ref cnn "conv_kernels"
                                          )
                                        )
                                         (
                                          cons "conv_bias" (
                                            hash-table-ref cnn "conv_bias"
                                          )
                                        )
                                         (
                                          cons "conv_step" (
                                            hash-table-ref cnn "conv_step"
                                          )
                                        )
                                         (
                                          cons "pool_size" (
                                            hash-table-ref cnn "pool_size"
                                          )
                                        )
                                         (
                                          cons "w_hidden" w_hidden
                                        )
                                         (
                                          cons "w_out" w_out
                                        )
                                         (
                                          cons "b_hidden" b_hidden
                                        )
                                         (
                                          cons "b_out" b_out
                                        )
                                         (
                                          cons "rate_weight" (
                                            hash-table-ref cnn "rate_weight"
                                          )
                                        )
                                         (
                                          cons "rate_bias" (
                                            hash-table-ref cnn "rate_bias"
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
            main
          )
           (
            call/cc (
              lambda (
                ret85
              )
               (
                let (
                  (
                    cnn (
                      new_cnn
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        image (
                          _list (
                            _list 1.0 0.0 1.0 0.0
                          )
                           (
                            _list 0.0 1.0 0.0 1.0
                          )
                           (
                            _list 1.0 0.0 1.0 0.0
                          )
                           (
                            _list 0.0 1.0 0.0 1.0
                          )
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            sample (
                              alist->hash-table (
                                _list (
                                  cons "image" image
                                )
                                 (
                                  cons "target" (
                                    _list 1.0 0.0
                                  )
                                )
                              )
                            )
                          )
                        )
                         (
                          begin (
                            _display (
                              if (
                                string? "Before training:"
                              )
                               "Before training:" (
                                to-str "Before training:"
                              )
                            )
                          )
                           (
                            _display " "
                          )
                           (
                            _display (
                              if (
                                string? (
                                  forward cnn image
                                )
                              )
                               (
                                forward cnn image
                              )
                               (
                                to-str (
                                  forward cnn image
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
                                trained (
                                  train cnn (
                                    _list sample
                                  )
                                   50
                                )
                              )
                            )
                             (
                              begin (
                                _display (
                                  if (
                                    string? "After training:"
                                  )
                                   "After training:" (
                                    to-str "After training:"
                                  )
                                )
                              )
                               (
                                _display " "
                              )
                               (
                                _display (
                                  if (
                                    string? (
                                      forward trained image
                                    )
                                  )
                                   (
                                    forward trained image
                                  )
                                   (
                                    to-str (
                                      forward trained image
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
        )
         (
          main
        )
      )
    )
     (
      let (
        (
          end87 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur88 (
              quotient (
                * (
                  - end87 start86
                )
                 1000000
              )
               jps89
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur88
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
