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
      start22 (
        current-jiffy
      )
    )
     (
      jps25 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        invert_matrix matrix
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
                  _len matrix
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
                                                                    _list (
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
                                                let (
                                                  (
                                                    k 0
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
                                                                  < k n
                                                                )
                                                                 (
                                                                  begin (
                                                                    if (
                                                                      equal? i k
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
                                                                    set! k (
                                                                      + k 1
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
                        let (
                          (
                            col 0
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
                                                pivot_row col
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    r col
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
                                                                  < r n
                                                                )
                                                                 (
                                                                  begin (
                                                                    if (
                                                                      not (
                                                                        equal? (
                                                                          cond (
                                                                            (
                                                                              string? (
                                                                                list-ref-safe aug r
                                                                              )
                                                                            )
                                                                             (
                                                                              _substring (
                                                                                list-ref-safe aug r
                                                                              )
                                                                               col (
                                                                                + col 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            (
                                                                              hash-table? (
                                                                                list-ref-safe aug r
                                                                              )
                                                                            )
                                                                             (
                                                                              hash-table-ref (
                                                                                list-ref-safe aug r
                                                                              )
                                                                               col
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref-safe (
                                                                                list-ref-safe aug r
                                                                              )
                                                                               col
                                                                            )
                                                                          )
                                                                        )
                                                                         0.0
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        set! pivot_row r
                                                                      )
                                                                       (
                                                                        break11 (
                                                                          quote (
                                                                            
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
                                                    if (
                                                      equal? (
                                                        cond (
                                                          (
                                                            string? (
                                                              list-ref-safe aug pivot_row
                                                            )
                                                          )
                                                           (
                                                            _substring (
                                                              list-ref-safe aug pivot_row
                                                            )
                                                             col (
                                                              + col 1
                                                            )
                                                          )
                                                        )
                                                         (
                                                          (
                                                            hash-table? (
                                                              list-ref-safe aug pivot_row
                                                            )
                                                          )
                                                           (
                                                            hash-table-ref (
                                                              list-ref-safe aug pivot_row
                                                            )
                                                             col
                                                          )
                                                        )
                                                         (
                                                          else (
                                                            list-ref-safe (
                                                              list-ref-safe aug pivot_row
                                                            )
                                                             col
                                                          )
                                                        )
                                                      )
                                                       0.0
                                                    )
                                                     (
                                                      begin (
                                                        panic "Matrix is not invertible"
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
                                                        equal? pivot_row col
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            temp (
                                                              list-ref-safe aug col
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            list-set! aug col (
                                                              list-ref-safe aug pivot_row
                                                            )
                                                          )
                                                           (
                                                            list-set! aug pivot_row temp
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
                                                        pivot (
                                                          cond (
                                                            (
                                                              string? (
                                                                list-ref-safe aug col
                                                              )
                                                            )
                                                             (
                                                              _substring (
                                                                list-ref-safe aug col
                                                              )
                                                               col (
                                                                + col 1
                                                              )
                                                            )
                                                          )
                                                           (
                                                            (
                                                              hash-table? (
                                                                list-ref-safe aug col
                                                              )
                                                            )
                                                             (
                                                              hash-table-ref (
                                                                list-ref-safe aug col
                                                              )
                                                               col
                                                            )
                                                          )
                                                           (
                                                            else (
                                                              list-ref-safe (
                                                                list-ref-safe aug col
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
                                                            c 0
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
                                                                          < c (
                                                                            * 2 n
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            list-set! (
                                                                              list-ref-safe aug col
                                                                            )
                                                                             c (
                                                                              _div (
                                                                                cond (
                                                                                  (
                                                                                    string? (
                                                                                      list-ref-safe aug col
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    _substring (
                                                                                      list-ref-safe aug col
                                                                                    )
                                                                                     c (
                                                                                      + c 1
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  (
                                                                                    hash-table? (
                                                                                      list-ref-safe aug col
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    hash-table-ref (
                                                                                      list-ref-safe aug col
                                                                                    )
                                                                                     c
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  else (
                                                                                    list-ref-safe (
                                                                                      list-ref-safe aug col
                                                                                    )
                                                                                     c
                                                                                  )
                                                                                )
                                                                              )
                                                                               pivot
                                                                            )
                                                                          )
                                                                           (
                                                                            set! c (
                                                                              + c 1
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
                                                            let (
                                                              (
                                                                r2 0
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
                                                                              < r2 n
                                                                            )
                                                                             (
                                                                              begin (
                                                                                if (
                                                                                  not (
                                                                                    equal? r2 col
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
                                                                                                list-ref-safe aug r2
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              _substring (
                                                                                                list-ref-safe aug r2
                                                                                              )
                                                                                               col (
                                                                                                + col 1
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            (
                                                                                              hash-table? (
                                                                                                list-ref-safe aug r2
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              hash-table-ref (
                                                                                                list-ref-safe aug r2
                                                                                              )
                                                                                               col
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            else (
                                                                                              list-ref-safe (
                                                                                                list-ref-safe aug r2
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
                                                                                            c2 0
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
                                                                                                        
                                                                                                      )
                                                                                                       (
                                                                                                        if (
                                                                                                          < c2 (
                                                                                                            * 2 n
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          begin (
                                                                                                            list-set! (
                                                                                                              list-ref-safe aug r2
                                                                                                            )
                                                                                                             c2 (
                                                                                                              - (
                                                                                                                cond (
                                                                                                                  (
                                                                                                                    string? (
                                                                                                                      list-ref-safe aug r2
                                                                                                                    )
                                                                                                                  )
                                                                                                                   (
                                                                                                                    _substring (
                                                                                                                      list-ref-safe aug r2
                                                                                                                    )
                                                                                                                     c2 (
                                                                                                                      + c2 1
                                                                                                                    )
                                                                                                                  )
                                                                                                                )
                                                                                                                 (
                                                                                                                  (
                                                                                                                    hash-table? (
                                                                                                                      list-ref-safe aug r2
                                                                                                                    )
                                                                                                                  )
                                                                                                                   (
                                                                                                                    hash-table-ref (
                                                                                                                      list-ref-safe aug r2
                                                                                                                    )
                                                                                                                     c2
                                                                                                                  )
                                                                                                                )
                                                                                                                 (
                                                                                                                  else (
                                                                                                                    list-ref-safe (
                                                                                                                      list-ref-safe aug r2
                                                                                                                    )
                                                                                                                     c2
                                                                                                                  )
                                                                                                                )
                                                                                                              )
                                                                                                               (
                                                                                                                * factor (
                                                                                                                  cond (
                                                                                                                    (
                                                                                                                      string? (
                                                                                                                        list-ref-safe aug col
                                                                                                                      )
                                                                                                                    )
                                                                                                                     (
                                                                                                                      _substring (
                                                                                                                        list-ref-safe aug col
                                                                                                                      )
                                                                                                                       c2 (
                                                                                                                        + c2 1
                                                                                                                      )
                                                                                                                    )
                                                                                                                  )
                                                                                                                   (
                                                                                                                    (
                                                                                                                      hash-table? (
                                                                                                                        list-ref-safe aug col
                                                                                                                      )
                                                                                                                    )
                                                                                                                     (
                                                                                                                      hash-table-ref (
                                                                                                                        list-ref-safe aug col
                                                                                                                      )
                                                                                                                       c2
                                                                                                                    )
                                                                                                                  )
                                                                                                                   (
                                                                                                                    else (
                                                                                                                      list-ref-safe (
                                                                                                                        list-ref-safe aug col
                                                                                                                      )
                                                                                                                       c2
                                                                                                                    )
                                                                                                                  )
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                           (
                                                                                                            set! c2 (
                                                                                                              + c2 1
                                                                                                            )
                                                                                                          )
                                                                                                           (
                                                                                                            loop16
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
                                                                                                  loop16
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
                                                                                  quote (
                                                                                    
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                set! r2 (
                                                                                  + r2 1
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
                            let (
                              (
                                inv (
                                  _list
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    r3 0
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
                                                  < r3 n
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
                                                            c3 0
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
                                                                          < c3 n
                                                                        )
                                                                         (
                                                                          begin (
                                                                            set! row (
                                                                              append row (
                                                                                _list (
                                                                                  cond (
                                                                                    (
                                                                                      string? (
                                                                                        list-ref-safe aug r3
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref-safe aug r3
                                                                                      )
                                                                                       (
                                                                                        + c3 n
                                                                                      )
                                                                                       (
                                                                                        + (
                                                                                          + c3 n
                                                                                        )
                                                                                         1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref-safe aug r3
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref-safe aug r3
                                                                                      )
                                                                                       (
                                                                                        + c3 n
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref-safe (
                                                                                        list-ref-safe aug r3
                                                                                      )
                                                                                       (
                                                                                        + c3 n
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            set! c3 (
                                                                              + c3 1
                                                                            )
                                                                          )
                                                                           (
                                                                            loop20
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
                                                                  loop20
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
                                                            set! r3 (
                                                              + r3 1
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
                                                 (
                                                  quote (
                                                    
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
                                    )
                                  )
                                   (
                                    ret1 inv
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
          mat (
            _list (
              _list 4.0 7.0
            )
             (
              _list 2.0 6.0
            )
          )
        )
      )
       (
        begin (
          _display (
            if (
              string? "Original Matrix:"
            )
             "Original Matrix:" (
              to-str "Original Matrix:"
            )
          )
        )
         (
          newline
        )
         (
          _display (
            if (
              string? mat
            )
             mat (
              to-str mat
            )
          )
        )
         (
          newline
        )
         (
          _display (
            if (
              string? "Inverted Matrix:"
            )
             "Inverted Matrix:" (
              to-str "Inverted Matrix:"
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
                invert_matrix mat
              )
            )
             (
              invert_matrix mat
            )
             (
              to-str (
                invert_matrix mat
              )
            )
          )
        )
         (
          newline
        )
      )
    )
     (
      let (
        (
          end23 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur24 (
              quotient (
                * (
                  - end23 start22
                )
                 1000000
              )
               jps25
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur24
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
