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
        retroactive_resolution coefficients vector
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                rows (
                  _len coefficients
                )
              )
            )
             (
              begin (
                let (
                  (
                    x (
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
                                      < i rows
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            inner (
                                              _list
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            set! inner (
                                              append inner (
                                                _list 0.0
                                              )
                                            )
                                          )
                                           (
                                            set! x (
                                              append x (
                                                _list inner
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
                            r (
                              - rows 1
                            )
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
                                          >= r 0
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                total 0.0
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    c (
                                                      + r 1
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
                                                                
                                                              )
                                                               (
                                                                if (
                                                                  < c rows
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! total (
                                                                      _add total (
                                                                        * (
                                                                          cond (
                                                                            (
                                                                              string? (
                                                                                list-ref-safe coefficients r
                                                                              )
                                                                            )
                                                                             (
                                                                              _substring (
                                                                                list-ref-safe coefficients r
                                                                              )
                                                                               c (
                                                                                + c 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            (
                                                                              hash-table? (
                                                                                list-ref-safe coefficients r
                                                                              )
                                                                            )
                                                                             (
                                                                              hash-table-ref (
                                                                                list-ref-safe coefficients r
                                                                              )
                                                                               c
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref-safe (
                                                                                list-ref-safe coefficients r
                                                                              )
                                                                               c
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          cond (
                                                                            (
                                                                              string? (
                                                                                list-ref-safe x c
                                                                              )
                                                                            )
                                                                             (
                                                                              _substring (
                                                                                list-ref-safe x c
                                                                              )
                                                                               0 (
                                                                                + 0 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            (
                                                                              hash-table? (
                                                                                list-ref-safe x c
                                                                              )
                                                                            )
                                                                             (
                                                                              hash-table-ref (
                                                                                list-ref-safe x c
                                                                              )
                                                                               0
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref-safe (
                                                                                list-ref-safe x c
                                                                              )
                                                                               0
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
                                                    list-set! (
                                                      list-ref-safe x r
                                                    )
                                                     0 (
                                                      _div (
                                                        - (
                                                          cond (
                                                            (
                                                              string? (
                                                                list-ref-safe vector r
                                                              )
                                                            )
                                                             (
                                                              _substring (
                                                                list-ref-safe vector r
                                                              )
                                                               0 (
                                                                + 0 1
                                                              )
                                                            )
                                                          )
                                                           (
                                                            (
                                                              hash-table? (
                                                                list-ref-safe vector r
                                                              )
                                                            )
                                                             (
                                                              hash-table-ref (
                                                                list-ref-safe vector r
                                                              )
                                                               0
                                                            )
                                                          )
                                                           (
                                                            else (
                                                              list-ref-safe (
                                                                list-ref-safe vector r
                                                              )
                                                               0
                                                            )
                                                          )
                                                        )
                                                         total
                                                      )
                                                       (
                                                        cond (
                                                          (
                                                            string? (
                                                              list-ref-safe coefficients r
                                                            )
                                                          )
                                                           (
                                                            _substring (
                                                              list-ref-safe coefficients r
                                                            )
                                                             r (
                                                              + r 1
                                                            )
                                                          )
                                                        )
                                                         (
                                                          (
                                                            hash-table? (
                                                              list-ref-safe coefficients r
                                                            )
                                                          )
                                                           (
                                                            hash-table-ref (
                                                              list-ref-safe coefficients r
                                                            )
                                                             r
                                                          )
                                                        )
                                                         (
                                                          else (
                                                            list-ref-safe (
                                                              list-ref-safe coefficients r
                                                            )
                                                             r
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    set! r (
                                                      - r 1
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
                            ret1 x
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
        gaussian_elimination coefficients vector
      )
       (
        call/cc (
          lambda (
            ret8
          )
           (
            let (
              (
                rows (
                  _len coefficients
                )
              )
            )
             (
              begin (
                let (
                  (
                    columns (
                      _len (
                        list-ref-safe coefficients 0
                      )
                    )
                  )
                )
                 (
                  begin (
                    if (
                      not (
                        equal? rows columns
                      )
                    )
                     (
                      begin (
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
                        augmented (
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
                                                                  < j columns
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! row (
                                                                      append row (
                                                                        _list (
                                                                          cond (
                                                                            (
                                                                              string? (
                                                                                list-ref-safe coefficients i
                                                                              )
                                                                            )
                                                                             (
                                                                              _substring (
                                                                                list-ref-safe coefficients i
                                                                              )
                                                                               j (
                                                                                + j 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            (
                                                                              hash-table? (
                                                                                list-ref-safe coefficients i
                                                                              )
                                                                            )
                                                                             (
                                                                              hash-table-ref (
                                                                                list-ref-safe coefficients i
                                                                              )
                                                                               j
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref-safe (
                                                                                list-ref-safe coefficients i
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
                                                    set! row (
                                                      append row (
                                                        _list (
                                                          cond (
                                                            (
                                                              string? (
                                                                list-ref-safe vector i
                                                              )
                                                            )
                                                             (
                                                              _substring (
                                                                list-ref-safe vector i
                                                              )
                                                               0 (
                                                                + 0 1
                                                              )
                                                            )
                                                          )
                                                           (
                                                            (
                                                              hash-table? (
                                                                list-ref-safe vector i
                                                              )
                                                            )
                                                             (
                                                              hash-table-ref (
                                                                list-ref-safe vector i
                                                              )
                                                               0
                                                            )
                                                          )
                                                           (
                                                            else (
                                                              list-ref-safe (
                                                                list-ref-safe vector i
                                                              )
                                                               0
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    set! augmented (
                                                      append augmented (
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
                                row_idx 0
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
                                              < row_idx (
                                                - rows 1
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    pivot (
                                                      cond (
                                                        (
                                                          string? (
                                                            list-ref-safe augmented row_idx
                                                          )
                                                        )
                                                         (
                                                          _substring (
                                                            list-ref-safe augmented row_idx
                                                          )
                                                           row_idx (
                                                            + row_idx 1
                                                          )
                                                        )
                                                      )
                                                       (
                                                        (
                                                          hash-table? (
                                                            list-ref-safe augmented row_idx
                                                          )
                                                        )
                                                         (
                                                          hash-table-ref (
                                                            list-ref-safe augmented row_idx
                                                          )
                                                           row_idx
                                                        )
                                                      )
                                                       (
                                                        else (
                                                          list-ref-safe (
                                                            list-ref-safe augmented row_idx
                                                          )
                                                           row_idx
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
                                                          + row_idx 1
                                                        )
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
                                                                      < col rows
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
                                                                                      list-ref-safe augmented col
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    _substring (
                                                                                      list-ref-safe augmented col
                                                                                    )
                                                                                     row_idx (
                                                                                      + row_idx 1
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  (
                                                                                    hash-table? (
                                                                                      list-ref-safe augmented col
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    hash-table-ref (
                                                                                      list-ref-safe augmented col
                                                                                    )
                                                                                     row_idx
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  else (
                                                                                    list-ref-safe (
                                                                                      list-ref-safe augmented col
                                                                                    )
                                                                                     row_idx
                                                                                  )
                                                                                )
                                                                              )
                                                                               pivot
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                k row_idx
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
                                                                                              < k (
                                                                                                + columns 1
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                list-set! (
                                                                                                  list-ref-safe augmented col
                                                                                                )
                                                                                                 k (
                                                                                                  - (
                                                                                                    cond (
                                                                                                      (
                                                                                                        string? (
                                                                                                          list-ref-safe augmented col
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        _substring (
                                                                                                          list-ref-safe augmented col
                                                                                                        )
                                                                                                         k (
                                                                                                          + k 1
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      (
                                                                                                        hash-table? (
                                                                                                          list-ref-safe augmented col
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        hash-table-ref (
                                                                                                          list-ref-safe augmented col
                                                                                                        )
                                                                                                         k
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      else (
                                                                                                        list-ref-safe (
                                                                                                          list-ref-safe augmented col
                                                                                                        )
                                                                                                         k
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    * factor (
                                                                                                      cond (
                                                                                                        (
                                                                                                          string? (
                                                                                                            list-ref-safe augmented row_idx
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          _substring (
                                                                                                            list-ref-safe augmented row_idx
                                                                                                          )
                                                                                                           k (
                                                                                                            + k 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? (
                                                                                                            list-ref-safe augmented row_idx
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref (
                                                                                                            list-ref-safe augmented row_idx
                                                                                                          )
                                                                                                           k
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref-safe (
                                                                                                            list-ref-safe augmented row_idx
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
                                                                                set! col (
                                                                                  + col 1
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
                                                        set! row_idx (
                                                          + row_idx 1
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
                               (
                                let (
                                  (
                                    coeffs (
                                      _list
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        vec (
                                          _list
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            r 0
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
                                                          < r rows
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
                                                                    c 0
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
                                                                                  < c columns
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    set! row (
                                                                                      append row (
                                                                                        _list (
                                                                                          cond (
                                                                                            (
                                                                                              string? (
                                                                                                list-ref-safe augmented r
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              _substring (
                                                                                                list-ref-safe augmented r
                                                                                              )
                                                                                               c (
                                                                                                + c 1
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            (
                                                                                              hash-table? (
                                                                                                list-ref-safe augmented r
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              hash-table-ref (
                                                                                                list-ref-safe augmented r
                                                                                              )
                                                                                               c
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            else (
                                                                                              list-ref-safe (
                                                                                                list-ref-safe augmented r
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
                                                                    set! coeffs (
                                                                      append coeffs (
                                                                        _list row
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    set! vec (
                                                                      append vec (
                                                                        _list (
                                                                          _list (
                                                                            cond (
                                                                              (
                                                                                string? (
                                                                                  list-ref-safe augmented r
                                                                                )
                                                                              )
                                                                               (
                                                                                _substring (
                                                                                  list-ref-safe augmented r
                                                                                )
                                                                                 columns (
                                                                                  + columns 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? (
                                                                                  list-ref-safe augmented r
                                                                                )
                                                                              )
                                                                               (
                                                                                hash-table-ref (
                                                                                  list-ref-safe augmented r
                                                                                )
                                                                                 columns
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref-safe (
                                                                                  list-ref-safe augmented r
                                                                                )
                                                                                 columns
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    set! r (
                                                                      + r 1
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
                                            let (
                                              (
                                                x (
                                                  retroactive_resolution coeffs vec
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                ret8 x
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
      _display (
        if (
          string? (
            gaussian_elimination (
              _list (
                _list 1.0 (
                  - 4.0
                )
                 (
                  - 2.0
                )
              )
               (
                _list 5.0 2.0 (
                  - 2.0
                )
              )
               (
                _list 1.0 (
                  - 1.0
                )
                 0.0
              )
            )
             (
              _list (
                _list (
                  - 2.0
                )
              )
               (
                _list (
                  - 3.0
                )
              )
               (
                _list 4.0
              )
            )
          )
        )
         (
          gaussian_elimination (
            _list (
              _list 1.0 (
                - 4.0
              )
               (
                - 2.0
              )
            )
             (
              _list 5.0 2.0 (
                - 2.0
              )
            )
             (
              _list 1.0 (
                - 1.0
              )
               0.0
            )
          )
           (
            _list (
              _list (
                - 2.0
              )
            )
             (
              _list (
                - 3.0
              )
            )
             (
              _list 4.0
            )
          )
        )
         (
          to-str (
            gaussian_elimination (
              _list (
                _list 1.0 (
                  - 4.0
                )
                 (
                  - 2.0
                )
              )
               (
                _list 5.0 2.0 (
                  - 2.0
                )
              )
               (
                _list 1.0 (
                  - 1.0
                )
                 0.0
              )
            )
             (
              _list (
                _list (
                  - 2.0
                )
              )
               (
                _list (
                  - 3.0
                )
              )
               (
                _list 4.0
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
            gaussian_elimination (
              _list (
                _list 1.0 2.0
              )
               (
                _list 5.0 2.0
              )
            )
             (
              _list (
                _list 5.0
              )
               (
                _list 5.0
              )
            )
          )
        )
         (
          gaussian_elimination (
            _list (
              _list 1.0 2.0
            )
             (
              _list 5.0 2.0
            )
          )
           (
            _list (
              _list 5.0
            )
             (
              _list 5.0
            )
          )
        )
         (
          to-str (
            gaussian_elimination (
              _list (
                _list 1.0 2.0
              )
               (
                _list 5.0 2.0
              )
            )
             (
              _list (
                _list 5.0
              )
               (
                _list 5.0
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
