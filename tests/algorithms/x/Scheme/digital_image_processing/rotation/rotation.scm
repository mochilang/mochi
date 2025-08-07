;; Generated on 2025-08-07 08:20 +0700
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
      start18 (
        current-jiffy
      )
    )
     (
      jps21 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        mat_inverse3 m
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                a (
                  cond (
                    (
                      string? (
                        list-ref m 0
                      )
                    )
                     (
                      _substring (
                        list-ref m 0
                      )
                       0 (
                        + 0 1
                      )
                    )
                  )
                   (
                    (
                      hash-table? (
                        list-ref m 0
                      )
                    )
                     (
                      hash-table-ref (
                        list-ref m 0
                      )
                       0
                    )
                  )
                   (
                    else (
                      list-ref (
                        list-ref m 0
                      )
                       0
                    )
                  )
                )
              )
            )
             (
              begin (
                let (
                  (
                    b (
                      cond (
                        (
                          string? (
                            list-ref m 0
                          )
                        )
                         (
                          _substring (
                            list-ref m 0
                          )
                           1 (
                            + 1 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? (
                            list-ref m 0
                          )
                        )
                         (
                          hash-table-ref (
                            list-ref m 0
                          )
                           1
                        )
                      )
                       (
                        else (
                          list-ref (
                            list-ref m 0
                          )
                           1
                        )
                      )
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        c (
                          cond (
                            (
                              string? (
                                list-ref m 0
                              )
                            )
                             (
                              _substring (
                                list-ref m 0
                              )
                               2 (
                                + 2 1
                              )
                            )
                          )
                           (
                            (
                              hash-table? (
                                list-ref m 0
                              )
                            )
                             (
                              hash-table-ref (
                                list-ref m 0
                              )
                               2
                            )
                          )
                           (
                            else (
                              list-ref (
                                list-ref m 0
                              )
                               2
                            )
                          )
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            d (
                              cond (
                                (
                                  string? (
                                    list-ref m 1
                                  )
                                )
                                 (
                                  _substring (
                                    list-ref m 1
                                  )
                                   0 (
                                    + 0 1
                                  )
                                )
                              )
                               (
                                (
                                  hash-table? (
                                    list-ref m 1
                                  )
                                )
                                 (
                                  hash-table-ref (
                                    list-ref m 1
                                  )
                                   0
                                )
                              )
                               (
                                else (
                                  list-ref (
                                    list-ref m 1
                                  )
                                   0
                                )
                              )
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                e (
                                  cond (
                                    (
                                      string? (
                                        list-ref m 1
                                      )
                                    )
                                     (
                                      _substring (
                                        list-ref m 1
                                      )
                                       1 (
                                        + 1 1
                                      )
                                    )
                                  )
                                   (
                                    (
                                      hash-table? (
                                        list-ref m 1
                                      )
                                    )
                                     (
                                      hash-table-ref (
                                        list-ref m 1
                                      )
                                       1
                                    )
                                  )
                                   (
                                    else (
                                      list-ref (
                                        list-ref m 1
                                      )
                                       1
                                    )
                                  )
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    f (
                                      cond (
                                        (
                                          string? (
                                            list-ref m 1
                                          )
                                        )
                                         (
                                          _substring (
                                            list-ref m 1
                                          )
                                           2 (
                                            + 2 1
                                          )
                                        )
                                      )
                                       (
                                        (
                                          hash-table? (
                                            list-ref m 1
                                          )
                                        )
                                         (
                                          hash-table-ref (
                                            list-ref m 1
                                          )
                                           2
                                        )
                                      )
                                       (
                                        else (
                                          list-ref (
                                            list-ref m 1
                                          )
                                           2
                                        )
                                      )
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        g (
                                          cond (
                                            (
                                              string? (
                                                list-ref m 2
                                              )
                                            )
                                             (
                                              _substring (
                                                list-ref m 2
                                              )
                                               0 (
                                                + 0 1
                                              )
                                            )
                                          )
                                           (
                                            (
                                              hash-table? (
                                                list-ref m 2
                                              )
                                            )
                                             (
                                              hash-table-ref (
                                                list-ref m 2
                                              )
                                               0
                                            )
                                          )
                                           (
                                            else (
                                              list-ref (
                                                list-ref m 2
                                              )
                                               0
                                            )
                                          )
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            h (
                                              cond (
                                                (
                                                  string? (
                                                    list-ref m 2
                                                  )
                                                )
                                                 (
                                                  _substring (
                                                    list-ref m 2
                                                  )
                                                   1 (
                                                    + 1 1
                                                  )
                                                )
                                              )
                                               (
                                                (
                                                  hash-table? (
                                                    list-ref m 2
                                                  )
                                                )
                                                 (
                                                  hash-table-ref (
                                                    list-ref m 2
                                                  )
                                                   1
                                                )
                                              )
                                               (
                                                else (
                                                  list-ref (
                                                    list-ref m 2
                                                  )
                                                   1
                                                )
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                i (
                                                  cond (
                                                    (
                                                      string? (
                                                        list-ref m 2
                                                      )
                                                    )
                                                     (
                                                      _substring (
                                                        list-ref m 2
                                                      )
                                                       2 (
                                                        + 2 1
                                                      )
                                                    )
                                                  )
                                                   (
                                                    (
                                                      hash-table? (
                                                        list-ref m 2
                                                      )
                                                    )
                                                     (
                                                      hash-table-ref (
                                                        list-ref m 2
                                                      )
                                                       2
                                                    )
                                                  )
                                                   (
                                                    else (
                                                      list-ref (
                                                        list-ref m 2
                                                      )
                                                       2
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    det (
                                                      _add (
                                                        - (
                                                          * a (
                                                            - (
                                                              * e i
                                                            )
                                                             (
                                                              * f h
                                                            )
                                                          )
                                                        )
                                                         (
                                                          * b (
                                                            - (
                                                              * d i
                                                            )
                                                             (
                                                              * f g
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        * c (
                                                          - (
                                                            * d h
                                                          )
                                                           (
                                                            * e g
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    if (
                                                      equal? det 0.0
                                                    )
                                                     (
                                                      begin (
                                                        panic "singular matrix"
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
                                                        adj00 (
                                                          - (
                                                            * e i
                                                          )
                                                           (
                                                            * f h
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            adj01 (
                                                              - (
                                                                * c h
                                                              )
                                                               (
                                                                * b i
                                                              )
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                adj02 (
                                                                  - (
                                                                    * b f
                                                                  )
                                                                   (
                                                                    * c e
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                let (
                                                                  (
                                                                    adj10 (
                                                                      - (
                                                                        * f g
                                                                      )
                                                                       (
                                                                        * d i
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        adj11 (
                                                                          - (
                                                                            * a i
                                                                          )
                                                                           (
                                                                            * c g
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            adj12 (
                                                                              - (
                                                                                * c d
                                                                              )
                                                                               (
                                                                                * a f
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                adj20 (
                                                                                  - (
                                                                                    * d h
                                                                                  )
                                                                                   (
                                                                                    * e g
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                let (
                                                                                  (
                                                                                    adj21 (
                                                                                      - (
                                                                                        * b g
                                                                                      )
                                                                                       (
                                                                                        * a h
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    let (
                                                                                      (
                                                                                        adj22 (
                                                                                          - (
                                                                                            * a e
                                                                                          )
                                                                                           (
                                                                                            * b d
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        let (
                                                                                          (
                                                                                            inv (
                                                                                              _list
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            set! inv (
                                                                                              append inv (
                                                                                                _list (
                                                                                                  _list (
                                                                                                    _div adj00 det
                                                                                                  )
                                                                                                   (
                                                                                                    _div adj01 det
                                                                                                  )
                                                                                                   (
                                                                                                    _div adj02 det
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            set! inv (
                                                                                              append inv (
                                                                                                _list (
                                                                                                  _list (
                                                                                                    _div adj10 det
                                                                                                  )
                                                                                                   (
                                                                                                    _div adj11 det
                                                                                                  )
                                                                                                   (
                                                                                                    _div adj12 det
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            set! inv (
                                                                                              append inv (
                                                                                                _list (
                                                                                                  _list (
                                                                                                    _div adj20 det
                                                                                                  )
                                                                                                   (
                                                                                                    _div adj21 det
                                                                                                  )
                                                                                                   (
                                                                                                    _div adj22 det
                                                                                                  )
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
        mat_vec_mul m v
      )
       (
        call/cc (
          lambda (
            ret2
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
                                  < i 3
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        val (
                                          _add (
                                            _add (
                                              * (
                                                cond (
                                                  (
                                                    string? (
                                                      list-ref m i
                                                    )
                                                  )
                                                   (
                                                    _substring (
                                                      list-ref m i
                                                    )
                                                     0 (
                                                      + 0 1
                                                    )
                                                  )
                                                )
                                                 (
                                                  (
                                                    hash-table? (
                                                      list-ref m i
                                                    )
                                                  )
                                                   (
                                                    hash-table-ref (
                                                      list-ref m i
                                                    )
                                                     0
                                                  )
                                                )
                                                 (
                                                  else (
                                                    list-ref (
                                                      list-ref m i
                                                    )
                                                     0
                                                  )
                                                )
                                              )
                                               (
                                                list-ref v 0
                                              )
                                            )
                                             (
                                              * (
                                                cond (
                                                  (
                                                    string? (
                                                      list-ref m i
                                                    )
                                                  )
                                                   (
                                                    _substring (
                                                      list-ref m i
                                                    )
                                                     1 (
                                                      + 1 1
                                                    )
                                                  )
                                                )
                                                 (
                                                  (
                                                    hash-table? (
                                                      list-ref m i
                                                    )
                                                  )
                                                   (
                                                    hash-table-ref (
                                                      list-ref m i
                                                    )
                                                     1
                                                  )
                                                )
                                                 (
                                                  else (
                                                    list-ref (
                                                      list-ref m i
                                                    )
                                                     1
                                                  )
                                                )
                                              )
                                               (
                                                list-ref v 1
                                              )
                                            )
                                          )
                                           (
                                            * (
                                              cond (
                                                (
                                                  string? (
                                                    list-ref m i
                                                  )
                                                )
                                                 (
                                                  _substring (
                                                    list-ref m i
                                                  )
                                                   2 (
                                                    + 2 1
                                                  )
                                                )
                                              )
                                               (
                                                (
                                                  hash-table? (
                                                    list-ref m i
                                                  )
                                                )
                                                 (
                                                  hash-table-ref (
                                                    list-ref m i
                                                  )
                                                   2
                                                )
                                              )
                                               (
                                                else (
                                                  list-ref (
                                                    list-ref m i
                                                  )
                                                   2
                                                )
                                              )
                                            )
                                             (
                                              list-ref v 2
                                            )
                                          )
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        set! res (
                                          append res (
                                            _list val
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
                    ret2 res
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
        create_matrix rows cols value
      )
       (
        call/cc (
          lambda (
            ret5
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
                    r 0
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
                                                          < c cols
                                                        )
                                                         (
                                                          begin (
                                                            set! row (
                                                              append row (
                                                                _list value
                                                              )
                                                            )
                                                          )
                                                           (
                                                            set! c (
                                                              + c 1
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
                                            set! result (
                                              append result (
                                                _list row
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
                    ret5 result
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
        round_to_int x
      )
       (
        call/cc (
          lambda (
            ret10
          )
           (
            begin (
              if (
                >= x 0.0
              )
               (
                begin (
                  ret10 (
                    let (
                      (
                        v11 (
                          + x 0.5
                        )
                      )
                    )
                     (
                      cond (
                        (
                          string? v11
                        )
                         (
                          exact (
                            floor (
                              string->number v11
                            )
                          )
                        )
                      )
                       (
                        (
                          boolean? v11
                        )
                         (
                          if v11 1 0
                        )
                      )
                       (
                        else (
                          exact (
                            floor v11
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
              ret10 (
                let (
                  (
                    v12 (
                      - x 0.5
                    )
                  )
                )
                 (
                  cond (
                    (
                      string? v12
                    )
                     (
                      exact (
                        floor (
                          string->number v12
                        )
                      )
                    )
                  )
                   (
                    (
                      boolean? v12
                    )
                     (
                      if v12 1 0
                    )
                  )
                   (
                    else (
                      exact (
                        floor v12
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
        get_rotation img pt1 pt2 rows cols
      )
       (
        call/cc (
          lambda (
            ret13
          )
           (
            let (
              (
                src (
                  _list (
                    _list (
                      cond (
                        (
                          string? (
                            list-ref pt1 0
                          )
                        )
                         (
                          _substring (
                            list-ref pt1 0
                          )
                           0 (
                            + 0 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? (
                            list-ref pt1 0
                          )
                        )
                         (
                          hash-table-ref (
                            list-ref pt1 0
                          )
                           0
                        )
                      )
                       (
                        else (
                          list-ref (
                            list-ref pt1 0
                          )
                           0
                        )
                      )
                    )
                     (
                      cond (
                        (
                          string? (
                            list-ref pt1 0
                          )
                        )
                         (
                          _substring (
                            list-ref pt1 0
                          )
                           1 (
                            + 1 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? (
                            list-ref pt1 0
                          )
                        )
                         (
                          hash-table-ref (
                            list-ref pt1 0
                          )
                           1
                        )
                      )
                       (
                        else (
                          list-ref (
                            list-ref pt1 0
                          )
                           1
                        )
                      )
                    )
                     1.0
                  )
                   (
                    _list (
                      cond (
                        (
                          string? (
                            list-ref pt1 1
                          )
                        )
                         (
                          _substring (
                            list-ref pt1 1
                          )
                           0 (
                            + 0 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? (
                            list-ref pt1 1
                          )
                        )
                         (
                          hash-table-ref (
                            list-ref pt1 1
                          )
                           0
                        )
                      )
                       (
                        else (
                          list-ref (
                            list-ref pt1 1
                          )
                           0
                        )
                      )
                    )
                     (
                      cond (
                        (
                          string? (
                            list-ref pt1 1
                          )
                        )
                         (
                          _substring (
                            list-ref pt1 1
                          )
                           1 (
                            + 1 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? (
                            list-ref pt1 1
                          )
                        )
                         (
                          hash-table-ref (
                            list-ref pt1 1
                          )
                           1
                        )
                      )
                       (
                        else (
                          list-ref (
                            list-ref pt1 1
                          )
                           1
                        )
                      )
                    )
                     1.0
                  )
                   (
                    _list (
                      cond (
                        (
                          string? (
                            list-ref pt1 2
                          )
                        )
                         (
                          _substring (
                            list-ref pt1 2
                          )
                           0 (
                            + 0 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? (
                            list-ref pt1 2
                          )
                        )
                         (
                          hash-table-ref (
                            list-ref pt1 2
                          )
                           0
                        )
                      )
                       (
                        else (
                          list-ref (
                            list-ref pt1 2
                          )
                           0
                        )
                      )
                    )
                     (
                      cond (
                        (
                          string? (
                            list-ref pt1 2
                          )
                        )
                         (
                          _substring (
                            list-ref pt1 2
                          )
                           1 (
                            + 1 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? (
                            list-ref pt1 2
                          )
                        )
                         (
                          hash-table-ref (
                            list-ref pt1 2
                          )
                           1
                        )
                      )
                       (
                        else (
                          list-ref (
                            list-ref pt1 2
                          )
                           1
                        )
                      )
                    )
                     1.0
                  )
                )
              )
            )
             (
              begin (
                let (
                  (
                    inv (
                      mat_inverse3 src
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        vecx (
                          _list (
                            cond (
                              (
                                string? (
                                  list-ref pt2 0
                                )
                              )
                               (
                                _substring (
                                  list-ref pt2 0
                                )
                                 0 (
                                  + 0 1
                                )
                              )
                            )
                             (
                              (
                                hash-table? (
                                  list-ref pt2 0
                                )
                              )
                               (
                                hash-table-ref (
                                  list-ref pt2 0
                                )
                                 0
                              )
                            )
                             (
                              else (
                                list-ref (
                                  list-ref pt2 0
                                )
                                 0
                              )
                            )
                          )
                           (
                            cond (
                              (
                                string? (
                                  list-ref pt2 1
                                )
                              )
                               (
                                _substring (
                                  list-ref pt2 1
                                )
                                 0 (
                                  + 0 1
                                )
                              )
                            )
                             (
                              (
                                hash-table? (
                                  list-ref pt2 1
                                )
                              )
                               (
                                hash-table-ref (
                                  list-ref pt2 1
                                )
                                 0
                              )
                            )
                             (
                              else (
                                list-ref (
                                  list-ref pt2 1
                                )
                                 0
                              )
                            )
                          )
                           (
                            cond (
                              (
                                string? (
                                  list-ref pt2 2
                                )
                              )
                               (
                                _substring (
                                  list-ref pt2 2
                                )
                                 0 (
                                  + 0 1
                                )
                              )
                            )
                             (
                              (
                                hash-table? (
                                  list-ref pt2 2
                                )
                              )
                               (
                                hash-table-ref (
                                  list-ref pt2 2
                                )
                                 0
                              )
                            )
                             (
                              else (
                                list-ref (
                                  list-ref pt2 2
                                )
                                 0
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
                            vecy (
                              _list (
                                cond (
                                  (
                                    string? (
                                      list-ref pt2 0
                                    )
                                  )
                                   (
                                    _substring (
                                      list-ref pt2 0
                                    )
                                     1 (
                                      + 1 1
                                    )
                                  )
                                )
                                 (
                                  (
                                    hash-table? (
                                      list-ref pt2 0
                                    )
                                  )
                                   (
                                    hash-table-ref (
                                      list-ref pt2 0
                                    )
                                     1
                                  )
                                )
                                 (
                                  else (
                                    list-ref (
                                      list-ref pt2 0
                                    )
                                     1
                                  )
                                )
                              )
                               (
                                cond (
                                  (
                                    string? (
                                      list-ref pt2 1
                                    )
                                  )
                                   (
                                    _substring (
                                      list-ref pt2 1
                                    )
                                     1 (
                                      + 1 1
                                    )
                                  )
                                )
                                 (
                                  (
                                    hash-table? (
                                      list-ref pt2 1
                                    )
                                  )
                                   (
                                    hash-table-ref (
                                      list-ref pt2 1
                                    )
                                     1
                                  )
                                )
                                 (
                                  else (
                                    list-ref (
                                      list-ref pt2 1
                                    )
                                     1
                                  )
                                )
                              )
                               (
                                cond (
                                  (
                                    string? (
                                      list-ref pt2 2
                                    )
                                  )
                                   (
                                    _substring (
                                      list-ref pt2 2
                                    )
                                     1 (
                                      + 1 1
                                    )
                                  )
                                )
                                 (
                                  (
                                    hash-table? (
                                      list-ref pt2 2
                                    )
                                  )
                                   (
                                    hash-table-ref (
                                      list-ref pt2 2
                                    )
                                     1
                                  )
                                )
                                 (
                                  else (
                                    list-ref (
                                      list-ref pt2 2
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
                                avec (
                                  mat_vec_mul inv vecx
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    bvec (
                                      mat_vec_mul inv vecy
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        a0 (
                                          cond (
                                            (
                                              string? avec
                                            )
                                             (
                                              _substring avec 0 (
                                                + 0 1
                                              )
                                            )
                                          )
                                           (
                                            (
                                              hash-table? avec
                                            )
                                             (
                                              hash-table-ref avec 0
                                            )
                                          )
                                           (
                                            else (
                                              list-ref avec 0
                                            )
                                          )
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            a1 (
                                              cond (
                                                (
                                                  string? avec
                                                )
                                                 (
                                                  _substring avec 1 (
                                                    + 1 1
                                                  )
                                                )
                                              )
                                               (
                                                (
                                                  hash-table? avec
                                                )
                                                 (
                                                  hash-table-ref avec 1
                                                )
                                              )
                                               (
                                                else (
                                                  list-ref avec 1
                                                )
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                a2 (
                                                  cond (
                                                    (
                                                      string? avec
                                                    )
                                                     (
                                                      _substring avec 2 (
                                                        + 2 1
                                                      )
                                                    )
                                                  )
                                                   (
                                                    (
                                                      hash-table? avec
                                                    )
                                                     (
                                                      hash-table-ref avec 2
                                                    )
                                                  )
                                                   (
                                                    else (
                                                      list-ref avec 2
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    b0 (
                                                      cond (
                                                        (
                                                          string? bvec
                                                        )
                                                         (
                                                          _substring bvec 0 (
                                                            + 0 1
                                                          )
                                                        )
                                                      )
                                                       (
                                                        (
                                                          hash-table? bvec
                                                        )
                                                         (
                                                          hash-table-ref bvec 0
                                                        )
                                                      )
                                                       (
                                                        else (
                                                          list-ref bvec 0
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        b1 (
                                                          cond (
                                                            (
                                                              string? bvec
                                                            )
                                                             (
                                                              _substring bvec 1 (
                                                                + 1 1
                                                              )
                                                            )
                                                          )
                                                           (
                                                            (
                                                              hash-table? bvec
                                                            )
                                                             (
                                                              hash-table-ref bvec 1
                                                            )
                                                          )
                                                           (
                                                            else (
                                                              list-ref bvec 1
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            b2 (
                                                              cond (
                                                                (
                                                                  string? bvec
                                                                )
                                                                 (
                                                                  _substring bvec 2 (
                                                                    + 2 1
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                (
                                                                  hash-table? bvec
                                                                )
                                                                 (
                                                                  hash-table-ref bvec 2
                                                                )
                                                              )
                                                               (
                                                                else (
                                                                  list-ref bvec 2
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                out (
                                                                  create_matrix rows cols 0
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                let (
                                                                  (
                                                                    y 0
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
                                                                                  < y rows
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    let (
                                                                                      (
                                                                                        x 0
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
                                                                                                      < x cols
                                                                                                    )
                                                                                                     (
                                                                                                      begin (
                                                                                                        let (
                                                                                                          (
                                                                                                            xf (
                                                                                                              _add (
                                                                                                                _add (
                                                                                                                  * a0 (
                                                                                                                    * 1.0 x
                                                                                                                  )
                                                                                                                )
                                                                                                                 (
                                                                                                                  * a1 (
                                                                                                                    * 1.0 y
                                                                                                                  )
                                                                                                                )
                                                                                                              )
                                                                                                               a2
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          begin (
                                                                                                            let (
                                                                                                              (
                                                                                                                yf (
                                                                                                                  _add (
                                                                                                                    _add (
                                                                                                                      * b0 (
                                                                                                                        * 1.0 x
                                                                                                                      )
                                                                                                                    )
                                                                                                                     (
                                                                                                                      * b1 (
                                                                                                                        * 1.0 y
                                                                                                                      )
                                                                                                                    )
                                                                                                                  )
                                                                                                                   b2
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              begin (
                                                                                                                let (
                                                                                                                  (
                                                                                                                    sx (
                                                                                                                      round_to_int xf
                                                                                                                    )
                                                                                                                  )
                                                                                                                )
                                                                                                                 (
                                                                                                                  begin (
                                                                                                                    let (
                                                                                                                      (
                                                                                                                        sy (
                                                                                                                          round_to_int yf
                                                                                                                        )
                                                                                                                      )
                                                                                                                    )
                                                                                                                     (
                                                                                                                      begin (
                                                                                                                        if (
                                                                                                                          and (
                                                                                                                            and (
                                                                                                                              and (
                                                                                                                                _ge sx 0
                                                                                                                              )
                                                                                                                               (
                                                                                                                                _lt sx cols
                                                                                                                              )
                                                                                                                            )
                                                                                                                             (
                                                                                                                              _ge sy 0
                                                                                                                            )
                                                                                                                          )
                                                                                                                           (
                                                                                                                            _lt sy rows
                                                                                                                          )
                                                                                                                        )
                                                                                                                         (
                                                                                                                          begin (
                                                                                                                            list-set! (
                                                                                                                              list-ref out sy
                                                                                                                            )
                                                                                                                             sx (
                                                                                                                              cond (
                                                                                                                                (
                                                                                                                                  string? (
                                                                                                                                    list-ref img y
                                                                                                                                  )
                                                                                                                                )
                                                                                                                                 (
                                                                                                                                  _substring (
                                                                                                                                    list-ref img y
                                                                                                                                  )
                                                                                                                                   x (
                                                                                                                                    + x 1
                                                                                                                                  )
                                                                                                                                )
                                                                                                                              )
                                                                                                                               (
                                                                                                                                (
                                                                                                                                  hash-table? (
                                                                                                                                    list-ref img y
                                                                                                                                  )
                                                                                                                                )
                                                                                                                                 (
                                                                                                                                  hash-table-ref (
                                                                                                                                    list-ref img y
                                                                                                                                  )
                                                                                                                                   x
                                                                                                                                )
                                                                                                                              )
                                                                                                                               (
                                                                                                                                else (
                                                                                                                                  list-ref (
                                                                                                                                    list-ref img y
                                                                                                                                  )
                                                                                                                                   x
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
                                                                                                                        set! x (
                                                                                                                          + x 1
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
                                                                                       (
                                                                                        set! y (
                                                                                          + y 1
                                                                                        )
                                                                                      )
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
                                                                    ret13 out
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
      let (
        (
          img (
            _list (
              _list 1 2 3
            )
             (
              _list 4 5 6
            )
             (
              _list 7 8 9
            )
          )
        )
      )
       (
        begin (
          let (
            (
              pts1 (
                _list (
                  _list 0.0 0.0
                )
                 (
                  _list 2.0 0.0
                )
                 (
                  _list 0.0 2.0
                )
              )
            )
          )
           (
            begin (
              let (
                (
                  pts2 (
                    _list (
                      _list 0.0 2.0
                    )
                     (
                      _list 0.0 0.0
                    )
                     (
                      _list 2.0 2.0
                    )
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      rotated (
                        get_rotation img pts1 pts2 3 3
                      )
                    )
                  )
                   (
                    begin (
                      _display (
                        if (
                          string? (
                            to-str-space rotated
                          )
                        )
                         (
                          to-str-space rotated
                        )
                         (
                          to-str (
                            to-str-space rotated
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
     (
      let (
        (
          end19 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur20 (
              quotient (
                * (
                  - end19 start18
                )
                 1000000
              )
               jps21
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur20
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
