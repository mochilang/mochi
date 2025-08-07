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
      start14 (
        current-jiffy
      )
    )
     (
      jps17 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        exp_taylor x
      )
       (
        call/cc (
          lambda (
            ret1
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
                        i 1.0
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
                                      < i 20.0
                                    )
                                     (
                                      begin (
                                        set! term (
                                          _div (
                                            * term x
                                          )
                                           i
                                        )
                                      )
                                       (
                                        set! sum (
                                          + sum term
                                        )
                                      )
                                       (
                                        set! i (
                                          + i 1.0
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
     (
      define (
        sigmoid x
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            ret4 (
              _div 1.0 (
                _add 1.0 (
                  exp_taylor (
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
      let (
        (
          X (
            _list (
              _list 0.0 0.0
            )
             (
              _list 1.0 1.0
            )
             (
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
              Y (
                _list 0.0 1.0 0.0 0.0
              )
            )
          )
           (
            begin (
              let (
                (
                  test_data (
                    _list (
                      _list 0.0 0.0
                    )
                     (
                      _list 0.0 1.0
                    )
                     (
                      _list 1.0 1.0
                    )
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      w1 (
                        _list (
                          _list 0.5 (
                            - 0.5
                          )
                        )
                         (
                          _list 0.5 0.5
                        )
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          b1 (
                            _list 0.0 0.0
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              w2 (
                                _list 0.5 (
                                  - 0.5
                                )
                              )
                            )
                          )
                           (
                            begin (
                              let (
                                (
                                  b2 0.0
                                )
                              )
                               (
                                begin (
                                  define (
                                    train epochs lr
                                  )
                                   (
                                    call/cc (
                                      lambda (
                                        ret5
                                      )
                                       (
                                        let (
                                          (
                                            e 0
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
                                                          < e epochs
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
                                                                                _len X
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                let (
                                                                                  (
                                                                                    x0 (
                                                                                      cond (
                                                                                        (
                                                                                          string? (
                                                                                            list-ref X i
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          _substring (
                                                                                            list-ref X i
                                                                                          )
                                                                                           0 (
                                                                                            + 0 1
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        (
                                                                                          hash-table? (
                                                                                            list-ref X i
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          hash-table-ref (
                                                                                            list-ref X i
                                                                                          )
                                                                                           0
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        else (
                                                                                          list-ref (
                                                                                            list-ref X i
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
                                                                                        x1 (
                                                                                          cond (
                                                                                            (
                                                                                              string? (
                                                                                                list-ref X i
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              _substring (
                                                                                                list-ref X i
                                                                                              )
                                                                                               1 (
                                                                                                + 1 1
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            (
                                                                                              hash-table? (
                                                                                                list-ref X i
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              hash-table-ref (
                                                                                                list-ref X i
                                                                                              )
                                                                                               1
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            else (
                                                                                              list-ref (
                                                                                                list-ref X i
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
                                                                                            target (
                                                                                              list-ref Y i
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            let (
                                                                                              (
                                                                                                z1 (
                                                                                                  _add (
                                                                                                    _add (
                                                                                                      * (
                                                                                                        cond (
                                                                                                          (
                                                                                                            string? (
                                                                                                              list-ref w1 0
                                                                                                            )
                                                                                                          )
                                                                                                           (
                                                                                                            _substring (
                                                                                                              list-ref w1 0
                                                                                                            )
                                                                                                             0 (
                                                                                                              + 0 1
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          (
                                                                                                            hash-table? (
                                                                                                              list-ref w1 0
                                                                                                            )
                                                                                                          )
                                                                                                           (
                                                                                                            hash-table-ref (
                                                                                                              list-ref w1 0
                                                                                                            )
                                                                                                             0
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          else (
                                                                                                            list-ref (
                                                                                                              list-ref w1 0
                                                                                                            )
                                                                                                             0
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       x0
                                                                                                    )
                                                                                                     (
                                                                                                      * (
                                                                                                        cond (
                                                                                                          (
                                                                                                            string? (
                                                                                                              list-ref w1 1
                                                                                                            )
                                                                                                          )
                                                                                                           (
                                                                                                            _substring (
                                                                                                              list-ref w1 1
                                                                                                            )
                                                                                                             0 (
                                                                                                              + 0 1
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          (
                                                                                                            hash-table? (
                                                                                                              list-ref w1 1
                                                                                                            )
                                                                                                          )
                                                                                                           (
                                                                                                            hash-table-ref (
                                                                                                              list-ref w1 1
                                                                                                            )
                                                                                                             0
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          else (
                                                                                                            list-ref (
                                                                                                              list-ref w1 1
                                                                                                            )
                                                                                                             0
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       x1
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    list-ref b1 0
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                let (
                                                                                                  (
                                                                                                    z2 (
                                                                                                      _add (
                                                                                                        _add (
                                                                                                          * (
                                                                                                            cond (
                                                                                                              (
                                                                                                                string? (
                                                                                                                  list-ref w1 0
                                                                                                                )
                                                                                                              )
                                                                                                               (
                                                                                                                _substring (
                                                                                                                  list-ref w1 0
                                                                                                                )
                                                                                                                 1 (
                                                                                                                  + 1 1
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              (
                                                                                                                hash-table? (
                                                                                                                  list-ref w1 0
                                                                                                                )
                                                                                                              )
                                                                                                               (
                                                                                                                hash-table-ref (
                                                                                                                  list-ref w1 0
                                                                                                                )
                                                                                                                 1
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              else (
                                                                                                                list-ref (
                                                                                                                  list-ref w1 0
                                                                                                                )
                                                                                                                 1
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                           x0
                                                                                                        )
                                                                                                         (
                                                                                                          * (
                                                                                                            cond (
                                                                                                              (
                                                                                                                string? (
                                                                                                                  list-ref w1 1
                                                                                                                )
                                                                                                              )
                                                                                                               (
                                                                                                                _substring (
                                                                                                                  list-ref w1 1
                                                                                                                )
                                                                                                                 1 (
                                                                                                                  + 1 1
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              (
                                                                                                                hash-table? (
                                                                                                                  list-ref w1 1
                                                                                                                )
                                                                                                              )
                                                                                                               (
                                                                                                                hash-table-ref (
                                                                                                                  list-ref w1 1
                                                                                                                )
                                                                                                                 1
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              else (
                                                                                                                list-ref (
                                                                                                                  list-ref w1 1
                                                                                                                )
                                                                                                                 1
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                           x1
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        list-ref b1 1
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  begin (
                                                                                                    let (
                                                                                                      (
                                                                                                        h1 (
                                                                                                          sigmoid z1
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      begin (
                                                                                                        let (
                                                                                                          (
                                                                                                            h2 (
                                                                                                              sigmoid z2
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          begin (
                                                                                                            let (
                                                                                                              (
                                                                                                                z3 (
                                                                                                                  _add (
                                                                                                                    _add (
                                                                                                                      * (
                                                                                                                        list-ref w2 0
                                                                                                                      )
                                                                                                                       h1
                                                                                                                    )
                                                                                                                     (
                                                                                                                      * (
                                                                                                                        list-ref w2 1
                                                                                                                      )
                                                                                                                       h2
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
                                                                                                                    out (
                                                                                                                      sigmoid z3
                                                                                                                    )
                                                                                                                  )
                                                                                                                )
                                                                                                                 (
                                                                                                                  begin (
                                                                                                                    let (
                                                                                                                      (
                                                                                                                        error (
                                                                                                                          - out target
                                                                                                                        )
                                                                                                                      )
                                                                                                                    )
                                                                                                                     (
                                                                                                                      begin (
                                                                                                                        let (
                                                                                                                          (
                                                                                                                            d1 (
                                                                                                                              * (
                                                                                                                                * (
                                                                                                                                  * h1 (
                                                                                                                                    - 1.0 h1
                                                                                                                                  )
                                                                                                                                )
                                                                                                                                 (
                                                                                                                                  list-ref w2 0
                                                                                                                                )
                                                                                                                              )
                                                                                                                               error
                                                                                                                            )
                                                                                                                          )
                                                                                                                        )
                                                                                                                         (
                                                                                                                          begin (
                                                                                                                            let (
                                                                                                                              (
                                                                                                                                d2 (
                                                                                                                                  * (
                                                                                                                                    * (
                                                                                                                                      * h2 (
                                                                                                                                        - 1.0 h2
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      list-ref w2 1
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   error
                                                                                                                                )
                                                                                                                              )
                                                                                                                            )
                                                                                                                             (
                                                                                                                              begin (
                                                                                                                                list-set! w2 0 (
                                                                                                                                  - (
                                                                                                                                    list-ref w2 0
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    * (
                                                                                                                                      * lr error
                                                                                                                                    )
                                                                                                                                     h1
                                                                                                                                  )
                                                                                                                                )
                                                                                                                              )
                                                                                                                               (
                                                                                                                                list-set! w2 1 (
                                                                                                                                  - (
                                                                                                                                    list-ref w2 1
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    * (
                                                                                                                                      * lr error
                                                                                                                                    )
                                                                                                                                     h2
                                                                                                                                  )
                                                                                                                                )
                                                                                                                              )
                                                                                                                               (
                                                                                                                                set! b2 (
                                                                                                                                  - b2 (
                                                                                                                                    * lr error
                                                                                                                                  )
                                                                                                                                )
                                                                                                                              )
                                                                                                                               (
                                                                                                                                list-set! (
                                                                                                                                  list-ref w1 0
                                                                                                                                )
                                                                                                                                 0 (
                                                                                                                                  - (
                                                                                                                                    cond (
                                                                                                                                      (
                                                                                                                                        string? (
                                                                                                                                          list-ref w1 0
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                       (
                                                                                                                                        _substring (
                                                                                                                                          list-ref w1 0
                                                                                                                                        )
                                                                                                                                         0 (
                                                                                                                                          + 0 1
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      (
                                                                                                                                        hash-table? (
                                                                                                                                          list-ref w1 0
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                       (
                                                                                                                                        hash-table-ref (
                                                                                                                                          list-ref w1 0
                                                                                                                                        )
                                                                                                                                         0
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      else (
                                                                                                                                        list-ref (
                                                                                                                                          list-ref w1 0
                                                                                                                                        )
                                                                                                                                         0
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    * (
                                                                                                                                      * lr d1
                                                                                                                                    )
                                                                                                                                     x0
                                                                                                                                  )
                                                                                                                                )
                                                                                                                              )
                                                                                                                               (
                                                                                                                                list-set! (
                                                                                                                                  list-ref w1 1
                                                                                                                                )
                                                                                                                                 0 (
                                                                                                                                  - (
                                                                                                                                    cond (
                                                                                                                                      (
                                                                                                                                        string? (
                                                                                                                                          list-ref w1 1
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                       (
                                                                                                                                        _substring (
                                                                                                                                          list-ref w1 1
                                                                                                                                        )
                                                                                                                                         0 (
                                                                                                                                          + 0 1
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      (
                                                                                                                                        hash-table? (
                                                                                                                                          list-ref w1 1
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                       (
                                                                                                                                        hash-table-ref (
                                                                                                                                          list-ref w1 1
                                                                                                                                        )
                                                                                                                                         0
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      else (
                                                                                                                                        list-ref (
                                                                                                                                          list-ref w1 1
                                                                                                                                        )
                                                                                                                                         0
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    * (
                                                                                                                                      * lr d1
                                                                                                                                    )
                                                                                                                                     x1
                                                                                                                                  )
                                                                                                                                )
                                                                                                                              )
                                                                                                                               (
                                                                                                                                list-set! b1 0 (
                                                                                                                                  - (
                                                                                                                                    list-ref b1 0
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    * lr d1
                                                                                                                                  )
                                                                                                                                )
                                                                                                                              )
                                                                                                                               (
                                                                                                                                list-set! (
                                                                                                                                  list-ref w1 0
                                                                                                                                )
                                                                                                                                 1 (
                                                                                                                                  - (
                                                                                                                                    cond (
                                                                                                                                      (
                                                                                                                                        string? (
                                                                                                                                          list-ref w1 0
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                       (
                                                                                                                                        _substring (
                                                                                                                                          list-ref w1 0
                                                                                                                                        )
                                                                                                                                         1 (
                                                                                                                                          + 1 1
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      (
                                                                                                                                        hash-table? (
                                                                                                                                          list-ref w1 0
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                       (
                                                                                                                                        hash-table-ref (
                                                                                                                                          list-ref w1 0
                                                                                                                                        )
                                                                                                                                         1
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      else (
                                                                                                                                        list-ref (
                                                                                                                                          list-ref w1 0
                                                                                                                                        )
                                                                                                                                         1
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    * (
                                                                                                                                      * lr d2
                                                                                                                                    )
                                                                                                                                     x0
                                                                                                                                  )
                                                                                                                                )
                                                                                                                              )
                                                                                                                               (
                                                                                                                                list-set! (
                                                                                                                                  list-ref w1 1
                                                                                                                                )
                                                                                                                                 1 (
                                                                                                                                  - (
                                                                                                                                    cond (
                                                                                                                                      (
                                                                                                                                        string? (
                                                                                                                                          list-ref w1 1
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                       (
                                                                                                                                        _substring (
                                                                                                                                          list-ref w1 1
                                                                                                                                        )
                                                                                                                                         1 (
                                                                                                                                          + 1 1
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      (
                                                                                                                                        hash-table? (
                                                                                                                                          list-ref w1 1
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                       (
                                                                                                                                        hash-table-ref (
                                                                                                                                          list-ref w1 1
                                                                                                                                        )
                                                                                                                                         1
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      else (
                                                                                                                                        list-ref (
                                                                                                                                          list-ref w1 1
                                                                                                                                        )
                                                                                                                                         1
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    * (
                                                                                                                                      * lr d2
                                                                                                                                    )
                                                                                                                                     x1
                                                                                                                                  )
                                                                                                                                )
                                                                                                                              )
                                                                                                                               (
                                                                                                                                list-set! b1 1 (
                                                                                                                                  - (
                                                                                                                                    list-ref b1 1
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    * lr d2
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
                                                                set! e (
                                                                  + e 1
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
                                        )
                                      )
                                    )
                                  )
                                )
                                 (
                                  define (
                                    predict samples
                                  )
                                   (
                                    call/cc (
                                      lambda (
                                        ret10
                                      )
                                       (
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
                                                              < i (
                                                                _len samples
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                let (
                                                                  (
                                                                    x0 (
                                                                      cond (
                                                                        (
                                                                          string? (
                                                                            list-ref samples i
                                                                          )
                                                                        )
                                                                         (
                                                                          _substring (
                                                                            list-ref samples i
                                                                          )
                                                                           0 (
                                                                            + 0 1
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        (
                                                                          hash-table? (
                                                                            list-ref samples i
                                                                          )
                                                                        )
                                                                         (
                                                                          hash-table-ref (
                                                                            list-ref samples i
                                                                          )
                                                                           0
                                                                        )
                                                                      )
                                                                       (
                                                                        else (
                                                                          list-ref (
                                                                            list-ref samples i
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
                                                                        x1 (
                                                                          cond (
                                                                            (
                                                                              string? (
                                                                                list-ref samples i
                                                                              )
                                                                            )
                                                                             (
                                                                              _substring (
                                                                                list-ref samples i
                                                                              )
                                                                               1 (
                                                                                + 1 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            (
                                                                              hash-table? (
                                                                                list-ref samples i
                                                                              )
                                                                            )
                                                                             (
                                                                              hash-table-ref (
                                                                                list-ref samples i
                                                                              )
                                                                               1
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref (
                                                                                list-ref samples i
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
                                                                            z1 (
                                                                              _add (
                                                                                _add (
                                                                                  * (
                                                                                    cond (
                                                                                      (
                                                                                        string? (
                                                                                          list-ref w1 0
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        _substring (
                                                                                          list-ref w1 0
                                                                                        )
                                                                                         0 (
                                                                                          + 0 1
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      (
                                                                                        hash-table? (
                                                                                          list-ref w1 0
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        hash-table-ref (
                                                                                          list-ref w1 0
                                                                                        )
                                                                                         0
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      else (
                                                                                        list-ref (
                                                                                          list-ref w1 0
                                                                                        )
                                                                                         0
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   x0
                                                                                )
                                                                                 (
                                                                                  * (
                                                                                    cond (
                                                                                      (
                                                                                        string? (
                                                                                          list-ref w1 1
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        _substring (
                                                                                          list-ref w1 1
                                                                                        )
                                                                                         0 (
                                                                                          + 0 1
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      (
                                                                                        hash-table? (
                                                                                          list-ref w1 1
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        hash-table-ref (
                                                                                          list-ref w1 1
                                                                                        )
                                                                                         0
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      else (
                                                                                        list-ref (
                                                                                          list-ref w1 1
                                                                                        )
                                                                                         0
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   x1
                                                                                )
                                                                              )
                                                                               (
                                                                                list-ref b1 0
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                z2 (
                                                                                  _add (
                                                                                    _add (
                                                                                      * (
                                                                                        cond (
                                                                                          (
                                                                                            string? (
                                                                                              list-ref w1 0
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            _substring (
                                                                                              list-ref w1 0
                                                                                            )
                                                                                             1 (
                                                                                              + 1 1
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          (
                                                                                            hash-table? (
                                                                                              list-ref w1 0
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            hash-table-ref (
                                                                                              list-ref w1 0
                                                                                            )
                                                                                             1
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          else (
                                                                                            list-ref (
                                                                                              list-ref w1 0
                                                                                            )
                                                                                             1
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       x0
                                                                                    )
                                                                                     (
                                                                                      * (
                                                                                        cond (
                                                                                          (
                                                                                            string? (
                                                                                              list-ref w1 1
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            _substring (
                                                                                              list-ref w1 1
                                                                                            )
                                                                                             1 (
                                                                                              + 1 1
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          (
                                                                                            hash-table? (
                                                                                              list-ref w1 1
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            hash-table-ref (
                                                                                              list-ref w1 1
                                                                                            )
                                                                                             1
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          else (
                                                                                            list-ref (
                                                                                              list-ref w1 1
                                                                                            )
                                                                                             1
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       x1
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    list-ref b1 1
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                let (
                                                                                  (
                                                                                    h1 (
                                                                                      sigmoid z1
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    let (
                                                                                      (
                                                                                        h2 (
                                                                                          sigmoid z2
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        let (
                                                                                          (
                                                                                            z3 (
                                                                                              _add (
                                                                                                _add (
                                                                                                  * (
                                                                                                    list-ref w2 0
                                                                                                  )
                                                                                                   h1
                                                                                                )
                                                                                                 (
                                                                                                  * (
                                                                                                    list-ref w2 1
                                                                                                  )
                                                                                                   h2
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
                                                                                                out (
                                                                                                  sigmoid z3
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                let (
                                                                                                  (
                                                                                                    label 0
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  begin (
                                                                                                    if (
                                                                                                      _ge out 0.5
                                                                                                    )
                                                                                                     (
                                                                                                      begin (
                                                                                                        set! label 1
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      quote (
                                                                                                        
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    set! preds (
                                                                                                      append preds (
                                                                                                        _list label
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
                                                                            )
                                                                          )
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
                                                ret10 preds
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
                                    wrapper y
                                  )
                                   (
                                    call/cc (
                                      lambda (
                                        ret13
                                      )
                                       (
                                        ret13 y
                                      )
                                    )
                                  )
                                )
                                 (
                                  train 4000 0.5
                                )
                                 (
                                  let (
                                    (
                                      preds (
                                        wrapper (
                                          predict test_data
                                        )
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      _display (
                                        if (
                                          string? (
                                            to-str-space preds
                                          )
                                        )
                                         (
                                          to-str-space preds
                                        )
                                         (
                                          to-str (
                                            to-str-space preds
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
            )
          )
        )
      )
    )
     (
      let (
        (
          end15 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur16 (
              quotient (
                * (
                  - end15 start14
                )
                 1000000
              )
               jps17
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur16
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
