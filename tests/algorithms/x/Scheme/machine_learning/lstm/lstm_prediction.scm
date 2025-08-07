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
        exp_approx x
      )
       (
        call/cc (
          lambda (
            ret1
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
                  exp_approx (
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
        tanh_approx x
      )
       (
        call/cc (
          lambda (
            ret5
          )
           (
            let (
              (
                e (
                  exp_approx (
                    * 2.0 x
                  )
                )
              )
            )
             (
              begin (
                ret5 (
                  _div (
                    - e 1.0
                  )
                   (
                    _add e 1.0
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
        forward seq w
      )
       (
        call/cc (
          lambda (
            ret6
          )
           (
            let (
              (
                i_arr (
                  _list
                )
              )
            )
             (
              begin (
                let (
                  (
                    f_arr (
                      _list
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        o_arr (
                          _list
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            g_arr (
                              _list
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                c_arr (
                                  _list 0.0
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    h_arr (
                                      _list 0.0
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
                                                      < t (
                                                        _len seq
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            x (
                                                              list-ref seq t
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                h_prev (
                                                                  list-ref h_arr t
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                let (
                                                                  (
                                                                    c_prev (
                                                                      list-ref c_arr t
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        i_t (
                                                                          sigmoid (
                                                                            _add (
                                                                              _add (
                                                                                * (
                                                                                  hash-table-ref w "w_i"
                                                                                )
                                                                                 x
                                                                              )
                                                                               (
                                                                                * (
                                                                                  hash-table-ref w "u_i"
                                                                                )
                                                                                 h_prev
                                                                              )
                                                                            )
                                                                             (
                                                                              hash-table-ref w "b_i"
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            f_t (
                                                                              sigmoid (
                                                                                _add (
                                                                                  _add (
                                                                                    * (
                                                                                      hash-table-ref w "w_f"
                                                                                    )
                                                                                     x
                                                                                  )
                                                                                   (
                                                                                    * (
                                                                                      hash-table-ref w "u_f"
                                                                                    )
                                                                                     h_prev
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  hash-table-ref w "b_f"
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                o_t (
                                                                                  sigmoid (
                                                                                    _add (
                                                                                      _add (
                                                                                        * (
                                                                                          hash-table-ref w "w_o"
                                                                                        )
                                                                                         x
                                                                                      )
                                                                                       (
                                                                                        * (
                                                                                          hash-table-ref w "u_o"
                                                                                        )
                                                                                         h_prev
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref w "b_o"
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                let (
                                                                                  (
                                                                                    g_t (
                                                                                      tanh_approx (
                                                                                        _add (
                                                                                          _add (
                                                                                            * (
                                                                                              hash-table-ref w "w_c"
                                                                                            )
                                                                                             x
                                                                                          )
                                                                                           (
                                                                                            * (
                                                                                              hash-table-ref w "u_c"
                                                                                            )
                                                                                             h_prev
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          hash-table-ref w "b_c"
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    let (
                                                                                      (
                                                                                        c_t (
                                                                                          _add (
                                                                                            * f_t c_prev
                                                                                          )
                                                                                           (
                                                                                            * i_t g_t
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        let (
                                                                                          (
                                                                                            h_t (
                                                                                              * o_t (
                                                                                                tanh_approx c_t
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            set! i_arr (
                                                                                              append i_arr (
                                                                                                _list i_t
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            set! f_arr (
                                                                                              append f_arr (
                                                                                                _list f_t
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            set! o_arr (
                                                                                              append o_arr (
                                                                                                _list o_t
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            set! g_arr (
                                                                                              append g_arr (
                                                                                                _list g_t
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            set! c_arr (
                                                                                              append c_arr (
                                                                                                _list c_t
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            set! h_arr (
                                                                                              append h_arr (
                                                                                                _list h_t
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            set! t (
                                                                                              + t 1
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
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
                                        ret6 (
                                          alist->hash-table (
                                            _list (
                                              cons "i" i_arr
                                            )
                                             (
                                              cons "f" f_arr
                                            )
                                             (
                                              cons "o" o_arr
                                            )
                                             (
                                              cons "g" g_arr
                                            )
                                             (
                                              cons "c" c_arr
                                            )
                                             (
                                              cons "h" h_arr
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
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
        backward seq target w s lr
      )
       (
        call/cc (
          lambda (
            ret9
          )
           (
            let (
              (
                dw_i 0.0
              )
            )
             (
              begin (
                let (
                  (
                    du_i 0.0
                  )
                )
                 (
                  begin (
                    let (
                      (
                        db_i 0.0
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            dw_f 0.0
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                du_f 0.0
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    db_f 0.0
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        dw_o 0.0
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            du_o 0.0
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                db_o 0.0
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    dw_c 0.0
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        du_c 0.0
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            db_c 0.0
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                dw_y 0.0
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                let (
                                                                  (
                                                                    db_y 0.0
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        T (
                                                                          _len seq
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            h_last (
                                                                              list-ref (
                                                                                hash-table-ref s "h"
                                                                              )
                                                                               T
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                y (
                                                                                  _add (
                                                                                    * (
                                                                                      hash-table-ref w "w_y"
                                                                                    )
                                                                                     h_last
                                                                                  )
                                                                                   (
                                                                                    hash-table-ref w "b_y"
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                let (
                                                                                  (
                                                                                    dy (
                                                                                      - y target
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    set! dw_y (
                                                                                      * dy h_last
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    set! db_y dy
                                                                                  )
                                                                                   (
                                                                                    let (
                                                                                      (
                                                                                        dh_next (
                                                                                          * dy (
                                                                                            hash-table-ref w "w_y"
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        let (
                                                                                          (
                                                                                            dc_next 0.0
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            let (
                                                                                              (
                                                                                                t (
                                                                                                  - T 1
                                                                                                )
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
                                                                                                              >= t 0
                                                                                                            )
                                                                                                             (
                                                                                                              begin (
                                                                                                                let (
                                                                                                                  (
                                                                                                                    i_t (
                                                                                                                      list-ref (
                                                                                                                        hash-table-ref s "i"
                                                                                                                      )
                                                                                                                       t
                                                                                                                    )
                                                                                                                  )
                                                                                                                )
                                                                                                                 (
                                                                                                                  begin (
                                                                                                                    let (
                                                                                                                      (
                                                                                                                        f_t (
                                                                                                                          list-ref (
                                                                                                                            hash-table-ref s "f"
                                                                                                                          )
                                                                                                                           t
                                                                                                                        )
                                                                                                                      )
                                                                                                                    )
                                                                                                                     (
                                                                                                                      begin (
                                                                                                                        let (
                                                                                                                          (
                                                                                                                            o_t (
                                                                                                                              list-ref (
                                                                                                                                hash-table-ref s "o"
                                                                                                                              )
                                                                                                                               t
                                                                                                                            )
                                                                                                                          )
                                                                                                                        )
                                                                                                                         (
                                                                                                                          begin (
                                                                                                                            let (
                                                                                                                              (
                                                                                                                                g_t (
                                                                                                                                  list-ref (
                                                                                                                                    hash-table-ref s "g"
                                                                                                                                  )
                                                                                                                                   t
                                                                                                                                )
                                                                                                                              )
                                                                                                                            )
                                                                                                                             (
                                                                                                                              begin (
                                                                                                                                let (
                                                                                                                                  (
                                                                                                                                    c_t (
                                                                                                                                      list-ref (
                                                                                                                                        hash-table-ref s "c"
                                                                                                                                      )
                                                                                                                                       (
                                                                                                                                        + t 1
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                )
                                                                                                                                 (
                                                                                                                                  begin (
                                                                                                                                    let (
                                                                                                                                      (
                                                                                                                                        c_prev (
                                                                                                                                          list-ref (
                                                                                                                                            hash-table-ref s "c"
                                                                                                                                          )
                                                                                                                                           t
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      begin (
                                                                                                                                        let (
                                                                                                                                          (
                                                                                                                                            h_prev (
                                                                                                                                              list-ref (
                                                                                                                                                hash-table-ref s "h"
                                                                                                                                              )
                                                                                                                                               t
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                         (
                                                                                                                                          begin (
                                                                                                                                            let (
                                                                                                                                              (
                                                                                                                                                tanh_c (
                                                                                                                                                  tanh_approx c_t
                                                                                                                                                )
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                             (
                                                                                                                                              begin (
                                                                                                                                                let (
                                                                                                                                                  (
                                                                                                                                                    do_t (
                                                                                                                                                      * dh_next tanh_c
                                                                                                                                                    )
                                                                                                                                                  )
                                                                                                                                                )
                                                                                                                                                 (
                                                                                                                                                  begin (
                                                                                                                                                    let (
                                                                                                                                                      (
                                                                                                                                                        da_o (
                                                                                                                                                          * (
                                                                                                                                                            * do_t o_t
                                                                                                                                                          )
                                                                                                                                                           (
                                                                                                                                                            - 1.0 o_t
                                                                                                                                                          )
                                                                                                                                                        )
                                                                                                                                                      )
                                                                                                                                                    )
                                                                                                                                                     (
                                                                                                                                                      begin (
                                                                                                                                                        let (
                                                                                                                                                          (
                                                                                                                                                            dc (
                                                                                                                                                              _add (
                                                                                                                                                                * (
                                                                                                                                                                  * dh_next o_t
                                                                                                                                                                )
                                                                                                                                                                 (
                                                                                                                                                                  - 1.0 (
                                                                                                                                                                    * tanh_c tanh_c
                                                                                                                                                                  )
                                                                                                                                                                )
                                                                                                                                                              )
                                                                                                                                                               dc_next
                                                                                                                                                            )
                                                                                                                                                          )
                                                                                                                                                        )
                                                                                                                                                         (
                                                                                                                                                          begin (
                                                                                                                                                            let (
                                                                                                                                                              (
                                                                                                                                                                di_t (
                                                                                                                                                                  * dc g_t
                                                                                                                                                                )
                                                                                                                                                              )
                                                                                                                                                            )
                                                                                                                                                             (
                                                                                                                                                              begin (
                                                                                                                                                                let (
                                                                                                                                                                  (
                                                                                                                                                                    da_i (
                                                                                                                                                                      * (
                                                                                                                                                                        * di_t i_t
                                                                                                                                                                      )
                                                                                                                                                                       (
                                                                                                                                                                        - 1.0 i_t
                                                                                                                                                                      )
                                                                                                                                                                    )
                                                                                                                                                                  )
                                                                                                                                                                )
                                                                                                                                                                 (
                                                                                                                                                                  begin (
                                                                                                                                                                    let (
                                                                                                                                                                      (
                                                                                                                                                                        dg_t (
                                                                                                                                                                          * dc i_t
                                                                                                                                                                        )
                                                                                                                                                                      )
                                                                                                                                                                    )
                                                                                                                                                                     (
                                                                                                                                                                      begin (
                                                                                                                                                                        let (
                                                                                                                                                                          (
                                                                                                                                                                            da_g (
                                                                                                                                                                              * dg_t (
                                                                                                                                                                                - 1.0 (
                                                                                                                                                                                  * g_t g_t
                                                                                                                                                                                )
                                                                                                                                                                              )
                                                                                                                                                                            )
                                                                                                                                                                          )
                                                                                                                                                                        )
                                                                                                                                                                         (
                                                                                                                                                                          begin (
                                                                                                                                                                            let (
                                                                                                                                                                              (
                                                                                                                                                                                df_t (
                                                                                                                                                                                  * dc c_prev
                                                                                                                                                                                )
                                                                                                                                                                              )
                                                                                                                                                                            )
                                                                                                                                                                             (
                                                                                                                                                                              begin (
                                                                                                                                                                                let (
                                                                                                                                                                                  (
                                                                                                                                                                                    da_f (
                                                                                                                                                                                      * (
                                                                                                                                                                                        * df_t f_t
                                                                                                                                                                                      )
                                                                                                                                                                                       (
                                                                                                                                                                                        - 1.0 f_t
                                                                                                                                                                                      )
                                                                                                                                                                                    )
                                                                                                                                                                                  )
                                                                                                                                                                                )
                                                                                                                                                                                 (
                                                                                                                                                                                  begin (
                                                                                                                                                                                    set! dw_i (
                                                                                                                                                                                      _add dw_i (
                                                                                                                                                                                        * da_i (
                                                                                                                                                                                          list-ref seq t
                                                                                                                                                                                        )
                                                                                                                                                                                      )
                                                                                                                                                                                    )
                                                                                                                                                                                  )
                                                                                                                                                                                   (
                                                                                                                                                                                    set! du_i (
                                                                                                                                                                                      _add du_i (
                                                                                                                                                                                        * da_i h_prev
                                                                                                                                                                                      )
                                                                                                                                                                                    )
                                                                                                                                                                                  )
                                                                                                                                                                                   (
                                                                                                                                                                                    set! db_i (
                                                                                                                                                                                      _add db_i da_i
                                                                                                                                                                                    )
                                                                                                                                                                                  )
                                                                                                                                                                                   (
                                                                                                                                                                                    set! dw_f (
                                                                                                                                                                                      _add dw_f (
                                                                                                                                                                                        * da_f (
                                                                                                                                                                                          list-ref seq t
                                                                                                                                                                                        )
                                                                                                                                                                                      )
                                                                                                                                                                                    )
                                                                                                                                                                                  )
                                                                                                                                                                                   (
                                                                                                                                                                                    set! du_f (
                                                                                                                                                                                      _add du_f (
                                                                                                                                                                                        * da_f h_prev
                                                                                                                                                                                      )
                                                                                                                                                                                    )
                                                                                                                                                                                  )
                                                                                                                                                                                   (
                                                                                                                                                                                    set! db_f (
                                                                                                                                                                                      _add db_f da_f
                                                                                                                                                                                    )
                                                                                                                                                                                  )
                                                                                                                                                                                   (
                                                                                                                                                                                    set! dw_o (
                                                                                                                                                                                      _add dw_o (
                                                                                                                                                                                        * da_o (
                                                                                                                                                                                          list-ref seq t
                                                                                                                                                                                        )
                                                                                                                                                                                      )
                                                                                                                                                                                    )
                                                                                                                                                                                  )
                                                                                                                                                                                   (
                                                                                                                                                                                    set! du_o (
                                                                                                                                                                                      _add du_o (
                                                                                                                                                                                        * da_o h_prev
                                                                                                                                                                                      )
                                                                                                                                                                                    )
                                                                                                                                                                                  )
                                                                                                                                                                                   (
                                                                                                                                                                                    set! db_o (
                                                                                                                                                                                      _add db_o da_o
                                                                                                                                                                                    )
                                                                                                                                                                                  )
                                                                                                                                                                                   (
                                                                                                                                                                                    set! dw_c (
                                                                                                                                                                                      _add dw_c (
                                                                                                                                                                                        * da_g (
                                                                                                                                                                                          list-ref seq t
                                                                                                                                                                                        )
                                                                                                                                                                                      )
                                                                                                                                                                                    )
                                                                                                                                                                                  )
                                                                                                                                                                                   (
                                                                                                                                                                                    set! du_c (
                                                                                                                                                                                      _add du_c (
                                                                                                                                                                                        * da_g h_prev
                                                                                                                                                                                      )
                                                                                                                                                                                    )
                                                                                                                                                                                  )
                                                                                                                                                                                   (
                                                                                                                                                                                    set! db_c (
                                                                                                                                                                                      _add db_c da_g
                                                                                                                                                                                    )
                                                                                                                                                                                  )
                                                                                                                                                                                   (
                                                                                                                                                                                    set! dh_next (
                                                                                                                                                                                      _add (
                                                                                                                                                                                        _add (
                                                                                                                                                                                          _add (
                                                                                                                                                                                            * da_i (
                                                                                                                                                                                              hash-table-ref w "u_i"
                                                                                                                                                                                            )
                                                                                                                                                                                          )
                                                                                                                                                                                           (
                                                                                                                                                                                            * da_f (
                                                                                                                                                                                              hash-table-ref w "u_f"
                                                                                                                                                                                            )
                                                                                                                                                                                          )
                                                                                                                                                                                        )
                                                                                                                                                                                         (
                                                                                                                                                                                          * da_o (
                                                                                                                                                                                            hash-table-ref w "u_o"
                                                                                                                                                                                          )
                                                                                                                                                                                        )
                                                                                                                                                                                      )
                                                                                                                                                                                       (
                                                                                                                                                                                        * da_g (
                                                                                                                                                                                          hash-table-ref w "u_c"
                                                                                                                                                                                        )
                                                                                                                                                                                      )
                                                                                                                                                                                    )
                                                                                                                                                                                  )
                                                                                                                                                                                   (
                                                                                                                                                                                    set! dc_next (
                                                                                                                                                                                      * dc f_t
                                                                                                                                                                                    )
                                                                                                                                                                                  )
                                                                                                                                                                                   (
                                                                                                                                                                                    set! t (
                                                                                                                                                                                      - t 1
                                                                                                                                                                                    )
                                                                                                                                                                                  )
                                                                                                                                                                                )
                                                                                                                                                                              )
                                                                                                                                                                            )
                                                                                                                                                                          )
                                                                                                                                                                        )
                                                                                                                                                                      )
                                                                                                                                                                    )
                                                                                                                                                                  )
                                                                                                                                                                )
                                                                                                                                                              )
                                                                                                                                                            )
                                                                                                                                                          )
                                                                                                                                                        )
                                                                                                                                                      )
                                                                                                                                                    )
                                                                                                                                                  )
                                                                                                                                                )
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                )
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
                                                                                                hash-table-set! w "w_y" (
                                                                                                  - (
                                                                                                    hash-table-ref w "w_y"
                                                                                                  )
                                                                                                   (
                                                                                                    * lr dw_y
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                hash-table-set! w "b_y" (
                                                                                                  - (
                                                                                                    hash-table-ref w "b_y"
                                                                                                  )
                                                                                                   (
                                                                                                    * lr db_y
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                hash-table-set! w "w_i" (
                                                                                                  - (
                                                                                                    hash-table-ref w "w_i"
                                                                                                  )
                                                                                                   (
                                                                                                    * lr dw_i
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                hash-table-set! w "u_i" (
                                                                                                  - (
                                                                                                    hash-table-ref w "u_i"
                                                                                                  )
                                                                                                   (
                                                                                                    * lr du_i
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                hash-table-set! w "b_i" (
                                                                                                  - (
                                                                                                    hash-table-ref w "b_i"
                                                                                                  )
                                                                                                   (
                                                                                                    * lr db_i
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                hash-table-set! w "w_f" (
                                                                                                  - (
                                                                                                    hash-table-ref w "w_f"
                                                                                                  )
                                                                                                   (
                                                                                                    * lr dw_f
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                hash-table-set! w "u_f" (
                                                                                                  - (
                                                                                                    hash-table-ref w "u_f"
                                                                                                  )
                                                                                                   (
                                                                                                    * lr du_f
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                hash-table-set! w "b_f" (
                                                                                                  - (
                                                                                                    hash-table-ref w "b_f"
                                                                                                  )
                                                                                                   (
                                                                                                    * lr db_f
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                hash-table-set! w "w_o" (
                                                                                                  - (
                                                                                                    hash-table-ref w "w_o"
                                                                                                  )
                                                                                                   (
                                                                                                    * lr dw_o
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                hash-table-set! w "u_o" (
                                                                                                  - (
                                                                                                    hash-table-ref w "u_o"
                                                                                                  )
                                                                                                   (
                                                                                                    * lr du_o
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                hash-table-set! w "b_o" (
                                                                                                  - (
                                                                                                    hash-table-ref w "b_o"
                                                                                                  )
                                                                                                   (
                                                                                                    * lr db_o
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                hash-table-set! w "w_c" (
                                                                                                  - (
                                                                                                    hash-table-ref w "w_c"
                                                                                                  )
                                                                                                   (
                                                                                                    * lr dw_c
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                hash-table-set! w "u_c" (
                                                                                                  - (
                                                                                                    hash-table-ref w "u_c"
                                                                                                  )
                                                                                                   (
                                                                                                    * lr du_c
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                hash-table-set! w "b_c" (
                                                                                                  - (
                                                                                                    hash-table-ref w "b_c"
                                                                                                  )
                                                                                                   (
                                                                                                    * lr db_c
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                ret9 w
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
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
        make_samples data look_back
      )
       (
        call/cc (
          lambda (
            ret12
          )
           (
            let (
              (
                X (
                  _list
                )
              )
            )
             (
              begin (
                let (
                  (
                    Y (
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
                                      < (
                                        + i look_back
                                      )
                                       (
                                        _len data
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            seq (
                                              slice data i (
                                                + i look_back
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            set! X (
                                              append X (
                                                _list seq
                                              )
                                            )
                                          )
                                           (
                                            set! Y (
                                              append Y (
                                                _list (
                                                  list-ref data (
                                                    + i look_back
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
                        ret12 (
                          alist->hash-table (
                            _list (
                              cons "x" X
                            )
                             (
                              cons "y" Y
                            )
                          )
                        )
                      )
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
        init_weights
      )
       (
        call/cc (
          lambda (
            ret15
          )
           (
            ret15 (
              alist->hash-table (
                _list (
                  cons "w_i" 0.1
                )
                 (
                  cons "u_i" 0.2
                )
                 (
                  cons "b_i" 0.0
                )
                 (
                  cons "w_f" 0.1
                )
                 (
                  cons "u_f" 0.2
                )
                 (
                  cons "b_f" 0.0
                )
                 (
                  cons "w_o" 0.1
                )
                 (
                  cons "u_o" 0.2
                )
                 (
                  cons "b_o" 0.0
                )
                 (
                  cons "w_c" 0.1
                )
                 (
                  cons "u_c" 0.2
                )
                 (
                  cons "b_c" 0.0
                )
                 (
                  cons "w_y" 0.1
                )
                 (
                  cons "b_y" 0.0
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        train data look_back epochs lr
      )
       (
        call/cc (
          lambda (
            ret16
          )
           (
            let (
              (
                samples (
                  make_samples data look_back
                )
              )
            )
             (
              begin (
                let (
                  (
                    w (
                      init_weights
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        ep 0
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
                                      < ep epochs
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
                                                              hash-table-ref samples "x"
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                seq (
                                                                  cond (
                                                                    (
                                                                      string? (
                                                                        hash-table-ref samples "x"
                                                                      )
                                                                    )
                                                                     (
                                                                      _substring (
                                                                        hash-table-ref samples "x"
                                                                      )
                                                                       j (
                                                                        + j 1
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    (
                                                                      hash-table? (
                                                                        hash-table-ref samples "x"
                                                                      )
                                                                    )
                                                                     (
                                                                      hash-table-ref (
                                                                        hash-table-ref samples "x"
                                                                      )
                                                                       j
                                                                    )
                                                                  )
                                                                   (
                                                                    else (
                                                                      list-ref (
                                                                        hash-table-ref samples "x"
                                                                      )
                                                                       j
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
                                                                      cond (
                                                                        (
                                                                          string? (
                                                                            hash-table-ref samples "y"
                                                                          )
                                                                        )
                                                                         (
                                                                          _substring (
                                                                            hash-table-ref samples "y"
                                                                          )
                                                                           j (
                                                                            + j 1
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        (
                                                                          hash-table? (
                                                                            hash-table-ref samples "y"
                                                                          )
                                                                        )
                                                                         (
                                                                          hash-table-ref (
                                                                            hash-table-ref samples "y"
                                                                          )
                                                                           j
                                                                        )
                                                                      )
                                                                       (
                                                                        else (
                                                                          list-ref (
                                                                            hash-table-ref samples "y"
                                                                          )
                                                                           j
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        state (
                                                                          forward seq w
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        set! w (
                                                                          backward seq target w state lr
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
                                            set! ep (
                                              + ep 1
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
                        ret16 w
                      )
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
        predict seq w
      )
       (
        call/cc (
          lambda (
            ret21
          )
           (
            let (
              (
                state (
                  forward seq w
                )
              )
            )
             (
              begin (
                let (
                  (
                    h_last (
                      cond (
                        (
                          string? (
                            hash-table-ref state "h"
                          )
                        )
                         (
                          _substring (
                            hash-table-ref state "h"
                          )
                           (
                            - (
                              _len (
                                hash-table-ref state "h"
                              )
                            )
                             1
                          )
                           (
                            + (
                              - (
                                _len (
                                  hash-table-ref state "h"
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
                            hash-table-ref state "h"
                          )
                        )
                         (
                          hash-table-ref (
                            hash-table-ref state "h"
                          )
                           (
                            - (
                              _len (
                                hash-table-ref state "h"
                              )
                            )
                             1
                          )
                        )
                      )
                       (
                        else (
                          list-ref (
                            hash-table-ref state "h"
                          )
                           (
                            - (
                              _len (
                                hash-table-ref state "h"
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
                    ret21 (
                      _add (
                        * (
                          hash-table-ref w "w_y"
                        )
                         h_last
                      )
                       (
                        hash-table-ref w "b_y"
                      )
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
          data (
            _list 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8
          )
        )
      )
       (
        begin (
          let (
            (
              look_back 3
            )
          )
           (
            begin (
              let (
                (
                  epochs 200
                )
              )
               (
                begin (
                  let (
                    (
                      lr 0.1
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          w (
                            train data look_back epochs lr
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              test_seq (
                                _list 0.6 0.7 0.8
                              )
                            )
                          )
                           (
                            begin (
                              let (
                                (
                                  pred (
                                    predict test_seq w
                                  )
                                )
                              )
                               (
                                begin (
                                  _display (
                                    if (
                                      string? (
                                        string-append "Predicted value: " (
                                          to-str-space pred
                                        )
                                      )
                                    )
                                     (
                                      string-append "Predicted value: " (
                                        to-str-space pred
                                      )
                                    )
                                     (
                                      to-str (
                                        string-append "Predicted value: " (
                                          to-str-space pred
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
