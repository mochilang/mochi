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
        get_greyscale blue green red
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                b (
                  + 0.0 blue
                )
              )
            )
             (
              begin (
                let (
                  (
                    g (
                      + 0.0 green
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        r (
                          + 0.0 red
                        )
                      )
                    )
                     (
                      begin (
                        ret1 (
                          let (
                            (
                              v2 (
                                _add (
                                  _add (
                                    * 0.114 b
                                  )
                                   (
                                    * 0.587 g
                                  )
                                )
                                 (
                                  * 0.299 r
                                )
                              )
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
        zeros h w
      )
       (
        call/cc (
          lambda (
            ret3
          )
           (
            let (
              (
                table (
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
                                  < i h
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
                                                          < j w
                                                        )
                                                         (
                                                          begin (
                                                            set! row (
                                                              append row (
                                                                _list 0
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
                                            set! table (
                                              append table (
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
                    ret3 table
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
        burkes_dither img threshold
      )
       (
        call/cc (
          lambda (
            ret8
          )
           (
            let (
              (
                height (
                  _len img
                )
              )
            )
             (
              begin (
                let (
                  (
                    width (
                      _len (
                        list-ref img 0
                      )
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        error_table (
                          zeros (
                            + height 1
                          )
                           (
                            + width 4
                          )
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            output (
                              _list
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
                                              < y height
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
                                                        x 0
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
                                                                      < x width
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            px (
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
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                grey (
                                                                                  get_greyscale (
                                                                                    list-ref px 0
                                                                                  )
                                                                                   (
                                                                                    list-ref px 1
                                                                                  )
                                                                                   (
                                                                                    list-ref px 2
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                let (
                                                                                  (
                                                                                    total (
                                                                                      _add grey (
                                                                                        cond (
                                                                                          (
                                                                                            string? (
                                                                                              cond (
                                                                                                (
                                                                                                  string? error_table
                                                                                                )
                                                                                                 (
                                                                                                  _substring error_table y (
                                                                                                    + y 1
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                (
                                                                                                  hash-table? error_table
                                                                                                )
                                                                                                 (
                                                                                                  hash-table-ref error_table y
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                else (
                                                                                                  list-ref error_table y
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            _substring (
                                                                                              cond (
                                                                                                (
                                                                                                  string? error_table
                                                                                                )
                                                                                                 (
                                                                                                  _substring error_table y (
                                                                                                    + y 1
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                (
                                                                                                  hash-table? error_table
                                                                                                )
                                                                                                 (
                                                                                                  hash-table-ref error_table y
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                else (
                                                                                                  list-ref error_table y
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              + x 2
                                                                                            )
                                                                                             (
                                                                                              + (
                                                                                                + x 2
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
                                                                                                  string? error_table
                                                                                                )
                                                                                                 (
                                                                                                  _substring error_table y (
                                                                                                    + y 1
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                (
                                                                                                  hash-table? error_table
                                                                                                )
                                                                                                 (
                                                                                                  hash-table-ref error_table y
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                else (
                                                                                                  list-ref error_table y
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            hash-table-ref (
                                                                                              cond (
                                                                                                (
                                                                                                  string? error_table
                                                                                                )
                                                                                                 (
                                                                                                  _substring error_table y (
                                                                                                    + y 1
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                (
                                                                                                  hash-table? error_table
                                                                                                )
                                                                                                 (
                                                                                                  hash-table-ref error_table y
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                else (
                                                                                                  list-ref error_table y
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              + x 2
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          else (
                                                                                            list-ref (
                                                                                              cond (
                                                                                                (
                                                                                                  string? error_table
                                                                                                )
                                                                                                 (
                                                                                                  _substring error_table y (
                                                                                                    + y 1
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                (
                                                                                                  hash-table? error_table
                                                                                                )
                                                                                                 (
                                                                                                  hash-table-ref error_table y
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                else (
                                                                                                  list-ref error_table y
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              + x 2
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
                                                                                        new_val 0
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        let (
                                                                                          (
                                                                                            current_error 0
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            if (
                                                                                              _gt threshold total
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                set! new_val 0
                                                                                              )
                                                                                               (
                                                                                                set! current_error total
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                set! new_val 255
                                                                                              )
                                                                                               (
                                                                                                set! current_error (
                                                                                                  - total 255
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            set! row (
                                                                                              append row (
                                                                                                _list new_val
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            list-set! (
                                                                                              list-ref error_table y
                                                                                            )
                                                                                             (
                                                                                              + x 3
                                                                                            )
                                                                                             (
                                                                                              _add (
                                                                                                cond (
                                                                                                  (
                                                                                                    string? (
                                                                                                      cond (
                                                                                                        (
                                                                                                          string? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          _substring error_table y (
                                                                                                            + y 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref error_table y
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref error_table y
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    _substring (
                                                                                                      cond (
                                                                                                        (
                                                                                                          string? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          _substring error_table y (
                                                                                                            + y 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref error_table y
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref error_table y
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      + x 3
                                                                                                    )
                                                                                                     (
                                                                                                      + (
                                                                                                        + x 3
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
                                                                                                          string? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          _substring error_table y (
                                                                                                            + y 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref error_table y
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref error_table y
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    hash-table-ref (
                                                                                                      cond (
                                                                                                        (
                                                                                                          string? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          _substring error_table y (
                                                                                                            + y 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref error_table y
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref error_table y
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      + x 3
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  else (
                                                                                                    list-ref (
                                                                                                      cond (
                                                                                                        (
                                                                                                          string? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          _substring error_table y (
                                                                                                            + y 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref error_table y
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref error_table y
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      + x 3
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                _div (
                                                                                                  * 8 current_error
                                                                                                )
                                                                                                 32
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            list-set! (
                                                                                              list-ref error_table y
                                                                                            )
                                                                                             (
                                                                                              + x 4
                                                                                            )
                                                                                             (
                                                                                              _add (
                                                                                                cond (
                                                                                                  (
                                                                                                    string? (
                                                                                                      cond (
                                                                                                        (
                                                                                                          string? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          _substring error_table y (
                                                                                                            + y 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref error_table y
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref error_table y
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    _substring (
                                                                                                      cond (
                                                                                                        (
                                                                                                          string? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          _substring error_table y (
                                                                                                            + y 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref error_table y
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref error_table y
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      + x 4
                                                                                                    )
                                                                                                     (
                                                                                                      + (
                                                                                                        + x 4
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
                                                                                                          string? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          _substring error_table y (
                                                                                                            + y 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref error_table y
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref error_table y
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    hash-table-ref (
                                                                                                      cond (
                                                                                                        (
                                                                                                          string? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          _substring error_table y (
                                                                                                            + y 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref error_table y
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref error_table y
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      + x 4
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  else (
                                                                                                    list-ref (
                                                                                                      cond (
                                                                                                        (
                                                                                                          string? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          _substring error_table y (
                                                                                                            + y 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref error_table y
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref error_table y
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      + x 4
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                _div (
                                                                                                  * 4 current_error
                                                                                                )
                                                                                                 32
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            list-set! (
                                                                                              list-ref error_table (
                                                                                                + y 1
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              + x 2
                                                                                            )
                                                                                             (
                                                                                              _add (
                                                                                                cond (
                                                                                                  (
                                                                                                    string? (
                                                                                                      cond (
                                                                                                        (
                                                                                                          string? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          _substring error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                           (
                                                                                                            + (
                                                                                                              + y 1
                                                                                                            )
                                                                                                             1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    _substring (
                                                                                                      cond (
                                                                                                        (
                                                                                                          string? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          _substring error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                           (
                                                                                                            + (
                                                                                                              + y 1
                                                                                                            )
                                                                                                             1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      + x 2
                                                                                                    )
                                                                                                     (
                                                                                                      + (
                                                                                                        + x 2
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
                                                                                                          string? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          _substring error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                           (
                                                                                                            + (
                                                                                                              + y 1
                                                                                                            )
                                                                                                             1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    hash-table-ref (
                                                                                                      cond (
                                                                                                        (
                                                                                                          string? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          _substring error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                           (
                                                                                                            + (
                                                                                                              + y 1
                                                                                                            )
                                                                                                             1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      + x 2
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  else (
                                                                                                    list-ref (
                                                                                                      cond (
                                                                                                        (
                                                                                                          string? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          _substring error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                           (
                                                                                                            + (
                                                                                                              + y 1
                                                                                                            )
                                                                                                             1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      + x 2
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                _div (
                                                                                                  * 8 current_error
                                                                                                )
                                                                                                 32
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            list-set! (
                                                                                              list-ref error_table (
                                                                                                + y 1
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              + x 3
                                                                                            )
                                                                                             (
                                                                                              _add (
                                                                                                cond (
                                                                                                  (
                                                                                                    string? (
                                                                                                      cond (
                                                                                                        (
                                                                                                          string? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          _substring error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                           (
                                                                                                            + (
                                                                                                              + y 1
                                                                                                            )
                                                                                                             1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    _substring (
                                                                                                      cond (
                                                                                                        (
                                                                                                          string? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          _substring error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                           (
                                                                                                            + (
                                                                                                              + y 1
                                                                                                            )
                                                                                                             1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      + x 3
                                                                                                    )
                                                                                                     (
                                                                                                      + (
                                                                                                        + x 3
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
                                                                                                          string? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          _substring error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                           (
                                                                                                            + (
                                                                                                              + y 1
                                                                                                            )
                                                                                                             1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    hash-table-ref (
                                                                                                      cond (
                                                                                                        (
                                                                                                          string? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          _substring error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                           (
                                                                                                            + (
                                                                                                              + y 1
                                                                                                            )
                                                                                                             1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      + x 3
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  else (
                                                                                                    list-ref (
                                                                                                      cond (
                                                                                                        (
                                                                                                          string? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          _substring error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                           (
                                                                                                            + (
                                                                                                              + y 1
                                                                                                            )
                                                                                                             1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      + x 3
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                _div (
                                                                                                  * 4 current_error
                                                                                                )
                                                                                                 32
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            list-set! (
                                                                                              list-ref error_table (
                                                                                                + y 1
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              + x 4
                                                                                            )
                                                                                             (
                                                                                              _add (
                                                                                                cond (
                                                                                                  (
                                                                                                    string? (
                                                                                                      cond (
                                                                                                        (
                                                                                                          string? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          _substring error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                           (
                                                                                                            + (
                                                                                                              + y 1
                                                                                                            )
                                                                                                             1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    _substring (
                                                                                                      cond (
                                                                                                        (
                                                                                                          string? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          _substring error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                           (
                                                                                                            + (
                                                                                                              + y 1
                                                                                                            )
                                                                                                             1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      + x 4
                                                                                                    )
                                                                                                     (
                                                                                                      + (
                                                                                                        + x 4
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
                                                                                                          string? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          _substring error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                           (
                                                                                                            + (
                                                                                                              + y 1
                                                                                                            )
                                                                                                             1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    hash-table-ref (
                                                                                                      cond (
                                                                                                        (
                                                                                                          string? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          _substring error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                           (
                                                                                                            + (
                                                                                                              + y 1
                                                                                                            )
                                                                                                             1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      + x 4
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  else (
                                                                                                    list-ref (
                                                                                                      cond (
                                                                                                        (
                                                                                                          string? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          _substring error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                           (
                                                                                                            + (
                                                                                                              + y 1
                                                                                                            )
                                                                                                             1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      + x 4
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                _div (
                                                                                                  * 2 current_error
                                                                                                )
                                                                                                 32
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            list-set! (
                                                                                              list-ref error_table (
                                                                                                + y 1
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              + x 1
                                                                                            )
                                                                                             (
                                                                                              _add (
                                                                                                cond (
                                                                                                  (
                                                                                                    string? (
                                                                                                      cond (
                                                                                                        (
                                                                                                          string? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          _substring error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                           (
                                                                                                            + (
                                                                                                              + y 1
                                                                                                            )
                                                                                                             1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    _substring (
                                                                                                      cond (
                                                                                                        (
                                                                                                          string? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          _substring error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                           (
                                                                                                            + (
                                                                                                              + y 1
                                                                                                            )
                                                                                                             1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      + x 1
                                                                                                    )
                                                                                                     (
                                                                                                      + (
                                                                                                        + x 1
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
                                                                                                          string? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          _substring error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                           (
                                                                                                            + (
                                                                                                              + y 1
                                                                                                            )
                                                                                                             1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    hash-table-ref (
                                                                                                      cond (
                                                                                                        (
                                                                                                          string? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          _substring error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                           (
                                                                                                            + (
                                                                                                              + y 1
                                                                                                            )
                                                                                                             1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      + x 1
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  else (
                                                                                                    list-ref (
                                                                                                      cond (
                                                                                                        (
                                                                                                          string? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          _substring error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                           (
                                                                                                            + (
                                                                                                              + y 1
                                                                                                            )
                                                                                                             1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      + x 1
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                _div (
                                                                                                  * 4 current_error
                                                                                                )
                                                                                                 32
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            list-set! (
                                                                                              list-ref error_table (
                                                                                                + y 1
                                                                                              )
                                                                                            )
                                                                                             x (
                                                                                              _add (
                                                                                                cond (
                                                                                                  (
                                                                                                    string? (
                                                                                                      cond (
                                                                                                        (
                                                                                                          string? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          _substring error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                           (
                                                                                                            + (
                                                                                                              + y 1
                                                                                                            )
                                                                                                             1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    _substring (
                                                                                                      cond (
                                                                                                        (
                                                                                                          string? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          _substring error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                           (
                                                                                                            + (
                                                                                                              + y 1
                                                                                                            )
                                                                                                             1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     x (
                                                                                                      + x 1
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  (
                                                                                                    hash-table? (
                                                                                                      cond (
                                                                                                        (
                                                                                                          string? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          _substring error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                           (
                                                                                                            + (
                                                                                                              + y 1
                                                                                                            )
                                                                                                             1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    hash-table-ref (
                                                                                                      cond (
                                                                                                        (
                                                                                                          string? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          _substring error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                           (
                                                                                                            + (
                                                                                                              + y 1
                                                                                                            )
                                                                                                             1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     x
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  else (
                                                                                                    list-ref (
                                                                                                      cond (
                                                                                                        (
                                                                                                          string? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          _substring error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                           (
                                                                                                            + (
                                                                                                              + y 1
                                                                                                            )
                                                                                                             1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? error_table
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref error_table (
                                                                                                            + y 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     x
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                _div (
                                                                                                  * 2 current_error
                                                                                                )
                                                                                                 32
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
                                                        set! output (
                                                          append output (
                                                            _list row
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
                                ret8 output
                              )
                            )
                          )
                        )
                      )
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
            ret13
          )
           (
            let (
              (
                img (
                  _list (
                    _list (
                      _list 0 0 0
                    )
                     (
                      _list 64 64 64
                    )
                     (
                      _list 128 128 128
                    )
                     (
                      _list 192 192 192
                    )
                  )
                   (
                    _list (
                      _list 255 255 255
                    )
                     (
                      _list 200 200 200
                    )
                     (
                      _list 150 150 150
                    )
                     (
                      _list 100 100 100
                    )
                  )
                   (
                    _list (
                      _list 30 144 255
                    )
                     (
                      _list 255 0 0
                    )
                     (
                      _list 0 255 0
                    )
                     (
                      _list 0 0 255
                    )
                  )
                   (
                    _list (
                      _list 50 100 150
                    )
                     (
                      _list 80 160 240
                    )
                     (
                      _list 70 140 210
                    )
                     (
                      _list 60 120 180
                    )
                  )
                )
              )
            )
             (
              begin (
                let (
                  (
                    result (
                      burkes_dither img 128
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
                                      < y (
                                        _len result
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            line ""
                                          )
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
                                                              < x (
                                                                _len (
                                                                  cond (
                                                                    (
                                                                      string? result
                                                                    )
                                                                     (
                                                                      _substring result y (
                                                                        + y 1
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    (
                                                                      hash-table? result
                                                                    )
                                                                     (
                                                                      hash-table-ref result y
                                                                    )
                                                                  )
                                                                   (
                                                                    else (
                                                                      list-ref result y
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                set! line (
                                                                  string-append line (
                                                                    to-str-space (
                                                                      cond (
                                                                        (
                                                                          string? (
                                                                            cond (
                                                                              (
                                                                                string? result
                                                                              )
                                                                               (
                                                                                _substring result y (
                                                                                  + y 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? result
                                                                              )
                                                                               (
                                                                                hash-table-ref result y
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref result y
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          _substring (
                                                                            cond (
                                                                              (
                                                                                string? result
                                                                              )
                                                                               (
                                                                                _substring result y (
                                                                                  + y 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? result
                                                                              )
                                                                               (
                                                                                hash-table-ref result y
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref result y
                                                                              )
                                                                            )
                                                                          )
                                                                           x (
                                                                            + x 1
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        (
                                                                          hash-table? (
                                                                            cond (
                                                                              (
                                                                                string? result
                                                                              )
                                                                               (
                                                                                _substring result y (
                                                                                  + y 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? result
                                                                              )
                                                                               (
                                                                                hash-table-ref result y
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref result y
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          hash-table-ref (
                                                                            cond (
                                                                              (
                                                                                string? result
                                                                              )
                                                                               (
                                                                                _substring result y (
                                                                                  + y 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? result
                                                                              )
                                                                               (
                                                                                hash-table-ref result y
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref result y
                                                                              )
                                                                            )
                                                                          )
                                                                           x
                                                                        )
                                                                      )
                                                                       (
                                                                        else (
                                                                          list-ref (
                                                                            cond (
                                                                              (
                                                                                string? result
                                                                              )
                                                                               (
                                                                                _substring result y (
                                                                                  + y 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? result
                                                                              )
                                                                               (
                                                                                hash-table-ref result y
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref result y
                                                                              )
                                                                            )
                                                                          )
                                                                           x
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                if (
                                                                  < x (
                                                                    - (
                                                                      _len (
                                                                        cond (
                                                                          (
                                                                            string? result
                                                                          )
                                                                           (
                                                                            _substring result y (
                                                                              + y 1
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          (
                                                                            hash-table? result
                                                                          )
                                                                           (
                                                                            hash-table-ref result y
                                                                          )
                                                                        )
                                                                         (
                                                                          else (
                                                                            list-ref result y
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     1
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! line (
                                                                      string-append line " "
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
                                                _display (
                                                  if (
                                                    string? line
                                                  )
                                                   line (
                                                    to-str line
                                                  )
                                                )
                                              )
                                               (
                                                newline
                                              )
                                               (
                                                set! y (
                                                  + y 1
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
