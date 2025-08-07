;; Generated on 2025-08-07 16:45 +0700
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
      start11 (
        current-jiffy
      )
    )
     (
      jps14 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      let (
        (
          c 299792458.0
        )
      )
       (
        begin (
          define (
            sqrtApprox x
          )
           (
            call/cc (
              lambda (
                ret1
              )
               (
                begin (
                  if (
                    <= x 0.0
                  )
                   (
                    begin (
                      ret1 0.0
                    )
                  )
                   '(
                    
                  )
                )
                 (
                  let (
                    (
                      guess (
                        _div x 2.0
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
                                        < i 20
                                      )
                                       (
                                        begin (
                                          set! guess (
                                            _div (
                                              _add guess (
                                                _div x guess
                                              )
                                            )
                                             2.0
                                          )
                                        )
                                         (
                                          set! i (
                                            + i 1
                                          )
                                        )
                                         (
                                          loop2
                                        )
                                      )
                                       '(
                                        
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
                          ret1 guess
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
            beta velocity
          )
           (
            call/cc (
              lambda (
                ret4
              )
               (
                begin (
                  if (
                    > velocity c
                  )
                   (
                    begin (
                      panic "Speed must not exceed light speed 299,792,458 [m/s]!"
                    )
                  )
                   '(
                    
                  )
                )
                 (
                  if (
                    < velocity 1.0
                  )
                   (
                    begin (
                      panic "Speed must be greater than or equal to 1!"
                    )
                  )
                   '(
                    
                  )
                )
                 (
                  ret4 (
                    _div velocity c
                  )
                )
              )
            )
          )
        )
         (
          define (
            gamma velocity
          )
           (
            call/cc (
              lambda (
                ret5
              )
               (
                let (
                  (
                    b (
                      beta velocity
                    )
                  )
                )
                 (
                  begin (
                    ret5 (
                      _div 1.0 (
                        sqrtApprox (
                          - 1.0 (
                            * b b
                          )
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
            transformation_matrix velocity
          )
           (
            call/cc (
              lambda (
                ret6
              )
               (
                let (
                  (
                    g (
                      gamma velocity
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        b (
                          beta velocity
                        )
                      )
                    )
                     (
                      begin (
                        ret6 (
                          _list (
                            _list g (
                              * (
                                - g
                              )
                               b
                            )
                             0.0 0.0
                          )
                           (
                            _list (
                              * (
                                - g
                              )
                               b
                            )
                             g 0.0 0.0
                          )
                           (
                            _list 0.0 0.0 1.0 0.0
                          )
                           (
                            _list 0.0 0.0 0.0 1.0
                          )
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
            mat_vec_mul mat vec
          )
           (
            call/cc (
              lambda (
                ret7
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
                                      < i 4
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            row (
                                              list-ref-safe mat i
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                value (
                                                  _add (
                                                    _add (
                                                      _add (
                                                        * (
                                                          list-ref-safe row 0
                                                        )
                                                         (
                                                          list-ref-safe vec 0
                                                        )
                                                      )
                                                       (
                                                        * (
                                                          list-ref-safe row 1
                                                        )
                                                         (
                                                          list-ref-safe vec 1
                                                        )
                                                      )
                                                    )
                                                     (
                                                      * (
                                                        list-ref-safe row 2
                                                      )
                                                       (
                                                        list-ref-safe vec 2
                                                      )
                                                    )
                                                  )
                                                   (
                                                    * (
                                                      list-ref-safe row 3
                                                    )
                                                     (
                                                      list-ref-safe vec 3
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                set! res (
                                                  append res (
                                                    _list value
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
                        ret7 res
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
            transform velocity event
          )
           (
            call/cc (
              lambda (
                ret10
              )
               (
                let (
                  (
                    g (
                      gamma velocity
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        b (
                          beta velocity
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            ct (
                              * (
                                list-ref-safe event 0
                              )
                               c
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                x (
                                  list-ref-safe event 1
                                )
                              )
                            )
                             (
                              begin (
                                ret10 (
                                  _list (
                                    - (
                                      * g ct
                                    )
                                     (
                                      * (
                                        * g b
                                      )
                                       x
                                    )
                                  )
                                   (
                                    _add (
                                      * (
                                        * (
                                          - g
                                        )
                                         b
                                      )
                                       ct
                                    )
                                     (
                                      * g x
                                    )
                                  )
                                   (
                                    list-ref-safe event 2
                                  )
                                   (
                                    list-ref-safe event 3
                                  )
                                )
                              )
                            )
                          )
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
                to-str-space (
                  beta c
                )
              )
            )
             (
              to-str-space (
                beta c
              )
            )
             (
              to-str (
                to-str-space (
                  beta c
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
                  beta 199792458.0
                )
              )
            )
             (
              to-str-space (
                beta 199792458.0
              )
            )
             (
              to-str (
                to-str-space (
                  beta 199792458.0
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
                  beta 100000.0
                )
              )
            )
             (
              to-str-space (
                beta 100000.0
              )
            )
             (
              to-str (
                to-str-space (
                  beta 100000.0
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
                  gamma 4.0
                )
              )
            )
             (
              to-str-space (
                gamma 4.0
              )
            )
             (
              to-str (
                to-str-space (
                  gamma 4.0
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
                  gamma 100000.0
                )
              )
            )
             (
              to-str-space (
                gamma 100000.0
              )
            )
             (
              to-str (
                to-str-space (
                  gamma 100000.0
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
                  gamma 30000000.0
                )
              )
            )
             (
              to-str-space (
                gamma 30000000.0
              )
            )
             (
              to-str (
                to-str-space (
                  gamma 30000000.0
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
                  transformation_matrix 29979245.0
                )
              )
            )
             (
              to-str-space (
                transformation_matrix 29979245.0
              )
            )
             (
              to-str (
                to-str-space (
                  transformation_matrix 29979245.0
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
              v (
                transform 29979245.0 (
                  _list 1.0 2.0 3.0 4.0
                )
              )
            )
          )
           (
            begin (
              _display (
                if (
                  string? (
                    to-str-space v
                  )
                )
                 (
                  to-str-space v
                )
                 (
                  to-str (
                    to-str-space v
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
          end12 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur13 (
              quotient (
                * (
                  - end12 start11
                )
                 1000000
              )
               jps14
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur13
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
