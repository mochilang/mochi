;; Generated on 2025-08-08 16:57 +0700
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
(define _floor floor)
(define (fmod a b) (- a (* (_floor (/ a b)) b)))
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
      start4 (
        current-jiffy
      )
    )
     (
      jps7 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      let (
        (
          PI 3.141592653589793
        )
      )
       (
        begin (
          let (
            (
              REDUCED_PLANCK_CONSTANT 1.054571817e-34
            )
          )
           (
            begin (
              let (
                (
                  SPEED_OF_LIGHT 300000000.0
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
                              guess x
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
                                  letrec (
                                    (
                                      loop2 (
                                        lambda (
                                          
                                        )
                                         (
                                          if (
                                            < i 100
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
                    casimir_force force area distance
                  )
                   (
                    call/cc (
                      lambda (
                        ret3
                      )
                       (
                        let (
                          (
                            zero_count 0
                          )
                        )
                         (
                          begin (
                            if (
                              equal? force 0.0
                            )
                             (
                              begin (
                                set! zero_count (
                                  + zero_count 1
                                )
                              )
                            )
                             '(
                              
                            )
                          )
                           (
                            if (
                              equal? area 0.0
                            )
                             (
                              begin (
                                set! zero_count (
                                  + zero_count 1
                                )
                              )
                            )
                             '(
                              
                            )
                          )
                           (
                            if (
                              equal? distance 0.0
                            )
                             (
                              begin (
                                set! zero_count (
                                  + zero_count 1
                                )
                              )
                            )
                             '(
                              
                            )
                          )
                           (
                            if (
                              not (
                                equal? zero_count 1
                              )
                            )
                             (
                              begin (
                                panic "One and only one argument must be 0"
                              )
                            )
                             '(
                              
                            )
                          )
                           (
                            if (
                              < force 0.0
                            )
                             (
                              begin (
                                panic "Magnitude of force can not be negative"
                              )
                            )
                             '(
                              
                            )
                          )
                           (
                            if (
                              < distance 0.0
                            )
                             (
                              begin (
                                panic "Distance can not be negative"
                              )
                            )
                             '(
                              
                            )
                          )
                           (
                            if (
                              < area 0.0
                            )
                             (
                              begin (
                                panic "Area can not be negative"
                              )
                            )
                             '(
                              
                            )
                          )
                           (
                            if (
                              equal? force 0.0
                            )
                             (
                              begin (
                                let (
                                  (
                                    num (
                                      * (
                                        * (
                                          * (
                                            * REDUCED_PLANCK_CONSTANT SPEED_OF_LIGHT
                                          )
                                           PI
                                        )
                                         PI
                                      )
                                       area
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        den (
                                          * (
                                            * (
                                              * (
                                                * 240.0 distance
                                              )
                                               distance
                                            )
                                             distance
                                          )
                                           distance
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            f (
                                              _div num den
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            ret3 (
                                              alist->hash-table (
                                                _list (
                                                  cons "force" f
                                                )
                                              )
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
                            if (
                              equal? area 0.0
                            )
                             (
                              begin (
                                let (
                                  (
                                    num (
                                      * (
                                        * (
                                          * (
                                            * (
                                              * 240.0 force
                                            )
                                             distance
                                          )
                                           distance
                                        )
                                         distance
                                      )
                                       distance
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        den (
                                          * (
                                            * (
                                              * REDUCED_PLANCK_CONSTANT SPEED_OF_LIGHT
                                            )
                                             PI
                                          )
                                           PI
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            a (
                                              _div num den
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            ret3 (
                                              alist->hash-table (
                                                _list (
                                                  cons "area" a
                                                )
                                              )
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
                            let (
                              (
                                num (
                                  * (
                                    * (
                                      * (
                                        * REDUCED_PLANCK_CONSTANT SPEED_OF_LIGHT
                                      )
                                       PI
                                    )
                                     PI
                                  )
                                   area
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    den (
                                      * 240.0 force
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        inner (
                                          _div num den
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            d (
                                              sqrtApprox (
                                                sqrtApprox inner
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            ret3 (
                                              alist->hash-table (
                                                _list (
                                                  cons "distance" d
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
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
                    begin (
                      _display (
                        if (
                          string? (
                            to-str-space (
                              casimir_force 0.0 4.0 0.03
                            )
                          )
                        )
                         (
                          to-str-space (
                            casimir_force 0.0 4.0 0.03
                          )
                        )
                         (
                          to-str (
                            to-str-space (
                              casimir_force 0.0 4.0 0.03
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
                              casimir_force 2.635e-10 0.0023 0.0
                            )
                          )
                        )
                         (
                          to-str-space (
                            casimir_force 2.635e-10 0.0023 0.0
                          )
                        )
                         (
                          to-str (
                            to-str-space (
                              casimir_force 2.635e-10 0.0023 0.0
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
                              casimir_force 2.737e-18 0.0 0.0023746
                            )
                          )
                        )
                         (
                          to-str-space (
                            casimir_force 2.737e-18 0.0 0.0023746
                          )
                        )
                         (
                          to-str (
                            to-str-space (
                              casimir_force 2.737e-18 0.0 0.0023746
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
                 (
                  main
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
          end5 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur6 (
              quotient (
                * (
                  - end5 start4
                )
                 1000000
              )
               jps7
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur6
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
