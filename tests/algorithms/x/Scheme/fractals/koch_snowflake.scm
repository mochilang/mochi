;; Generated on 2025-08-07 08:56 +0700
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
      start16 (
        current-jiffy
      )
    )
     (
      jps19 (
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
              TWO_PI 6.283185307179586
            )
          )
           (
            begin (
              define (
                _mod x m
              )
               (
                call/cc (
                  lambda (
                    ret1
                  )
                   (
                    ret1 (
                      - x (
                        * (
                          + 0.0 (
                            let (
                              (
                                v2 (
                                  _div x m
                                )
                              )
                            )
                             (
                              cond (
                                (
                                  string? v2
                                )
                                 (
                                  exact (
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
                                  exact (
                                    floor v2
                                  )
                                )
                              )
                            )
                          )
                        )
                         m
                      )
                    )
                  )
                )
              )
            )
             (
              define (
                sin x
              )
               (
                call/cc (
                  lambda (
                    ret3
                  )
                   (
                    let (
                      (
                        y (
                          - (
                            _mod (
                              + x PI
                            )
                             TWO_PI
                          )
                           PI
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            y2 (
                              * y y
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                y3 (
                                  * y2 y
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    y5 (
                                      * y3 y2
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        y7 (
                                          * y5 y2
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        ret3 (
                                          - (
                                            _add (
                                              - y (
                                                _div y3 6.0
                                              )
                                            )
                                             (
                                              _div y5 120.0
                                            )
                                          )
                                           (
                                            _div y7 5040.0
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
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
                cos x
              )
               (
                call/cc (
                  lambda (
                    ret4
                  )
                   (
                    let (
                      (
                        y (
                          - (
                            _mod (
                              + x PI
                            )
                             TWO_PI
                          )
                           PI
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            y2 (
                              * y y
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                y4 (
                                  * y2 y2
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    y6 (
                                      * y4 y2
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    ret4 (
                                      - (
                                        _add (
                                          - 1.0 (
                                            _div y2 2.0
                                          )
                                        )
                                         (
                                          _div y4 24.0
                                        )
                                      )
                                       (
                                        _div y6 720.0
                                      )
                                    )
                                  )
                                )
                              )
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
                rotate v angle_deg
              )
               (
                call/cc (
                  lambda (
                    ret5
                  )
                   (
                    let (
                      (
                        theta (
                          _div (
                            * angle_deg PI
                          )
                           180.0
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            c (
                              cos theta
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                s (
                                  sin theta
                                )
                              )
                            )
                             (
                              begin (
                                ret5 (
                                  alist->hash-table (
                                    _list (
                                      cons "x" (
                                        - (
                                          * (
                                            hash-table-ref v "x"
                                          )
                                           c
                                        )
                                         (
                                          * (
                                            hash-table-ref v "y"
                                          )
                                           s
                                        )
                                      )
                                    )
                                     (
                                      cons "y" (
                                        _add (
                                          * (
                                            hash-table-ref v "x"
                                          )
                                           s
                                        )
                                         (
                                          * (
                                            hash-table-ref v "y"
                                          )
                                           c
                                        )
                                      )
                                    )
                                  )
                                )
                              )
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
                iteration_step vectors
              )
               (
                call/cc (
                  lambda (
                    ret6
                  )
                   (
                    let (
                      (
                        new_vectors (
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
                                          < i (
                                            - (
                                              _len vectors
                                            )
                                             1
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                start (
                                                  list-ref vectors i
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    end (
                                                      list-ref vectors (
                                                        + i 1
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    set! new_vectors (
                                                      append new_vectors (
                                                        _list start
                                                      )
                                                    )
                                                  )
                                                   (
                                                    let (
                                                      (
                                                        dx (
                                                          - (
                                                            hash-table-ref end "x"
                                                          )
                                                           (
                                                            hash-table-ref start "x"
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            dy (
                                                              - (
                                                                hash-table-ref end "y"
                                                              )
                                                               (
                                                                hash-table-ref start "y"
                                                              )
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                one_third (
                                                                  alist->hash-table (
                                                                    _list (
                                                                      cons "x" (
                                                                        _add (
                                                                          hash-table-ref start "x"
                                                                        )
                                                                         (
                                                                          _div dx 3.0
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      cons "y" (
                                                                        _add (
                                                                          hash-table-ref start "y"
                                                                        )
                                                                         (
                                                                          _div dy 3.0
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
                                                                    mid (
                                                                      rotate (
                                                                        alist->hash-table (
                                                                          _list (
                                                                            cons "x" (
                                                                              _div dx 3.0
                                                                            )
                                                                          )
                                                                           (
                                                                            cons "y" (
                                                                              _div dy 3.0
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                       60.0
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        peak (
                                                                          alist->hash-table (
                                                                            _list (
                                                                              cons "x" (
                                                                                _add (
                                                                                  hash-table-ref one_third "x"
                                                                                )
                                                                                 (
                                                                                  hash-table-ref mid "x"
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              cons "y" (
                                                                                _add (
                                                                                  hash-table-ref one_third "y"
                                                                                )
                                                                                 (
                                                                                  hash-table-ref mid "y"
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
                                                                            two_third (
                                                                              alist->hash-table (
                                                                                _list (
                                                                                  cons "x" (
                                                                                    _add (
                                                                                      hash-table-ref start "x"
                                                                                    )
                                                                                     (
                                                                                      _div (
                                                                                        * dx 2.0
                                                                                      )
                                                                                       3.0
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  cons "y" (
                                                                                    _add (
                                                                                      hash-table-ref start "y"
                                                                                    )
                                                                                     (
                                                                                      _div (
                                                                                        * dy 2.0
                                                                                      )
                                                                                       3.0
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            set! new_vectors (
                                                                              append new_vectors (
                                                                                _list one_third
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            set! new_vectors (
                                                                              append new_vectors (
                                                                                _list peak
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            set! new_vectors (
                                                                              append new_vectors (
                                                                                _list two_third
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
                            set! new_vectors (
                              append new_vectors (
                                _list (
                                  list-ref vectors (
                                    - (
                                      _len vectors
                                    )
                                     1
                                  )
                                )
                              )
                            )
                          )
                           (
                            ret6 new_vectors
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
                iterate initial steps
              )
               (
                call/cc (
                  lambda (
                    ret9
                  )
                   (
                    let (
                      (
                        vectors initial
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
                                          < i steps
                                        )
                                         (
                                          begin (
                                            set! vectors (
                                              iteration_step vectors
                                            )
                                          )
                                           (
                                            set! i (
                                              + i 1
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
                            ret9 vectors
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
                vec_to_string v
              )
               (
                call/cc (
                  lambda (
                    ret12
                  )
                   (
                    ret12 (
                      string-append (
                        string-append (
                          string-append (
                            string-append "(" (
                              to-str-space (
                                hash-table-ref v "x"
                              )
                            )
                          )
                           ", "
                        )
                         (
                          to-str-space (
                            hash-table-ref v "y"
                          )
                        )
                      )
                       ")"
                    )
                  )
                )
              )
            )
             (
              define (
                vec_list_to_string lst
              )
               (
                call/cc (
                  lambda (
                    ret13
                  )
                   (
                    let (
                      (
                        res "["
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
                                          < i (
                                            _len lst
                                          )
                                        )
                                         (
                                          begin (
                                            set! res (
                                              string-append res (
                                                vec_to_string (
                                                  list-ref lst i
                                                )
                                              )
                                            )
                                          )
                                           (
                                            if (
                                              < i (
                                                - (
                                                  _len lst
                                                )
                                                 1
                                              )
                                            )
                                             (
                                              begin (
                                                set! res (
                                                  string-append res ", "
                                                )
                                              )
                                            )
                                             (
                                              quote (
                                                
                                              )
                                            )
                                          )
                                           (
                                            set! i (
                                              + i 1
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
                            set! res (
                              string-append res "]"
                            )
                          )
                           (
                            ret13 res
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
                  VECTOR_1 (
                    alist->hash-table (
                      _list (
                        cons "x" 0.0
                      )
                       (
                        cons "y" 0.0
                      )
                    )
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      VECTOR_2 (
                        alist->hash-table (
                          _list (
                            cons "x" 0.5
                          )
                           (
                            cons "y" 0.8660254
                          )
                        )
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          VECTOR_3 (
                            alist->hash-table (
                              _list (
                                cons "x" 1.0
                              )
                               (
                                cons "y" 0.0
                              )
                            )
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              INITIAL_VECTORS (
                                _list VECTOR_1 VECTOR_2 VECTOR_3 VECTOR_1
                              )
                            )
                          )
                           (
                            begin (
                              let (
                                (
                                  example (
                                    iterate (
                                      _list VECTOR_1 VECTOR_3
                                    )
                                     1
                                  )
                                )
                              )
                               (
                                begin (
                                  _display (
                                    if (
                                      string? (
                                        vec_list_to_string example
                                      )
                                    )
                                     (
                                      vec_list_to_string example
                                    )
                                     (
                                      to-str (
                                        vec_list_to_string example
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
          end17 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur18 (
              quotient (
                * (
                  - end17 start16
                )
                 1000000
              )
               jps19
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur18
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
