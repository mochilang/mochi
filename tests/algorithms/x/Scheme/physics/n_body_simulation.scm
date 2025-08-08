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
      start5 (
        current-jiffy
      )
    )
     (
      jps8 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        make_body px py vx vy mass
      )
       (
        alist->hash-table (
          _list (
            cons "position_x" px
          )
           (
            cons "position_y" py
          )
           (
            cons "velocity_x" vx
          )
           (
            cons "velocity_y" vy
          )
           (
            cons "mass" mass
          )
        )
      )
    )
     (
      define (
        update_velocity body force_x force_y delta_time
      )
       (
        begin (
          hash-table-set! body "velocity_x" (
            _add (
              hash-table-ref body "velocity_x"
            )
             (
              * force_x delta_time
            )
          )
        )
         (
          hash-table-set! body "velocity_y" (
            _add (
              hash-table-ref body "velocity_y"
            )
             (
              * force_y delta_time
            )
          )
        )
         body
      )
    )
     (
      define (
        update_position body delta_time
      )
       (
        begin (
          hash-table-set! body "position_x" (
            _add (
              hash-table-ref body "position_x"
            )
             (
              * (
                hash-table-ref body "velocity_x"
              )
               delta_time
            )
          )
        )
         (
          hash-table-set! body "position_y" (
            _add (
              hash-table-ref body "position_y"
            )
             (
              * (
                hash-table-ref body "velocity_y"
              )
               delta_time
            )
          )
        )
         body
      )
    )
     (
      define (
        make_body_system bodies g tf sf
      )
       (
        alist->hash-table (
          _list (
            cons "bodies" bodies
          )
           (
            cons "gravitation_constant" g
          )
           (
            cons "time_factor" tf
          )
           (
            cons "softening_factor" sf
          )
        )
      )
    )
     (
      define (
        sqrtApprox x
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
                letrec (
                  (
                    loop1 (
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
                            loop1
                          )
                        )
                         '(
                          
                        )
                      )
                    )
                  )
                )
                 (
                  loop1
                )
              )
               guess
            )
          )
        )
      )
    )
     (
      define (
        update_system system delta_time
      )
       (
        let (
          (
            bodies (
              hash-table-ref system "bodies"
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
                letrec (
                  (
                    loop2 (
                      lambda (
                        
                      )
                       (
                        if (
                          < i (
                            _len bodies
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                body1 (
                                  list-ref-safe bodies i
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    force_x 0.0
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        force_y 0.0
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
                                            letrec (
                                              (
                                                loop3 (
                                                  lambda (
                                                    
                                                  )
                                                   (
                                                    if (
                                                      < j (
                                                        _len bodies
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        if (
                                                          not (
                                                            equal? i j
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                body2 (
                                                                  list-ref-safe bodies j
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                let (
                                                                  (
                                                                    dif_x (
                                                                      - (
                                                                        hash-table-ref body2 "position_x"
                                                                      )
                                                                       (
                                                                        hash-table-ref body1 "position_x"
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        dif_y (
                                                                          - (
                                                                            hash-table-ref body2 "position_y"
                                                                          )
                                                                           (
                                                                            hash-table-ref body1 "position_y"
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            distance_sq (
                                                                              _add (
                                                                                _add (
                                                                                  * dif_x dif_x
                                                                                )
                                                                                 (
                                                                                  * dif_y dif_y
                                                                                )
                                                                              )
                                                                               (
                                                                                hash-table-ref system "softening_factor"
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                distance (
                                                                                  sqrtApprox distance_sq
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                let (
                                                                                  (
                                                                                    denom (
                                                                                      * (
                                                                                        * distance distance
                                                                                      )
                                                                                       distance
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    set! force_x (
                                                                                      _add force_x (
                                                                                        _div (
                                                                                          * (
                                                                                            * (
                                                                                              hash-table-ref system "gravitation_constant"
                                                                                            )
                                                                                             (
                                                                                              hash-table-ref body2 "mass"
                                                                                            )
                                                                                          )
                                                                                           dif_x
                                                                                        )
                                                                                         denom
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    set! force_y (
                                                                                      _add force_y (
                                                                                        _div (
                                                                                          * (
                                                                                            * (
                                                                                              hash-table-ref system "gravitation_constant"
                                                                                            )
                                                                                             (
                                                                                              hash-table-ref body2 "mass"
                                                                                            )
                                                                                          )
                                                                                           dif_y
                                                                                        )
                                                                                         denom
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                          )
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
                                                        set! j (
                                                          + j 1
                                                        )
                                                      )
                                                       (
                                                        loop3
                                                      )
                                                    )
                                                     '(
                                                      
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              loop3
                                            )
                                          )
                                           (
                                            set! body1 (
                                              update_velocity body1 force_x force_y (
                                                * delta_time (
                                                  hash-table-ref system "time_factor"
                                                )
                                              )
                                            )
                                          )
                                           (
                                            list-set! bodies i body1
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
                set! i 0
              )
               (
                letrec (
                  (
                    loop4 (
                      lambda (
                        
                      )
                       (
                        if (
                          < i (
                            _len bodies
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                body (
                                  list-ref-safe bodies i
                                )
                              )
                            )
                             (
                              begin (
                                set! body (
                                  update_position body (
                                    * delta_time (
                                      hash-table-ref system "time_factor"
                                    )
                                  )
                                )
                              )
                               (
                                list-set! bodies i body
                              )
                               (
                                set! i (
                                  + i 1
                                )
                              )
                            )
                          )
                           (
                            loop4
                          )
                        )
                         '(
                          
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
                hash-table-set! system "bodies" bodies
              )
               system
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
        let (
          (
            b1 (
              make_body 0.0 0.0 0.0 0.0 1.0
            )
          )
        )
         (
          begin (
            let (
              (
                b2 (
                  make_body 10.0 0.0 0.0 0.0 1.0
                )
              )
            )
             (
              begin (
                let (
                  (
                    sys1 (
                      make_body_system (
                        _list b1 b2
                      )
                       1.0 1.0 0.0
                    )
                  )
                )
                 (
                  begin (
                    set! sys1 (
                      update_system sys1 1.0
                    )
                  )
                   (
                    let (
                      (
                        b1_after (
                          cond (
                            (
                              string? (
                                hash-table-ref sys1 "bodies"
                              )
                            )
                             (
                              _substring (
                                hash-table-ref sys1 "bodies"
                              )
                               0 (
                                + 0 1
                              )
                            )
                          )
                           (
                            (
                              hash-table? (
                                hash-table-ref sys1 "bodies"
                              )
                            )
                             (
                              hash-table-ref (
                                hash-table-ref sys1 "bodies"
                              )
                               0
                            )
                          )
                           (
                            else (
                              list-ref-safe (
                                hash-table-ref sys1 "bodies"
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
                            pos1x (
                              hash-table-ref b1_after "position_x"
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                pos1y (
                                  hash-table-ref b1_after "position_y"
                                )
                              )
                            )
                             (
                              begin (
                                _display (
                                  alist->hash-table (
                                    _list (
                                      cons "x" pos1x
                                    )
                                     (
                                      cons "y" pos1y
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
                                    vel1x (
                                      hash-table-ref b1_after "velocity_x"
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        vel1y (
                                          hash-table-ref b1_after "velocity_y"
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        _display (
                                          alist->hash-table (
                                            _list (
                                              cons "vx" vel1x
                                            )
                                             (
                                              cons "vy" vel1y
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
                                            b3 (
                                              make_body (
                                                - 10.0
                                              )
                                               0.0 0.0 0.0 1.0
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                b4 (
                                                  make_body 10.0 0.0 0.0 0.0 4.0
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    sys2 (
                                                      make_body_system (
                                                        _list b3 b4
                                                      )
                                                       1.0 10.0 0.0
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    set! sys2 (
                                                      update_system sys2 1.0
                                                    )
                                                  )
                                                   (
                                                    let (
                                                      (
                                                        b2_after (
                                                          cond (
                                                            (
                                                              string? (
                                                                hash-table-ref sys2 "bodies"
                                                              )
                                                            )
                                                             (
                                                              _substring (
                                                                hash-table-ref sys2 "bodies"
                                                              )
                                                               0 (
                                                                + 0 1
                                                              )
                                                            )
                                                          )
                                                           (
                                                            (
                                                              hash-table? (
                                                                hash-table-ref sys2 "bodies"
                                                              )
                                                            )
                                                             (
                                                              hash-table-ref (
                                                                hash-table-ref sys2 "bodies"
                                                              )
                                                               0
                                                            )
                                                          )
                                                           (
                                                            else (
                                                              list-ref-safe (
                                                                hash-table-ref sys2 "bodies"
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
                                                            pos2x (
                                                              hash-table-ref b2_after "position_x"
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                pos2y (
                                                                  hash-table-ref b2_after "position_y"
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                _display (
                                                                  alist->hash-table (
                                                                    _list (
                                                                      cons "x" pos2x
                                                                    )
                                                                     (
                                                                      cons "y" pos2y
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
                                                                    vel2x (
                                                                      hash-table-ref b2_after "velocity_x"
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        vel2y (
                                                                          hash-table-ref b2_after "velocity_y"
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        _display (
                                                                          alist->hash-table (
                                                                            _list (
                                                                              cons "vx" vel2x
                                                                            )
                                                                             (
                                                                              cons "vy" vel2y
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
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
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
          end6 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur7 (
              quotient (
                * (
                  - end6 start5
                )
                 1000000
              )
               jps8
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur7
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
