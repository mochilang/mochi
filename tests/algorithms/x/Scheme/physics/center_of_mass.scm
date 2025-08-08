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
      define (
        round2 x
      )
       (
        let (
          (
            scaled (
              * x 100.0
            )
          )
        )
         (
          begin (
            let (
              (
                rounded (
                  + 0.0 (
                    let (
                      (
                        v1 (
                          + scaled 0.5
                        )
                      )
                    )
                     (
                      cond (
                        (
                          string? v1
                        )
                         (
                          inexact->exact (
                            _floor (
                              string->number v1
                            )
                          )
                        )
                      )
                       (
                        (
                          boolean? v1
                        )
                         (
                          if v1 1 0
                        )
                      )
                       (
                        else (
                          inexact->exact (
                            _floor v1
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
             (
              begin (
                _div rounded 100.0
              )
            )
          )
        )
      )
    )
     (
      define (
        center_of_mass ps
      )
       (
        begin (
          if (
            equal? (
              _len ps
            )
             0
          )
           (
            begin (
              panic "No particles provided"
            )
          )
           '(
            
          )
        )
         (
          let (
            (
              i 0
            )
          )
           (
            begin (
              let (
                (
                  total_mass 0.0
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
                              _len ps
                            )
                          )
                           (
                            begin (
                              let (
                                (
                                  p (
                                    list-ref-safe ps i
                                  )
                                )
                              )
                               (
                                begin (
                                  if (
                                    <= (
                                      hash-table-ref p "mass"
                                    )
                                     0.0
                                  )
                                   (
                                    begin (
                                      panic "Mass of all particles must be greater than 0"
                                    )
                                  )
                                   '(
                                    
                                  )
                                )
                                 (
                                  set! total_mass (
                                    + total_mass (
                                      hash-table-ref p "mass"
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
                  let (
                    (
                      sum_x 0.0
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          sum_y 0.0
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              sum_z 0.0
                            )
                          )
                           (
                            begin (
                              set! i 0
                            )
                             (
                              letrec (
                                (
                                  loop3 (
                                    lambda (
                                      
                                    )
                                     (
                                      if (
                                        < i (
                                          _len ps
                                        )
                                      )
                                       (
                                        begin (
                                          let (
                                            (
                                              p (
                                                list-ref-safe ps i
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              set! sum_x (
                                                _add sum_x (
                                                  * (
                                                    hash-table-ref p "x"
                                                  )
                                                   (
                                                    hash-table-ref p "mass"
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              set! sum_y (
                                                _add sum_y (
                                                  * (
                                                    hash-table-ref p "y"
                                                  )
                                                   (
                                                    hash-table-ref p "mass"
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              set! sum_z (
                                                _add sum_z (
                                                  * (
                                                    hash-table-ref p "z"
                                                  )
                                                   (
                                                    hash-table-ref p "mass"
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
                              let (
                                (
                                  cm_x (
                                    round2 (
                                      _div sum_x total_mass
                                    )
                                  )
                                )
                              )
                               (
                                begin (
                                  let (
                                    (
                                      cm_y (
                                        round2 (
                                          _div sum_y total_mass
                                        )
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      let (
                                        (
                                          cm_z (
                                            round2 (
                                              _div sum_z total_mass
                                            )
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          alist->hash-table (
                                            _list (
                                              cons "x" cm_x
                                            )
                                             (
                                              cons "y" cm_y
                                            )
                                             (
                                              cons "z" cm_z
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
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
        coord_to_string c
      )
       (
        string-append (
          string-append (
            string-append (
              string-append (
                string-append (
                  string-append "Coord3D(x=" (
                    to-str-space (
                      hash-table-ref c "x"
                    )
                  )
                )
                 ", y="
              )
               (
                to-str-space (
                  hash-table-ref c "y"
                )
              )
            )
             ", z="
          )
           (
            to-str-space (
              hash-table-ref c "z"
            )
          )
        )
         ")"
      )
    )
     (
      let (
        (
          r1 (
            center_of_mass (
              _list (
                alist->hash-table (
                  _list (
                    cons "x" 1.5
                  )
                   (
                    cons "y" 4.0
                  )
                   (
                    cons "z" 3.4
                  )
                   (
                    cons "mass" 4.0
                  )
                )
              )
               (
                alist->hash-table (
                  _list (
                    cons "x" 5.0
                  )
                   (
                    cons "y" 6.8
                  )
                   (
                    cons "z" 7.0
                  )
                   (
                    cons "mass" 8.1
                  )
                )
              )
               (
                alist->hash-table (
                  _list (
                    cons "x" 9.4
                  )
                   (
                    cons "y" 10.1
                  )
                   (
                    cons "z" 11.6
                  )
                   (
                    cons "mass" 12.0
                  )
                )
              )
            )
          )
        )
      )
       (
        begin (
          _display (
            if (
              string? (
                coord_to_string r1
              )
            )
             (
              coord_to_string r1
            )
             (
              to-str (
                coord_to_string r1
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
              r2 (
                center_of_mass (
                  _list (
                    alist->hash-table (
                      _list (
                        cons "x" 1.0
                      )
                       (
                        cons "y" 2.0
                      )
                       (
                        cons "z" 3.0
                      )
                       (
                        cons "mass" 4.0
                      )
                    )
                  )
                   (
                    alist->hash-table (
                      _list (
                        cons "x" 5.0
                      )
                       (
                        cons "y" 6.0
                      )
                       (
                        cons "z" 7.0
                      )
                       (
                        cons "mass" 8.0
                      )
                    )
                  )
                   (
                    alist->hash-table (
                      _list (
                        cons "x" 9.0
                      )
                       (
                        cons "y" 10.0
                      )
                       (
                        cons "z" 11.0
                      )
                       (
                        cons "mass" 12.0
                      )
                    )
                  )
                )
              )
            )
          )
           (
            begin (
              _display (
                if (
                  string? (
                    coord_to_string r2
                  )
                )
                 (
                  coord_to_string r2
                )
                 (
                  to-str (
                    coord_to_string r2
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
