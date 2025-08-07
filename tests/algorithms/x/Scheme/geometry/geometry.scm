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
      start20 (
        current-jiffy
      )
    )
     (
      jps23 (
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
          define (
            make_angle deg
          )
           (
            call/cc (
              lambda (
                ret1
              )
               (
                begin (
                  if (
                    or (
                      < deg 0.0
                    )
                     (
                      > deg 360.0
                    )
                  )
                   (
                    begin (
                      panic "degrees must be between 0 and 360"
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  ret1 (
                    alist->hash-table (
                      _list (
                        cons "degrees" deg
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
            make_side length angle
          )
           (
            call/cc (
              lambda (
                ret2
              )
               (
                begin (
                  if (
                    <= length 0.0
                  )
                   (
                    begin (
                      panic "length must be positive"
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  ret2 (
                    alist->hash-table (
                      _list (
                        cons "length" length
                      )
                       (
                        cons "angle" angle
                      )
                       (
                        cons "next" (
                          - 1
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
            ellipse_area e
          )
           (
            call/cc (
              lambda (
                ret3
              )
               (
                ret3 (
                  * (
                    * PI (
                      hash-table-ref e "major"
                    )
                  )
                   (
                    hash-table-ref e "minor"
                  )
                )
              )
            )
          )
        )
         (
          define (
            ellipse_perimeter e
          )
           (
            call/cc (
              lambda (
                ret4
              )
               (
                ret4 (
                  * PI (
                    + (
                      hash-table-ref e "major"
                    )
                     (
                      hash-table-ref e "minor"
                    )
                  )
                )
              )
            )
          )
        )
         (
          define (
            circle_area c
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
                      alist->hash-table (
                        _list (
                          cons "major" (
                            hash-table-ref c "radius"
                          )
                        )
                         (
                          cons "minor" (
                            hash-table-ref c "radius"
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
                        area (
                          ellipse_area e
                        )
                      )
                    )
                     (
                      begin (
                        ret5 area
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
            circle_perimeter c
          )
           (
            call/cc (
              lambda (
                ret6
              )
               (
                let (
                  (
                    e (
                      alist->hash-table (
                        _list (
                          cons "major" (
                            hash-table-ref c "radius"
                          )
                        )
                         (
                          cons "minor" (
                            hash-table-ref c "radius"
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
                        per (
                          ellipse_perimeter e
                        )
                      )
                    )
                     (
                      begin (
                        ret6 per
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
            circle_diameter c
          )
           (
            call/cc (
              lambda (
                ret7
              )
               (
                ret7 (
                  * (
                    hash-table-ref c "radius"
                  )
                   2.0
                )
              )
            )
          )
        )
         (
          define (
            circle_max_parts num_cuts
          )
           (
            call/cc (
              lambda (
                ret8
              )
               (
                begin (
                  if (
                    < num_cuts 0.0
                  )
                   (
                    begin (
                      panic "num_cuts must be positive"
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  ret8 (
                    * (
                      _add (
                        + num_cuts 2.0
                      )
                       (
                        * num_cuts num_cuts
                      )
                    )
                     0.5
                  )
                )
              )
            )
          )
        )
         (
          define (
            make_polygon
          )
           (
            call/cc (
              lambda (
                ret9
              )
               (
                let (
                  (
                    s (
                      _list
                    )
                  )
                )
                 (
                  begin (
                    ret9 (
                      alist->hash-table (
                        _list (
                          cons "sides" s
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
            polygon_add_side p s
          )
           (
            call/cc (
              lambda (
                ret10
              )
               (
                hash-table-set! p "sides" (
                  append (
                    hash-table-ref p "sides"
                  )
                   (
                    _list s
                  )
                )
              )
            )
          )
        )
         (
          define (
            polygon_get_side p index
          )
           (
            call/cc (
              lambda (
                ret11
              )
               (
                ret11 (
                  list-ref (
                    hash-table-ref p "sides"
                  )
                   index
                )
              )
            )
          )
        )
         (
          define (
            polygon_set_side p index s
          )
           (
            call/cc (
              lambda (
                ret12
              )
               (
                let (
                  (
                    tmp (
                      hash-table-ref p "sides"
                    )
                  )
                )
                 (
                  begin (
                    list-set! tmp index s
                  )
                   (
                    hash-table-set! p "sides" tmp
                  )
                )
              )
            )
          )
        )
         (
          define (
            make_rectangle short_len long_len
          )
           (
            call/cc (
              lambda (
                ret13
              )
               (
                begin (
                  if (
                    or (
                      <= short_len 0.0
                    )
                     (
                      <= long_len 0.0
                    )
                  )
                   (
                    begin (
                      panic "length must be positive"
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
                      short (
                        make_side short_len (
                          make_angle 90.0
                        )
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          long (
                            make_side long_len (
                              make_angle 90.0
                            )
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              p (
                                make_polygon
                              )
                            )
                          )
                           (
                            begin (
                              polygon_add_side p short
                            )
                             (
                              polygon_add_side p long
                            )
                             (
                              ret13 (
                                alist->hash-table (
                                  _list (
                                    cons "short_side" short
                                  )
                                   (
                                    cons "long_side" long
                                  )
                                   (
                                    cons "poly" p
                                  )
                                )
                              )
                            )
                          )
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
            rectangle_perimeter r
          )
           (
            call/cc (
              lambda (
                ret14
              )
               (
                ret14 (
                  * (
                    + (
                      hash-table-ref (
                        hash-table-ref r "short_side"
                      )
                       "length"
                    )
                     (
                      hash-table-ref (
                        hash-table-ref r "long_side"
                      )
                       "length"
                    )
                  )
                   2.0
                )
              )
            )
          )
        )
         (
          define (
            rectangle_area r
          )
           (
            call/cc (
              lambda (
                ret15
              )
               (
                ret15 (
                  * (
                    hash-table-ref (
                      hash-table-ref r "short_side"
                    )
                     "length"
                  )
                   (
                    hash-table-ref (
                      hash-table-ref r "long_side"
                    )
                     "length"
                  )
                )
              )
            )
          )
        )
         (
          define (
            make_square side_len
          )
           (
            call/cc (
              lambda (
                ret16
              )
               (
                let (
                  (
                    rect (
                      make_rectangle side_len side_len
                    )
                  )
                )
                 (
                  begin (
                    ret16 (
                      alist->hash-table (
                        _list (
                          cons "side" (
                            hash-table-ref rect "short_side"
                          )
                        )
                         (
                          cons "rect" rect
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
            square_perimeter s
          )
           (
            call/cc (
              lambda (
                ret17
              )
               (
                let (
                  (
                    p (
                      rectangle_perimeter (
                        hash-table-ref s "rect"
                      )
                    )
                  )
                )
                 (
                  begin (
                    ret17 p
                  )
                )
              )
            )
          )
        )
         (
          define (
            square_area s
          )
           (
            call/cc (
              lambda (
                ret18
              )
               (
                let (
                  (
                    a (
                      rectangle_area (
                        hash-table-ref s "rect"
                      )
                    )
                  )
                )
                 (
                  begin (
                    ret18 a
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
                ret19
              )
               (
                let (
                  (
                    a (
                      make_angle 90.0
                    )
                  )
                )
                 (
                  begin (
                    _display (
                      if (
                        string? (
                          hash-table-ref a "degrees"
                        )
                      )
                       (
                        hash-table-ref a "degrees"
                      )
                       (
                        to-str (
                          hash-table-ref a "degrees"
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
                        s (
                          make_side 5.0 a
                        )
                      )
                    )
                     (
                      begin (
                        _display (
                          if (
                            string? (
                              hash-table-ref s "length"
                            )
                          )
                           (
                            hash-table-ref s "length"
                          )
                           (
                            to-str (
                              hash-table-ref s "length"
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
                            e (
                              alist->hash-table (
                                _list (
                                  cons "major" 5.0
                                )
                                 (
                                  cons "minor" 10.0
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
                                  ellipse_area e
                                )
                              )
                               (
                                ellipse_area e
                              )
                               (
                                to-str (
                                  ellipse_area e
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
                                  ellipse_perimeter e
                                )
                              )
                               (
                                ellipse_perimeter e
                              )
                               (
                                to-str (
                                  ellipse_perimeter e
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
                                c (
                                  alist->hash-table (
                                    _list (
                                      cons "radius" 5.0
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
                                      circle_area c
                                    )
                                  )
                                   (
                                    circle_area c
                                  )
                                   (
                                    to-str (
                                      circle_area c
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
                                      circle_perimeter c
                                    )
                                  )
                                   (
                                    circle_perimeter c
                                  )
                                   (
                                    to-str (
                                      circle_perimeter c
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
                                      circle_diameter c
                                    )
                                  )
                                   (
                                    circle_diameter c
                                  )
                                   (
                                    to-str (
                                      circle_diameter c
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
                                      circle_max_parts 7.0
                                    )
                                  )
                                   (
                                    circle_max_parts 7.0
                                  )
                                   (
                                    to-str (
                                      circle_max_parts 7.0
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
                                    r (
                                      make_rectangle 5.0 10.0
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    _display (
                                      if (
                                        string? (
                                          rectangle_perimeter r
                                        )
                                      )
                                       (
                                        rectangle_perimeter r
                                      )
                                       (
                                        to-str (
                                          rectangle_perimeter r
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
                                          rectangle_area r
                                        )
                                      )
                                       (
                                        rectangle_area r
                                      )
                                       (
                                        to-str (
                                          rectangle_area r
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
                                        q (
                                          make_square 5.0
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        _display (
                                          if (
                                            string? (
                                              square_perimeter q
                                            )
                                          )
                                           (
                                            square_perimeter q
                                          )
                                           (
                                            to-str (
                                              square_perimeter q
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
                                              square_area q
                                            )
                                          )
                                           (
                                            square_area q
                                          )
                                           (
                                            to-str (
                                              square_area q
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
         (
          main
        )
      )
    )
     (
      let (
        (
          end21 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur22 (
              quotient (
                * (
                  - end21 start20
                )
                 1000000
              )
               jps23
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur22
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
