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
      start27 (
        current-jiffy
      )
    )
     (
      jps30 (
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
                sin_approx x
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
                cos_approx x
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
                tan_approx x
              )
               (
                call/cc (
                  lambda (
                    ret5
                  )
                   (
                    ret5 (
                      _div (
                        sin_approx x
                      )
                       (
                        cos_approx x
                      )
                    )
                  )
                )
              )
            )
             (
              define (
                sqrt_approx x
              )
               (
                call/cc (
                  lambda (
                    ret6
                  )
                   (
                    begin (
                      if (
                        <= x 0.0
                      )
                       (
                        begin (
                          ret6 0.0
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
                              ret6 guess
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
                surface_area_cube side_length
              )
               (
                call/cc (
                  lambda (
                    ret9
                  )
                   (
                    begin (
                      if (
                        < side_length 0.0
                      )
                       (
                        begin (
                          _display (
                            if (
                              string? "ValueError: surface_area_cube() only accepts non-negative values"
                            )
                             "ValueError: surface_area_cube() only accepts non-negative values" (
                              to-str "ValueError: surface_area_cube() only accepts non-negative values"
                            )
                          )
                        )
                         (
                          newline
                        )
                         (
                          ret9 0.0
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      ret9 (
                        * (
                          * 6.0 side_length
                        )
                         side_length
                      )
                    )
                  )
                )
              )
            )
             (
              define (
                surface_area_cuboid length breadth height
              )
               (
                call/cc (
                  lambda (
                    ret10
                  )
                   (
                    begin (
                      if (
                        or (
                          or (
                            < length 0.0
                          )
                           (
                            < breadth 0.0
                          )
                        )
                         (
                          < height 0.0
                        )
                      )
                       (
                        begin (
                          _display (
                            if (
                              string? "ValueError: surface_area_cuboid() only accepts non-negative values"
                            )
                             "ValueError: surface_area_cuboid() only accepts non-negative values" (
                              to-str "ValueError: surface_area_cuboid() only accepts non-negative values"
                            )
                          )
                        )
                         (
                          newline
                        )
                         (
                          ret10 0.0
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      ret10 (
                        * 2.0 (
                          _add (
                            + (
                              * length breadth
                            )
                             (
                              * breadth height
                            )
                          )
                           (
                            * length height
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
                surface_area_sphere radius
              )
               (
                call/cc (
                  lambda (
                    ret11
                  )
                   (
                    begin (
                      if (
                        < radius 0.0
                      )
                       (
                        begin (
                          _display (
                            if (
                              string? "ValueError: surface_area_sphere() only accepts non-negative values"
                            )
                             "ValueError: surface_area_sphere() only accepts non-negative values" (
                              to-str "ValueError: surface_area_sphere() only accepts non-negative values"
                            )
                          )
                        )
                         (
                          newline
                        )
                         (
                          ret11 0.0
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      ret11 (
                        * (
                          * (
                            * 4.0 PI
                          )
                           radius
                        )
                         radius
                      )
                    )
                  )
                )
              )
            )
             (
              define (
                surface_area_hemisphere radius
              )
               (
                call/cc (
                  lambda (
                    ret12
                  )
                   (
                    begin (
                      if (
                        < radius 0.0
                      )
                       (
                        begin (
                          _display (
                            if (
                              string? "ValueError: surface_area_hemisphere() only accepts non-negative values"
                            )
                             "ValueError: surface_area_hemisphere() only accepts non-negative values" (
                              to-str "ValueError: surface_area_hemisphere() only accepts non-negative values"
                            )
                          )
                        )
                         (
                          newline
                        )
                         (
                          ret12 0.0
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      ret12 (
                        * (
                          * (
                            * 3.0 PI
                          )
                           radius
                        )
                         radius
                      )
                    )
                  )
                )
              )
            )
             (
              define (
                surface_area_cone radius height
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
                          < radius 0.0
                        )
                         (
                          < height 0.0
                        )
                      )
                       (
                        begin (
                          _display (
                            if (
                              string? "ValueError: surface_area_cone() only accepts non-negative values"
                            )
                             "ValueError: surface_area_cone() only accepts non-negative values" (
                              to-str "ValueError: surface_area_cone() only accepts non-negative values"
                            )
                          )
                        )
                         (
                          newline
                        )
                         (
                          ret13 0.0
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
                          slant (
                            sqrt_approx (
                              _add (
                                * height height
                              )
                               (
                                * radius radius
                              )
                            )
                          )
                        )
                      )
                       (
                        begin (
                          ret13 (
                            * (
                              * PI radius
                            )
                             (
                              _add radius slant
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
                surface_area_conical_frustum radius1 radius2 height
              )
               (
                call/cc (
                  lambda (
                    ret14
                  )
                   (
                    begin (
                      if (
                        or (
                          or (
                            < radius1 0.0
                          )
                           (
                            < radius2 0.0
                          )
                        )
                         (
                          < height 0.0
                        )
                      )
                       (
                        begin (
                          _display (
                            if (
                              string? "ValueError: surface_area_conical_frustum() only accepts non-negative values"
                            )
                             "ValueError: surface_area_conical_frustum() only accepts non-negative values" (
                              to-str "ValueError: surface_area_conical_frustum() only accepts non-negative values"
                            )
                          )
                        )
                         (
                          newline
                        )
                         (
                          ret14 0.0
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
                          slant (
                            sqrt_approx (
                              _add (
                                * height height
                              )
                               (
                                * (
                                  - radius1 radius2
                                )
                                 (
                                  - radius1 radius2
                                )
                              )
                            )
                          )
                        )
                      )
                       (
                        begin (
                          ret14 (
                            * PI (
                              _add (
                                _add (
                                  * slant (
                                    + radius1 radius2
                                  )
                                )
                                 (
                                  * radius1 radius1
                                )
                              )
                               (
                                * radius2 radius2
                              )
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
                surface_area_cylinder radius height
              )
               (
                call/cc (
                  lambda (
                    ret15
                  )
                   (
                    begin (
                      if (
                        or (
                          < radius 0.0
                        )
                         (
                          < height 0.0
                        )
                      )
                       (
                        begin (
                          _display (
                            if (
                              string? "ValueError: surface_area_cylinder() only accepts non-negative values"
                            )
                             "ValueError: surface_area_cylinder() only accepts non-negative values" (
                              to-str "ValueError: surface_area_cylinder() only accepts non-negative values"
                            )
                          )
                        )
                         (
                          newline
                        )
                         (
                          ret15 0.0
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      ret15 (
                        * (
                          * (
                            * 2.0 PI
                          )
                           radius
                        )
                         (
                          + height radius
                        )
                      )
                    )
                  )
                )
              )
            )
             (
              define (
                surface_area_torus torus_radius tube_radius
              )
               (
                call/cc (
                  lambda (
                    ret16
                  )
                   (
                    begin (
                      if (
                        or (
                          < torus_radius 0.0
                        )
                         (
                          < tube_radius 0.0
                        )
                      )
                       (
                        begin (
                          _display (
                            if (
                              string? "ValueError: surface_area_torus() only accepts non-negative values"
                            )
                             "ValueError: surface_area_torus() only accepts non-negative values" (
                              to-str "ValueError: surface_area_torus() only accepts non-negative values"
                            )
                          )
                        )
                         (
                          newline
                        )
                         (
                          ret16 0.0
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      if (
                        < torus_radius tube_radius
                      )
                       (
                        begin (
                          _display (
                            if (
                              string? "ValueError: surface_area_torus() does not support spindle or self intersecting tori"
                            )
                             "ValueError: surface_area_torus() does not support spindle or self intersecting tori" (
                              to-str "ValueError: surface_area_torus() does not support spindle or self intersecting tori"
                            )
                          )
                        )
                         (
                          newline
                        )
                         (
                          ret16 0.0
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      ret16 (
                        * (
                          * (
                            * (
                              * 4.0 PI
                            )
                             PI
                          )
                           torus_radius
                        )
                         tube_radius
                      )
                    )
                  )
                )
              )
            )
             (
              define (
                area_rectangle length width
              )
               (
                call/cc (
                  lambda (
                    ret17
                  )
                   (
                    begin (
                      if (
                        or (
                          < length 0.0
                        )
                         (
                          < width 0.0
                        )
                      )
                       (
                        begin (
                          _display (
                            if (
                              string? "ValueError: area_rectangle() only accepts non-negative values"
                            )
                             "ValueError: area_rectangle() only accepts non-negative values" (
                              to-str "ValueError: area_rectangle() only accepts non-negative values"
                            )
                          )
                        )
                         (
                          newline
                        )
                         (
                          ret17 0.0
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      ret17 (
                        * length width
                      )
                    )
                  )
                )
              )
            )
             (
              define (
                area_square side_length
              )
               (
                call/cc (
                  lambda (
                    ret18
                  )
                   (
                    begin (
                      if (
                        < side_length 0.0
                      )
                       (
                        begin (
                          _display (
                            if (
                              string? "ValueError: area_square() only accepts non-negative values"
                            )
                             "ValueError: area_square() only accepts non-negative values" (
                              to-str "ValueError: area_square() only accepts non-negative values"
                            )
                          )
                        )
                         (
                          newline
                        )
                         (
                          ret18 0.0
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      ret18 (
                        * side_length side_length
                      )
                    )
                  )
                )
              )
            )
             (
              define (
                area_triangle base height
              )
               (
                call/cc (
                  lambda (
                    ret19
                  )
                   (
                    begin (
                      if (
                        or (
                          < base 0.0
                        )
                         (
                          < height 0.0
                        )
                      )
                       (
                        begin (
                          _display (
                            if (
                              string? "ValueError: area_triangle() only accepts non-negative values"
                            )
                             "ValueError: area_triangle() only accepts non-negative values" (
                              to-str "ValueError: area_triangle() only accepts non-negative values"
                            )
                          )
                        )
                         (
                          newline
                        )
                         (
                          ret19 0.0
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      ret19 (
                        _div (
                          * base height
                        )
                         2.0
                      )
                    )
                  )
                )
              )
            )
             (
              define (
                area_triangle_three_sides side1 side2 side3
              )
               (
                call/cc (
                  lambda (
                    ret20
                  )
                   (
                    begin (
                      if (
                        or (
                          or (
                            < side1 0.0
                          )
                           (
                            < side2 0.0
                          )
                        )
                         (
                          < side3 0.0
                        )
                      )
                       (
                        begin (
                          _display (
                            if (
                              string? "ValueError: area_triangle_three_sides() only accepts non-negative values"
                            )
                             "ValueError: area_triangle_three_sides() only accepts non-negative values" (
                              to-str "ValueError: area_triangle_three_sides() only accepts non-negative values"
                            )
                          )
                        )
                         (
                          newline
                        )
                         (
                          ret20 0.0
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      if (
                        or (
                          or (
                            _lt (
                              + side1 side2
                            )
                             side3
                          )
                           (
                            _lt (
                              + side1 side3
                            )
                             side2
                          )
                        )
                         (
                          _lt (
                            + side2 side3
                          )
                           side1
                        )
                      )
                       (
                        begin (
                          _display (
                            if (
                              string? "ValueError: Given three sides do not form a triangle"
                            )
                             "ValueError: Given three sides do not form a triangle" (
                              to-str "ValueError: Given three sides do not form a triangle"
                            )
                          )
                        )
                         (
                          newline
                        )
                         (
                          ret20 0.0
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
                          s (
                            _div (
                              _add (
                                + side1 side2
                              )
                               side3
                            )
                             2.0
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              prod (
                                * (
                                  * (
                                    * s (
                                      - s side1
                                    )
                                  )
                                   (
                                    - s side2
                                  )
                                )
                                 (
                                  - s side3
                                )
                              )
                            )
                          )
                           (
                            begin (
                              let (
                                (
                                  res (
                                    sqrt_approx prod
                                  )
                                )
                              )
                               (
                                begin (
                                  ret20 res
                                )
                              )
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
                area_parallelogram base height
              )
               (
                call/cc (
                  lambda (
                    ret21
                  )
                   (
                    begin (
                      if (
                        or (
                          < base 0.0
                        )
                         (
                          < height 0.0
                        )
                      )
                       (
                        begin (
                          _display (
                            if (
                              string? "ValueError: area_parallelogram() only accepts non-negative values"
                            )
                             "ValueError: area_parallelogram() only accepts non-negative values" (
                              to-str "ValueError: area_parallelogram() only accepts non-negative values"
                            )
                          )
                        )
                         (
                          newline
                        )
                         (
                          ret21 0.0
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      ret21 (
                        * base height
                      )
                    )
                  )
                )
              )
            )
             (
              define (
                area_trapezium base1 base2 height
              )
               (
                call/cc (
                  lambda (
                    ret22
                  )
                   (
                    begin (
                      if (
                        or (
                          or (
                            < base1 0.0
                          )
                           (
                            < base2 0.0
                          )
                        )
                         (
                          < height 0.0
                        )
                      )
                       (
                        begin (
                          _display (
                            if (
                              string? "ValueError: area_trapezium() only accepts non-negative values"
                            )
                             "ValueError: area_trapezium() only accepts non-negative values" (
                              to-str "ValueError: area_trapezium() only accepts non-negative values"
                            )
                          )
                        )
                         (
                          newline
                        )
                         (
                          ret22 0.0
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      ret22 (
                        * (
                          * 0.5 (
                            + base1 base2
                          )
                        )
                         height
                      )
                    )
                  )
                )
              )
            )
             (
              define (
                area_circle radius
              )
               (
                call/cc (
                  lambda (
                    ret23
                  )
                   (
                    begin (
                      if (
                        < radius 0.0
                      )
                       (
                        begin (
                          _display (
                            if (
                              string? "ValueError: area_circle() only accepts non-negative values"
                            )
                             "ValueError: area_circle() only accepts non-negative values" (
                              to-str "ValueError: area_circle() only accepts non-negative values"
                            )
                          )
                        )
                         (
                          newline
                        )
                         (
                          ret23 0.0
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      ret23 (
                        * (
                          * PI radius
                        )
                         radius
                      )
                    )
                  )
                )
              )
            )
             (
              define (
                area_ellipse radius_x radius_y
              )
               (
                call/cc (
                  lambda (
                    ret24
                  )
                   (
                    begin (
                      if (
                        or (
                          < radius_x 0.0
                        )
                         (
                          < radius_y 0.0
                        )
                      )
                       (
                        begin (
                          _display (
                            if (
                              string? "ValueError: area_ellipse() only accepts non-negative values"
                            )
                             "ValueError: area_ellipse() only accepts non-negative values" (
                              to-str "ValueError: area_ellipse() only accepts non-negative values"
                            )
                          )
                        )
                         (
                          newline
                        )
                         (
                          ret24 0.0
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      ret24 (
                        * (
                          * PI radius_x
                        )
                         radius_y
                      )
                    )
                  )
                )
              )
            )
             (
              define (
                area_rhombus diagonal1 diagonal2
              )
               (
                call/cc (
                  lambda (
                    ret25
                  )
                   (
                    begin (
                      if (
                        or (
                          < diagonal1 0.0
                        )
                         (
                          < diagonal2 0.0
                        )
                      )
                       (
                        begin (
                          _display (
                            if (
                              string? "ValueError: area_rhombus() only accepts non-negative values"
                            )
                             "ValueError: area_rhombus() only accepts non-negative values" (
                              to-str "ValueError: area_rhombus() only accepts non-negative values"
                            )
                          )
                        )
                         (
                          newline
                        )
                         (
                          ret25 0.0
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      ret25 (
                        * (
                          * 0.5 diagonal1
                        )
                         diagonal2
                      )
                    )
                  )
                )
              )
            )
             (
              define (
                area_reg_polygon sides length
              )
               (
                call/cc (
                  lambda (
                    ret26
                  )
                   (
                    begin (
                      if (
                        < sides 3
                      )
                       (
                        begin (
                          _display (
                            if (
                              string? "ValueError: area_reg_polygon() only accepts integers greater than or equal to three as number of sides"
                            )
                             "ValueError: area_reg_polygon() only accepts integers greater than or equal to three as number of sides" (
                              to-str "ValueError: area_reg_polygon() only accepts integers greater than or equal to three as number of sides"
                            )
                          )
                        )
                         (
                          newline
                        )
                         (
                          ret26 0.0
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      if (
                        < length 0.0
                      )
                       (
                        begin (
                          _display (
                            if (
                              string? "ValueError: area_reg_polygon() only accepts non-negative values as length of a side"
                            )
                             "ValueError: area_reg_polygon() only accepts non-negative values as length of a side" (
                              to-str "ValueError: area_reg_polygon() only accepts non-negative values as length of a side"
                            )
                          )
                        )
                         (
                          newline
                        )
                         (
                          ret26 0.0
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
                          n (
                            + 0.0 sides
                          )
                        )
                      )
                       (
                        begin (
                          ret26 (
                            _div (
                              * (
                                * n length
                              )
                               length
                            )
                             (
                              * 4.0 (
                                tan_approx (
                                  _div PI n
                                )
                              )
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
                  string? "[DEMO] Areas of various geometric shapes:"
                )
                 "[DEMO] Areas of various geometric shapes:" (
                  to-str "[DEMO] Areas of various geometric shapes:"
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
                    string-append "Rectangle: " (
                      to-str-space (
                        area_rectangle 10.0 20.0
                      )
                    )
                  )
                )
                 (
                  string-append "Rectangle: " (
                    to-str-space (
                      area_rectangle 10.0 20.0
                    )
                  )
                )
                 (
                  to-str (
                    string-append "Rectangle: " (
                      to-str-space (
                        area_rectangle 10.0 20.0
                      )
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
                    string-append "Square: " (
                      to-str-space (
                        area_square 10.0
                      )
                    )
                  )
                )
                 (
                  string-append "Square: " (
                    to-str-space (
                      area_square 10.0
                    )
                  )
                )
                 (
                  to-str (
                    string-append "Square: " (
                      to-str-space (
                        area_square 10.0
                      )
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
                    string-append "Triangle: " (
                      to-str-space (
                        area_triangle 10.0 10.0
                      )
                    )
                  )
                )
                 (
                  string-append "Triangle: " (
                    to-str-space (
                      area_triangle 10.0 10.0
                    )
                  )
                )
                 (
                  to-str (
                    string-append "Triangle: " (
                      to-str-space (
                        area_triangle 10.0 10.0
                      )
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
                  TRI_THREE_SIDES (
                    area_triangle_three_sides 5.0 12.0 13.0
                  )
                )
              )
               (
                begin (
                  _display (
                    if (
                      string? (
                        string-append "Triangle Three Sides: " (
                          to-str-space TRI_THREE_SIDES
                        )
                      )
                    )
                     (
                      string-append "Triangle Three Sides: " (
                        to-str-space TRI_THREE_SIDES
                      )
                    )
                     (
                      to-str (
                        string-append "Triangle Three Sides: " (
                          to-str-space TRI_THREE_SIDES
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
                        string-append "Parallelogram: " (
                          to-str-space (
                            area_parallelogram 10.0 20.0
                          )
                        )
                      )
                    )
                     (
                      string-append "Parallelogram: " (
                        to-str-space (
                          area_parallelogram 10.0 20.0
                        )
                      )
                    )
                     (
                      to-str (
                        string-append "Parallelogram: " (
                          to-str-space (
                            area_parallelogram 10.0 20.0
                          )
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
                        string-append "Rhombus: " (
                          to-str-space (
                            area_rhombus 10.0 20.0
                          )
                        )
                      )
                    )
                     (
                      string-append "Rhombus: " (
                        to-str-space (
                          area_rhombus 10.0 20.0
                        )
                      )
                    )
                     (
                      to-str (
                        string-append "Rhombus: " (
                          to-str-space (
                            area_rhombus 10.0 20.0
                          )
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
                        string-append "Trapezium: " (
                          to-str-space (
                            area_trapezium 10.0 20.0 30.0
                          )
                        )
                      )
                    )
                     (
                      string-append "Trapezium: " (
                        to-str-space (
                          area_trapezium 10.0 20.0 30.0
                        )
                      )
                    )
                     (
                      to-str (
                        string-append "Trapezium: " (
                          to-str-space (
                            area_trapezium 10.0 20.0 30.0
                          )
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
                        string-append "Circle: " (
                          to-str-space (
                            area_circle 20.0
                          )
                        )
                      )
                    )
                     (
                      string-append "Circle: " (
                        to-str-space (
                          area_circle 20.0
                        )
                      )
                    )
                     (
                      to-str (
                        string-append "Circle: " (
                          to-str-space (
                            area_circle 20.0
                          )
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
                        string-append "Ellipse: " (
                          to-str-space (
                            area_ellipse 10.0 20.0
                          )
                        )
                      )
                    )
                     (
                      string-append "Ellipse: " (
                        to-str-space (
                          area_ellipse 10.0 20.0
                        )
                      )
                    )
                     (
                      to-str (
                        string-append "Ellipse: " (
                          to-str-space (
                            area_ellipse 10.0 20.0
                          )
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
                      string? ""
                    )
                     "" (
                      to-str ""
                    )
                  )
                )
                 (
                  newline
                )
                 (
                  _display (
                    if (
                      string? "Surface Areas of various geometric shapes:"
                    )
                     "Surface Areas of various geometric shapes:" (
                      to-str "Surface Areas of various geometric shapes:"
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
                        string-append "Cube: " (
                          to-str-space (
                            surface_area_cube 20.0
                          )
                        )
                      )
                    )
                     (
                      string-append "Cube: " (
                        to-str-space (
                          surface_area_cube 20.0
                        )
                      )
                    )
                     (
                      to-str (
                        string-append "Cube: " (
                          to-str-space (
                            surface_area_cube 20.0
                          )
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
                        string-append "Cuboid: " (
                          to-str-space (
                            surface_area_cuboid 10.0 20.0 30.0
                          )
                        )
                      )
                    )
                     (
                      string-append "Cuboid: " (
                        to-str-space (
                          surface_area_cuboid 10.0 20.0 30.0
                        )
                      )
                    )
                     (
                      to-str (
                        string-append "Cuboid: " (
                          to-str-space (
                            surface_area_cuboid 10.0 20.0 30.0
                          )
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
                        string-append "Sphere: " (
                          to-str-space (
                            surface_area_sphere 20.0
                          )
                        )
                      )
                    )
                     (
                      string-append "Sphere: " (
                        to-str-space (
                          surface_area_sphere 20.0
                        )
                      )
                    )
                     (
                      to-str (
                        string-append "Sphere: " (
                          to-str-space (
                            surface_area_sphere 20.0
                          )
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
                        string-append "Hemisphere: " (
                          to-str-space (
                            surface_area_hemisphere 20.0
                          )
                        )
                      )
                    )
                     (
                      string-append "Hemisphere: " (
                        to-str-space (
                          surface_area_hemisphere 20.0
                        )
                      )
                    )
                     (
                      to-str (
                        string-append "Hemisphere: " (
                          to-str-space (
                            surface_area_hemisphere 20.0
                          )
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
                        string-append "Cone: " (
                          to-str-space (
                            surface_area_cone 10.0 20.0
                          )
                        )
                      )
                    )
                     (
                      string-append "Cone: " (
                        to-str-space (
                          surface_area_cone 10.0 20.0
                        )
                      )
                    )
                     (
                      to-str (
                        string-append "Cone: " (
                          to-str-space (
                            surface_area_cone 10.0 20.0
                          )
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
                        string-append "Conical Frustum: " (
                          to-str-space (
                            surface_area_conical_frustum 10.0 20.0 30.0
                          )
                        )
                      )
                    )
                     (
                      string-append "Conical Frustum: " (
                        to-str-space (
                          surface_area_conical_frustum 10.0 20.0 30.0
                        )
                      )
                    )
                     (
                      to-str (
                        string-append "Conical Frustum: " (
                          to-str-space (
                            surface_area_conical_frustum 10.0 20.0 30.0
                          )
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
                        string-append "Cylinder: " (
                          to-str-space (
                            surface_area_cylinder 10.0 20.0
                          )
                        )
                      )
                    )
                     (
                      string-append "Cylinder: " (
                        to-str-space (
                          surface_area_cylinder 10.0 20.0
                        )
                      )
                    )
                     (
                      to-str (
                        string-append "Cylinder: " (
                          to-str-space (
                            surface_area_cylinder 10.0 20.0
                          )
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
                        string-append "Torus: " (
                          to-str-space (
                            surface_area_torus 20.0 10.0
                          )
                        )
                      )
                    )
                     (
                      string-append "Torus: " (
                        to-str-space (
                          surface_area_torus 20.0 10.0
                        )
                      )
                    )
                     (
                      to-str (
                        string-append "Torus: " (
                          to-str-space (
                            surface_area_torus 20.0 10.0
                          )
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
                        string-append "Equilateral Triangle: " (
                          to-str-space (
                            area_reg_polygon 3 10.0
                          )
                        )
                      )
                    )
                     (
                      string-append "Equilateral Triangle: " (
                        to-str-space (
                          area_reg_polygon 3 10.0
                        )
                      )
                    )
                     (
                      to-str (
                        string-append "Equilateral Triangle: " (
                          to-str-space (
                            area_reg_polygon 3 10.0
                          )
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
                        string-append "Square: " (
                          to-str-space (
                            area_reg_polygon 4 10.0
                          )
                        )
                      )
                    )
                     (
                      string-append "Square: " (
                        to-str-space (
                          area_reg_polygon 4 10.0
                        )
                      )
                    )
                     (
                      to-str (
                        string-append "Square: " (
                          to-str-space (
                            area_reg_polygon 4 10.0
                          )
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
                        string-append "Regular Pentagon: " (
                          to-str-space (
                            area_reg_polygon 5 10.0
                          )
                        )
                      )
                    )
                     (
                      string-append "Regular Pentagon: " (
                        to-str-space (
                          area_reg_polygon 5 10.0
                        )
                      )
                    )
                     (
                      to-str (
                        string-append "Regular Pentagon: " (
                          to-str-space (
                            area_reg_polygon 5 10.0
                          )
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
     (
      let (
        (
          end28 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur29 (
              quotient (
                * (
                  - end28 start27
                )
                 1000000
              )
               jps30
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur29
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
