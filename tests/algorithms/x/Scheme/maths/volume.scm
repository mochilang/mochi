;; Generated on 2025-08-07 16:11 +0700
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
          let (
            (
              SQRT5 2.23606797749979
            )
          )
           (
            begin (
              define (
                minf a b
              )
               (
                call/cc (
                  lambda (
                    ret1
                  )
                   (
                    begin (
                      if (
                        < a b
                      )
                       (
                        begin (
                          ret1 a
                        )
                      )
                       '(
                        
                      )
                    )
                     (
                      ret1 b
                    )
                  )
                )
              )
            )
             (
              define (
                maxf a b
              )
               (
                call/cc (
                  lambda (
                    ret2
                  )
                   (
                    begin (
                      if (
                        > a b
                      )
                       (
                        begin (
                          ret2 a
                        )
                      )
                       '(
                        
                      )
                    )
                     (
                      ret2 b
                    )
                  )
                )
              )
            )
             (
              define (
                vol_cube side_length
              )
               (
                call/cc (
                  lambda (
                    ret3
                  )
                   (
                    begin (
                      if (
                        < side_length 0.0
                      )
                       (
                        begin (
                          panic "vol_cube() only accepts non-negative values"
                        )
                      )
                       '(
                        
                      )
                    )
                     (
                      ret3 (
                        * (
                          * side_length side_length
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
                vol_spherical_cap height radius
              )
               (
                call/cc (
                  lambda (
                    ret4
                  )
                   (
                    begin (
                      if (
                        or (
                          < height 0.0
                        )
                         (
                          < radius 0.0
                        )
                      )
                       (
                        begin (
                          panic "vol_spherical_cap() only accepts non-negative values"
                        )
                      )
                       '(
                        
                      )
                    )
                     (
                      ret4 (
                        * (
                          * (
                            * (
                              * (
                                _div 1.0 3.0
                              )
                               PI
                            )
                             height
                          )
                           height
                        )
                         (
                          - (
                            * 3.0 radius
                          )
                           height
                        )
                      )
                    )
                  )
                )
              )
            )
             (
              define (
                vol_sphere radius
              )
               (
                call/cc (
                  lambda (
                    ret5
                  )
                   (
                    begin (
                      if (
                        < radius 0.0
                      )
                       (
                        begin (
                          panic "vol_sphere() only accepts non-negative values"
                        )
                      )
                       '(
                        
                      )
                    )
                     (
                      ret5 (
                        * (
                          * (
                            * (
                              * (
                                _div 4.0 3.0
                              )
                               PI
                            )
                             radius
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
                vol_spheres_intersect radius_1 radius_2 centers_distance
              )
               (
                call/cc (
                  lambda (
                    ret6
                  )
                   (
                    begin (
                      if (
                        or (
                          or (
                            < radius_1 0.0
                          )
                           (
                            < radius_2 0.0
                          )
                        )
                         (
                          < centers_distance 0.0
                        )
                      )
                       (
                        begin (
                          panic "vol_spheres_intersect() only accepts non-negative values"
                        )
                      )
                       '(
                        
                      )
                    )
                     (
                      if (
                        equal? centers_distance 0.0
                      )
                       (
                        begin (
                          ret6 (
                            vol_sphere (
                              minf radius_1 radius_2
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
                          h1 (
                            _div (
                              * (
                                _add (
                                  - radius_1 radius_2
                                )
                                 centers_distance
                              )
                               (
                                - (
                                  + radius_1 radius_2
                                )
                                 centers_distance
                              )
                            )
                             (
                              * 2.0 centers_distance
                            )
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              h2 (
                                _div (
                                  * (
                                    _add (
                                      - radius_2 radius_1
                                    )
                                     centers_distance
                                  )
                                   (
                                    - (
                                      + radius_2 radius_1
                                    )
                                     centers_distance
                                  )
                                )
                                 (
                                  * 2.0 centers_distance
                                )
                              )
                            )
                          )
                           (
                            begin (
                              ret6 (
                                _add (
                                  vol_spherical_cap h1 radius_2
                                )
                                 (
                                  vol_spherical_cap h2 radius_1
                                )
                              )
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
                vol_spheres_union radius_1 radius_2 centers_distance
              )
               (
                call/cc (
                  lambda (
                    ret7
                  )
                   (
                    begin (
                      if (
                        or (
                          or (
                            <= radius_1 0.0
                          )
                           (
                            <= radius_2 0.0
                          )
                        )
                         (
                          < centers_distance 0.0
                        )
                      )
                       (
                        begin (
                          panic "vol_spheres_union() only accepts non-negative values, non-zero radius"
                        )
                      )
                       '(
                        
                      )
                    )
                     (
                      if (
                        equal? centers_distance 0.0
                      )
                       (
                        begin (
                          ret7 (
                            vol_sphere (
                              maxf radius_1 radius_2
                            )
                          )
                        )
                      )
                       '(
                        
                      )
                    )
                     (
                      ret7 (
                        - (
                          _add (
                            vol_sphere radius_1
                          )
                           (
                            vol_sphere radius_2
                          )
                        )
                         (
                          vol_spheres_intersect radius_1 radius_2 centers_distance
                        )
                      )
                    )
                  )
                )
              )
            )
             (
              define (
                vol_cuboid width height length
              )
               (
                call/cc (
                  lambda (
                    ret8
                  )
                   (
                    begin (
                      if (
                        or (
                          or (
                            < width 0.0
                          )
                           (
                            < height 0.0
                          )
                        )
                         (
                          < length 0.0
                        )
                      )
                       (
                        begin (
                          panic "vol_cuboid() only accepts non-negative values"
                        )
                      )
                       '(
                        
                      )
                    )
                     (
                      ret8 (
                        * (
                          * width height
                        )
                         length
                      )
                    )
                  )
                )
              )
            )
             (
              define (
                vol_cone area_of_base height
              )
               (
                call/cc (
                  lambda (
                    ret9
                  )
                   (
                    begin (
                      if (
                        or (
                          < height 0.0
                        )
                         (
                          < area_of_base 0.0
                        )
                      )
                       (
                        begin (
                          panic "vol_cone() only accepts non-negative values"
                        )
                      )
                       '(
                        
                      )
                    )
                     (
                      ret9 (
                        _div (
                          * area_of_base height
                        )
                         3.0
                      )
                    )
                  )
                )
              )
            )
             (
              define (
                vol_right_circ_cone radius height
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
                          < height 0.0
                        )
                         (
                          < radius 0.0
                        )
                      )
                       (
                        begin (
                          panic "vol_right_circ_cone() only accepts non-negative values"
                        )
                      )
                       '(
                        
                      )
                    )
                     (
                      ret10 (
                        _div (
                          * (
                            * (
                              * PI radius
                            )
                             radius
                          )
                           height
                        )
                         3.0
                      )
                    )
                  )
                )
              )
            )
             (
              define (
                vol_prism area_of_base height
              )
               (
                call/cc (
                  lambda (
                    ret11
                  )
                   (
                    begin (
                      if (
                        or (
                          < height 0.0
                        )
                         (
                          < area_of_base 0.0
                        )
                      )
                       (
                        begin (
                          panic "vol_prism() only accepts non-negative values"
                        )
                      )
                       '(
                        
                      )
                    )
                     (
                      ret11 (
                        * area_of_base height
                      )
                    )
                  )
                )
              )
            )
             (
              define (
                vol_pyramid area_of_base height
              )
               (
                call/cc (
                  lambda (
                    ret12
                  )
                   (
                    begin (
                      if (
                        or (
                          < height 0.0
                        )
                         (
                          < area_of_base 0.0
                        )
                      )
                       (
                        begin (
                          panic "vol_pyramid() only accepts non-negative values"
                        )
                      )
                       '(
                        
                      )
                    )
                     (
                      ret12 (
                        _div (
                          * area_of_base height
                        )
                         3.0
                      )
                    )
                  )
                )
              )
            )
             (
              define (
                vol_hemisphere radius
              )
               (
                call/cc (
                  lambda (
                    ret13
                  )
                   (
                    begin (
                      if (
                        < radius 0.0
                      )
                       (
                        begin (
                          panic "vol_hemisphere() only accepts non-negative values"
                        )
                      )
                       '(
                        
                      )
                    )
                     (
                      ret13 (
                        _div (
                          * (
                            * (
                              * (
                                * radius radius
                              )
                               radius
                            )
                             PI
                          )
                           2.0
                        )
                         3.0
                      )
                    )
                  )
                )
              )
            )
             (
              define (
                vol_circular_cylinder radius height
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
                          < height 0.0
                        )
                         (
                          < radius 0.0
                        )
                      )
                       (
                        begin (
                          panic "vol_circular_cylinder() only accepts non-negative values"
                        )
                      )
                       '(
                        
                      )
                    )
                     (
                      ret14 (
                        * (
                          * (
                            * radius radius
                          )
                           height
                        )
                         PI
                      )
                    )
                  )
                )
              )
            )
             (
              define (
                vol_hollow_circular_cylinder inner_radius outer_radius height
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
                          or (
                            < inner_radius 0.0
                          )
                           (
                            < outer_radius 0.0
                          )
                        )
                         (
                          < height 0.0
                        )
                      )
                       (
                        begin (
                          panic "vol_hollow_circular_cylinder() only accepts non-negative values"
                        )
                      )
                       '(
                        
                      )
                    )
                     (
                      if (
                        <= outer_radius inner_radius
                      )
                       (
                        begin (
                          panic "outer_radius must be greater than inner_radius"
                        )
                      )
                       '(
                        
                      )
                    )
                     (
                      ret15 (
                        * (
                          * PI (
                            - (
                              * outer_radius outer_radius
                            )
                             (
                              * inner_radius inner_radius
                            )
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
                vol_conical_frustum height radius_1 radius_2
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
                          or (
                            < radius_1 0.0
                          )
                           (
                            < radius_2 0.0
                          )
                        )
                         (
                          < height 0.0
                        )
                      )
                       (
                        begin (
                          panic "vol_conical_frustum() only accepts non-negative values"
                        )
                      )
                       '(
                        
                      )
                    )
                     (
                      ret16 (
                        * (
                          * (
                            * (
                              _div 1.0 3.0
                            )
                             PI
                          )
                           height
                        )
                         (
                          _add (
                            _add (
                              * radius_1 radius_1
                            )
                             (
                              * radius_2 radius_2
                            )
                          )
                           (
                            * radius_1 radius_2
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
                vol_torus torus_radius tube_radius
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
                          < torus_radius 0.0
                        )
                         (
                          < tube_radius 0.0
                        )
                      )
                       (
                        begin (
                          panic "vol_torus() only accepts non-negative values"
                        )
                      )
                       '(
                        
                      )
                    )
                     (
                      ret17 (
                        * (
                          * (
                            * (
                              * (
                                * 2.0 PI
                              )
                               PI
                            )
                             torus_radius
                          )
                           tube_radius
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
                vol_icosahedron tri_side
              )
               (
                call/cc (
                  lambda (
                    ret18
                  )
                   (
                    begin (
                      if (
                        < tri_side 0.0
                      )
                       (
                        begin (
                          panic "vol_icosahedron() only accepts non-negative values"
                        )
                      )
                       '(
                        
                      )
                    )
                     (
                      ret18 (
                        _div (
                          * (
                            * (
                              * (
                                * tri_side tri_side
                              )
                               tri_side
                            )
                             (
                              + 3.0 SQRT5
                            )
                          )
                           5.0
                        )
                         12.0
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
                    begin (
                      _display (
                        if (
                          string? "Volumes:"
                        )
                         "Volumes:" (
                          to-str "Volumes:"
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
                                vol_cube 2.0
                              )
                            )
                          )
                        )
                         (
                          string-append "Cube: " (
                            to-str-space (
                              vol_cube 2.0
                            )
                          )
                        )
                         (
                          to-str (
                            string-append "Cube: " (
                              to-str-space (
                                vol_cube 2.0
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
                                vol_cuboid 2.0 2.0 2.0
                              )
                            )
                          )
                        )
                         (
                          string-append "Cuboid: " (
                            to-str-space (
                              vol_cuboid 2.0 2.0 2.0
                            )
                          )
                        )
                         (
                          to-str (
                            string-append "Cuboid: " (
                              to-str-space (
                                vol_cuboid 2.0 2.0 2.0
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
                                vol_cone 2.0 2.0
                              )
                            )
                          )
                        )
                         (
                          string-append "Cone: " (
                            to-str-space (
                              vol_cone 2.0 2.0
                            )
                          )
                        )
                         (
                          to-str (
                            string-append "Cone: " (
                              to-str-space (
                                vol_cone 2.0 2.0
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
                            string-append "Right Circular Cone: " (
                              to-str-space (
                                vol_right_circ_cone 2.0 2.0
                              )
                            )
                          )
                        )
                         (
                          string-append "Right Circular Cone: " (
                            to-str-space (
                              vol_right_circ_cone 2.0 2.0
                            )
                          )
                        )
                         (
                          to-str (
                            string-append "Right Circular Cone: " (
                              to-str-space (
                                vol_right_circ_cone 2.0 2.0
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
                            string-append "Prism: " (
                              to-str-space (
                                vol_prism 2.0 2.0
                              )
                            )
                          )
                        )
                         (
                          string-append "Prism: " (
                            to-str-space (
                              vol_prism 2.0 2.0
                            )
                          )
                        )
                         (
                          to-str (
                            string-append "Prism: " (
                              to-str-space (
                                vol_prism 2.0 2.0
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
                            string-append "Pyramid: " (
                              to-str-space (
                                vol_pyramid 2.0 2.0
                              )
                            )
                          )
                        )
                         (
                          string-append "Pyramid: " (
                            to-str-space (
                              vol_pyramid 2.0 2.0
                            )
                          )
                        )
                         (
                          to-str (
                            string-append "Pyramid: " (
                              to-str-space (
                                vol_pyramid 2.0 2.0
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
                                vol_sphere 2.0
                              )
                            )
                          )
                        )
                         (
                          string-append "Sphere: " (
                            to-str-space (
                              vol_sphere 2.0
                            )
                          )
                        )
                         (
                          to-str (
                            string-append "Sphere: " (
                              to-str-space (
                                vol_sphere 2.0
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
                                vol_hemisphere 2.0
                              )
                            )
                          )
                        )
                         (
                          string-append "Hemisphere: " (
                            to-str-space (
                              vol_hemisphere 2.0
                            )
                          )
                        )
                         (
                          to-str (
                            string-append "Hemisphere: " (
                              to-str-space (
                                vol_hemisphere 2.0
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
                            string-append "Circular Cylinder: " (
                              to-str-space (
                                vol_circular_cylinder 2.0 2.0
                              )
                            )
                          )
                        )
                         (
                          string-append "Circular Cylinder: " (
                            to-str-space (
                              vol_circular_cylinder 2.0 2.0
                            )
                          )
                        )
                         (
                          to-str (
                            string-append "Circular Cylinder: " (
                              to-str-space (
                                vol_circular_cylinder 2.0 2.0
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
                                vol_torus 2.0 2.0
                              )
                            )
                          )
                        )
                         (
                          string-append "Torus: " (
                            to-str-space (
                              vol_torus 2.0 2.0
                            )
                          )
                        )
                         (
                          to-str (
                            string-append "Torus: " (
                              to-str-space (
                                vol_torus 2.0 2.0
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
                                vol_conical_frustum 2.0 2.0 4.0
                              )
                            )
                          )
                        )
                         (
                          string-append "Conical Frustum: " (
                            to-str-space (
                              vol_conical_frustum 2.0 2.0 4.0
                            )
                          )
                        )
                         (
                          to-str (
                            string-append "Conical Frustum: " (
                              to-str-space (
                                vol_conical_frustum 2.0 2.0 4.0
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
                            string-append "Spherical cap: " (
                              to-str-space (
                                vol_spherical_cap 1.0 2.0
                              )
                            )
                          )
                        )
                         (
                          string-append "Spherical cap: " (
                            to-str-space (
                              vol_spherical_cap 1.0 2.0
                            )
                          )
                        )
                         (
                          to-str (
                            string-append "Spherical cap: " (
                              to-str-space (
                                vol_spherical_cap 1.0 2.0
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
                            string-append "Spheres intersection: " (
                              to-str-space (
                                vol_spheres_intersect 2.0 2.0 1.0
                              )
                            )
                          )
                        )
                         (
                          string-append "Spheres intersection: " (
                            to-str-space (
                              vol_spheres_intersect 2.0 2.0 1.0
                            )
                          )
                        )
                         (
                          to-str (
                            string-append "Spheres intersection: " (
                              to-str-space (
                                vol_spheres_intersect 2.0 2.0 1.0
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
                            string-append "Spheres union: " (
                              to-str-space (
                                vol_spheres_union 2.0 2.0 1.0
                              )
                            )
                          )
                        )
                         (
                          string-append "Spheres union: " (
                            to-str-space (
                              vol_spheres_union 2.0 2.0 1.0
                            )
                          )
                        )
                         (
                          to-str (
                            string-append "Spheres union: " (
                              to-str-space (
                                vol_spheres_union 2.0 2.0 1.0
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
                            string-append "Hollow Circular Cylinder: " (
                              to-str-space (
                                vol_hollow_circular_cylinder 1.0 2.0 3.0
                              )
                            )
                          )
                        )
                         (
                          string-append "Hollow Circular Cylinder: " (
                            to-str-space (
                              vol_hollow_circular_cylinder 1.0 2.0 3.0
                            )
                          )
                        )
                         (
                          to-str (
                            string-append "Hollow Circular Cylinder: " (
                              to-str-space (
                                vol_hollow_circular_cylinder 1.0 2.0 3.0
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
                            string-append "Icosahedron: " (
                              to-str-space (
                                vol_icosahedron 2.5
                              )
                            )
                          )
                        )
                         (
                          string-append "Icosahedron: " (
                            to-str-space (
                              vol_icosahedron 2.5
                            )
                          )
                        )
                         (
                          to-str (
                            string-append "Icosahedron: " (
                              to-str-space (
                                vol_icosahedron 2.5
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
             (
              main
            )
          )
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
