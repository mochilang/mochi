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
      define (
        round_int x
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            ret1 (
              let (
                (
                  v2 (
                    + x 0.5
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
     (
      define (
        hsv_to_rgb h s v
      )
       (
        call/cc (
          lambda (
            ret3
          )
           (
            let (
              (
                i (
                  let (
                    (
                      v4 (
                        * h 6.0
                      )
                    )
                  )
                   (
                    cond (
                      (
                        string? v4
                      )
                       (
                        inexact->exact (
                          floor (
                            string->number v4
                          )
                        )
                      )
                    )
                     (
                      (
                        boolean? v4
                      )
                       (
                        if v4 1 0
                      )
                    )
                     (
                      else (
                        inexact->exact (
                          floor v4
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
                    f (
                      - (
                        * h 6.0
                      )
                       (
                        + 0.0 i
                      )
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        p (
                          * v (
                            - 1.0 s
                          )
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            q (
                              * v (
                                - 1.0 (
                                  * f s
                                )
                              )
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                t (
                                  * v (
                                    - 1.0 (
                                      * (
                                        - 1.0 f
                                      )
                                       s
                                    )
                                  )
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    mod (
                                      _mod i 6
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        r 0.0
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            g 0.0
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                b 0.0
                                              )
                                            )
                                             (
                                              begin (
                                                if (
                                                  equal? mod 0
                                                )
                                                 (
                                                  begin (
                                                    set! r v
                                                  )
                                                   (
                                                    set! g t
                                                  )
                                                   (
                                                    set! b p
                                                  )
                                                )
                                                 (
                                                  if (
                                                    equal? mod 1
                                                  )
                                                   (
                                                    begin (
                                                      set! r q
                                                    )
                                                     (
                                                      set! g v
                                                    )
                                                     (
                                                      set! b p
                                                    )
                                                  )
                                                   (
                                                    if (
                                                      equal? mod 2
                                                    )
                                                     (
                                                      begin (
                                                        set! r p
                                                      )
                                                       (
                                                        set! g v
                                                      )
                                                       (
                                                        set! b t
                                                      )
                                                    )
                                                     (
                                                      if (
                                                        equal? mod 3
                                                      )
                                                       (
                                                        begin (
                                                          set! r p
                                                        )
                                                         (
                                                          set! g q
                                                        )
                                                         (
                                                          set! b v
                                                        )
                                                      )
                                                       (
                                                        if (
                                                          equal? mod 4
                                                        )
                                                         (
                                                          begin (
                                                            set! r t
                                                          )
                                                           (
                                                            set! g p
                                                          )
                                                           (
                                                            set! b v
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            set! r v
                                                          )
                                                           (
                                                            set! g p
                                                          )
                                                           (
                                                            set! b q
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                ret3 (
                                                  alist->hash-table (
                                                    _list (
                                                      cons "r" (
                                                        round_int (
                                                          * r 255.0
                                                        )
                                                      )
                                                    )
                                                     (
                                                      cons "g" (
                                                        round_int (
                                                          * g 255.0
                                                        )
                                                      )
                                                    )
                                                     (
                                                      cons "b" (
                                                        round_int (
                                                          * b 255.0
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
        get_distance x y max_step
      )
       (
        call/cc (
          lambda (
            ret5
          )
           (
            let (
              (
                a x
              )
            )
             (
              begin (
                let (
                  (
                    b y
                  )
                )
                 (
                  begin (
                    let (
                      (
                        step (
                          - 1
                        )
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
                                      < step (
                                        - max_step 1
                                      )
                                    )
                                     (
                                      begin (
                                        set! step (
                                          + step 1
                                        )
                                      )
                                       (
                                        let (
                                          (
                                            a_new (
                                              _add (
                                                - (
                                                  * a a
                                                )
                                                 (
                                                  * b b
                                                )
                                              )
                                               x
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            set! b (
                                              _add (
                                                * (
                                                  * 2.0 a
                                                )
                                                 b
                                              )
                                               y
                                            )
                                          )
                                           (
                                            set! a a_new
                                          )
                                           (
                                            if (
                                              _gt (
                                                _add (
                                                  * a a
                                                )
                                                 (
                                                  * b b
                                                )
                                              )
                                               4.0
                                            )
                                             (
                                              begin (
                                                break7 (
                                                  quote (
                                                    
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              quote (
                                                
                                              )
                                            )
                                          )
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
                        ret5 (
                          _div (
                            + 0.0 step
                          )
                           (
                            + 0.0 (
                              - max_step 1
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
        get_black_and_white_rgb distance
      )
       (
        call/cc (
          lambda (
            ret8
          )
           (
            if (
              equal? distance 1.0
            )
             (
              begin (
                ret8 (
                  alist->hash-table (
                    _list (
                      cons "r" 0
                    )
                     (
                      cons "g" 0
                    )
                     (
                      cons "b" 0
                    )
                  )
                )
              )
            )
             (
              begin (
                ret8 (
                  alist->hash-table (
                    _list (
                      cons "r" 255
                    )
                     (
                      cons "g" 255
                    )
                     (
                      cons "b" 255
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
        get_color_coded_rgb distance
      )
       (
        call/cc (
          lambda (
            ret9
          )
           (
            if (
              equal? distance 1.0
            )
             (
              begin (
                ret9 (
                  alist->hash-table (
                    _list (
                      cons "r" 0
                    )
                     (
                      cons "g" 0
                    )
                     (
                      cons "b" 0
                    )
                  )
                )
              )
            )
             (
              begin (
                ret9 (
                  hsv_to_rgb distance 1.0 1.0
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        get_image image_width image_height figure_center_x figure_center_y figure_width max_step use_distance_color_coding
      )
       (
        call/cc (
          lambda (
            ret10
          )
           (
            let (
              (
                img (
                  _list
                )
              )
            )
             (
              begin (
                let (
                  (
                    figure_height (
                      * (
                        _div figure_width (
                          + 0.0 image_width
                        )
                      )
                       (
                        + 0.0 image_height
                      )
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        image_y 0
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
                                      < image_y image_height
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
                                                image_x 0
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
                                                              < image_x image_width
                                                            )
                                                             (
                                                              begin (
                                                                let (
                                                                  (
                                                                    fx (
                                                                      _add figure_center_x (
                                                                        * (
                                                                          - (
                                                                            _div (
                                                                              + 0.0 image_x
                                                                            )
                                                                             (
                                                                              + 0.0 image_width
                                                                            )
                                                                          )
                                                                           0.5
                                                                        )
                                                                         figure_width
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        fy (
                                                                          _add figure_center_y (
                                                                            * (
                                                                              - (
                                                                                _div (
                                                                                  + 0.0 image_y
                                                                                )
                                                                                 (
                                                                                  + 0.0 image_height
                                                                                )
                                                                              )
                                                                               0.5
                                                                            )
                                                                             figure_height
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            distance (
                                                                              get_distance fx fy max_step
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                rgb (
                                                                                  quote (
                                                                                    
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                if use_distance_color_coding (
                                                                                  begin (
                                                                                    set! rgb (
                                                                                      get_color_coded_rgb distance
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    set! rgb (
                                                                                      get_black_and_white_rgb distance
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                set! row (
                                                                                  append row (
                                                                                    _list rgb
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                set! image_x (
                                                                                  + image_x 1
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
                                                set! img (
                                                  append img (
                                                    _list row
                                                  )
                                                )
                                              )
                                               (
                                                set! image_y (
                                                  + image_y 1
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
                        ret10 img
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
        rgb_to_string c
      )
       (
        call/cc (
          lambda (
            ret15
          )
           (
            ret15 (
              string-append (
                string-append (
                  string-append (
                    string-append (
                      string-append (
                        string-append "(" (
                          to-str-space (
                            hash-table-ref c "r"
                          )
                        )
                      )
                       ", "
                    )
                     (
                      to-str-space (
                        hash-table-ref c "g"
                      )
                    )
                  )
                   ", "
                )
                 (
                  to-str-space (
                    hash-table-ref c "b"
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
      let (
        (
          img1 (
            get_image 10 10 (
              - 0.6
            )
             0.0 3.2 50 #t
          )
        )
      )
       (
        begin (
          _display (
            if (
              string? (
                rgb_to_string (
                  cond (
                    (
                      string? (
                        cond (
                          (
                            string? img1
                          )
                           (
                            _substring img1 0 (
                              + 0 1
                            )
                          )
                        )
                         (
                          (
                            hash-table? img1
                          )
                           (
                            hash-table-ref img1 0
                          )
                        )
                         (
                          else (
                            list-ref img1 0
                          )
                        )
                      )
                    )
                     (
                      _substring (
                        cond (
                          (
                            string? img1
                          )
                           (
                            _substring img1 0 (
                              + 0 1
                            )
                          )
                        )
                         (
                          (
                            hash-table? img1
                          )
                           (
                            hash-table-ref img1 0
                          )
                        )
                         (
                          else (
                            list-ref img1 0
                          )
                        )
                      )
                       0 (
                        + 0 1
                      )
                    )
                  )
                   (
                    (
                      hash-table? (
                        cond (
                          (
                            string? img1
                          )
                           (
                            _substring img1 0 (
                              + 0 1
                            )
                          )
                        )
                         (
                          (
                            hash-table? img1
                          )
                           (
                            hash-table-ref img1 0
                          )
                        )
                         (
                          else (
                            list-ref img1 0
                          )
                        )
                      )
                    )
                     (
                      hash-table-ref (
                        cond (
                          (
                            string? img1
                          )
                           (
                            _substring img1 0 (
                              + 0 1
                            )
                          )
                        )
                         (
                          (
                            hash-table? img1
                          )
                           (
                            hash-table-ref img1 0
                          )
                        )
                         (
                          else (
                            list-ref img1 0
                          )
                        )
                      )
                       0
                    )
                  )
                   (
                    else (
                      list-ref (
                        cond (
                          (
                            string? img1
                          )
                           (
                            _substring img1 0 (
                              + 0 1
                            )
                          )
                        )
                         (
                          (
                            hash-table? img1
                          )
                           (
                            hash-table-ref img1 0
                          )
                        )
                         (
                          else (
                            list-ref img1 0
                          )
                        )
                      )
                       0
                    )
                  )
                )
              )
            )
             (
              rgb_to_string (
                cond (
                  (
                    string? (
                      cond (
                        (
                          string? img1
                        )
                         (
                          _substring img1 0 (
                            + 0 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? img1
                        )
                         (
                          hash-table-ref img1 0
                        )
                      )
                       (
                        else (
                          list-ref img1 0
                        )
                      )
                    )
                  )
                   (
                    _substring (
                      cond (
                        (
                          string? img1
                        )
                         (
                          _substring img1 0 (
                            + 0 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? img1
                        )
                         (
                          hash-table-ref img1 0
                        )
                      )
                       (
                        else (
                          list-ref img1 0
                        )
                      )
                    )
                     0 (
                      + 0 1
                    )
                  )
                )
                 (
                  (
                    hash-table? (
                      cond (
                        (
                          string? img1
                        )
                         (
                          _substring img1 0 (
                            + 0 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? img1
                        )
                         (
                          hash-table-ref img1 0
                        )
                      )
                       (
                        else (
                          list-ref img1 0
                        )
                      )
                    )
                  )
                   (
                    hash-table-ref (
                      cond (
                        (
                          string? img1
                        )
                         (
                          _substring img1 0 (
                            + 0 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? img1
                        )
                         (
                          hash-table-ref img1 0
                        )
                      )
                       (
                        else (
                          list-ref img1 0
                        )
                      )
                    )
                     0
                  )
                )
                 (
                  else (
                    list-ref (
                      cond (
                        (
                          string? img1
                        )
                         (
                          _substring img1 0 (
                            + 0 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? img1
                        )
                         (
                          hash-table-ref img1 0
                        )
                      )
                       (
                        else (
                          list-ref img1 0
                        )
                      )
                    )
                     0
                  )
                )
              )
            )
             (
              to-str (
                rgb_to_string (
                  cond (
                    (
                      string? (
                        cond (
                          (
                            string? img1
                          )
                           (
                            _substring img1 0 (
                              + 0 1
                            )
                          )
                        )
                         (
                          (
                            hash-table? img1
                          )
                           (
                            hash-table-ref img1 0
                          )
                        )
                         (
                          else (
                            list-ref img1 0
                          )
                        )
                      )
                    )
                     (
                      _substring (
                        cond (
                          (
                            string? img1
                          )
                           (
                            _substring img1 0 (
                              + 0 1
                            )
                          )
                        )
                         (
                          (
                            hash-table? img1
                          )
                           (
                            hash-table-ref img1 0
                          )
                        )
                         (
                          else (
                            list-ref img1 0
                          )
                        )
                      )
                       0 (
                        + 0 1
                      )
                    )
                  )
                   (
                    (
                      hash-table? (
                        cond (
                          (
                            string? img1
                          )
                           (
                            _substring img1 0 (
                              + 0 1
                            )
                          )
                        )
                         (
                          (
                            hash-table? img1
                          )
                           (
                            hash-table-ref img1 0
                          )
                        )
                         (
                          else (
                            list-ref img1 0
                          )
                        )
                      )
                    )
                     (
                      hash-table-ref (
                        cond (
                          (
                            string? img1
                          )
                           (
                            _substring img1 0 (
                              + 0 1
                            )
                          )
                        )
                         (
                          (
                            hash-table? img1
                          )
                           (
                            hash-table-ref img1 0
                          )
                        )
                         (
                          else (
                            list-ref img1 0
                          )
                        )
                      )
                       0
                    )
                  )
                   (
                    else (
                      list-ref (
                        cond (
                          (
                            string? img1
                          )
                           (
                            _substring img1 0 (
                              + 0 1
                            )
                          )
                        )
                         (
                          (
                            hash-table? img1
                          )
                           (
                            hash-table-ref img1 0
                          )
                        )
                         (
                          else (
                            list-ref img1 0
                          )
                        )
                      )
                       0
                    )
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
              img2 (
                get_image 10 10 (
                  - 0.6
                )
                 0.0 3.2 50 #f
              )
            )
          )
           (
            begin (
              _display (
                if (
                  string? (
                    rgb_to_string (
                      cond (
                        (
                          string? (
                            cond (
                              (
                                string? img2
                              )
                               (
                                _substring img2 0 (
                                  + 0 1
                                )
                              )
                            )
                             (
                              (
                                hash-table? img2
                              )
                               (
                                hash-table-ref img2 0
                              )
                            )
                             (
                              else (
                                list-ref img2 0
                              )
                            )
                          )
                        )
                         (
                          _substring (
                            cond (
                              (
                                string? img2
                              )
                               (
                                _substring img2 0 (
                                  + 0 1
                                )
                              )
                            )
                             (
                              (
                                hash-table? img2
                              )
                               (
                                hash-table-ref img2 0
                              )
                            )
                             (
                              else (
                                list-ref img2 0
                              )
                            )
                          )
                           0 (
                            + 0 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? (
                            cond (
                              (
                                string? img2
                              )
                               (
                                _substring img2 0 (
                                  + 0 1
                                )
                              )
                            )
                             (
                              (
                                hash-table? img2
                              )
                               (
                                hash-table-ref img2 0
                              )
                            )
                             (
                              else (
                                list-ref img2 0
                              )
                            )
                          )
                        )
                         (
                          hash-table-ref (
                            cond (
                              (
                                string? img2
                              )
                               (
                                _substring img2 0 (
                                  + 0 1
                                )
                              )
                            )
                             (
                              (
                                hash-table? img2
                              )
                               (
                                hash-table-ref img2 0
                              )
                            )
                             (
                              else (
                                list-ref img2 0
                              )
                            )
                          )
                           0
                        )
                      )
                       (
                        else (
                          list-ref (
                            cond (
                              (
                                string? img2
                              )
                               (
                                _substring img2 0 (
                                  + 0 1
                                )
                              )
                            )
                             (
                              (
                                hash-table? img2
                              )
                               (
                                hash-table-ref img2 0
                              )
                            )
                             (
                              else (
                                list-ref img2 0
                              )
                            )
                          )
                           0
                        )
                      )
                    )
                  )
                )
                 (
                  rgb_to_string (
                    cond (
                      (
                        string? (
                          cond (
                            (
                              string? img2
                            )
                             (
                              _substring img2 0 (
                                + 0 1
                              )
                            )
                          )
                           (
                            (
                              hash-table? img2
                            )
                             (
                              hash-table-ref img2 0
                            )
                          )
                           (
                            else (
                              list-ref img2 0
                            )
                          )
                        )
                      )
                       (
                        _substring (
                          cond (
                            (
                              string? img2
                            )
                             (
                              _substring img2 0 (
                                + 0 1
                              )
                            )
                          )
                           (
                            (
                              hash-table? img2
                            )
                             (
                              hash-table-ref img2 0
                            )
                          )
                           (
                            else (
                              list-ref img2 0
                            )
                          )
                        )
                         0 (
                          + 0 1
                        )
                      )
                    )
                     (
                      (
                        hash-table? (
                          cond (
                            (
                              string? img2
                            )
                             (
                              _substring img2 0 (
                                + 0 1
                              )
                            )
                          )
                           (
                            (
                              hash-table? img2
                            )
                             (
                              hash-table-ref img2 0
                            )
                          )
                           (
                            else (
                              list-ref img2 0
                            )
                          )
                        )
                      )
                       (
                        hash-table-ref (
                          cond (
                            (
                              string? img2
                            )
                             (
                              _substring img2 0 (
                                + 0 1
                              )
                            )
                          )
                           (
                            (
                              hash-table? img2
                            )
                             (
                              hash-table-ref img2 0
                            )
                          )
                           (
                            else (
                              list-ref img2 0
                            )
                          )
                        )
                         0
                      )
                    )
                     (
                      else (
                        list-ref (
                          cond (
                            (
                              string? img2
                            )
                             (
                              _substring img2 0 (
                                + 0 1
                              )
                            )
                          )
                           (
                            (
                              hash-table? img2
                            )
                             (
                              hash-table-ref img2 0
                            )
                          )
                           (
                            else (
                              list-ref img2 0
                            )
                          )
                        )
                         0
                      )
                    )
                  )
                )
                 (
                  to-str (
                    rgb_to_string (
                      cond (
                        (
                          string? (
                            cond (
                              (
                                string? img2
                              )
                               (
                                _substring img2 0 (
                                  + 0 1
                                )
                              )
                            )
                             (
                              (
                                hash-table? img2
                              )
                               (
                                hash-table-ref img2 0
                              )
                            )
                             (
                              else (
                                list-ref img2 0
                              )
                            )
                          )
                        )
                         (
                          _substring (
                            cond (
                              (
                                string? img2
                              )
                               (
                                _substring img2 0 (
                                  + 0 1
                                )
                              )
                            )
                             (
                              (
                                hash-table? img2
                              )
                               (
                                hash-table-ref img2 0
                              )
                            )
                             (
                              else (
                                list-ref img2 0
                              )
                            )
                          )
                           0 (
                            + 0 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? (
                            cond (
                              (
                                string? img2
                              )
                               (
                                _substring img2 0 (
                                  + 0 1
                                )
                              )
                            )
                             (
                              (
                                hash-table? img2
                              )
                               (
                                hash-table-ref img2 0
                              )
                            )
                             (
                              else (
                                list-ref img2 0
                              )
                            )
                          )
                        )
                         (
                          hash-table-ref (
                            cond (
                              (
                                string? img2
                              )
                               (
                                _substring img2 0 (
                                  + 0 1
                                )
                              )
                            )
                             (
                              (
                                hash-table? img2
                              )
                               (
                                hash-table-ref img2 0
                              )
                            )
                             (
                              else (
                                list-ref img2 0
                              )
                            )
                          )
                           0
                        )
                      )
                       (
                        else (
                          list-ref (
                            cond (
                              (
                                string? img2
                              )
                               (
                                _substring img2 0 (
                                  + 0 1
                                )
                              )
                            )
                             (
                              (
                                hash-table? img2
                              )
                               (
                                hash-table-ref img2 0
                              )
                            )
                             (
                              else (
                                list-ref img2 0
                              )
                            )
                          )
                           0
                        )
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
