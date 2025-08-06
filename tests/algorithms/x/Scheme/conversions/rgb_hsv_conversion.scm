;; Generated on 2025-08-06 23:15 +0700
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
      start12 (
        current-jiffy
      )
    )
     (
      jps15 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        absf x
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            begin (
              if (
                < x 0.0
              )
               (
                begin (
                  ret1 (
                    - x
                  )
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              ret1 x
            )
          )
        )
      )
    )
     (
      define (
        fmod a b
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            ret2 (
              - a (
                * b (
                  let (
                    (
                      v3 (
                        _div a b
                      )
                    )
                  )
                   (
                    cond (
                      (
                        string? v3
                      )
                       (
                        exact (
                          floor (
                            string->number v3
                          )
                        )
                      )
                    )
                     (
                      (
                        boolean? v3
                      )
                       (
                        if v3 1 0
                      )
                    )
                     (
                      else (
                        exact (
                          floor v3
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
        roundf x
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            begin (
              if (
                >= x 0.0
              )
               (
                begin (
                  ret4 (
                    let (
                      (
                        v5 (
                          + x 0.5
                        )
                      )
                    )
                     (
                      cond (
                        (
                          string? v5
                        )
                         (
                          exact (
                            floor (
                              string->number v5
                            )
                          )
                        )
                      )
                       (
                        (
                          boolean? v5
                        )
                         (
                          if v5 1 0
                        )
                      )
                       (
                        else (
                          exact (
                            floor v5
                          )
                        )
                      )
                    )
                  )
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              ret4 (
                let (
                  (
                    v6 (
                      - x 0.5
                    )
                  )
                )
                 (
                  cond (
                    (
                      string? v6
                    )
                     (
                      exact (
                        floor (
                          string->number v6
                        )
                      )
                    )
                  )
                   (
                    (
                      boolean? v6
                    )
                     (
                      if v6 1 0
                    )
                  )
                   (
                    else (
                      exact (
                        floor v6
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
        maxf a b c
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            let (
              (
                m a
              )
            )
             (
              begin (
                if (
                  > b m
                )
                 (
                  begin (
                    set! m b
                  )
                )
                 (
                  quote (
                    
                  )
                )
              )
               (
                if (
                  > c m
                )
                 (
                  begin (
                    set! m c
                  )
                )
                 (
                  quote (
                    
                  )
                )
              )
               (
                ret7 m
              )
            )
          )
        )
      )
    )
     (
      define (
        minf a b c
      )
       (
        call/cc (
          lambda (
            ret8
          )
           (
            let (
              (
                m a
              )
            )
             (
              begin (
                if (
                  < b m
                )
                 (
                  begin (
                    set! m b
                  )
                )
                 (
                  quote (
                    
                  )
                )
              )
               (
                if (
                  < c m
                )
                 (
                  begin (
                    set! m c
                  )
                )
                 (
                  quote (
                    
                  )
                )
              )
               (
                ret8 m
              )
            )
          )
        )
      )
    )
     (
      define (
        hsv_to_rgb hue saturation value
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
                  < hue 0.0
                )
                 (
                  > hue 360.0
                )
              )
               (
                begin (
                  _display (
                    if (
                      string? "hue should be between 0 and 360"
                    )
                     "hue should be between 0 and 360" (
                      to-str "hue should be between 0 and 360"
                    )
                  )
                )
                 (
                  newline
                )
                 (
                  ret9 (
                    _list
                  )
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
                  < saturation 0.0
                )
                 (
                  > saturation 1.0
                )
              )
               (
                begin (
                  _display (
                    if (
                      string? "saturation should be between 0 and 1"
                    )
                     "saturation should be between 0 and 1" (
                      to-str "saturation should be between 0 and 1"
                    )
                  )
                )
                 (
                  newline
                )
                 (
                  ret9 (
                    _list
                  )
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
                  < value 0.0
                )
                 (
                  > value 1.0
                )
              )
               (
                begin (
                  _display (
                    if (
                      string? "value should be between 0 and 1"
                    )
                     "value should be between 0 and 1" (
                      to-str "value should be between 0 and 1"
                    )
                  )
                )
                 (
                  newline
                )
                 (
                  ret9 (
                    _list
                  )
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
                  chroma (
                    * value saturation
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      hue_section (
                        _div hue 60.0
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          second_largest_component (
                            * chroma (
                              - 1.0 (
                                absf (
                                  - (
                                    fmod hue_section 2.0
                                  )
                                   1.0
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
                              match_value (
                                - value chroma
                              )
                            )
                          )
                           (
                            begin (
                              let (
                                (
                                  red 0
                                )
                              )
                               (
                                begin (
                                  let (
                                    (
                                      green 0
                                    )
                                  )
                                   (
                                    begin (
                                      let (
                                        (
                                          blue 0
                                        )
                                      )
                                       (
                                        begin (
                                          if (
                                            and (
                                              >= hue_section 0.0
                                            )
                                             (
                                              <= hue_section 1.0
                                            )
                                          )
                                           (
                                            begin (
                                              set! red (
                                                roundf (
                                                  * 255.0 (
                                                    + chroma match_value
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              set! green (
                                                roundf (
                                                  * 255.0 (
                                                    _add second_largest_component match_value
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              set! blue (
                                                roundf (
                                                  * 255.0 match_value
                                                )
                                              )
                                            )
                                          )
                                           (
                                            if (
                                              and (
                                                > hue_section 1.0
                                              )
                                               (
                                                <= hue_section 2.0
                                              )
                                            )
                                             (
                                              begin (
                                                set! red (
                                                  roundf (
                                                    * 255.0 (
                                                      _add second_largest_component match_value
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                set! green (
                                                  roundf (
                                                    * 255.0 (
                                                      + chroma match_value
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                set! blue (
                                                  roundf (
                                                    * 255.0 match_value
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              if (
                                                and (
                                                  > hue_section 2.0
                                                )
                                                 (
                                                  <= hue_section 3.0
                                                )
                                              )
                                               (
                                                begin (
                                                  set! red (
                                                    roundf (
                                                      * 255.0 match_value
                                                    )
                                                  )
                                                )
                                                 (
                                                  set! green (
                                                    roundf (
                                                      * 255.0 (
                                                        + chroma match_value
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  set! blue (
                                                    roundf (
                                                      * 255.0 (
                                                        _add second_largest_component match_value
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                if (
                                                  and (
                                                    > hue_section 3.0
                                                  )
                                                   (
                                                    <= hue_section 4.0
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    set! red (
                                                      roundf (
                                                        * 255.0 match_value
                                                      )
                                                    )
                                                  )
                                                   (
                                                    set! green (
                                                      roundf (
                                                        * 255.0 (
                                                          _add second_largest_component match_value
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    set! blue (
                                                      roundf (
                                                        * 255.0 (
                                                          + chroma match_value
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  if (
                                                    and (
                                                      > hue_section 4.0
                                                    )
                                                     (
                                                      <= hue_section 5.0
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      set! red (
                                                        roundf (
                                                          * 255.0 (
                                                            _add second_largest_component match_value
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      set! green (
                                                        roundf (
                                                          * 255.0 match_value
                                                        )
                                                      )
                                                    )
                                                     (
                                                      set! blue (
                                                        roundf (
                                                          * 255.0 (
                                                            + chroma match_value
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      set! red (
                                                        roundf (
                                                          * 255.0 (
                                                            + chroma match_value
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      set! green (
                                                        roundf (
                                                          * 255.0 match_value
                                                        )
                                                      )
                                                    )
                                                     (
                                                      set! blue (
                                                        roundf (
                                                          * 255.0 (
                                                            _add second_largest_component match_value
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
                                          ret9 (
                                            _list red green blue
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
        rgb_to_hsv red green blue
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
                  < red 0
                )
                 (
                  > red 255
                )
              )
               (
                begin (
                  _display (
                    if (
                      string? "red should be between 0 and 255"
                    )
                     "red should be between 0 and 255" (
                      to-str "red should be between 0 and 255"
                    )
                  )
                )
                 (
                  newline
                )
                 (
                  ret10 (
                    _list
                  )
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
                  < green 0
                )
                 (
                  > green 255
                )
              )
               (
                begin (
                  _display (
                    if (
                      string? "green should be between 0 and 255"
                    )
                     "green should be between 0 and 255" (
                      to-str "green should be between 0 and 255"
                    )
                  )
                )
                 (
                  newline
                )
                 (
                  ret10 (
                    _list
                  )
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
                  < blue 0
                )
                 (
                  > blue 255
                )
              )
               (
                begin (
                  _display (
                    if (
                      string? "blue should be between 0 and 255"
                    )
                     "blue should be between 0 and 255" (
                      to-str "blue should be between 0 and 255"
                    )
                  )
                )
                 (
                  newline
                )
                 (
                  ret10 (
                    _list
                  )
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
                  float_red (
                    _div red 255.0
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      float_green (
                        _div green 255.0
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          float_blue (
                            _div blue 255.0
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              value (
                                maxf float_red float_green float_blue
                              )
                            )
                          )
                           (
                            begin (
                              let (
                                (
                                  min_val (
                                    minf float_red float_green float_blue
                                  )
                                )
                              )
                               (
                                begin (
                                  let (
                                    (
                                      chroma (
                                        - value min_val
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      let (
                                        (
                                          saturation (
                                            if (
                                              equal? value 0.0
                                            )
                                             0.0 (
                                              _div chroma value
                                            )
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          let (
                                            (
                                              hue 0.0
                                            )
                                          )
                                           (
                                            begin (
                                              if (
                                                equal? chroma 0.0
                                              )
                                               (
                                                begin (
                                                  set! hue 0.0
                                                )
                                              )
                                               (
                                                if (
                                                  equal? value float_red
                                                )
                                                 (
                                                  begin (
                                                    set! hue (
                                                      * 60.0 (
                                                        _add 0.0 (
                                                          _div (
                                                            - float_green float_blue
                                                          )
                                                           chroma
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  if (
                                                    equal? value float_green
                                                  )
                                                   (
                                                    begin (
                                                      set! hue (
                                                        * 60.0 (
                                                          _add 2.0 (
                                                            _div (
                                                              - float_blue float_red
                                                            )
                                                             chroma
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      set! hue (
                                                        * 60.0 (
                                                          _add 4.0 (
                                                            _div (
                                                              - float_red float_green
                                                            )
                                                             chroma
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              set! hue (
                                                fmod (
                                                  + hue 360.0
                                                )
                                                 360.0
                                              )
                                            )
                                             (
                                              ret10 (
                                                _list hue saturation value
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
        approximately_equal_hsv hsv1 hsv2
      )
       (
        call/cc (
          lambda (
            ret11
          )
           (
            let (
              (
                check_hue (
                  _lt (
                    absf (
                      - (
                        list-ref hsv1 0
                      )
                       (
                        list-ref hsv2 0
                      )
                    )
                  )
                   0.2
                )
              )
            )
             (
              begin (
                let (
                  (
                    check_saturation (
                      _lt (
                        absf (
                          - (
                            list-ref hsv1 1
                          )
                           (
                            list-ref hsv2 1
                          )
                        )
                      )
                       0.002
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        check_value (
                          _lt (
                            absf (
                              - (
                                list-ref hsv1 2
                              )
                               (
                                list-ref hsv2 2
                              )
                            )
                          )
                           0.002
                        )
                      )
                    )
                     (
                      begin (
                        ret11 (
                          and (
                            and check_hue check_saturation
                          )
                           check_value
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
          rgb (
            hsv_to_rgb 180.0 0.5 0.5
          )
        )
      )
       (
        begin (
          _display (
            if (
              string? (
                to-str-space rgb
              )
            )
             (
              to-str-space rgb
            )
             (
              to-str (
                to-str-space rgb
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
              hsv (
                rgb_to_hsv 64 128 128
              )
            )
          )
           (
            begin (
              _display (
                if (
                  string? (
                    to-str-space hsv
                  )
                )
                 (
                  to-str-space hsv
                )
                 (
                  to-str (
                    to-str-space hsv
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
                      approximately_equal_hsv hsv (
                        _list 180.0 0.5 0.5
                      )
                    )
                  )
                )
                 (
                  to-str-space (
                    approximately_equal_hsv hsv (
                      _list 180.0 0.5 0.5
                    )
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      approximately_equal_hsv hsv (
                        _list 180.0 0.5 0.5
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
          end13 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur14 (
              quotient (
                * (
                  - end13 start12
                )
                 1000000
              )
               jps15
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur14
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
