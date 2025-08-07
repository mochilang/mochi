;; Generated on 2025-08-07 08:40 +0700
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
      start17 (
        current-jiffy
      )
    )
     (
      jps20 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      let (
        (
          valid_colors (
            _list "Black" "Brown" "Red" "Orange" "Yellow" "Green" "Blue" "Violet" "Grey" "White" "Gold" "Silver"
          )
        )
      )
       (
        begin (
          let (
            (
              significant_figures_color_values (
                alist->hash-table (
                  _list (
                    cons "Black" 0
                  )
                   (
                    cons "Brown" 1
                  )
                   (
                    cons "Red" 2
                  )
                   (
                    cons "Orange" 3
                  )
                   (
                    cons "Yellow" 4
                  )
                   (
                    cons "Green" 5
                  )
                   (
                    cons "Blue" 6
                  )
                   (
                    cons "Violet" 7
                  )
                   (
                    cons "Grey" 8
                  )
                   (
                    cons "White" 9
                  )
                )
              )
            )
          )
           (
            begin (
              let (
                (
                  multiplier_color_values (
                    alist->hash-table (
                      _list (
                        cons "Black" 1.0
                      )
                       (
                        cons "Brown" 10.0
                      )
                       (
                        cons "Red" 100.0
                      )
                       (
                        cons "Orange" 1000.0
                      )
                       (
                        cons "Yellow" 10000.0
                      )
                       (
                        cons "Green" 100000.0
                      )
                       (
                        cons "Blue" 1000000.0
                      )
                       (
                        cons "Violet" 10000000.0
                      )
                       (
                        cons "Grey" 100000000.0
                      )
                       (
                        cons "White" 1000000000.0
                      )
                       (
                        cons "Gold" 0.1
                      )
                       (
                        cons "Silver" 0.01
                      )
                    )
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      tolerance_color_values (
                        alist->hash-table (
                          _list (
                            cons "Brown" 1.0
                          )
                           (
                            cons "Red" 2.0
                          )
                           (
                            cons "Orange" 0.05
                          )
                           (
                            cons "Yellow" 0.02
                          )
                           (
                            cons "Green" 0.5
                          )
                           (
                            cons "Blue" 0.25
                          )
                           (
                            cons "Violet" 0.1
                          )
                           (
                            cons "Grey" 0.01
                          )
                           (
                            cons "Gold" 5.0
                          )
                           (
                            cons "Silver" 10.0
                          )
                        )
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          temperature_coeffecient_color_values (
                            alist->hash-table (
                              _list (
                                cons "Black" 250
                              )
                               (
                                cons "Brown" 100
                              )
                               (
                                cons "Red" 50
                              )
                               (
                                cons "Orange" 15
                              )
                               (
                                cons "Yellow" 25
                              )
                               (
                                cons "Green" 20
                              )
                               (
                                cons "Blue" 10
                              )
                               (
                                cons "Violet" 5
                              )
                               (
                                cons "Grey" 1
                              )
                            )
                          )
                        )
                      )
                       (
                        begin (
                          define (
                            contains list value
                          )
                           (
                            call/cc (
                              lambda (
                                ret1
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
                                              xs
                                            )
                                             (
                                              if (
                                                null? xs
                                              )
                                               (
                                                quote (
                                                  
                                                )
                                              )
                                               (
                                                begin (
                                                  let (
                                                    (
                                                      c (
                                                        car xs
                                                      )
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      if (
                                                        string=? c value
                                                      )
                                                       (
                                                        begin (
                                                          ret1 #t
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
                                                  loop2 (
                                                    cdr xs
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                       (
                                        loop2 list
                                      )
                                    )
                                  )
                                )
                                 (
                                  ret1 #f
                                )
                              )
                            )
                          )
                        )
                         (
                          define (
                            get_significant_digits colors
                          )
                           (
                            call/cc (
                              lambda (
                                ret4
                              )
                               (
                                let (
                                  (
                                    digit 0
                                  )
                                )
                                 (
                                  begin (
                                    call/cc (
                                      lambda (
                                        break6
                                      )
                                       (
                                        letrec (
                                          (
                                            loop5 (
                                              lambda (
                                                xs
                                              )
                                               (
                                                if (
                                                  null? xs
                                                )
                                                 (
                                                  quote (
                                                    
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        color (
                                                          car xs
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        if (
                                                          not (
                                                            cond (
                                                              (
                                                                string? significant_figures_color_values
                                                              )
                                                               (
                                                                if (
                                                                  string-contains significant_figures_color_values color
                                                                )
                                                                 #t #f
                                                              )
                                                            )
                                                             (
                                                              (
                                                                hash-table? significant_figures_color_values
                                                              )
                                                               (
                                                                if (
                                                                  hash-table-exists? significant_figures_color_values color
                                                                )
                                                                 #t #f
                                                              )
                                                            )
                                                             (
                                                              else (
                                                                if (
                                                                  member color significant_figures_color_values
                                                                )
                                                                 #t #f
                                                              )
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            panic (
                                                              string-append color " is not a valid color for significant figure bands"
                                                            )
                                                          )
                                                        )
                                                         (
                                                          quote (
                                                            
                                                          )
                                                        )
                                                      )
                                                       (
                                                        set! digit (
                                                          _add (
                                                            * digit 10
                                                          )
                                                           (
                                                            cond (
                                                              (
                                                                string? significant_figures_color_values
                                                              )
                                                               (
                                                                _substring significant_figures_color_values color (
                                                                  + color 1
                                                                )
                                                              )
                                                            )
                                                             (
                                                              (
                                                                hash-table? significant_figures_color_values
                                                              )
                                                               (
                                                                hash-table-ref significant_figures_color_values color
                                                              )
                                                            )
                                                             (
                                                              else (
                                                                list-ref significant_figures_color_values color
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    loop5 (
                                                      cdr xs
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                         (
                                          loop5 colors
                                        )
                                      )
                                    )
                                  )
                                   (
                                    ret4 digit
                                  )
                                )
                              )
                            )
                          )
                        )
                         (
                          define (
                            get_multiplier color
                          )
                           (
                            call/cc (
                              lambda (
                                ret7
                              )
                               (
                                begin (
                                  if (
                                    not (
                                      cond (
                                        (
                                          string? multiplier_color_values
                                        )
                                         (
                                          if (
                                            string-contains multiplier_color_values color
                                          )
                                           #t #f
                                        )
                                      )
                                       (
                                        (
                                          hash-table? multiplier_color_values
                                        )
                                         (
                                          if (
                                            hash-table-exists? multiplier_color_values color
                                          )
                                           #t #f
                                        )
                                      )
                                       (
                                        else (
                                          if (
                                            member color multiplier_color_values
                                          )
                                           #t #f
                                        )
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      panic (
                                        string-append color " is not a valid color for multiplier band"
                                      )
                                    )
                                  )
                                   (
                                    quote (
                                      
                                    )
                                  )
                                )
                                 (
                                  ret7 (
                                    cond (
                                      (
                                        string? multiplier_color_values
                                      )
                                       (
                                        _substring multiplier_color_values color (
                                          + color 1
                                        )
                                      )
                                    )
                                     (
                                      (
                                        hash-table? multiplier_color_values
                                      )
                                       (
                                        hash-table-ref multiplier_color_values color
                                      )
                                    )
                                     (
                                      else (
                                        list-ref multiplier_color_values color
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
                            get_tolerance color
                          )
                           (
                            call/cc (
                              lambda (
                                ret8
                              )
                               (
                                begin (
                                  if (
                                    not (
                                      cond (
                                        (
                                          string? tolerance_color_values
                                        )
                                         (
                                          if (
                                            string-contains tolerance_color_values color
                                          )
                                           #t #f
                                        )
                                      )
                                       (
                                        (
                                          hash-table? tolerance_color_values
                                        )
                                         (
                                          if (
                                            hash-table-exists? tolerance_color_values color
                                          )
                                           #t #f
                                        )
                                      )
                                       (
                                        else (
                                          if (
                                            member color tolerance_color_values
                                          )
                                           #t #f
                                        )
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      panic (
                                        string-append color " is not a valid color for tolerance band"
                                      )
                                    )
                                  )
                                   (
                                    quote (
                                      
                                    )
                                  )
                                )
                                 (
                                  ret8 (
                                    cond (
                                      (
                                        string? tolerance_color_values
                                      )
                                       (
                                        _substring tolerance_color_values color (
                                          + color 1
                                        )
                                      )
                                    )
                                     (
                                      (
                                        hash-table? tolerance_color_values
                                      )
                                       (
                                        hash-table-ref tolerance_color_values color
                                      )
                                    )
                                     (
                                      else (
                                        list-ref tolerance_color_values color
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
                            get_temperature_coeffecient color
                          )
                           (
                            call/cc (
                              lambda (
                                ret9
                              )
                               (
                                begin (
                                  if (
                                    not (
                                      cond (
                                        (
                                          string? temperature_coeffecient_color_values
                                        )
                                         (
                                          if (
                                            string-contains temperature_coeffecient_color_values color
                                          )
                                           #t #f
                                        )
                                      )
                                       (
                                        (
                                          hash-table? temperature_coeffecient_color_values
                                        )
                                         (
                                          if (
                                            hash-table-exists? temperature_coeffecient_color_values color
                                          )
                                           #t #f
                                        )
                                      )
                                       (
                                        else (
                                          if (
                                            member color temperature_coeffecient_color_values
                                          )
                                           #t #f
                                        )
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      panic (
                                        string-append color " is not a valid color for temperature coeffecient band"
                                      )
                                    )
                                  )
                                   (
                                    quote (
                                      
                                    )
                                  )
                                )
                                 (
                                  ret9 (
                                    cond (
                                      (
                                        string? temperature_coeffecient_color_values
                                      )
                                       (
                                        _substring temperature_coeffecient_color_values color (
                                          + color 1
                                        )
                                      )
                                    )
                                     (
                                      (
                                        hash-table? temperature_coeffecient_color_values
                                      )
                                       (
                                        hash-table-ref temperature_coeffecient_color_values color
                                      )
                                    )
                                     (
                                      else (
                                        list-ref temperature_coeffecient_color_values color
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
                            get_band_type_count total typ
                          )
                           (
                            call/cc (
                              lambda (
                                ret10
                              )
                               (
                                if (
                                  equal? total 3
                                )
                                 (
                                  begin (
                                    if (
                                      string=? typ "significant"
                                    )
                                     (
                                      begin (
                                        ret10 2
                                      )
                                    )
                                     (
                                      quote (
                                        
                                      )
                                    )
                                  )
                                   (
                                    if (
                                      string=? typ "multiplier"
                                    )
                                     (
                                      begin (
                                        ret10 1
                                      )
                                    )
                                     (
                                      quote (
                                        
                                      )
                                    )
                                  )
                                   (
                                    panic (
                                      string-append typ " is not valid for a 3 band resistor"
                                    )
                                  )
                                )
                                 (
                                  if (
                                    equal? total 4
                                  )
                                   (
                                    begin (
                                      if (
                                        string=? typ "significant"
                                      )
                                       (
                                        begin (
                                          ret10 2
                                        )
                                      )
                                       (
                                        quote (
                                          
                                        )
                                      )
                                    )
                                     (
                                      if (
                                        string=? typ "multiplier"
                                      )
                                       (
                                        begin (
                                          ret10 1
                                        )
                                      )
                                       (
                                        quote (
                                          
                                        )
                                      )
                                    )
                                     (
                                      if (
                                        string=? typ "tolerance"
                                      )
                                       (
                                        begin (
                                          ret10 1
                                        )
                                      )
                                       (
                                        quote (
                                          
                                        )
                                      )
                                    )
                                     (
                                      panic (
                                        string-append typ " is not valid for a 4 band resistor"
                                      )
                                    )
                                  )
                                   (
                                    if (
                                      equal? total 5
                                    )
                                     (
                                      begin (
                                        if (
                                          string=? typ "significant"
                                        )
                                         (
                                          begin (
                                            ret10 3
                                          )
                                        )
                                         (
                                          quote (
                                            
                                          )
                                        )
                                      )
                                       (
                                        if (
                                          string=? typ "multiplier"
                                        )
                                         (
                                          begin (
                                            ret10 1
                                          )
                                        )
                                         (
                                          quote (
                                            
                                          )
                                        )
                                      )
                                       (
                                        if (
                                          string=? typ "tolerance"
                                        )
                                         (
                                          begin (
                                            ret10 1
                                          )
                                        )
                                         (
                                          quote (
                                            
                                          )
                                        )
                                      )
                                       (
                                        panic (
                                          string-append typ " is not valid for a 5 band resistor"
                                        )
                                      )
                                    )
                                     (
                                      if (
                                        equal? total 6
                                      )
                                       (
                                        begin (
                                          if (
                                            string=? typ "significant"
                                          )
                                           (
                                            begin (
                                              ret10 3
                                            )
                                          )
                                           (
                                            quote (
                                              
                                            )
                                          )
                                        )
                                         (
                                          if (
                                            string=? typ "multiplier"
                                          )
                                           (
                                            begin (
                                              ret10 1
                                            )
                                          )
                                           (
                                            quote (
                                              
                                            )
                                          )
                                        )
                                         (
                                          if (
                                            string=? typ "tolerance"
                                          )
                                           (
                                            begin (
                                              ret10 1
                                            )
                                          )
                                           (
                                            quote (
                                              
                                            )
                                          )
                                        )
                                         (
                                          if (
                                            string=? typ "temp_coeffecient"
                                          )
                                           (
                                            begin (
                                              ret10 1
                                            )
                                          )
                                           (
                                            quote (
                                              
                                            )
                                          )
                                        )
                                         (
                                          panic (
                                            string-append typ " is not valid for a 6 band resistor"
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          panic (
                                            string-append (
                                              to-str-space total
                                            )
                                             " is not a valid number of bands"
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
                            check_validity number_of_bands colors
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
                                      < number_of_bands 3
                                    )
                                     (
                                      > number_of_bands 6
                                    )
                                  )
                                   (
                                    begin (
                                      panic "Invalid number of bands. Resistor bands must be 3 to 6"
                                    )
                                  )
                                   (
                                    quote (
                                      
                                    )
                                  )
                                )
                                 (
                                  if (
                                    not (
                                      equal? number_of_bands (
                                        _len colors
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      panic (
                                        string-append (
                                          string-append (
                                            string-append (
                                              string-append "Expecting " (
                                                to-str-space number_of_bands
                                              )
                                            )
                                             " colors, provided "
                                          )
                                           (
                                            to-str-space (
                                              _len colors
                                            )
                                          )
                                        )
                                         " colors"
                                      )
                                    )
                                  )
                                   (
                                    quote (
                                      
                                    )
                                  )
                                )
                                 (
                                  call/cc (
                                    lambda (
                                      break13
                                    )
                                     (
                                      letrec (
                                        (
                                          loop12 (
                                            lambda (
                                              xs
                                            )
                                             (
                                              if (
                                                null? xs
                                              )
                                               (
                                                quote (
                                                  
                                                )
                                              )
                                               (
                                                begin (
                                                  let (
                                                    (
                                                      color (
                                                        car xs
                                                      )
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      if (
                                                        not (
                                                          contains valid_colors color
                                                        )
                                                      )
                                                       (
                                                        begin (
                                                          panic (
                                                            string-append color " is not a valid color"
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
                                                  loop12 (
                                                    cdr xs
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                       (
                                        loop12 colors
                                      )
                                    )
                                  )
                                )
                                 (
                                  ret11 #t
                                )
                              )
                            )
                          )
                        )
                         (
                          define (
                            calculate_resistance number_of_bands color_code_list
                          )
                           (
                            call/cc (
                              lambda (
                                ret14
                              )
                               (
                                begin (
                                  check_validity number_of_bands color_code_list
                                )
                                 (
                                  let (
                                    (
                                      sig_count (
                                        get_band_type_count number_of_bands "significant"
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      let (
                                        (
                                          significant_colors (
                                            take (
                                              drop color_code_list 0
                                            )
                                             (
                                              - sig_count 0
                                            )
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          let (
                                            (
                                              significant_digits (
                                                get_significant_digits significant_colors
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              let (
                                                (
                                                  multiplier_color (
                                                    list-ref color_code_list sig_count
                                                  )
                                                )
                                              )
                                               (
                                                begin (
                                                  let (
                                                    (
                                                      multiplier (
                                                        get_multiplier multiplier_color
                                                      )
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      let (
                                                        (
                                                          tolerance 20.0
                                                        )
                                                      )
                                                       (
                                                        begin (
                                                          if (
                                                            >= number_of_bands 4
                                                          )
                                                           (
                                                            begin (
                                                              let (
                                                                (
                                                                  tolerance_color (
                                                                    list-ref color_code_list (
                                                                      _add sig_count 1
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                begin (
                                                                  set! tolerance (
                                                                    get_tolerance tolerance_color
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
                                                          let (
                                                            (
                                                              temp_coeff 0
                                                            )
                                                          )
                                                           (
                                                            begin (
                                                              if (
                                                                equal? number_of_bands 6
                                                              )
                                                               (
                                                                begin (
                                                                  let (
                                                                    (
                                                                      temp_color (
                                                                        list-ref color_code_list (
                                                                          _add sig_count 2
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    begin (
                                                                      set! temp_coeff (
                                                                        get_temperature_coeffecient temp_color
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
                                                              let (
                                                                (
                                                                  resistance_value (
                                                                    * multiplier significant_digits
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                begin (
                                                                  let (
                                                                    (
                                                                      resistance_str (
                                                                        to-str-space resistance_value
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    begin (
                                                                      if (
                                                                        equal? resistance_value (
                                                                          let (
                                                                            (
                                                                              v15 resistance_value
                                                                            )
                                                                          )
                                                                           (
                                                                            cond (
                                                                              (
                                                                                string? v15
                                                                              )
                                                                               (
                                                                                exact (
                                                                                  floor (
                                                                                    string->number v15
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                boolean? v15
                                                                              )
                                                                               (
                                                                                if v15 1 0
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                exact (
                                                                                  floor v15
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        begin (
                                                                          set! resistance_str (
                                                                            to-str-space (
                                                                              let (
                                                                                (
                                                                                  v16 resistance_value
                                                                                )
                                                                              )
                                                                               (
                                                                                cond (
                                                                                  (
                                                                                    string? v16
                                                                                  )
                                                                                   (
                                                                                    exact (
                                                                                      floor (
                                                                                        string->number v16
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  (
                                                                                    boolean? v16
                                                                                  )
                                                                                   (
                                                                                    if v16 1 0
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  else (
                                                                                    exact (
                                                                                      floor v16
                                                                                    )
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
                                                                      let (
                                                                        (
                                                                          answer (
                                                                            string-append (
                                                                              string-append (
                                                                                string-append resistance_str "Ω ±"
                                                                              )
                                                                               (
                                                                                to-str-space tolerance
                                                                              )
                                                                            )
                                                                             "% "
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        begin (
                                                                          if (
                                                                            not (
                                                                              equal? temp_coeff 0
                                                                            )
                                                                          )
                                                                           (
                                                                            begin (
                                                                              set! answer (
                                                                                string-append (
                                                                                  string-append answer (
                                                                                    to-str-space temp_coeff
                                                                                  )
                                                                                )
                                                                                 " ppm/K"
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            quote (
                                                                              
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          ret14 answer
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
    )
     (
      let (
        (
          end18 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur19 (
              quotient (
                * (
                  - end18 start17
                )
                 1000000
              )
               jps20
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur19
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
