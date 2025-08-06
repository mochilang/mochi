;; Generated on 2025-08-06 23:57 +0700
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
      start7 (
        current-jiffy
      )
    )
     (
      jps10 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        pow10 exp
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                result 1.0
              )
            )
             (
              begin (
                if (
                  >= exp 0
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
                            break3
                          )
                           (
                            letrec (
                              (
                                loop2 (
                                  lambda (
                                    
                                  )
                                   (
                                    if (
                                      < i exp
                                    )
                                     (
                                      begin (
                                        set! result (
                                          * result 10.0
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
                                     (
                                      quote (
                                        
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
                        )
                      )
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
                            break5
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
                                        - 0 exp
                                      )
                                    )
                                     (
                                      begin (
                                        set! result (
                                          _div result 10.0
                                        )
                                      )
                                       (
                                        set! i (
                                          + i 1
                                        )
                                      )
                                       (
                                        loop4
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
                              loop4
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
               (
                ret1 result
              )
            )
          )
        )
      )
    )
     (
      let (
        (
          KILOGRAM_CHART (
            alist->hash-table (
              _list (
                cons "kilogram" 1.0
              )
               (
                cons "gram" 1000.0
              )
               (
                cons "milligram" 1000000.0
              )
               (
                cons "metric-ton" 0.001
              )
               (
                cons "long-ton" 0.0009842073
              )
               (
                cons "short-ton" 0.0011023122
              )
               (
                cons "pound" 2.2046244202
              )
               (
                cons "stone" 0.1574731728
              )
               (
                cons "ounce" 35.273990723
              )
               (
                cons "carrat" 5000.0
              )
               (
                cons "atomic-mass-unit" (
                  * 6.022136652 (
                    pow10 26
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
              WEIGHT_TYPE_CHART (
                alist->hash-table (
                  _list (
                    cons "kilogram" 1.0
                  )
                   (
                    cons "gram" 0.001
                  )
                   (
                    cons "milligram" 1e-06
                  )
                   (
                    cons "metric-ton" 1000.0
                  )
                   (
                    cons "long-ton" 1016.04608
                  )
                   (
                    cons "short-ton" 907.184
                  )
                   (
                    cons "pound" 0.453592
                  )
                   (
                    cons "stone" 6.35029
                  )
                   (
                    cons "ounce" 0.0283495
                  )
                   (
                    cons "carrat" 0.0002
                  )
                   (
                    cons "atomic-mass-unit" (
                      * 1.660540199 (
                        pow10 (
                          - 27
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
              define (
                weight_conversion from_type to_type value
              )
               (
                call/cc (
                  lambda (
                    ret6
                  )
                   (
                    let (
                      (
                        has_to (
                          cond (
                            (
                              string? KILOGRAM_CHART
                            )
                             (
                              if (
                                string-contains KILOGRAM_CHART to_type
                              )
                               #t #f
                            )
                          )
                           (
                            (
                              hash-table? KILOGRAM_CHART
                            )
                             (
                              if (
                                hash-table-exists? KILOGRAM_CHART to_type
                              )
                               #t #f
                            )
                          )
                           (
                            else (
                              if (
                                member to_type KILOGRAM_CHART
                              )
                               #t #f
                            )
                          )
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            has_from (
                              cond (
                                (
                                  string? WEIGHT_TYPE_CHART
                                )
                                 (
                                  if (
                                    string-contains WEIGHT_TYPE_CHART from_type
                                  )
                                   #t #f
                                )
                              )
                               (
                                (
                                  hash-table? WEIGHT_TYPE_CHART
                                )
                                 (
                                  if (
                                    hash-table-exists? WEIGHT_TYPE_CHART from_type
                                  )
                                   #t #f
                                )
                              )
                               (
                                else (
                                  if (
                                    member from_type WEIGHT_TYPE_CHART
                                  )
                                   #t #f
                                )
                              )
                            )
                          )
                        )
                         (
                          begin (
                            if (
                              and has_to has_from
                            )
                             (
                              begin (
                                ret6 (
                                  * (
                                    * value (
                                      hash-table-ref/default KILOGRAM_CHART to_type (
                                        quote (
                                          
                                        )
                                      )
                                    )
                                  )
                                   (
                                    hash-table-ref/default WEIGHT_TYPE_CHART from_type (
                                      quote (
                                        
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
                            _display (
                              if (
                                string? "Invalid 'from_type' or 'to_type'"
                              )
                               "Invalid 'from_type' or 'to_type'" (
                                to-str "Invalid 'from_type' or 'to_type'"
                              )
                            )
                          )
                           (
                            newline
                          )
                           (
                            ret6 0.0
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
                  string? (
                    weight_conversion "kilogram" "gram" 1.0
                  )
                )
                 (
                  weight_conversion "kilogram" "gram" 1.0
                )
                 (
                  to-str (
                    weight_conversion "kilogram" "gram" 1.0
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
                    weight_conversion "gram" "pound" 3.0
                  )
                )
                 (
                  weight_conversion "gram" "pound" 3.0
                )
                 (
                  to-str (
                    weight_conversion "gram" "pound" 3.0
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
                    weight_conversion "ounce" "kilogram" 3.0
                  )
                )
                 (
                  weight_conversion "ounce" "kilogram" 3.0
                )
                 (
                  to-str (
                    weight_conversion "ounce" "kilogram" 3.0
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
          end8 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur9 (
              quotient (
                * (
                  - end8 start7
                )
                 1000000
              )
               jps10
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur9
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
