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
        mod a n
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                r (
                  _mod a n
                )
              )
            )
             (
              begin (
                if (
                  < r 0
                )
                 (
                  begin (
                    ret1 (
                      + r n
                    )
                  )
                )
                 (
                  quote (
                    
                  )
                )
              )
               (
                ret1 r
              )
            )
          )
        )
      )
    )
     (
      define (
        greatest_common_divisor a b
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            let (
              (
                x (
                  if (
                    < a 0
                  )
                   (
                    - a
                  )
                   a
                )
              )
            )
             (
              begin (
                let (
                  (
                    y (
                      if (
                        < b 0
                      )
                       (
                        - b
                      )
                       b
                    )
                  )
                )
                 (
                  begin (
                    call/cc (
                      lambda (
                        break4
                      )
                       (
                        letrec (
                          (
                            loop3 (
                              lambda (
                                
                              )
                               (
                                if (
                                  not (
                                    equal? y 0
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        t (
                                          _mod x y
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        set! x y
                                      )
                                       (
                                        set! y t
                                      )
                                    )
                                  )
                                   (
                                    loop3
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
                          loop3
                        )
                      )
                    )
                  )
                   (
                    ret2 x
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
        extended_gcd a b
      )
       (
        call/cc (
          lambda (
            ret5
          )
           (
            begin (
              if (
                equal? b 0
              )
               (
                begin (
                  ret5 (
                    _list a 1 0
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
                  res (
                    extended_gcd b (
                      _mod a b
                    )
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      d (
                        cond (
                          (
                            string? res
                          )
                           (
                            _substring res 0 (
                              + 0 1
                            )
                          )
                        )
                         (
                          (
                            hash-table? res
                          )
                           (
                            hash-table-ref res 0
                          )
                        )
                         (
                          else (
                            list-ref res 0
                          )
                        )
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          p (
                            cond (
                              (
                                string? res
                              )
                               (
                                _substring res 1 (
                                  + 1 1
                                )
                              )
                            )
                             (
                              (
                                hash-table? res
                              )
                               (
                                hash-table-ref res 1
                              )
                            )
                             (
                              else (
                                list-ref res 1
                              )
                            )
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              q (
                                cond (
                                  (
                                    string? res
                                  )
                                   (
                                    _substring res 2 (
                                      + 2 1
                                    )
                                  )
                                )
                                 (
                                  (
                                    hash-table? res
                                  )
                                   (
                                    hash-table-ref res 2
                                  )
                                )
                                 (
                                  else (
                                    list-ref res 2
                                  )
                                )
                              )
                            )
                          )
                           (
                            begin (
                              let (
                                (
                                  x q
                                )
                              )
                               (
                                begin (
                                  let (
                                    (
                                      y (
                                        - p (
                                          * q (
                                            _div a b
                                          )
                                        )
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      ret5 (
                                        _list d x y
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
        extended_euclid a b
      )
       (
        call/cc (
          lambda (
            ret6
          )
           (
            begin (
              if (
                equal? b 0
              )
               (
                begin (
                  ret6 (
                    _list 1 0
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
                  res (
                    extended_euclid b (
                      _mod a b
                    )
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      x (
                        cond (
                          (
                            string? res
                          )
                           (
                            _substring res 1 (
                              + 1 1
                            )
                          )
                        )
                         (
                          (
                            hash-table? res
                          )
                           (
                            hash-table-ref res 1
                          )
                        )
                         (
                          else (
                            list-ref res 1
                          )
                        )
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          y (
                            - (
                              cond (
                                (
                                  string? res
                                )
                                 (
                                  _substring res 0 (
                                    + 0 1
                                  )
                                )
                              )
                               (
                                (
                                  hash-table? res
                                )
                                 (
                                  hash-table-ref res 0
                                )
                              )
                               (
                                else (
                                  list-ref res 0
                                )
                              )
                            )
                             (
                              * (
                                _div a b
                              )
                               (
                                cond (
                                  (
                                    string? res
                                  )
                                   (
                                    _substring res 1 (
                                      + 1 1
                                    )
                                  )
                                )
                                 (
                                  (
                                    hash-table? res
                                  )
                                   (
                                    hash-table-ref res 1
                                  )
                                )
                                 (
                                  else (
                                    list-ref res 1
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                       (
                        begin (
                          ret6 (
                            _list x y
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
        invert_modulo a n
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            let (
              (
                res (
                  extended_euclid a n
                )
              )
            )
             (
              begin (
                let (
                  (
                    inv (
                      cond (
                        (
                          string? res
                        )
                         (
                          _substring res 0 (
                            + 0 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? res
                        )
                         (
                          hash-table-ref res 0
                        )
                      )
                       (
                        else (
                          list-ref res 0
                        )
                      )
                    )
                  )
                )
                 (
                  begin (
                    ret7 (
                      mod inv n
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
        modular_division a b n
      )
       (
        call/cc (
          lambda (
            ret8
          )
           (
            begin (
              if (
                <= n 1
              )
               (
                begin (
                  panic "n must be > 1"
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                <= a 0
              )
               (
                begin (
                  panic "a must be > 0"
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
                  equal? (
                    greatest_common_divisor a n
                  )
                   1
                )
              )
               (
                begin (
                  panic "gcd(a,n) != 1"
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
                  eg (
                    extended_gcd n a
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      s (
                        cond (
                          (
                            string? eg
                          )
                           (
                            _substring eg 2 (
                              + 2 1
                            )
                          )
                        )
                         (
                          (
                            hash-table? eg
                          )
                           (
                            hash-table-ref eg 2
                          )
                        )
                         (
                          else (
                            list-ref eg 2
                          )
                        )
                      )
                    )
                  )
                   (
                    begin (
                      ret8 (
                        mod (
                          * b s
                        )
                         n
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
        modular_division2 a b n
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
                  invert_modulo a n
                )
              )
            )
             (
              begin (
                ret9 (
                  mod (
                    * b s
                  )
                   n
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        tests
      )
       (
        call/cc (
          lambda (
            ret10
          )
           (
            begin (
              if (
                not (
                  equal? (
                    modular_division 4 8 5
                  )
                   2
                )
              )
               (
                begin (
                  panic "md1"
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
                  equal? (
                    modular_division 3 8 5
                  )
                   1
                )
              )
               (
                begin (
                  panic "md2"
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
                  equal? (
                    modular_division 4 11 5
                  )
                   4
                )
              )
               (
                begin (
                  panic "md3"
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
                  equal? (
                    modular_division2 4 8 5
                  )
                   2
                )
              )
               (
                begin (
                  panic "md21"
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
                  equal? (
                    modular_division2 3 8 5
                  )
                   1
                )
              )
               (
                begin (
                  panic "md22"
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
                  equal? (
                    modular_division2 4 11 5
                  )
                   4
                )
              )
               (
                begin (
                  panic "md23"
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
                  equal? (
                    invert_modulo 2 5
                  )
                   3
                )
              )
               (
                begin (
                  panic "inv"
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
                  eg (
                    extended_gcd 10 6
                  )
                )
              )
               (
                begin (
                  if (
                    or (
                      or (
                        not (
                          equal? (
                            cond (
                              (
                                string? eg
                              )
                               (
                                _substring eg 0 (
                                  + 0 1
                                )
                              )
                            )
                             (
                              (
                                hash-table? eg
                              )
                               (
                                hash-table-ref eg 0
                              )
                            )
                             (
                              else (
                                list-ref eg 0
                              )
                            )
                          )
                           2
                        )
                      )
                       (
                        not (
                          equal? (
                            cond (
                              (
                                string? eg
                              )
                               (
                                _substring eg 1 (
                                  + 1 1
                                )
                              )
                            )
                             (
                              (
                                hash-table? eg
                              )
                               (
                                hash-table-ref eg 1
                              )
                            )
                             (
                              else (
                                list-ref eg 1
                              )
                            )
                          )
                           (
                            - 1
                          )
                        )
                      )
                    )
                     (
                      not (
                        equal? (
                          cond (
                            (
                              string? eg
                            )
                             (
                              _substring eg 2 (
                                + 2 1
                              )
                            )
                          )
                           (
                            (
                              hash-table? eg
                            )
                             (
                              hash-table-ref eg 2
                            )
                          )
                           (
                            else (
                              list-ref eg 2
                            )
                          )
                        )
                         2
                      )
                    )
                  )
                   (
                    begin (
                      panic "eg"
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
                      eu (
                        extended_euclid 10 6
                      )
                    )
                  )
                   (
                    begin (
                      if (
                        or (
                          not (
                            equal? (
                              cond (
                                (
                                  string? eu
                                )
                                 (
                                  _substring eu 0 (
                                    + 0 1
                                  )
                                )
                              )
                               (
                                (
                                  hash-table? eu
                                )
                                 (
                                  hash-table-ref eu 0
                                )
                              )
                               (
                                else (
                                  list-ref eu 0
                                )
                              )
                            )
                             (
                              - 1
                            )
                          )
                        )
                         (
                          not (
                            equal? (
                              cond (
                                (
                                  string? eu
                                )
                                 (
                                  _substring eu 1 (
                                    + 1 1
                                  )
                                )
                              )
                               (
                                (
                                  hash-table? eu
                                )
                                 (
                                  hash-table-ref eu 1
                                )
                              )
                               (
                                else (
                                  list-ref eu 1
                                )
                              )
                            )
                             2
                          )
                        )
                      )
                       (
                        begin (
                          panic "eu"
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
                          equal? (
                            greatest_common_divisor 121 11
                          )
                           11
                        )
                      )
                       (
                        begin (
                          panic "gcd"
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
            ret11
          )
           (
            begin (
              tests
            )
             (
              _display (
                if (
                  string? (
                    to-str-space (
                      modular_division 4 8 5
                    )
                  )
                )
                 (
                  to-str-space (
                    modular_division 4 8 5
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      modular_division 4 8 5
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
