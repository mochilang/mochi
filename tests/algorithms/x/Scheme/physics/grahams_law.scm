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
      start10 (
        current-jiffy
      )
    )
     (
      jps13 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        to_float x
      )
       (
        * x 1.0
      )
    )
     (
      define (
        round6 x
      )
       (
        let (
          (
            factor 1000000.0
          )
        )
         (
          begin (
            _div (
              to_float (
                let (
                  (
                    v1 (
                      _add (
                        * x factor
                      )
                       0.5
                    )
                  )
                )
                 (
                  cond (
                    (
                      string? v1
                    )
                     (
                      exact (
                        floor (
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
                      exact (
                        floor v1
                      )
                    )
                  )
                )
              )
            )
             factor
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
                    loop2 (
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
               guess
            )
          )
        )
      )
    )
     (
      define (
        validate values
      )
       (
        call/cc (
          lambda (
            ret3
          )
           (
            begin (
              if (
                equal? (
                  _len values
                )
                 0
              )
               (
                begin (
                  ret3 #f
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
                  letrec (
                    (
                      loop4 (
                        lambda (
                          
                        )
                         (
                          if (
                            < i (
                              _len values
                            )
                          )
                           (
                            begin (
                              if (
                                <= (
                                  list-ref-safe values i
                                )
                                 0.0
                              )
                               (
                                begin (
                                  ret3 #f
                                )
                              )
                               '(
                                
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
                  ret3 #t
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        effusion_ratio m1 m2
      )
       (
        call/cc (
          lambda (
            ret5
          )
           (
            begin (
              if (
                not (
                  validate (
                    _list m1 m2
                  )
                )
              )
               (
                begin (
                  _display (
                    if (
                      string? "ValueError: Molar mass values must greater than 0."
                    )
                     "ValueError: Molar mass values must greater than 0." (
                      to-str "ValueError: Molar mass values must greater than 0."
                    )
                  )
                )
                 (
                  newline
                )
                 (
                  ret5 0.0
                )
              )
               '(
                
              )
            )
             (
              ret5 (
                round6 (
                  sqrtApprox (
                    _div m2 m1
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
        first_effusion_rate rate m1 m2
      )
       (
        call/cc (
          lambda (
            ret6
          )
           (
            begin (
              if (
                not (
                  validate (
                    _list rate m1 m2
                  )
                )
              )
               (
                begin (
                  _display (
                    if (
                      string? "ValueError: Molar mass and effusion rate values must greater than 0."
                    )
                     "ValueError: Molar mass and effusion rate values must greater than 0." (
                      to-str "ValueError: Molar mass and effusion rate values must greater than 0."
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
               '(
                
              )
            )
             (
              ret6 (
                round6 (
                  * rate (
                    sqrtApprox (
                      _div m2 m1
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
        second_effusion_rate rate m1 m2
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
                  validate (
                    _list rate m1 m2
                  )
                )
              )
               (
                begin (
                  _display (
                    if (
                      string? "ValueError: Molar mass and effusion rate values must greater than 0."
                    )
                     "ValueError: Molar mass and effusion rate values must greater than 0." (
                      to-str "ValueError: Molar mass and effusion rate values must greater than 0."
                    )
                  )
                )
                 (
                  newline
                )
                 (
                  ret7 0.0
                )
              )
               '(
                
              )
            )
             (
              ret7 (
                round6 (
                  _div rate (
                    sqrtApprox (
                      _div m2 m1
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
        first_molar_mass mass r1 r2
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
                  validate (
                    _list mass r1 r2
                  )
                )
              )
               (
                begin (
                  _display (
                    if (
                      string? "ValueError: Molar mass and effusion rate values must greater than 0."
                    )
                     "ValueError: Molar mass and effusion rate values must greater than 0." (
                      to-str "ValueError: Molar mass and effusion rate values must greater than 0."
                    )
                  )
                )
                 (
                  newline
                )
                 (
                  ret8 0.0
                )
              )
               '(
                
              )
            )
             (
              let (
                (
                  ratio (
                    _div r1 r2
                  )
                )
              )
               (
                begin (
                  ret8 (
                    round6 (
                      _div mass (
                        * ratio ratio
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
        second_molar_mass mass r1 r2
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
                  validate (
                    _list mass r1 r2
                  )
                )
              )
               (
                begin (
                  _display (
                    if (
                      string? "ValueError: Molar mass and effusion rate values must greater than 0."
                    )
                     "ValueError: Molar mass and effusion rate values must greater than 0." (
                      to-str "ValueError: Molar mass and effusion rate values must greater than 0."
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
               '(
                
              )
            )
             (
              let (
                (
                  ratio (
                    _div r1 r2
                  )
                )
              )
               (
                begin (
                  ret9 (
                    round6 (
                      _div (
                        * ratio ratio
                      )
                       mass
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
          string? (
            effusion_ratio 2.016 4.002
          )
        )
         (
          effusion_ratio 2.016 4.002
        )
         (
          to-str (
            effusion_ratio 2.016 4.002
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
            first_effusion_rate 1.0 2.016 4.002
          )
        )
         (
          first_effusion_rate 1.0 2.016 4.002
        )
         (
          to-str (
            first_effusion_rate 1.0 2.016 4.002
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
            second_effusion_rate 1.0 2.016 4.002
          )
        )
         (
          second_effusion_rate 1.0 2.016 4.002
        )
         (
          to-str (
            second_effusion_rate 1.0 2.016 4.002
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
            first_molar_mass 2.0 1.408943 0.709752
          )
        )
         (
          first_molar_mass 2.0 1.408943 0.709752
        )
         (
          to-str (
            first_molar_mass 2.0 1.408943 0.709752
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
            second_molar_mass 2.0 1.408943 0.709752
          )
        )
         (
          second_molar_mass 2.0 1.408943 0.709752
        )
         (
          to-str (
            second_molar_mass 2.0 1.408943 0.709752
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
          end11 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur12 (
              quotient (
                * (
                  - end11 start10
                )
                 1000000
              )
               jps13
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur12
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
