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
      let (
        (
          roman_values (
            _list 1000 900 500 400 100 90 50 40 10 9 5 4 1
          )
        )
      )
       (
        begin (
          let (
            (
              roman_symbols (
                _list "M" "CM" "D" "CD" "C" "XC" "L" "XL" "X" "IX" "V" "IV" "I"
              )
            )
          )
           (
            begin (
              define (
                char_value c
              )
               (
                call/cc (
                  lambda (
                    ret1
                  )
                   (
                    begin (
                      if (
                        string=? c "I"
                      )
                       (
                        begin (
                          ret1 1
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      if (
                        string=? c "V"
                      )
                       (
                        begin (
                          ret1 5
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      if (
                        string=? c "X"
                      )
                       (
                        begin (
                          ret1 10
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      if (
                        string=? c "L"
                      )
                       (
                        begin (
                          ret1 50
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      if (
                        string=? c "C"
                      )
                       (
                        begin (
                          ret1 100
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      if (
                        string=? c "D"
                      )
                       (
                        begin (
                          ret1 500
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      if (
                        string=? c "M"
                      )
                       (
                        begin (
                          ret1 1000
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      ret1 0
                    )
                  )
                )
              )
            )
             (
              define (
                roman_to_int roman
              )
               (
                call/cc (
                  lambda (
                    ret2
                  )
                   (
                    let (
                      (
                        total 0
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
                                          < i (
                                            _len roman
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              and (
                                                < (
                                                  + i 1
                                                )
                                                 (
                                                  _len roman
                                                )
                                              )
                                               (
                                                _lt (
                                                  char_value (
                                                    _substring roman i (
                                                      + i 1
                                                    )
                                                  )
                                                )
                                                 (
                                                  char_value (
                                                    _substring roman (
                                                      + i 1
                                                    )
                                                     (
                                                      + (
                                                        + i 1
                                                      )
                                                       1
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                set! total (
                                                  - (
                                                    _add total (
                                                      char_value (
                                                        _substring roman (
                                                          + i 1
                                                        )
                                                         (
                                                          + (
                                                            + i 1
                                                          )
                                                           1
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    char_value (
                                                      _substring roman i (
                                                        + i 1
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                set! i (
                                                  + i 2
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                set! total (
                                                  _add total (
                                                    char_value (
                                                      _substring roman i (
                                                        + i 1
                                                      )
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
                            ret2 total
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
                int_to_roman number
              )
               (
                call/cc (
                  lambda (
                    ret5
                  )
                   (
                    let (
                      (
                        num number
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            res ""
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
                                              < i (
                                                _len roman_values
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    value (
                                                      list-ref roman_values i
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        symbol (
                                                          list-ref roman_symbols i
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            factor (
                                                              _div num value
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            set! num (
                                                              _mod num value
                                                            )
                                                          )
                                                           (
                                                            let (
                                                              (
                                                                j 0
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                call/cc (
                                                                  lambda (
                                                                    break9
                                                                  )
                                                                   (
                                                                    letrec (
                                                                      (
                                                                        loop8 (
                                                                          lambda (
                                                                            
                                                                          )
                                                                           (
                                                                            if (
                                                                              < j factor
                                                                            )
                                                                             (
                                                                              begin (
                                                                                set! res (
                                                                                  string-append res symbol
                                                                                )
                                                                              )
                                                                               (
                                                                                set! j (
                                                                                  + j 1
                                                                                )
                                                                              )
                                                                               (
                                                                                loop8
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
                                                                      loop8
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                if (
                                                                  equal? num 0
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
                                                               (
                                                                set! i (
                                                                  + i 1
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
                                ret5 res
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
