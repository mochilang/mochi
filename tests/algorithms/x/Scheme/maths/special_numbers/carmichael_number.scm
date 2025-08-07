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
        abs_int x
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            begin (
              if (
                < x 0
              )
               (
                begin (
                  ret1 (
                    - x
                  )
                )
              )
               '(
                
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
        gcd a b
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            begin (
              if (
                equal? a 0
              )
               (
                begin (
                  ret2 (
                    abs_int b
                  )
                )
              )
               '(
                
              )
            )
             (
              ret2 (
                gcd (
                  _mod b a
                )
                 a
              )
            )
          )
        )
      )
    )
     (
      define (
        power x y m
      )
       (
        call/cc (
          lambda (
            ret3
          )
           (
            begin (
              if (
                equal? y 0
              )
               (
                begin (
                  ret3 (
                    _mod 1 m
                  )
                )
              )
               '(
                
              )
            )
             (
              let (
                (
                  temp (
                    _mod (
                      power x (
                        _div y 2
                      )
                       m
                    )
                     m
                  )
                )
              )
               (
                begin (
                  set! temp (
                    _mod (
                      * temp temp
                    )
                     m
                  )
                )
                 (
                  if (
                    equal? (
                      _mod y 2
                    )
                     1
                  )
                   (
                    begin (
                      set! temp (
                        _mod (
                          * temp x
                        )
                         m
                      )
                    )
                  )
                   '(
                    
                  )
                )
                 (
                  ret3 temp
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        is_carmichael_number n
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            begin (
              if (
                <= n 0
              )
               (
                begin (
                  panic "Number must be positive"
                )
              )
               '(
                
              )
            )
             (
              let (
                (
                  b 2
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
                              
                            )
                             (
                              if (
                                < b n
                              )
                               (
                                begin (
                                  if (
                                    equal? (
                                      gcd b n
                                    )
                                     1
                                  )
                                   (
                                    begin (
                                      if (
                                        not (
                                          equal? (
                                            power b (
                                              - n 1
                                            )
                                             n
                                          )
                                           1
                                        )
                                      )
                                       (
                                        begin (
                                          ret4 #f
                                        )
                                      )
                                       '(
                                        
                                      )
                                    )
                                  )
                                   '(
                                    
                                  )
                                )
                                 (
                                  set! b (
                                    + b 1
                                  )
                                )
                                 (
                                  loop5
                                )
                              )
                               '(
                                
                              )
                            )
                          )
                        )
                      )
                       (
                        loop5
                      )
                    )
                  )
                )
                 (
                  ret4 #t
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
            to-str-space (
              power 2 15 3
            )
          )
        )
         (
          to-str-space (
            power 2 15 3
          )
        )
         (
          to-str (
            to-str-space (
              power 2 15 3
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
            to-str-space (
              power 5 1 30
            )
          )
        )
         (
          to-str-space (
            power 5 1 30
          )
        )
         (
          to-str (
            to-str-space (
              power 5 1 30
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
            to-str-space (
              is_carmichael_number 4
            )
          )
        )
         (
          to-str-space (
            is_carmichael_number 4
          )
        )
         (
          to-str (
            to-str-space (
              is_carmichael_number 4
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
            to-str-space (
              is_carmichael_number 561
            )
          )
        )
         (
          to-str-space (
            is_carmichael_number 561
          )
        )
         (
          to-str (
            to-str-space (
              is_carmichael_number 561
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
            to-str-space (
              is_carmichael_number 562
            )
          )
        )
         (
          to-str-space (
            is_carmichael_number 562
          )
        )
         (
          to-str (
            to-str-space (
              is_carmichael_number 562
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
            to-str-space (
              is_carmichael_number 1105
            )
          )
        )
         (
          to-str-space (
            is_carmichael_number 1105
          )
        )
         (
          to-str (
            to-str-space (
              is_carmichael_number 1105
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
