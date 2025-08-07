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
      start9 (
        current-jiffy
      )
    )
     (
      jps12 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        double_factorial_recursive n
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            begin (
              if (
                < n 0
              )
               (
                begin (
                  panic "double_factorial_recursive() not defined for negative values"
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                <= n 1
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
              ret1 (
                * n (
                  double_factorial_recursive (
                    - n 2
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
        double_factorial_iterative n
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            begin (
              if (
                < n 0
              )
               (
                begin (
                  panic "double_factorial_iterative() not defined for negative values"
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
                  result 1
                )
              )
               (
                begin (
                  let (
                    (
                      i n
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
                                    > i 0
                                  )
                                   (
                                    begin (
                                      set! result (
                                        * result i
                                      )
                                    )
                                     (
                                      set! i (
                                        - i 2
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
                      ret2 result
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
        test_double_factorial
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
                  equal? (
                    double_factorial_recursive 0
                  )
                   1
                )
              )
               (
                begin (
                  panic "0!! recursive failed"
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
                    double_factorial_iterative 0
                  )
                   1
                )
              )
               (
                begin (
                  panic "0!! iterative failed"
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
                    double_factorial_recursive 1
                  )
                   1
                )
              )
               (
                begin (
                  panic "1!! recursive failed"
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
                    double_factorial_iterative 1
                  )
                   1
                )
              )
               (
                begin (
                  panic "1!! iterative failed"
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
                    double_factorial_recursive 5
                  )
                   15
                )
              )
               (
                begin (
                  panic "5!! recursive failed"
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
                    double_factorial_iterative 5
                  )
                   15
                )
              )
               (
                begin (
                  panic "5!! iterative failed"
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
                    double_factorial_recursive 6
                  )
                   48
                )
              )
               (
                begin (
                  panic "6!! recursive failed"
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
                    double_factorial_iterative 6
                  )
                   48
                )
              )
               (
                begin (
                  panic "6!! iterative failed"
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
                  n 0
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
                                <= n 10
                              )
                               (
                                begin (
                                  if (
                                    not (
                                      equal? (
                                        double_factorial_recursive n
                                      )
                                       (
                                        double_factorial_iterative n
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      panic "double factorial mismatch"
                                    )
                                  )
                                   (
                                    quote (
                                      
                                    )
                                  )
                                )
                                 (
                                  set! n (
                                    + n 1
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
            ret8
          )
           (
            begin (
              test_double_factorial
            )
             (
              _display (
                if (
                  string? (
                    double_factorial_iterative 10
                  )
                )
                 (
                  double_factorial_iterative 10
                )
                 (
                  to-str (
                    double_factorial_iterative 10
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
          end10 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur11 (
              quotient (
                * (
                  - end10 start9
                )
                 1000000
              )
               jps12
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur11
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
