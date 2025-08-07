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
        ln x
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                t (
                  _div (
                    - x 1.0
                  )
                   (
                    + x 1.0
                  )
                )
              )
            )
             (
              begin (
                let (
                  (
                    term t
                  )
                )
                 (
                  begin (
                    let (
                      (
                        sum 0.0
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            k 1
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
                                          <= k 99
                                        )
                                         (
                                          begin (
                                            set! sum (
                                              _add sum (
                                                _div term (
                                                  + 0.0 k
                                                )
                                              )
                                            )
                                          )
                                           (
                                            set! term (
                                              * (
                                                * term t
                                              )
                                               t
                                            )
                                          )
                                           (
                                            set! k (
                                              + k 2
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
                           (
                            ret1 (
                              * 2.0 sum
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
        log10 x
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            ret4 (
              _div (
                ln x
              )
               (
                ln 10.0
              )
            )
          )
        )
      )
    )
     (
      define (
        absf x
      )
       (
        call/cc (
          lambda (
            ret5
          )
           (
            begin (
              if (
                < x 0.0
              )
               (
                begin (
                  ret5 (
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
              ret5 x
            )
          )
        )
      )
    )
     (
      define (
        res x y
      )
       (
        call/cc (
          lambda (
            ret6
          )
           (
            begin (
              if (
                equal? x 0
              )
               (
                begin (
                  ret6 0.0
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                equal? y 0
              )
               (
                begin (
                  ret6 1.0
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                < x 0
              )
               (
                begin (
                  panic "math domain error"
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              ret6 (
                * (
                  + 0.0 y
                )
                 (
                  log10 (
                    + 0.0 x
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
        test_res
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            begin (
              if (
                _gt (
                  absf (
                    - (
                      res 5 7
                    )
                     4.892790030352132
                  )
                )
                 1e-07
              )
               (
                begin (
                  panic "res(5,7) failed"
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
                    res 0 5
                  )
                   0.0
                )
              )
               (
                begin (
                  panic "res(0,5) failed"
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
                    res 3 0
                  )
                   1.0
                )
              )
               (
                begin (
                  panic "res(3,0) failed"
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
     (
      define (
        compare x1 y1 x2 y2
      )
       (
        call/cc (
          lambda (
            ret8
          )
           (
            let (
              (
                r1 (
                  res x1 y1
                )
              )
            )
             (
              begin (
                let (
                  (
                    r2 (
                      res x2 y2
                    )
                  )
                )
                 (
                  begin (
                    if (
                      _gt r1 r2
                    )
                     (
                      begin (
                        ret8 (
                          string-append (
                            string-append (
                              string-append "Largest number is " (
                                to-str-space x1
                              )
                            )
                             " ^ "
                          )
                           (
                            to-str-space y1
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
                    if (
                      _gt r2 r1
                    )
                     (
                      begin (
                        ret8 (
                          string-append (
                            string-append (
                              string-append "Largest number is " (
                                to-str-space x2
                              )
                            )
                             " ^ "
                          )
                           (
                            to-str-space y2
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
                    ret8 "Both are equal"
                  )
                )
              )
            )
          )
        )
      )
    )
     (
      test_res
    )
     (
      _display (
        if (
          string? (
            compare 5 7 4 8
          )
        )
         (
          compare 5 7 4 8
        )
         (
          to-str (
            compare 5 7 4 8
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
