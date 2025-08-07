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
      start8 (
        current-jiffy
      )
    )
     (
      jps11 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        f x
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            ret1 (
              * (
                - x 0.0
              )
               (
                - x 0.0
              )
            )
          )
        )
      )
    )
     (
      define (
        make_points a b h
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            let (
              (
                points (
                  _list
                )
              )
            )
             (
              begin (
                let (
                  (
                    x (
                      + a h
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
                                  < x (
                                    - b h
                                  )
                                )
                                 (
                                  begin (
                                    set! points (
                                      append points (
                                        _list x
                                      )
                                    )
                                  )
                                   (
                                    set! x (
                                      + x h
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
                    ret2 points
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
        simpson_rule boundary steps
      )
       (
        call/cc (
          lambda (
            ret5
          )
           (
            begin (
              if (
                <= steps 0
              )
               (
                begin (
                  panic "Number of steps must be greater than zero"
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
                  a (
                    list-ref boundary 0
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      b (
                        list-ref boundary 1
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          h (
                            _div (
                              - b a
                            )
                             (
                              + 0.0 steps
                            )
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              pts (
                                make_points a b h
                              )
                            )
                          )
                           (
                            begin (
                              let (
                                (
                                  y (
                                    * (
                                      _div h 3.0
                                    )
                                     (
                                      f a
                                    )
                                  )
                                )
                              )
                               (
                                begin (
                                  let (
                                    (
                                      cnt 2
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
                                                          _len pts
                                                        )
                                                      )
                                                       (
                                                        begin (
                                                          let (
                                                            (
                                                              coeff (
                                                                - 4.0 (
                                                                  * 2.0 (
                                                                    + 0.0 (
                                                                      _mod cnt 2
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                           (
                                                            begin (
                                                              set! y (
                                                                _add y (
                                                                  * (
                                                                    * (
                                                                      _div h 3.0
                                                                    )
                                                                     coeff
                                                                  )
                                                                   (
                                                                    f (
                                                                      cond (
                                                                        (
                                                                          string? pts
                                                                        )
                                                                         (
                                                                          _substring pts i (
                                                                            + i 1
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        (
                                                                          hash-table? pts
                                                                        )
                                                                         (
                                                                          hash-table-ref pts i
                                                                        )
                                                                      )
                                                                       (
                                                                        else (
                                                                          list-ref pts i
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             (
                                                              set! cnt (
                                                                + cnt 1
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
                                          set! y (
                                            _add y (
                                              * (
                                                _div h 3.0
                                              )
                                               (
                                                f b
                                              )
                                            )
                                          )
                                        )
                                         (
                                          ret5 y
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
          result (
            simpson_rule (
              _list 0.0 1.0
            )
             10
          )
        )
      )
       (
        begin (
          _display (
            if (
              string? (
                to-str-space result
              )
            )
             (
              to-str-space result
            )
             (
              to-str (
                to-str-space result
              )
            )
          )
        )
         (
          newline
        )
      )
    )
     (
      let (
        (
          end9 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur10 (
              quotient (
                * (
                  - end9 start8
                )
                 1000000
              )
               jps11
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur10
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
