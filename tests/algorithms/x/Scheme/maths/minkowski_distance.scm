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
      start13 (
        current-jiffy
      )
    )
     (
      jps16 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        abs_val x
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
        pow_float base exp
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            let (
              (
                result 1.0
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
                                  < i exp
                                )
                                 (
                                  begin (
                                    set! result (
                                      * result base
                                    )
                                  )
                                   (
                                    set! i (
                                      + i 1
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
     (
      define (
        nth_root value n
      )
       (
        call/cc (
          lambda (
            ret5
          )
           (
            begin (
              if (
                equal? value 0.0
              )
               (
                begin (
                  ret5 0.0
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
                  x (
                    _div value (
                      + 0.0 n
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
                                    < i 20
                                  )
                                   (
                                    begin (
                                      let (
                                        (
                                          num (
                                            _add (
                                              * (
                                                + 0.0 (
                                                  - n 1
                                                )
                                              )
                                               x
                                            )
                                             (
                                              _div value (
                                                pow_float x (
                                                  - n 1
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          set! x (
                                            _div num (
                                              + 0.0 n
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
                      ret5 x
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
        minkowski_distance point_a point_b order
      )
       (
        call/cc (
          lambda (
            ret8
          )
           (
            begin (
              if (
                < order 1
              )
               (
                begin (
                  panic "The order must be greater than or equal to 1."
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
                    _len point_a
                  )
                   (
                    _len point_b
                  )
                )
              )
               (
                begin (
                  panic "Both points must have the same dimension."
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
                  total 0.0
                )
              )
               (
                begin (
                  let (
                    (
                      idx 0
                    )
                  )
                   (
                    begin (
                      call/cc (
                        lambda (
                          break10
                        )
                         (
                          letrec (
                            (
                              loop9 (
                                lambda (
                                  
                                )
                                 (
                                  if (
                                    < idx (
                                      _len point_a
                                    )
                                  )
                                   (
                                    begin (
                                      let (
                                        (
                                          diff (
                                            abs_val (
                                              - (
                                                list-ref point_a idx
                                              )
                                               (
                                                list-ref point_b idx
                                              )
                                            )
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          set! total (
                                            _add total (
                                              pow_float diff order
                                            )
                                          )
                                        )
                                         (
                                          set! idx (
                                            + idx 1
                                          )
                                        )
                                      )
                                    )
                                     (
                                      loop9
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
                            loop9
                          )
                        )
                      )
                    )
                     (
                      ret8 (
                        nth_root total order
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
        test_minkowski
      )
       (
        call/cc (
          lambda (
            ret11
          )
           (
            begin (
              if (
                _gt (
                  abs_val (
                    - (
                      minkowski_distance (
                        _list 1.0 1.0
                      )
                       (
                        _list 2.0 2.0
                      )
                       1
                    )
                     2.0
                  )
                )
                 0.0001
              )
               (
                begin (
                  panic "minkowski_distance test1 failed"
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                _gt (
                  abs_val (
                    - (
                      minkowski_distance (
                        _list 1.0 2.0 3.0 4.0
                      )
                       (
                        _list 5.0 6.0 7.0 8.0
                      )
                       2
                    )
                     8.0
                  )
                )
                 0.0001
              )
               (
                begin (
                  panic "minkowski_distance test2 failed"
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
        main
      )
       (
        call/cc (
          lambda (
            ret12
          )
           (
            begin (
              test_minkowski
            )
             (
              _display (
                if (
                  string? (
                    minkowski_distance (
                      _list 5.0
                    )
                     (
                      _list 0.0
                    )
                     3
                  )
                )
                 (
                  minkowski_distance (
                    _list 5.0
                  )
                   (
                    _list 0.0
                  )
                   3
                )
                 (
                  to-str (
                    minkowski_distance (
                      _list 5.0
                    )
                     (
                      _list 0.0
                    )
                     3
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
          end14 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur15 (
              quotient (
                * (
                  - end14 start13
                )
                 1000000
              )
               jps16
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur15
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
