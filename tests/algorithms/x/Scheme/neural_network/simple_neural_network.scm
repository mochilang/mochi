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
      start11 (
        current-jiffy
      )
    )
     (
      jps14 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      let (
        (
          seed 1
        )
      )
       (
        begin (
          define (
            rand
          )
           (
            call/cc (
              lambda (
                ret1
              )
               (
                begin (
                  set! seed (
                    _mod (
                      + (
                        * seed 1103515245
                      )
                       12345
                    )
                     2147483648
                  )
                )
                 (
                  ret1 seed
                )
              )
            )
          )
        )
         (
          define (
            randint low high
          )
           (
            call/cc (
              lambda (
                ret2
              )
               (
                ret2 (
                  _add (
                    _mod (
                      rand
                    )
                     (
                      + (
                        - high low
                      )
                       1
                    )
                  )
                   low
                )
              )
            )
          )
        )
         (
          define (
            expApprox x
          )
           (
            call/cc (
              lambda (
                ret3
              )
               (
                let (
                  (
                    y x
                  )
                )
                 (
                  begin (
                    let (
                      (
                        is_neg #f
                      )
                    )
                     (
                      begin (
                        if (
                          < x 0.0
                        )
                         (
                          begin (
                            set! is_neg #t
                          )
                           (
                            set! y (
                              - x
                            )
                          )
                        )
                         '(
                          
                        )
                      )
                       (
                        let (
                          (
                            term 1.0
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                sum 1.0
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    n 1
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
                                                  < n 30
                                                )
                                                 (
                                                  begin (
                                                    set! term (
                                                      _div (
                                                        * term y
                                                      )
                                                       (
                                                        + 0.0 n
                                                      )
                                                    )
                                                  )
                                                   (
                                                    set! sum (
                                                      + sum term
                                                    )
                                                  )
                                                   (
                                                    set! n (
                                                      + n 1
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
                                    )
                                  )
                                   (
                                    if is_neg (
                                      begin (
                                        ret3 (
                                          _div 1.0 sum
                                        )
                                      )
                                    )
                                     '(
                                      
                                    )
                                  )
                                   (
                                    ret3 sum
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
            sigmoid x
          )
           (
            call/cc (
              lambda (
                ret6
              )
               (
                ret6 (
                  _div 1.0 (
                    _add 1.0 (
                      expApprox (
                        - x
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
            sigmoid_derivative sig_val
          )
           (
            call/cc (
              lambda (
                ret7
              )
               (
                ret7 (
                  * sig_val (
                    - 1.0 sig_val
                  )
                )
              )
            )
          )
        )
         (
          let (
            (
              INITIAL_VALUE 0.02
            )
          )
           (
            begin (
              define (
                forward_propagation expected number_propagations
              )
               (
                call/cc (
                  lambda (
                    ret8
                  )
                   (
                    let (
                      (
                        weight (
                          - (
                            * 2.0 (
                              + 0.0 (
                                randint 1 100
                              )
                            )
                          )
                           1.0
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            layer_1 0.0
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
                                              < i number_propagations
                                            )
                                             (
                                              begin (
                                                set! layer_1 (
                                                  sigmoid (
                                                    * INITIAL_VALUE weight
                                                  )
                                                )
                                              )
                                               (
                                                let (
                                                  (
                                                    layer_1_error (
                                                      - (
                                                        _div (
                                                          + 0.0 expected
                                                        )
                                                         100.0
                                                      )
                                                       layer_1
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        layer_1_delta (
                                                          * layer_1_error (
                                                            sigmoid_derivative layer_1
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        set! weight (
                                                          _add weight (
                                                            * INITIAL_VALUE layer_1_delta
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
                                               (
                                                loop9
                                              )
                                            )
                                             '(
                                              
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
                                  * layer_1 100.0
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
              set! seed 1
            )
             (
              let (
                (
                  result (
                    forward_propagation 32 450000
                  )
                )
              )
               (
                begin (
                  _display (
                    if (
                      string? result
                    )
                     result (
                      to-str result
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
      )
    )
     (
      let (
        (
          end12 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur13 (
              quotient (
                * (
                  - end12 start11
                )
                 1000000
              )
               jps14
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur13
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
