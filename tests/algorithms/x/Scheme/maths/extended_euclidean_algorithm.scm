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
        abs_val n
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
                  ret1 (
                    - n
                  )
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              ret1 n
            )
          )
        )
      )
    )
     (
      define (
        extended_euclidean_algorithm a b
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            begin (
              if (
                equal? (
                  abs_val a
                )
                 1
              )
               (
                begin (
                  ret2 (
                    alist->hash-table (
                      _list (
                        cons "x" a
                      )
                       (
                        cons "y" 0
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
              if (
                equal? (
                  abs_val b
                )
                 1
              )
               (
                begin (
                  ret2 (
                    alist->hash-table (
                      _list (
                        cons "x" 0
                      )
                       (
                        cons "y" b
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
              let (
                (
                  old_remainder a
                )
              )
               (
                begin (
                  let (
                    (
                      remainder b
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          old_coeff_a 1
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              coeff_a 0
                            )
                          )
                           (
                            begin (
                              let (
                                (
                                  old_coeff_b 0
                                )
                              )
                               (
                                begin (
                                  let (
                                    (
                                      coeff_b 1
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
                                                      equal? remainder 0
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      let (
                                                        (
                                                          quotient (
                                                            _div old_remainder remainder
                                                          )
                                                        )
                                                      )
                                                       (
                                                        begin (
                                                          let (
                                                            (
                                                              temp_remainder (
                                                                - old_remainder (
                                                                  * quotient remainder
                                                                )
                                                              )
                                                            )
                                                          )
                                                           (
                                                            begin (
                                                              set! old_remainder remainder
                                                            )
                                                             (
                                                              set! remainder temp_remainder
                                                            )
                                                             (
                                                              let (
                                                                (
                                                                  temp_a (
                                                                    - old_coeff_a (
                                                                      * quotient coeff_a
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                begin (
                                                                  set! old_coeff_a coeff_a
                                                                )
                                                                 (
                                                                  set! coeff_a temp_a
                                                                )
                                                                 (
                                                                  let (
                                                                    (
                                                                      temp_b (
                                                                        - old_coeff_b (
                                                                          * quotient coeff_b
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    begin (
                                                                      set! old_coeff_b coeff_b
                                                                    )
                                                                     (
                                                                      set! coeff_b temp_b
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
                                      if (
                                        < a 0
                                      )
                                       (
                                        begin (
                                          set! old_coeff_a (
                                            - old_coeff_a
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
                                        < b 0
                                      )
                                       (
                                        begin (
                                          set! old_coeff_b (
                                            - old_coeff_b
                                          )
                                        )
                                      )
                                       (
                                        quote (
                                          
                                        )
                                      )
                                    )
                                     (
                                      ret2 (
                                        alist->hash-table (
                                          _list (
                                            cons "x" old_coeff_a
                                          )
                                           (
                                            cons "y" old_coeff_b
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
    )
     (
      define (
        test_extended_euclidean_algorithm
      )
       (
        call/cc (
          lambda (
            ret5
          )
           (
            let (
              (
                r1 (
                  extended_euclidean_algorithm 1 24
                )
              )
            )
             (
              begin (
                if (
                  or (
                    not (
                      equal? (
                        hash-table-ref r1 "x"
                      )
                       1
                    )
                  )
                   (
                    not (
                      equal? (
                        hash-table-ref r1 "y"
                      )
                       0
                    )
                  )
                )
                 (
                  begin (
                    panic "test1 failed"
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
                    r2 (
                      extended_euclidean_algorithm 8 14
                    )
                  )
                )
                 (
                  begin (
                    if (
                      or (
                        not (
                          equal? (
                            hash-table-ref r2 "x"
                          )
                           2
                        )
                      )
                       (
                        not (
                          equal? (
                            hash-table-ref r2 "y"
                          )
                           (
                            - 1
                          )
                        )
                      )
                    )
                     (
                      begin (
                        panic "test2 failed"
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
                        r3 (
                          extended_euclidean_algorithm 240 46
                        )
                      )
                    )
                     (
                      begin (
                        if (
                          or (
                            not (
                              equal? (
                                hash-table-ref r3 "x"
                              )
                               (
                                - 9
                              )
                            )
                          )
                           (
                            not (
                              equal? (
                                hash-table-ref r3 "y"
                              )
                               47
                            )
                          )
                        )
                         (
                          begin (
                            panic "test3 failed"
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
                            r4 (
                              extended_euclidean_algorithm 1 (
                                - 4
                              )
                            )
                          )
                        )
                         (
                          begin (
                            if (
                              or (
                                not (
                                  equal? (
                                    hash-table-ref r4 "x"
                                  )
                                   1
                                )
                              )
                               (
                                not (
                                  equal? (
                                    hash-table-ref r4 "y"
                                  )
                                   0
                                )
                              )
                            )
                             (
                              begin (
                                panic "test4 failed"
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
                                r5 (
                                  extended_euclidean_algorithm (
                                    - 2
                                  )
                                   (
                                    - 4
                                  )
                                )
                              )
                            )
                             (
                              begin (
                                if (
                                  or (
                                    not (
                                      equal? (
                                        hash-table-ref r5 "x"
                                      )
                                       (
                                        - 1
                                      )
                                    )
                                  )
                                   (
                                    not (
                                      equal? (
                                        hash-table-ref r5 "y"
                                      )
                                       0
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    panic "test5 failed"
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
                                    r6 (
                                      extended_euclidean_algorithm 0 (
                                        - 4
                                      )
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      or (
                                        not (
                                          equal? (
                                            hash-table-ref r6 "x"
                                          )
                                           0
                                        )
                                      )
                                       (
                                        not (
                                          equal? (
                                            hash-table-ref r6 "y"
                                          )
                                           (
                                            - 1
                                          )
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        panic "test6 failed"
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
                                        r7 (
                                          extended_euclidean_algorithm 2 0
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        if (
                                          or (
                                            not (
                                              equal? (
                                                hash-table-ref r7 "x"
                                              )
                                               1
                                            )
                                          )
                                           (
                                            not (
                                              equal? (
                                                hash-table-ref r7 "y"
                                              )
                                               0
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            panic "test7 failed"
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
            ret6
          )
           (
            begin (
              test_extended_euclidean_algorithm
            )
             (
              let (
                (
                  res (
                    extended_euclidean_algorithm 240 46
                  )
                )
              )
               (
                begin (
                  _display (
                    if (
                      string? (
                        string-append (
                          string-append (
                            string-append (
                              string-append "(" (
                                to-str-space (
                                  hash-table-ref res "x"
                                )
                              )
                            )
                             ", "
                          )
                           (
                            to-str-space (
                              hash-table-ref res "y"
                            )
                          )
                        )
                         ")"
                      )
                    )
                     (
                      string-append (
                        string-append (
                          string-append (
                            string-append "(" (
                              to-str-space (
                                hash-table-ref res "x"
                              )
                            )
                          )
                           ", "
                        )
                         (
                          to-str-space (
                            hash-table-ref res "y"
                          )
                        )
                      )
                       ")"
                    )
                     (
                      to-str (
                        string-append (
                          string-append (
                            string-append (
                              string-append "(" (
                                to-str-space (
                                  hash-table-ref res "x"
                                )
                              )
                            )
                             ", "
                          )
                           (
                            to-str-space (
                              hash-table-ref res "y"
                            )
                          )
                        )
                         ")"
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
      )
    )
     (
      main
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
