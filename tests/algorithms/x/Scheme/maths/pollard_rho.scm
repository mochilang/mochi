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
        gcd a b
      )
       (
        call/cc (
          lambda (
            ret1
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
                    )
                  )
                   (
                    ret1 x
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
        rand_fn value step modulus
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            ret4 (
              _mod (
                + (
                  * value value
                )
                 step
              )
               modulus
            )
          )
        )
      )
    )
     (
      define (
        pollard_rho num seed step attempts
      )
       (
        call/cc (
          lambda (
            ret5
          )
           (
            begin (
              if (
                < num 2
              )
               (
                begin (
                  panic "The input value cannot be less than 2"
                )
              )
               '(
                
              )
            )
             (
              if (
                and (
                  > num 2
                )
                 (
                  equal? (
                    _mod num 2
                  )
                   0
                )
              )
               (
                begin (
                  ret5 (
                    alist->hash-table (
                      _list (
                        cons "factor" 2
                      )
                       (
                        cons "ok" #t
                      )
                    )
                  )
                )
              )
               '(
                
              )
            )
             (
              let (
                (
                  s seed
                )
              )
               (
                begin (
                  let (
                    (
                      st step
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
                                        < i attempts
                                      )
                                       (
                                        begin (
                                          let (
                                            (
                                              tortoise s
                                            )
                                          )
                                           (
                                            begin (
                                              let (
                                                (
                                                  hare s
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
                                                              if #t (
                                                                begin (
                                                                  set! tortoise (
                                                                    rand_fn tortoise st num
                                                                  )
                                                                )
                                                                 (
                                                                  set! hare (
                                                                    rand_fn hare st num
                                                                  )
                                                                )
                                                                 (
                                                                  set! hare (
                                                                    rand_fn hare st num
                                                                  )
                                                                )
                                                                 (
                                                                  let (
                                                                    (
                                                                      divisor (
                                                                        gcd (
                                                                          - hare tortoise
                                                                        )
                                                                         num
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    begin (
                                                                      if (
                                                                        equal? divisor 1
                                                                      )
                                                                       (
                                                                        begin (
                                                                          loop8
                                                                        )
                                                                      )
                                                                       (
                                                                        if (
                                                                          equal? divisor num
                                                                        )
                                                                         (
                                                                          begin (
                                                                            break9 '(
                                                                              
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            ret5 (
                                                                              alist->hash-table (
                                                                                _list (
                                                                                  cons "factor" divisor
                                                                                )
                                                                                 (
                                                                                  cons "ok" #t
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
                                                                  loop8
                                                                )
                                                              )
                                                               '(
                                                                
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
                                                  set! s hare
                                                )
                                                 (
                                                  set! st (
                                                    + st 1
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
                                          loop6
                                        )
                                      )
                                       '(
                                        
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
                          ret5 (
                            alist->hash-table (
                              _list (
                                cons "factor" 0
                              )
                               (
                                cons "ok" #f
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
        test_pollard_rho
      )
       (
        call/cc (
          lambda (
            ret10
          )
           (
            let (
              (
                r1 (
                  pollard_rho 8051 2 1 5
                )
              )
            )
             (
              begin (
                if (
                  or (
                    not (
                      hash-table-ref r1 "ok"
                    )
                  )
                   (
                    and (
                      not (
                        equal? (
                          hash-table-ref r1 "factor"
                        )
                         83
                      )
                    )
                     (
                      not (
                        equal? (
                          hash-table-ref r1 "factor"
                        )
                         97
                      )
                    )
                  )
                )
                 (
                  begin (
                    panic "test1 failed"
                  )
                )
                 '(
                  
                )
              )
               (
                let (
                  (
                    r2 (
                      pollard_rho 10403 2 1 5
                    )
                  )
                )
                 (
                  begin (
                    if (
                      or (
                        not (
                          hash-table-ref r2 "ok"
                        )
                      )
                       (
                        and (
                          not (
                            equal? (
                              hash-table-ref r2 "factor"
                            )
                             101
                          )
                        )
                         (
                          not (
                            equal? (
                              hash-table-ref r2 "factor"
                            )
                             103
                          )
                        )
                      )
                    )
                     (
                      begin (
                        panic "test2 failed"
                      )
                    )
                     '(
                      
                    )
                  )
                   (
                    let (
                      (
                        r3 (
                          pollard_rho 100 2 1 3
                        )
                      )
                    )
                     (
                      begin (
                        if (
                          or (
                            not (
                              hash-table-ref r3 "ok"
                            )
                          )
                           (
                            not (
                              equal? (
                                hash-table-ref r3 "factor"
                              )
                               2
                            )
                          )
                        )
                         (
                          begin (
                            panic "test3 failed"
                          )
                        )
                         '(
                          
                        )
                      )
                       (
                        let (
                          (
                            r4 (
                              pollard_rho 17 2 1 3
                            )
                          )
                        )
                         (
                          begin (
                            if (
                              hash-table-ref r4 "ok"
                            )
                             (
                              begin (
                                panic "test4 failed"
                              )
                            )
                             '(
                              
                            )
                          )
                           (
                            let (
                              (
                                r5 (
                                  pollard_rho (
                                    * (
                                      * 17 17
                                    )
                                     17
                                  )
                                   2 1 3
                                )
                              )
                            )
                             (
                              begin (
                                if (
                                  or (
                                    not (
                                      hash-table-ref r5 "ok"
                                    )
                                  )
                                   (
                                    not (
                                      equal? (
                                        hash-table-ref r5 "factor"
                                      )
                                       17
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    panic "test5 failed"
                                  )
                                )
                                 '(
                                  
                                )
                              )
                               (
                                let (
                                  (
                                    r6 (
                                      pollard_rho (
                                        * (
                                          * 17 17
                                        )
                                         17
                                      )
                                       2 1 1
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      hash-table-ref r6 "ok"
                                    )
                                     (
                                      begin (
                                        panic "test6 failed"
                                      )
                                    )
                                     '(
                                      
                                    )
                                  )
                                   (
                                    let (
                                      (
                                        r7 (
                                          pollard_rho (
                                            * (
                                              * 3 5
                                            )
                                             7
                                          )
                                           2 1 3
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        if (
                                          or (
                                            not (
                                              hash-table-ref r7 "ok"
                                            )
                                          )
                                           (
                                            not (
                                              equal? (
                                                hash-table-ref r7 "factor"
                                              )
                                               21
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            panic "test7 failed"
                                          )
                                        )
                                         '(
                                          
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
            ret11
          )
           (
            begin (
              test_pollard_rho
            )
             (
              let (
                (
                  a (
                    pollard_rho 100 2 1 3
                  )
                )
              )
               (
                begin (
                  if (
                    hash-table-ref a "ok"
                  )
                   (
                    begin (
                      _display (
                        if (
                          string? (
                            to-str-space (
                              hash-table-ref a "factor"
                            )
                          )
                        )
                         (
                          to-str-space (
                            hash-table-ref a "factor"
                          )
                        )
                         (
                          to-str (
                            to-str-space (
                              hash-table-ref a "factor"
                            )
                          )
                        )
                      )
                    )
                     (
                      newline
                    )
                  )
                   (
                    begin (
                      _display (
                        if (
                          string? "None"
                        )
                         "None" (
                          to-str "None"
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
                      b (
                        pollard_rho 17 2 1 3
                      )
                    )
                  )
                   (
                    begin (
                      if (
                        hash-table-ref b "ok"
                      )
                       (
                        begin (
                          _display (
                            if (
                              string? (
                                to-str-space (
                                  hash-table-ref b "factor"
                                )
                              )
                            )
                             (
                              to-str-space (
                                hash-table-ref b "factor"
                              )
                            )
                             (
                              to-str (
                                to-str-space (
                                  hash-table-ref b "factor"
                                )
                              )
                            )
                          )
                        )
                         (
                          newline
                        )
                      )
                       (
                        begin (
                          _display (
                            if (
                              string? "None"
                            )
                             "None" (
                              to-str "None"
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
