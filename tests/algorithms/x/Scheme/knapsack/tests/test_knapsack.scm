;; Generated on 2025-08-07 14:57 +0700
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
      start5 (
        current-jiffy
      )
    )
     (
      jps8 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        knapsack capacity weights values counter
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            begin (
              if (
                or (
                  equal? counter 0
                )
                 (
                  equal? capacity 0
                )
              )
               (
                begin (
                  ret1 0
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                > (
                  list-ref-safe weights (
                    - counter 1
                  )
                )
                 capacity
              )
               (
                begin (
                  ret1 (
                    knapsack capacity weights values (
                      - counter 1
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
                  left_capacity (
                    - capacity (
                      list-ref-safe weights (
                        - counter 1
                      )
                    )
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      include_val (
                        _add (
                          list-ref-safe values (
                            - counter 1
                          )
                        )
                         (
                          knapsack left_capacity weights values (
                            - counter 1
                          )
                        )
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          exclude_val (
                            knapsack capacity weights values (
                              - counter 1
                            )
                          )
                        )
                      )
                       (
                        begin (
                          if (
                            _gt include_val exclude_val
                          )
                           (
                            begin (
                              ret1 include_val
                            )
                          )
                           (
                            quote (
                              
                            )
                          )
                        )
                         (
                          ret1 exclude_val
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
        test_base_case
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            let (
              (
                cap 0
              )
            )
             (
              begin (
                let (
                  (
                    val (
                      _list 0
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        w (
                          _list 0
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            c (
                              _len val
                            )
                          )
                        )
                         (
                          begin (
                            if (
                              not (
                                equal? (
                                  knapsack cap w val c
                                )
                                 0
                              )
                            )
                             (
                              begin (
                                ret2 #f
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
                                val2 (
                                  _list 60
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    w2 (
                                      _list 10
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        c2 (
                                          _len val2
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        ret2 (
                                          equal? (
                                            knapsack cap w2 val2 c2
                                          )
                                           0
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
        test_easy_case
      )
       (
        call/cc (
          lambda (
            ret3
          )
           (
            let (
              (
                cap 3
              )
            )
             (
              begin (
                let (
                  (
                    val (
                      _list 1 2 3
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        w (
                          _list 3 2 1
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            c (
                              _len val
                            )
                          )
                        )
                         (
                          begin (
                            ret3 (
                              equal? (
                                knapsack cap w val c
                              )
                               5
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
        test_knapsack
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            let (
              (
                cap 50
              )
            )
             (
              begin (
                let (
                  (
                    val (
                      _list 60 100 120
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        w (
                          _list 10 20 30
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            c (
                              _len val
                            )
                          )
                        )
                         (
                          begin (
                            ret4 (
                              equal? (
                                knapsack cap w val c
                              )
                               220
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
      _display (
        if (
          string? (
            test_base_case
          )
        )
         (
          test_base_case
        )
         (
          to-str (
            test_base_case
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
            test_easy_case
          )
        )
         (
          test_easy_case
        )
         (
          to-str (
            test_easy_case
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
            test_knapsack
          )
        )
         (
          test_knapsack
        )
         (
          to-str (
            test_knapsack
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
            if #t #t #f
          )
        )
         (
          if #t #t #f
        )
         (
          to-str (
            if #t #t #f
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
          end6 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur7 (
              quotient (
                * (
                  - end6 start5
                )
                 1000000
              )
               jps8
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur7
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
