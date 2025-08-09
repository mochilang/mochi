;; Generated on 2025-08-09 10:22 +0700
(import (scheme base))
(import (scheme time))
(import (chibi string))
(import (only (scheme char) string-upcase string-downcase))
(import (srfi 69))
(import (srfi 1))
(define _list list)
(import (chibi io))
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
(define _floor floor)
(define (fmod a b) (- a (* (_floor (/ a b)) b)))
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
(define (_input)
  (let ((l (read-line)))
    (if (eof-object? l) "" l)))
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
        solution n
      )
       (
        let (
          (
            counters '(
              
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
                letrec (
                  (
                    loop1 (
                      lambda (
                        
                      )
                       (
                        if (
                          <= i n
                        )
                         (
                          begin (
                            set! counters (
                              append counters (
                                _list 0
                              )
                            )
                          )
                           (
                            set! i (
                              + i 1
                            )
                          )
                           (
                            loop1
                          )
                        )
                         '(
                          
                        )
                      )
                    )
                  )
                )
                 (
                  loop1
                )
              )
               (
                list-set! counters 1 1
              )
               (
                let (
                  (
                    largest_number 1
                  )
                )
                 (
                  begin (
                    let (
                      (
                        pre_counter 1
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            start 2
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
                                          < start n
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                number start
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    counter 0
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
                                                                if #t (
                                                                  begin (
                                                                    if (
                                                                      and (
                                                                        < number (
                                                                          _len counters
                                                                        )
                                                                      )
                                                                       (
                                                                        not (
                                                                          equal? (
                                                                            list-ref-safe counters number
                                                                          )
                                                                           0
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        set! counter (
                                                                          + counter (
                                                                            list-ref-safe counters number
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        break5 '(
                                                                          
                                                                        )
                                                                      )
                                                                    )
                                                                     '(
                                                                      
                                                                    )
                                                                  )
                                                                   (
                                                                    if (
                                                                      equal? (
                                                                        _mod number 2
                                                                      )
                                                                       0
                                                                    )
                                                                     (
                                                                      begin (
                                                                        set! number (
                                                                          _div number 2
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        set! number (
                                                                          + (
                                                                            * 3 number
                                                                          )
                                                                           1
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    set! counter (
                                                                      + counter 1
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
                                                    if (
                                                      and (
                                                        < start (
                                                          _len counters
                                                        )
                                                      )
                                                       (
                                                        equal? (
                                                          list-ref-safe counters start
                                                        )
                                                         0
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        list-set! counters start counter
                                                      )
                                                    )
                                                     '(
                                                      
                                                    )
                                                  )
                                                   (
                                                    if (
                                                      > counter pre_counter
                                                    )
                                                     (
                                                      begin (
                                                        set! largest_number start
                                                      )
                                                       (
                                                        set! pre_counter counter
                                                      )
                                                    )
                                                     '(
                                                      
                                                    )
                                                  )
                                                   (
                                                    set! start (
                                                      + start 1
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
                           largest_number
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
          input_str (
            _input
          )
        )
      )
       (
        begin (
          let (
            (
              n (
                let (
                  (
                    v6 input_str
                  )
                )
                 (
                  cond (
                    (
                      string? v6
                    )
                     (
                      exact (
                        _floor (
                          string->number v6
                        )
                      )
                    )
                  )
                   (
                    (
                      boolean? v6
                    )
                     (
                      if v6 1 0
                    )
                  )
                   (
                    else (
                      exact (
                        _floor v6
                      )
                    )
                  )
                )
              )
            )
          )
           (
            begin (
              _display (
                if (
                  string? (
                    to-str-space (
                      solution n
                    )
                  )
                )
                 (
                  to-str-space (
                    solution n
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      solution n
                    )
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
