;; Generated on 2025-08-06 18:11 +0700
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
      define (
        sum_list nums
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                s 0
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
                            xs
                          )
                           (
                            if (
                              null? xs
                            )
                             (
                              quote (
                                
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    n (
                                      car xs
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    set! s (
                                      _add s n
                                    )
                                  )
                                )
                              )
                               (
                                loop2 (
                                  cdr xs
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                     (
                      loop2 nums
                    )
                  )
                )
              )
               (
                ret1 s
              )
            )
          )
        )
      )
    )
     (
      define (
        create_state_space_tree nums max_sum num_index path curr_sum remaining_sum
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            let (
              (
                result (
                  _list
                )
              )
            )
             (
              begin (
                if (
                  or (
                    > curr_sum max_sum
                  )
                   (
                    < (
                      + curr_sum remaining_sum
                    )
                     max_sum
                  )
                )
                 (
                  begin (
                    ret4 result
                  )
                )
                 (
                  quote (
                    
                  )
                )
              )
               (
                if (
                  equal? curr_sum max_sum
                )
                 (
                  begin (
                    set! result (
                      append result (
                        _list path
                      )
                    )
                  )
                   (
                    ret4 result
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
                    index num_index
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
                                  < index (
                                    _len nums
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        value (
                                          list-ref nums index
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            subres (
                                              create_state_space_tree nums max_sum (
                                                + index 1
                                              )
                                               (
                                                append path (
                                                  _list value
                                                )
                                              )
                                               (
                                                + curr_sum value
                                              )
                                               (
                                                - remaining_sum value
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                j 0
                                              )
                                            )
                                             (
                                              begin (
                                                call/cc (
                                                  lambda (
                                                    break8
                                                  )
                                                   (
                                                    letrec (
                                                      (
                                                        loop7 (
                                                          lambda (
                                                            
                                                          )
                                                           (
                                                            if (
                                                              < j (
                                                                _len subres
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                set! result (
                                                                  append result (
                                                                    _list (
                                                                      cond (
                                                                        (
                                                                          string? subres
                                                                        )
                                                                         (
                                                                          _substring subres j (
                                                                            + j 1
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        (
                                                                          hash-table? subres
                                                                        )
                                                                         (
                                                                          hash-table-ref subres j
                                                                        )
                                                                      )
                                                                       (
                                                                        else (
                                                                          list-ref subres j
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                set! j (
                                                                  + j 1
                                                                )
                                                              )
                                                               (
                                                                loop7
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
                                                      loop7
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                set! index (
                                                  + index 1
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                   (
                                    loop5
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
                          loop5
                        )
                      )
                    )
                  )
                   (
                    ret4 result
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
        generate_sum_of_subsets_solutions nums max_sum
      )
       (
        call/cc (
          lambda (
            ret9
          )
           (
            let (
              (
                total (
                  sum_list nums
                )
              )
            )
             (
              begin (
                ret9 (
                  create_state_space_tree nums max_sum 0 (
                    _list
                  )
                   0 total
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
            ret10
          )
           (
            begin (
              _display (
                generate_sum_of_subsets_solutions (
                  _list 3 34 4 12 5 2
                )
                 9
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
