;; Generated on 2025-08-06 16:21 +0700
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
        ((boolean? x) (if x "1" "0"))
        (else (number->string x))))
(define (to-str-space x)
  (cond ((pair? x)
         (string-append "[" (string-join (map to-str-space x) " ") "]"))
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
      start10 (
        current-jiffy
      )
    )
     (
      jps13 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        valid_connection graph next_ver curr_ind path
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            begin (
              if (
                equal? (
                  cond (
                    (
                      string? (
                        list-ref graph (
                          list-ref path (
                            - curr_ind 1
                          )
                        )
                      )
                    )
                     (
                      _substring (
                        list-ref graph (
                          list-ref path (
                            - curr_ind 1
                          )
                        )
                      )
                       next_ver (
                        + next_ver 1
                      )
                    )
                  )
                   (
                    (
                      hash-table? (
                        list-ref graph (
                          list-ref path (
                            - curr_ind 1
                          )
                        )
                      )
                    )
                     (
                      hash-table-ref (
                        list-ref graph (
                          list-ref path (
                            - curr_ind 1
                          )
                        )
                      )
                       next_ver
                    )
                  )
                   (
                    else (
                      list-ref (
                        list-ref graph (
                          list-ref path (
                            - curr_ind 1
                          )
                        )
                      )
                       next_ver
                    )
                  )
                )
                 0
              )
               (
                begin (
                  ret1 #f
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
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
                                  v (
                                    car xs
                                  )
                                )
                              )
                               (
                                begin (
                                  if (
                                    equal? v next_ver
                                  )
                                   (
                                    begin (
                                      ret1 #f
                                    )
                                  )
                                   (
                                    quote (
                                      
                                    )
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
                    loop2 path
                  )
                )
              )
            )
             (
              ret1 #t
            )
          )
        )
      )
    )
     (
      define (
        util_hamilton_cycle graph path curr_ind
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            begin (
              if (
                equal? curr_ind (
                  _len graph
                )
              )
               (
                begin (
                  ret4 (
                    equal? (
                      cond (
                        (
                          string? (
                            list-ref graph (
                              list-ref path (
                                - curr_ind 1
                              )
                            )
                          )
                        )
                         (
                          _substring (
                            list-ref graph (
                              list-ref path (
                                - curr_ind 1
                              )
                            )
                          )
                           (
                            list-ref path 0
                          )
                           (
                            + (
                              list-ref path 0
                            )
                             1
                          )
                        )
                      )
                       (
                        (
                          hash-table? (
                            list-ref graph (
                              list-ref path (
                                - curr_ind 1
                              )
                            )
                          )
                        )
                         (
                          hash-table-ref (
                            list-ref graph (
                              list-ref path (
                                - curr_ind 1
                              )
                            )
                          )
                           (
                            list-ref path 0
                          )
                        )
                      )
                       (
                        else (
                          list-ref (
                            list-ref graph (
                              list-ref path (
                                - curr_ind 1
                              )
                            )
                          )
                           (
                            list-ref path 0
                          )
                        )
                      )
                    )
                     1
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
                  next_ver 0
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
                                < next_ver (
                                  _len graph
                                )
                              )
                               (
                                begin (
                                  if (
                                    valid_connection graph next_ver curr_ind path
                                  )
                                   (
                                    begin (
                                      list-set! path curr_ind next_ver
                                    )
                                     (
                                      if (
                                        util_hamilton_cycle graph path (
                                          + curr_ind 1
                                        )
                                      )
                                       (
                                        begin (
                                          ret4 #t
                                        )
                                      )
                                       (
                                        quote (
                                          
                                        )
                                      )
                                    )
                                     (
                                      list-set! path curr_ind (
                                        - 1
                                      )
                                    )
                                  )
                                   (
                                    quote (
                                      
                                    )
                                  )
                                )
                                 (
                                  set! next_ver (
                                    + next_ver 1
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
                  ret4 #f
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        hamilton_cycle graph start_index
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            let (
              (
                path (
                  quote (
                    
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
                        break9
                      )
                       (
                        letrec (
                          (
                            loop8 (
                              lambda (
                                
                              )
                               (
                                if (
                                  < i (
                                    + (
                                      _len graph
                                    )
                                     1
                                  )
                                )
                                 (
                                  begin (
                                    list-set! path i (
                                      - 1
                                    )
                                  )
                                   (
                                    set! i (
                                      + i 1
                                    )
                                  )
                                   (
                                    loop8
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
                          loop8
                        )
                      )
                    )
                  )
                   (
                    list-set! path 0 start_index
                  )
                   (
                    let (
                      (
                        last (
                          - (
                            _len path
                          )
                           1
                        )
                      )
                    )
                     (
                      begin (
                        list-set! path last start_index
                      )
                       (
                        if (
                          util_hamilton_cycle graph path 1
                        )
                         (
                          begin (
                            ret7 path
                          )
                        )
                         (
                          quote (
                            
                          )
                        )
                      )
                       (
                        ret7 (
                          _list
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
          end11 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur12 (
              quotient (
                * (
                  - end11 start10
                )
                 1000000
              )
               jps13
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur12
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
