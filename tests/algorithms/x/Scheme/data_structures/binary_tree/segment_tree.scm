;; Generated on 2025-08-06 23:57 +0700
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
      start14 (
        current-jiffy
      )
    )
     (
      jps17 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      let (
        (
          A (
            _list
          )
        )
      )
       (
        begin (
          let (
            (
              N 0
            )
          )
           (
            begin (
              let (
                (
                  st (
                    _list
                  )
                )
              )
               (
                begin (
                  define (
                    left_child idx
                  )
                   (
                    call/cc (
                      lambda (
                        ret1
                      )
                       (
                        ret1 (
                          * idx 2
                        )
                      )
                    )
                  )
                )
                 (
                  define (
                    right_child idx
                  )
                   (
                    call/cc (
                      lambda (
                        ret2
                      )
                       (
                        ret2 (
                          + (
                            * idx 2
                          )
                           1
                        )
                      )
                    )
                  )
                )
                 (
                  define (
                    build idx left right
                  )
                   (
                    call/cc (
                      lambda (
                        ret3
                      )
                       (
                        if (
                          equal? left right
                        )
                         (
                          begin (
                            list-set! st idx (
                              list-ref A left
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                mid (
                                  _div (
                                    + left right
                                  )
                                   2
                                )
                              )
                            )
                             (
                              begin (
                                build (
                                  left_child idx
                                )
                                 left mid
                              )
                               (
                                build (
                                  right_child idx
                                )
                                 (
                                  + mid 1
                                )
                                 right
                              )
                               (
                                let (
                                  (
                                    left_val (
                                      list-ref st (
                                        left_child idx
                                      )
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        right_val (
                                          list-ref st (
                                            right_child idx
                                          )
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        list-set! st idx (
                                          if (
                                            > left_val right_val
                                          )
                                           left_val right_val
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
                    update_recursive idx left right a b val
                  )
                   (
                    call/cc (
                      lambda (
                        ret4
                      )
                       (
                        begin (
                          if (
                            or (
                              < right a
                            )
                             (
                              > left b
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
                          if (
                            equal? left right
                          )
                           (
                            begin (
                              list-set! st idx val
                            )
                             (
                              ret4 #t
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
                              mid (
                                _div (
                                  + left right
                                )
                                 2
                              )
                            )
                          )
                           (
                            begin (
                              update_recursive (
                                left_child idx
                              )
                               left mid a b val
                            )
                             (
                              update_recursive (
                                right_child idx
                              )
                               (
                                + mid 1
                              )
                               right a b val
                            )
                             (
                              let (
                                (
                                  left_val (
                                    list-ref st (
                                      left_child idx
                                    )
                                  )
                                )
                              )
                               (
                                begin (
                                  let (
                                    (
                                      right_val (
                                        list-ref st (
                                          right_child idx
                                        )
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      list-set! st idx (
                                        if (
                                          > left_val right_val
                                        )
                                         left_val right_val
                                      )
                                    )
                                     (
                                      ret4 #t
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
                    update a b val
                  )
                   (
                    call/cc (
                      lambda (
                        ret5
                      )
                       (
                        ret5 (
                          update_recursive 1 0 (
                            - N 1
                          )
                           (
                            - a 1
                          )
                           (
                            - b 1
                          )
                           val
                        )
                      )
                    )
                  )
                )
                 (
                  let (
                    (
                      NEG_INF (
                        - 1000000000
                      )
                    )
                  )
                   (
                    begin (
                      define (
                        query_recursive idx left right a b
                      )
                       (
                        call/cc (
                          lambda (
                            ret6
                          )
                           (
                            begin (
                              if (
                                or (
                                  < right a
                                )
                                 (
                                  > left b
                                )
                              )
                               (
                                begin (
                                  ret6 NEG_INF
                                )
                              )
                               (
                                quote (
                                  
                                )
                              )
                            )
                             (
                              if (
                                and (
                                  >= left a
                                )
                                 (
                                  <= right b
                                )
                              )
                               (
                                begin (
                                  ret6 (
                                    list-ref st idx
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
                                  mid (
                                    _div (
                                      + left right
                                    )
                                     2
                                  )
                                )
                              )
                               (
                                begin (
                                  let (
                                    (
                                      q1 (
                                        query_recursive (
                                          left_child idx
                                        )
                                         left mid a b
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      let (
                                        (
                                          q2 (
                                            query_recursive (
                                              right_child idx
                                            )
                                             (
                                              + mid 1
                                            )
                                             right a b
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          ret6 (
                                            if (
                                              _gt q1 q2
                                            )
                                             q1 q2
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
                        query a b
                      )
                       (
                        call/cc (
                          lambda (
                            ret7
                          )
                           (
                            ret7 (
                              query_recursive 1 0 (
                                - N 1
                              )
                               (
                                - a 1
                              )
                               (
                                - b 1
                              )
                            )
                          )
                        )
                      )
                    )
                     (
                      define (
                        show_data
                      )
                       (
                        call/cc (
                          lambda (
                            ret8
                          )
                           (
                            let (
                              (
                                i 0
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    show_list (
                                      _list
                                    )
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
                                                  < i N
                                                )
                                                 (
                                                  begin (
                                                    set! show_list (
                                                      append show_list (
                                                        _list (
                                                          query (
                                                            + i 1
                                                          )
                                                           (
                                                            + i 1
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    set! i (
                                                      + i 1
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
                                    _display (
                                      if (
                                        string? show_list
                                      )
                                       show_list (
                                        to-str show_list
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
                              set! A (
                                _list 1 2 (
                                  - 4
                                )
                                 7 3 (
                                  - 5
                                )
                                 6 11 (
                                  - 20
                                )
                                 9 14 15 5 2 (
                                  - 8
                                )
                              )
                            )
                             (
                              set! N (
                                _len A
                              )
                            )
                             (
                              let (
                                (
                                  i 0
                                )
                              )
                               (
                                begin (
                                  call/cc (
                                    lambda (
                                      break13
                                    )
                                     (
                                      letrec (
                                        (
                                          loop12 (
                                            lambda (
                                              
                                            )
                                             (
                                              if (
                                                < i (
                                                  * 4 N
                                                )
                                              )
                                               (
                                                begin (
                                                  set! st (
                                                    append st (
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
                                                  loop12
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
                                        loop12
                                      )
                                    )
                                  )
                                )
                                 (
                                  if (
                                    > N 0
                                  )
                                   (
                                    begin (
                                      build 1 0 (
                                        - N 1
                                      )
                                    )
                                  )
                                   (
                                    quote (
                                      
                                    )
                                  )
                                )
                                 (
                                  _display (
                                    if (
                                      string? (
                                        query 4 6
                                      )
                                    )
                                     (
                                      query 4 6
                                    )
                                     (
                                      to-str (
                                        query 4 6
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
                                        query 7 11
                                      )
                                    )
                                     (
                                      query 7 11
                                    )
                                     (
                                      to-str (
                                        query 7 11
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
                                        query 7 12
                                      )
                                    )
                                     (
                                      query 7 12
                                    )
                                     (
                                      to-str (
                                        query 7 12
                                      )
                                    )
                                  )
                                )
                                 (
                                  newline
                                )
                                 (
                                  update 1 3 111
                                )
                                 (
                                  _display (
                                    if (
                                      string? (
                                        query 1 15
                                      )
                                    )
                                     (
                                      query 1 15
                                    )
                                     (
                                      to-str (
                                        query 1 15
                                      )
                                    )
                                  )
                                )
                                 (
                                  newline
                                )
                                 (
                                  update 7 8 235
                                )
                                 (
                                  show_data
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
          end15 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur16 (
              quotient (
                * (
                  - end15 start14
                )
                 1000000
              )
               jps17
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur16
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
