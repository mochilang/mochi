;; Generated on 2025-08-07 08:56 +0700
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
      start81 (
        current-jiffy
      )
    )
     (
      jps84 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      let (
        (
          EDGE_ARRAY (
            _list (
              _list (
                _list "ab" "e1"
              )
               (
                _list "ac" "e3"
              )
               (
                _list "ad" "e5"
              )
               (
                _list "bc" "e4"
              )
               (
                _list "bd" "e2"
              )
               (
                _list "be" "e6"
              )
               (
                _list "bh" "e12"
              )
               (
                _list "cd" "e2"
              )
               (
                _list "ce" "e4"
              )
               (
                _list "de" "e1"
              )
               (
                _list "df" "e8"
              )
               (
                _list "dg" "e5"
              )
               (
                _list "dh" "e10"
              )
               (
                _list "ef" "e3"
              )
               (
                _list "eg" "e2"
              )
               (
                _list "fg" "e6"
              )
               (
                _list "gh" "e6"
              )
               (
                _list "hi" "e3"
              )
            )
             (
              _list (
                _list "ab" "e1"
              )
               (
                _list "ac" "e3"
              )
               (
                _list "ad" "e5"
              )
               (
                _list "bc" "e4"
              )
               (
                _list "bd" "e2"
              )
               (
                _list "be" "e6"
              )
               (
                _list "cd" "e2"
              )
               (
                _list "de" "e1"
              )
               (
                _list "df" "e8"
              )
               (
                _list "ef" "e3"
              )
               (
                _list "eg" "e2"
              )
               (
                _list "fg" "e6"
              )
            )
             (
              _list (
                _list "ab" "e1"
              )
               (
                _list "ac" "e3"
              )
               (
                _list "bc" "e4"
              )
               (
                _list "bd" "e2"
              )
               (
                _list "de" "e1"
              )
               (
                _list "df" "e8"
              )
               (
                _list "dg" "e5"
              )
               (
                _list "ef" "e3"
              )
               (
                _list "eg" "e2"
              )
               (
                _list "eh" "e12"
              )
               (
                _list "fg" "e6"
              )
               (
                _list "fh" "e10"
              )
               (
                _list "gh" "e6"
              )
            )
             (
              _list (
                _list "ab" "e1"
              )
               (
                _list "ac" "e3"
              )
               (
                _list "bc" "e4"
              )
               (
                _list "bd" "e2"
              )
               (
                _list "bh" "e12"
              )
               (
                _list "cd" "e2"
              )
               (
                _list "df" "e8"
              )
               (
                _list "dh" "e10"
              )
            )
             (
              _list (
                _list "ab" "e1"
              )
               (
                _list "ac" "e3"
              )
               (
                _list "ad" "e5"
              )
               (
                _list "bc" "e4"
              )
               (
                _list "bd" "e2"
              )
               (
                _list "cd" "e2"
              )
               (
                _list "ce" "e4"
              )
               (
                _list "de" "e1"
              )
               (
                _list "df" "e8"
              )
               (
                _list "dg" "e5"
              )
               (
                _list "ef" "e3"
              )
               (
                _list "eg" "e2"
              )
               (
                _list "fg" "e6"
              )
            )
          )
        )
      )
       (
        begin (
          define (
            contains lst item
          )
           (
            call/cc (
              lambda (
                ret1
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
                                      v (
                                        car xs
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      if (
                                        string=? v item
                                      )
                                       (
                                        begin (
                                          ret1 #t
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
                        loop2 lst
                      )
                    )
                  )
                )
                 (
                  ret1 #f
                )
              )
            )
          )
        )
         (
          define (
            get_distinct_edge edge_array
          )
           (
            call/cc (
              lambda (
                ret4
              )
               (
                let (
                  (
                    distinct (
                      _list
                    )
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
                                        row (
                                          car xs
                                        )
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
                                                            item (
                                                              car xs
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                e (
                                                                  cond (
                                                                    (
                                                                      string? item
                                                                    )
                                                                     (
                                                                      _substring item 0 (
                                                                        + 0 1
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    (
                                                                      hash-table? item
                                                                    )
                                                                     (
                                                                      hash-table-ref item 0
                                                                    )
                                                                  )
                                                                   (
                                                                    else (
                                                                      list-ref item 0
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                if (
                                                                  not (
                                                                    contains distinct e
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! distinct (
                                                                      append distinct (
                                                                        _list e
                                                                      )
                                                                    )
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
                                                        loop7 (
                                                          cdr xs
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              loop7 row
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                   (
                                    loop5 (
                                      cdr xs
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                         (
                          loop5 edge_array
                        )
                      )
                    )
                  )
                   (
                    ret4 distinct
                  )
                )
              )
            )
          )
        )
         (
          define (
            get_bitcode edge_array de
          )
           (
            call/cc (
              lambda (
                ret9
              )
               (
                let (
                  (
                    bitcode ""
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
                            break11
                          )
                           (
                            letrec (
                              (
                                loop10 (
                                  lambda (
                                    
                                  )
                                   (
                                    if (
                                      < i (
                                        _len edge_array
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            found #f
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
                                                                item (
                                                                  car xs
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                if (
                                                                  string=? (
                                                                    cond (
                                                                      (
                                                                        string? item
                                                                      )
                                                                       (
                                                                        _substring item 0 (
                                                                          + 0 1
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      (
                                                                        hash-table? item
                                                                      )
                                                                       (
                                                                        hash-table-ref item 0
                                                                      )
                                                                    )
                                                                     (
                                                                      else (
                                                                        list-ref item 0
                                                                      )
                                                                    )
                                                                  )
                                                                   de
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! found #t
                                                                  )
                                                                   (
                                                                    break13 (
                                                                      quote (
                                                                        
                                                                      )
                                                                    )
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
                                                            loop12 (
                                                              cdr xs
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  loop12 (
                                                    list-ref edge_array i
                                                  )
                                                )
                                              )
                                            )
                                          )
                                           (
                                            if found (
                                              begin (
                                                set! bitcode (
                                                  string-append bitcode "1"
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                set! bitcode (
                                                  string-append bitcode "0"
                                                )
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
                                        loop10
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
                              loop10
                            )
                          )
                        )
                      )
                       (
                        ret9 bitcode
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
            count_ones s
          )
           (
            call/cc (
              lambda (
                ret14
              )
               (
                let (
                  (
                    c 0
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
                            break16
                          )
                           (
                            letrec (
                              (
                                loop15 (
                                  lambda (
                                    
                                  )
                                   (
                                    if (
                                      < i (
                                        _len s
                                      )
                                    )
                                     (
                                      begin (
                                        if (
                                          string=? (
                                            _substring s i (
                                              + i 1
                                            )
                                          )
                                           "1"
                                        )
                                         (
                                          begin (
                                            set! c (
                                              + c 1
                                            )
                                          )
                                        )
                                         (
                                          quote (
                                            
                                          )
                                        )
                                      )
                                       (
                                        set! i (
                                          + i 1
                                        )
                                      )
                                       (
                                        loop15
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
                              loop15
                            )
                          )
                        )
                      )
                       (
                        ret14 c
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
            get_frequency_table edge_array
          )
           (
            call/cc (
              lambda (
                ret17
              )
               (
                let (
                  (
                    distinct (
                      get_distinct_edge edge_array
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        table (
                          _list
                        )
                      )
                    )
                     (
                      begin (
                        call/cc (
                          lambda (
                            break19
                          )
                           (
                            letrec (
                              (
                                loop18 (
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
                                            e (
                                              car xs
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                bit (
                                                  get_bitcode edge_array e
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    cnt (
                                                      count_ones bit
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        entry (
                                                          alist->hash-table (
                                                            _list (
                                                              cons "edge" e
                                                            )
                                                             (
                                                              cons "count" (
                                                                to-str-space cnt
                                                              )
                                                            )
                                                             (
                                                              cons "bit" bit
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        set! table (
                                                          append table (
                                                            _list entry
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
                                        loop18 (
                                          cdr xs
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                             (
                              loop18 distinct
                            )
                          )
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
                                break21
                              )
                               (
                                letrec (
                                  (
                                    loop20 (
                                      lambda (
                                        
                                      )
                                       (
                                        if (
                                          < i (
                                            _len table
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                max_i i
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    j (
                                                      + i 1
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    call/cc (
                                                      lambda (
                                                        break23
                                                      )
                                                       (
                                                        letrec (
                                                          (
                                                            loop22 (
                                                              lambda (
                                                                
                                                              )
                                                               (
                                                                if (
                                                                  < j (
                                                                    _len table
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    if (
                                                                      _gt (
                                                                        toi (
                                                                          cond (
                                                                            (
                                                                              string? (
                                                                                list-ref table j
                                                                              )
                                                                            )
                                                                             (
                                                                              _substring (
                                                                                list-ref table j
                                                                              )
                                                                               "count" (
                                                                                + "count" 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            (
                                                                              hash-table? (
                                                                                list-ref table j
                                                                              )
                                                                            )
                                                                             (
                                                                              hash-table-ref (
                                                                                list-ref table j
                                                                              )
                                                                               "count"
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref (
                                                                                list-ref table j
                                                                              )
                                                                               "count"
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        toi (
                                                                          cond (
                                                                            (
                                                                              string? (
                                                                                list-ref table max_i
                                                                              )
                                                                            )
                                                                             (
                                                                              _substring (
                                                                                list-ref table max_i
                                                                              )
                                                                               "count" (
                                                                                + "count" 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            (
                                                                              hash-table? (
                                                                                list-ref table max_i
                                                                              )
                                                                            )
                                                                             (
                                                                              hash-table-ref (
                                                                                list-ref table max_i
                                                                              )
                                                                               "count"
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref (
                                                                                list-ref table max_i
                                                                              )
                                                                               "count"
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        set! max_i j
                                                                      )
                                                                    )
                                                                     (
                                                                      quote (
                                                                        
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    set! j (
                                                                      + j 1
                                                                    )
                                                                  )
                                                                   (
                                                                    loop22
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
                                                          loop22
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    let (
                                                      (
                                                        tmp (
                                                          list-ref table i
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        list-set! table i (
                                                          list-ref table max_i
                                                        )
                                                      )
                                                       (
                                                        list-set! table max_i tmp
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
                                            )
                                          )
                                           (
                                            loop20
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
                                  loop20
                                )
                              )
                            )
                          )
                           (
                            ret17 table
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
            get_nodes freq_table
          )
           (
            call/cc (
              lambda (
                ret24
              )
               (
                let (
                  (
                    nodes (
                      alist->hash-table (
                        _list
                      )
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        keys (
                          _list
                        )
                      )
                    )
                     (
                      begin (
                        call/cc (
                          lambda (
                            break26
                          )
                           (
                            letrec (
                              (
                                loop25 (
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
                                            f (
                                              car xs
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                code (
                                                  cond (
                                                    (
                                                      string? f
                                                    )
                                                     (
                                                      _substring f "bit" (
                                                        + "bit" 1
                                                      )
                                                    )
                                                  )
                                                   (
                                                    (
                                                      hash-table? f
                                                    )
                                                     (
                                                      hash-table-ref f "bit"
                                                    )
                                                  )
                                                   (
                                                    else (
                                                      list-ref f "bit"
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    edge (
                                                      cond (
                                                        (
                                                          string? f
                                                        )
                                                         (
                                                          _substring f "edge" (
                                                            + "edge" 1
                                                          )
                                                        )
                                                      )
                                                       (
                                                        (
                                                          hash-table? f
                                                        )
                                                         (
                                                          hash-table-ref f "edge"
                                                        )
                                                      )
                                                       (
                                                        else (
                                                          list-ref f "edge"
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    if (
                                                      cond (
                                                        (
                                                          string? nodes
                                                        )
                                                         (
                                                          if (
                                                            string-contains nodes code
                                                          )
                                                           #t #f
                                                        )
                                                      )
                                                       (
                                                        (
                                                          hash-table? nodes
                                                        )
                                                         (
                                                          if (
                                                            hash-table-exists? nodes code
                                                          )
                                                           #t #f
                                                        )
                                                      )
                                                       (
                                                        else (
                                                          if (
                                                            member code nodes
                                                          )
                                                           #t #f
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        hash-table-set! nodes code (
                                                          append (
                                                            hash-table-ref/default nodes code (
                                                              quote (
                                                                
                                                              )
                                                            )
                                                          )
                                                           (
                                                            _list edge
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        hash-table-set! nodes code (
                                                          _list edge
                                                        )
                                                      )
                                                       (
                                                        set! keys (
                                                          append keys (
                                                            _list code
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
                                        loop25 (
                                          cdr xs
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                             (
                              loop25 freq_table
                            )
                          )
                        )
                      )
                       (
                        ret24 (
                          alist->hash-table (
                            _list (
                              cons "map" nodes
                            )
                             (
                              cons "keys" keys
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
            get_cluster nodes
          )
           (
            call/cc (
              lambda (
                ret27
              )
               (
                let (
                  (
                    clusters (
                      alist->hash-table (
                        _list
                      )
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        weights (
                          _list
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
                                break29
                              )
                               (
                                letrec (
                                  (
                                    loop28 (
                                      lambda (
                                        
                                      )
                                       (
                                        if (
                                          < i (
                                            _len (
                                              hash-table-ref nodes "keys"
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                code (
                                                  list-ref (
                                                    hash-table-ref nodes "keys"
                                                  )
                                                   i
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    wt (
                                                      count_ones code
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    if (
                                                      cond (
                                                        (
                                                          string? clusters
                                                        )
                                                         (
                                                          if (
                                                            string-contains clusters wt
                                                          )
                                                           #t #f
                                                        )
                                                      )
                                                       (
                                                        (
                                                          hash-table? clusters
                                                        )
                                                         (
                                                          if (
                                                            hash-table-exists? clusters wt
                                                          )
                                                           #t #f
                                                        )
                                                      )
                                                       (
                                                        else (
                                                          if (
                                                            member wt clusters
                                                          )
                                                           #t #f
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        hash-table-set! clusters wt (
                                                          append (
                                                            hash-table-ref/default clusters wt (
                                                              quote (
                                                                
                                                              )
                                                            )
                                                          )
                                                           (
                                                            _list code
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        hash-table-set! clusters wt (
                                                          _list code
                                                        )
                                                      )
                                                       (
                                                        set! weights (
                                                          append weights (
                                                            _list wt
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
                                                )
                                              )
                                            )
                                          )
                                           (
                                            loop28
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
                                  loop28
                                )
                              )
                            )
                          )
                           (
                            ret27 (
                              alist->hash-table (
                                _list (
                                  cons "clusters" clusters
                                )
                                 (
                                  cons "weights" weights
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
            get_support clusters
          )
           (
            call/cc (
              lambda (
                ret30
              )
               (
                let (
                  (
                    sup (
                      _list
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
                            break32
                          )
                           (
                            letrec (
                              (
                                loop31 (
                                  lambda (
                                    
                                  )
                                   (
                                    if (
                                      < i (
                                        _len (
                                          hash-table-ref clusters "weights"
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            w (
                                              list-ref (
                                                hash-table-ref clusters "weights"
                                              )
                                               i
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            set! sup (
                                              append sup (
                                                _list (
                                                  _div (
                                                    * w 100
                                                  )
                                                   (
                                                    _len (
                                                      hash-table-ref clusters "weights"
                                                    )
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
                                        )
                                      )
                                       (
                                        loop31
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
                              loop31
                            )
                          )
                        )
                      )
                       (
                        ret30 sup
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
            contains_bits a b
          )
           (
            call/cc (
              lambda (
                ret33
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
                        break35
                      )
                       (
                        letrec (
                          (
                            loop34 (
                              lambda (
                                
                              )
                               (
                                if (
                                  < i (
                                    _len a
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        c1 (
                                          _substring a i (
                                            + i 1
                                          )
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            c2 (
                                              _substring b i (
                                                + i 1
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              and (
                                                string=? c1 "1"
                                              )
                                               (
                                                not (
                                                  string=? c2 "1"
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                ret33 #f
                                              )
                                            )
                                             (
                                              quote (
                                                
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
                                    loop34
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
                          loop34
                        )
                      )
                    )
                  )
                   (
                    ret33 #t
                  )
                )
              )
            )
          )
        )
         (
          define (
            max_cluster_key clusters
          )
           (
            call/cc (
              lambda (
                ret36
              )
               (
                let (
                  (
                    m 0
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
                            break38
                          )
                           (
                            letrec (
                              (
                                loop37 (
                                  lambda (
                                    
                                  )
                                   (
                                    if (
                                      < i (
                                        _len (
                                          hash-table-ref clusters "weights"
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            w (
                                              list-ref (
                                                hash-table-ref clusters "weights"
                                              )
                                               i
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              > w m
                                            )
                                             (
                                              begin (
                                                set! m w
                                              )
                                            )
                                             (
                                              quote (
                                                
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
                                        loop37
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
                              loop37
                            )
                          )
                        )
                      )
                       (
                        ret36 m
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
            get_cluster_codes clusters wt
          )
           (
            call/cc (
              lambda (
                ret39
              )
               (
                begin (
                  if (
                    cond (
                      (
                        string? (
                          hash-table-ref clusters "clusters"
                        )
                      )
                       (
                        if (
                          string-contains (
                            hash-table-ref clusters "clusters"
                          )
                           wt
                        )
                         #t #f
                      )
                    )
                     (
                      (
                        hash-table? (
                          hash-table-ref clusters "clusters"
                        )
                      )
                       (
                        if (
                          hash-table-exists? (
                            hash-table-ref clusters "clusters"
                          )
                           wt
                        )
                         #t #f
                      )
                    )
                     (
                      else (
                        if (
                          member wt (
                            hash-table-ref clusters "clusters"
                          )
                        )
                         #t #f
                      )
                    )
                  )
                   (
                    begin (
                      ret39 (
                        hash-table-ref/default (
                          hash-table-ref clusters "clusters"
                        )
                         wt (
                          quote (
                            
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
                  ret39 (
                    _list
                  )
                )
              )
            )
          )
        )
         (
          define (
            create_edge nodes graph gkeys clusters c1 maxk
          )
           (
            call/cc (
              lambda (
                ret40
              )
               (
                let (
                  (
                    keys gkeys
                  )
                )
                 (
                  begin (
                    let (
                      (
                        codes1 (
                          get_cluster_codes clusters c1
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            idx1 0
                          )
                        )
                         (
                          begin (
                            call/cc (
                              lambda (
                                break42
                              )
                               (
                                letrec (
                                  (
                                    loop41 (
                                      lambda (
                                        
                                      )
                                       (
                                        if (
                                          < idx1 (
                                            _len codes1
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                i_code (
                                                  cond (
                                                    (
                                                      string? codes1
                                                    )
                                                     (
                                                      _substring codes1 idx1 (
                                                        + idx1 1
                                                      )
                                                    )
                                                  )
                                                   (
                                                    (
                                                      hash-table? codes1
                                                    )
                                                     (
                                                      hash-table-ref codes1 idx1
                                                    )
                                                  )
                                                   (
                                                    else (
                                                      list-ref codes1 idx1
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    count 0
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        c2 (
                                                          + c1 1
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        call/cc (
                                                          lambda (
                                                            break44
                                                          )
                                                           (
                                                            letrec (
                                                              (
                                                                loop43 (
                                                                  lambda (
                                                                    
                                                                  )
                                                                   (
                                                                    if (
                                                                      <= c2 maxk
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            codes2 (
                                                                              get_cluster_codes clusters c2
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
                                                                                    break46
                                                                                  )
                                                                                   (
                                                                                    letrec (
                                                                                      (
                                                                                        loop45 (
                                                                                          lambda (
                                                                                            
                                                                                          )
                                                                                           (
                                                                                            if (
                                                                                              < j (
                                                                                                _len codes2
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                let (
                                                                                                  (
                                                                                                    j_code (
                                                                                                      cond (
                                                                                                        (
                                                                                                          string? codes2
                                                                                                        )
                                                                                                         (
                                                                                                          _substring codes2 j (
                                                                                                            + j 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? codes2
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref codes2 j
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref codes2 j
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  begin (
                                                                                                    if (
                                                                                                      contains_bits i_code j_code
                                                                                                    )
                                                                                                     (
                                                                                                      begin (
                                                                                                        if (
                                                                                                          cond (
                                                                                                            (
                                                                                                              string? graph
                                                                                                            )
                                                                                                             (
                                                                                                              if (
                                                                                                                string-contains graph i_code
                                                                                                              )
                                                                                                               #t #f
                                                                                                            )
                                                                                                          )
                                                                                                           (
                                                                                                            (
                                                                                                              hash-table? graph
                                                                                                            )
                                                                                                             (
                                                                                                              if (
                                                                                                                hash-table-exists? graph i_code
                                                                                                              )
                                                                                                               #t #f
                                                                                                            )
                                                                                                          )
                                                                                                           (
                                                                                                            else (
                                                                                                              if (
                                                                                                                member i_code graph
                                                                                                              )
                                                                                                               #t #f
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          begin (
                                                                                                            hash-table-set! graph i_code (
                                                                                                              append (
                                                                                                                hash-table-ref/default graph i_code (
                                                                                                                  quote (
                                                                                                                    
                                                                                                                  )
                                                                                                                )
                                                                                                              )
                                                                                                               (
                                                                                                                _list j_code
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          begin (
                                                                                                            hash-table-set! graph i_code (
                                                                                                              _list j_code
                                                                                                            )
                                                                                                          )
                                                                                                           (
                                                                                                            if (
                                                                                                              not (
                                                                                                                contains keys i_code
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              begin (
                                                                                                                set! keys (
                                                                                                                  append keys (
                                                                                                                    _list i_code
                                                                                                                  )
                                                                                                                )
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
                                                                                                        if (
                                                                                                          not (
                                                                                                            contains keys j_code
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          begin (
                                                                                                            set! keys (
                                                                                                              append keys (
                                                                                                                _list j_code
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
                                                                                                        set! count (
                                                                                                          + count 1
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      quote (
                                                                                                        
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    set! j (
                                                                                                      + j 1
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                loop45
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
                                                                                      loop45
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                if (
                                                                                  equal? count 0
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    set! c2 (
                                                                                      + c2 1
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    break44 (
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
                                                                       (
                                                                        loop43
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
                                                              loop43
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        set! idx1 (
                                                          + idx1 1
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                           (
                                            loop41
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
                                  loop41
                                )
                              )
                            )
                          )
                           (
                            ret40 keys
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
            construct_graph clusters nodes
          )
           (
            call/cc (
              lambda (
                ret47
              )
               (
                let (
                  (
                    maxk (
                      max_cluster_key clusters
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        top_codes (
                          get_cluster_codes clusters maxk
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            graph (
                              alist->hash-table (
                                _list
                              )
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                keys (
                                  _list "Header"
                                )
                              )
                            )
                             (
                              begin (
                                hash-table-set! graph "Header" (
                                  _list
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
                                        break49
                                      )
                                       (
                                        letrec (
                                          (
                                            loop48 (
                                              lambda (
                                                
                                              )
                                               (
                                                if (
                                                  < i (
                                                    _len top_codes
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        code (
                                                          cond (
                                                            (
                                                              string? top_codes
                                                            )
                                                             (
                                                              _substring top_codes i (
                                                                + i 1
                                                              )
                                                            )
                                                          )
                                                           (
                                                            (
                                                              hash-table? top_codes
                                                            )
                                                             (
                                                              hash-table-ref top_codes i
                                                            )
                                                          )
                                                           (
                                                            else (
                                                              list-ref top_codes i
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        hash-table-set! graph "Header" (
                                                          append (
                                                            hash-table-ref/default graph "Header" (
                                                              quote (
                                                                
                                                              )
                                                            )
                                                          )
                                                           (
                                                            _list code
                                                          )
                                                        )
                                                      )
                                                       (
                                                        hash-table-set! graph code (
                                                          _list "Header"
                                                        )
                                                      )
                                                       (
                                                        set! keys (
                                                          append keys (
                                                            _list code
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
                                                    loop48
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
                                          loop48
                                        )
                                      )
                                    )
                                  )
                                   (
                                    let (
                                      (
                                        c 1
                                      )
                                    )
                                     (
                                      begin (
                                        call/cc (
                                          lambda (
                                            break51
                                          )
                                           (
                                            letrec (
                                              (
                                                loop50 (
                                                  lambda (
                                                    
                                                  )
                                                   (
                                                    if (
                                                      _lt c maxk
                                                    )
                                                     (
                                                      begin (
                                                        set! keys (
                                                          create_edge nodes graph keys clusters c maxk
                                                        )
                                                      )
                                                       (
                                                        set! c (
                                                          + c 1
                                                        )
                                                      )
                                                       (
                                                        loop50
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
                                              loop50
                                            )
                                          )
                                        )
                                      )
                                       (
                                        ret47 (
                                          alist->hash-table (
                                            _list (
                                              cons "edges" graph
                                            )
                                             (
                                              cons "keys" keys
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
              paths (
                _list
              )
            )
          )
           (
            begin (
              define (
                copy_list lst
              )
               (
                call/cc (
                  lambda (
                    ret52
                  )
                   (
                    let (
                      (
                        n (
                          _list
                        )
                      )
                    )
                     (
                      begin (
                        call/cc (
                          lambda (
                            break54
                          )
                           (
                            letrec (
                              (
                                loop53 (
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
                                            set! n (
                                              append n (
                                                _list v
                                              )
                                            )
                                          )
                                        )
                                      )
                                       (
                                        loop53 (
                                          cdr xs
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                             (
                              loop53 lst
                            )
                          )
                        )
                      )
                       (
                        ret52 n
                      )
                    )
                  )
                )
              )
            )
             (
              define (
                my_dfs graph start end path
              )
               (
                call/cc (
                  lambda (
                    ret55
                  )
                   (
                    let (
                      (
                        new_path (
                          copy_list path
                        )
                      )
                    )
                     (
                      begin (
                        set! new_path (
                          append new_path (
                            _list start
                          )
                        )
                      )
                       (
                        if (
                          string=? start end
                        )
                         (
                          begin (
                            set! paths (
                              append paths (
                                _list new_path
                              )
                            )
                          )
                           (
                            ret55 (
                              quote (
                                
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
                        call/cc (
                          lambda (
                            break57
                          )
                           (
                            letrec (
                              (
                                loop56 (
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
                                            node (
                                              car xs
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                seen #f
                                              )
                                            )
                                             (
                                              begin (
                                                call/cc (
                                                  lambda (
                                                    break59
                                                  )
                                                   (
                                                    letrec (
                                                      (
                                                        loop58 (
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
                                                                    p (
                                                                      car xs
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    if (
                                                                      equal? p node
                                                                    )
                                                                     (
                                                                      begin (
                                                                        set! seen #t
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
                                                                loop58 (
                                                                  cdr xs
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      loop58 new_path
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                if (
                                                  not seen
                                                )
                                                 (
                                                  begin (
                                                    my_dfs graph node end new_path
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
                                        loop56 (
                                          cdr xs
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                             (
                              loop56 (
                                hash-table-ref/default graph start (
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
             (
              define (
                find_freq_subgraph_given_support s clusters graph
              )
               (
                call/cc (
                  lambda (
                    ret60
                  )
                   (
                    let (
                      (
                        k (
                          _div (
                            * s (
                              _len (
                                hash-table-ref clusters "weights"
                              )
                            )
                          )
                           100
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            codes (
                              get_cluster_codes clusters k
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
                                    break62
                                  )
                                   (
                                    letrec (
                                      (
                                        loop61 (
                                          lambda (
                                            
                                          )
                                           (
                                            if (
                                              < i (
                                                _len codes
                                              )
                                            )
                                             (
                                              begin (
                                                my_dfs (
                                                  hash-table-ref graph "edges"
                                                )
                                                 (
                                                  cond (
                                                    (
                                                      string? codes
                                                    )
                                                     (
                                                      _substring codes i (
                                                        + i 1
                                                      )
                                                    )
                                                  )
                                                   (
                                                    (
                                                      hash-table? codes
                                                    )
                                                     (
                                                      hash-table-ref codes i
                                                    )
                                                  )
                                                   (
                                                    else (
                                                      list-ref codes i
                                                    )
                                                  )
                                                )
                                                 "Header" (
                                                  _list
                                                )
                                              )
                                               (
                                                set! i (
                                                  + i 1
                                                )
                                              )
                                               (
                                                loop61
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
                                      loop61
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
                node_edges nodes code
              )
               (
                call/cc (
                  lambda (
                    ret63
                  )
                   (
                    ret63 (
                      hash-table-ref/default (
                        hash-table-ref nodes "map"
                      )
                       code (
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
                freq_subgraphs_edge_list paths nodes
              )
               (
                call/cc (
                  lambda (
                    ret64
                  )
                   (
                    let (
                      (
                        freq_sub_el (
                          _list
                        )
                      )
                    )
                     (
                      begin (
                        call/cc (
                          lambda (
                            break66
                          )
                           (
                            letrec (
                              (
                                loop65 (
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
                                            path (
                                              car xs
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                el (
                                                  _list
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
                                                        break68
                                                      )
                                                       (
                                                        letrec (
                                                          (
                                                            loop67 (
                                                              lambda (
                                                                
                                                              )
                                                               (
                                                                if (
                                                                  < j (
                                                                    - (
                                                                      _len path
                                                                    )
                                                                     1
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        code (
                                                                          cond (
                                                                            (
                                                                              string? path
                                                                            )
                                                                             (
                                                                              _substring path j (
                                                                                + j 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            (
                                                                              hash-table? path
                                                                            )
                                                                             (
                                                                              hash-table-ref path j
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref path j
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            edge_list (
                                                                              node_edges nodes code
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                e 0
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                call/cc (
                                                                                  lambda (
                                                                                    break70
                                                                                  )
                                                                                   (
                                                                                    letrec (
                                                                                      (
                                                                                        loop69 (
                                                                                          lambda (
                                                                                            
                                                                                          )
                                                                                           (
                                                                                            if (
                                                                                              < e (
                                                                                                _len edge_list
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                let (
                                                                                                  (
                                                                                                    edge (
                                                                                                      cond (
                                                                                                        (
                                                                                                          string? edge_list
                                                                                                        )
                                                                                                         (
                                                                                                          _substring edge_list e (
                                                                                                            + e 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? edge_list
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref edge_list e
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref edge_list e
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  begin (
                                                                                                    let (
                                                                                                      (
                                                                                                        a (
                                                                                                          _substring edge 0 1
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      begin (
                                                                                                        let (
                                                                                                          (
                                                                                                            b (
                                                                                                              _substring edge 1 2
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          begin (
                                                                                                            set! el (
                                                                                                              append el (
                                                                                                                _list (
                                                                                                                  _list a b
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                           (
                                                                                                            set! e (
                                                                                                              + e 1
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                loop69
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
                                                                                      loop69
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                set! j (
                                                                                  + j 1
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    loop67
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
                                                          loop67
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    set! freq_sub_el (
                                                      append freq_sub_el (
                                                        _list el
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
                                        loop65 (
                                          cdr xs
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                             (
                              loop65 paths
                            )
                          )
                        )
                      )
                       (
                        ret64 freq_sub_el
                      )
                    )
                  )
                )
              )
            )
             (
              define (
                print_all nodes support clusters graph freq_subgraph_edge_list
              )
               (
                call/cc (
                  lambda (
                    ret71
                  )
                   (
                    begin (
                      _display (
                        if (
                          string? "\nNodes\n"
                        )
                         "\nNodes\n" (
                          to-str "\nNodes\n"
                        )
                      )
                    )
                     (
                      newline
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
                              break73
                            )
                             (
                              letrec (
                                (
                                  loop72 (
                                    lambda (
                                      
                                    )
                                     (
                                      if (
                                        < i (
                                          _len (
                                            hash-table-ref nodes "keys"
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          let (
                                            (
                                              code (
                                                list-ref (
                                                  hash-table-ref nodes "keys"
                                                )
                                                 i
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              _display (
                                                if (
                                                  string? code
                                                )
                                                 code (
                                                  to-str code
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
                                                    hash-table-ref/default (
                                                      hash-table-ref nodes "map"
                                                    )
                                                     code (
                                                      quote (
                                                        
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  hash-table-ref/default (
                                                    hash-table-ref nodes "map"
                                                  )
                                                   code (
                                                    quote (
                                                      
                                                    )
                                                  )
                                                )
                                                 (
                                                  to-str (
                                                    hash-table-ref/default (
                                                      hash-table-ref nodes "map"
                                                    )
                                                     code (
                                                      quote (
                                                        
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              newline
                                            )
                                             (
                                              set! i (
                                                + i 1
                                              )
                                            )
                                          )
                                        )
                                         (
                                          loop72
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
                                loop72
                              )
                            )
                          )
                        )
                         (
                          _display (
                            if (
                              string? "\nSupport\n"
                            )
                             "\nSupport\n" (
                              to-str "\nSupport\n"
                            )
                          )
                        )
                         (
                          newline
                        )
                         (
                          _display (
                            if (
                              string? support
                            )
                             support (
                              to-str support
                            )
                          )
                        )
                         (
                          newline
                        )
                         (
                          _display (
                            if (
                              string? "\nCluster\n"
                            )
                             "\nCluster\n" (
                              to-str "\nCluster\n"
                            )
                          )
                        )
                         (
                          newline
                        )
                         (
                          let (
                            (
                              j 0
                            )
                          )
                           (
                            begin (
                              call/cc (
                                lambda (
                                  break75
                                )
                                 (
                                  letrec (
                                    (
                                      loop74 (
                                        lambda (
                                          
                                        )
                                         (
                                          if (
                                            < j (
                                              _len (
                                                hash-table-ref clusters "weights"
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              let (
                                                (
                                                  w (
                                                    list-ref (
                                                      hash-table-ref clusters "weights"
                                                    )
                                                     j
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
                                                            to-str-space w
                                                          )
                                                           ":"
                                                        )
                                                         (
                                                          to-str-space (
                                                            hash-table-ref/default (
                                                              hash-table-ref clusters "clusters"
                                                            )
                                                             w (
                                                              quote (
                                                                
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      string-append (
                                                        string-append (
                                                          to-str-space w
                                                        )
                                                         ":"
                                                      )
                                                       (
                                                        to-str-space (
                                                          hash-table-ref/default (
                                                            hash-table-ref clusters "clusters"
                                                          )
                                                           w (
                                                            quote (
                                                              
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      to-str (
                                                        string-append (
                                                          string-append (
                                                            to-str-space w
                                                          )
                                                           ":"
                                                        )
                                                         (
                                                          to-str-space (
                                                            hash-table-ref/default (
                                                              hash-table-ref clusters "clusters"
                                                            )
                                                             w (
                                                              quote (
                                                                
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  newline
                                                )
                                                 (
                                                  set! j (
                                                    + j 1
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              loop74
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
                                    loop74
                                  )
                                )
                              )
                            )
                             (
                              _display (
                                if (
                                  string? "\nGraph\n"
                                )
                                 "\nGraph\n" (
                                  to-str "\nGraph\n"
                                )
                              )
                            )
                             (
                              newline
                            )
                             (
                              let (
                                (
                                  k 0
                                )
                              )
                               (
                                begin (
                                  call/cc (
                                    lambda (
                                      break77
                                    )
                                     (
                                      letrec (
                                        (
                                          loop76 (
                                            lambda (
                                              
                                            )
                                             (
                                              if (
                                                < k (
                                                  _len (
                                                    hash-table-ref graph "keys"
                                                  )
                                                )
                                              )
                                               (
                                                begin (
                                                  let (
                                                    (
                                                      key (
                                                        list-ref (
                                                          hash-table-ref graph "keys"
                                                        )
                                                         k
                                                      )
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      _display (
                                                        if (
                                                          string? key
                                                        )
                                                         key (
                                                          to-str key
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
                                                            hash-table-ref/default (
                                                              hash-table-ref graph "edges"
                                                            )
                                                             key (
                                                              quote (
                                                                
                                                              )
                                                            )
                                                          )
                                                        )
                                                         (
                                                          hash-table-ref/default (
                                                            hash-table-ref graph "edges"
                                                          )
                                                           key (
                                                            quote (
                                                              
                                                            )
                                                          )
                                                        )
                                                         (
                                                          to-str (
                                                            hash-table-ref/default (
                                                              hash-table-ref graph "edges"
                                                            )
                                                             key (
                                                              quote (
                                                                
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      newline
                                                    )
                                                     (
                                                      set! k (
                                                        + k 1
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  loop76
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
                                        loop76
                                      )
                                    )
                                  )
                                )
                                 (
                                  _display (
                                    if (
                                      string? "\nEdge List of Frequent subgraphs\n"
                                    )
                                     "\nEdge List of Frequent subgraphs\n" (
                                      to-str "\nEdge List of Frequent subgraphs\n"
                                    )
                                  )
                                )
                                 (
                                  newline
                                )
                                 (
                                  call/cc (
                                    lambda (
                                      break79
                                    )
                                     (
                                      letrec (
                                        (
                                          loop78 (
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
                                                      el (
                                                        car xs
                                                      )
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      _display (
                                                        if (
                                                          string? el
                                                        )
                                                         el (
                                                          to-str el
                                                        )
                                                      )
                                                    )
                                                     (
                                                      newline
                                                    )
                                                  )
                                                )
                                                 (
                                                  loop78 (
                                                    cdr xs
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                       (
                                        loop78 freq_subgraph_edge_list
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
                    ret80
                  )
                   (
                    let (
                      (
                        frequency_table (
                          get_frequency_table EDGE_ARRAY
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            nodes (
                              get_nodes frequency_table
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                clusters (
                                  get_cluster nodes
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    support (
                                      get_support clusters
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        graph (
                                          construct_graph clusters nodes
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        find_freq_subgraph_given_support 60 clusters graph
                                      )
                                       (
                                        let (
                                          (
                                            freq_subgraph_edge_list (
                                              freq_subgraphs_edge_list paths nodes
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            print_all nodes support clusters graph freq_subgraph_edge_list
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
              main
            )
          )
        )
      )
    )
     (
      let (
        (
          end82 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur83 (
              quotient (
                * (
                  - end82 start81
                )
                 1000000
              )
               jps84
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur83
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
