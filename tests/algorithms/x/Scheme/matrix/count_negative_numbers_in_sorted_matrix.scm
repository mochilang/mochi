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
      start28 (
        current-jiffy
      )
    )
     (
      jps31 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        generate_large_matrix
      )
       (
        call/cc (
          lambda (
            ret1
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
                let (
                  (
                    i 0
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
                                  < i 1000
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        row (
                                          _list
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            j (
                                              - 1000 i
                                            )
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
                                                          > j (
                                                            - (
                                                              - 1000
                                                            )
                                                             i
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            set! row (
                                                              append row (
                                                                _list j
                                                              )
                                                            )
                                                          )
                                                           (
                                                            set! j (
                                                              - j 1
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
                                            set! result (
                                              append result (
                                                _list row
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
                    ret1 result
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
        find_negative_index arr
      )
       (
        call/cc (
          lambda (
            ret6
          )
           (
            let (
              (
                left 0
              )
            )
             (
              begin (
                let (
                  (
                    right (
                      - (
                        _len arr
                      )
                       1
                    )
                  )
                )
                 (
                  begin (
                    if (
                      equal? (
                        _len arr
                      )
                       0
                    )
                     (
                      begin (
                        ret6 0
                      )
                    )
                     '(
                      
                    )
                  )
                   (
                    if (
                      < (
                        list-ref-safe arr 0
                      )
                       0
                    )
                     (
                      begin (
                        ret6 0
                      )
                    )
                     '(
                      
                    )
                  )
                   (
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
                                  <= left right
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
                                        let (
                                          (
                                            num (
                                              list-ref-safe arr mid
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              < num 0
                                            )
                                             (
                                              begin (
                                                if (
                                                  equal? mid 0
                                                )
                                                 (
                                                  begin (
                                                    ret6 0
                                                  )
                                                )
                                                 '(
                                                  
                                                )
                                              )
                                               (
                                                if (
                                                  >= (
                                                    list-ref-safe arr (
                                                      - mid 1
                                                    )
                                                  )
                                                   0
                                                )
                                                 (
                                                  begin (
                                                    ret6 mid
                                                  )
                                                )
                                                 '(
                                                  
                                                )
                                              )
                                               (
                                                set! right (
                                                  - mid 1
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                set! left (
                                                  + mid 1
                                                )
                                              )
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
                                 '(
                                  
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
                    ret6 (
                      _len arr
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
        count_negatives_binary_search grid
      )
       (
        call/cc (
          lambda (
            ret9
          )
           (
            let (
              (
                total 0
              )
            )
             (
              begin (
                let (
                  (
                    bound (
                      _len (
                        list-ref-safe grid 0
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
                                        _len grid
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            row (
                                              list-ref-safe grid i
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                idx (
                                                  find_negative_index (
                                                    slice row 0 bound
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                set! bound idx
                                              )
                                               (
                                                set! total (
                                                  _add total idx
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
                                        loop10
                                      )
                                    )
                                     '(
                                      
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
                        ret9 (
                          - (
                            * (
                              _len grid
                            )
                             (
                              _len (
                                list-ref-safe grid 0
                              )
                            )
                          )
                           total
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
        count_negatives_brute_force grid
      )
       (
        call/cc (
          lambda (
            ret12
          )
           (
            let (
              (
                count 0
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
                        break14
                      )
                       (
                        letrec (
                          (
                            loop13 (
                              lambda (
                                
                              )
                               (
                                if (
                                  < i (
                                    _len grid
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        row (
                                          list-ref-safe grid i
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
                                                          < j (
                                                            _len row
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            if (
                                                              < (
                                                                list-ref-safe row j
                                                              )
                                                               0
                                                            )
                                                             (
                                                              begin (
                                                                set! count (
                                                                  + count 1
                                                                )
                                                              )
                                                            )
                                                             '(
                                                              
                                                            )
                                                          )
                                                           (
                                                            set! j (
                                                              + j 1
                                                            )
                                                          )
                                                           (
                                                            loop15
                                                          )
                                                        )
                                                         '(
                                                          
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
                                            set! i (
                                              + i 1
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                   (
                                    loop13
                                  )
                                )
                                 '(
                                  
                                )
                              )
                            )
                          )
                        )
                         (
                          loop13
                        )
                      )
                    )
                  )
                   (
                    ret12 count
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
        count_negatives_brute_force_with_break grid
      )
       (
        call/cc (
          lambda (
            ret17
          )
           (
            let (
              (
                total 0
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
                        break19
                      )
                       (
                        letrec (
                          (
                            loop18 (
                              lambda (
                                
                              )
                               (
                                if (
                                  < i (
                                    _len grid
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        row (
                                          list-ref-safe grid i
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
                                                          < j (
                                                            _len row
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                number (
                                                                  list-ref-safe row j
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                if (
                                                                  < number 0
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! total (
                                                                      + total (
                                                                        - (
                                                                          _len row
                                                                        )
                                                                         j
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    break21 '(
                                                                      
                                                                    )
                                                                  )
                                                                )
                                                                 '(
                                                                  
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
                                                            loop20
                                                          )
                                                        )
                                                         '(
                                                          
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
                                            set! i (
                                              + i 1
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                   (
                                    loop18
                                  )
                                )
                                 '(
                                  
                                )
                              )
                            )
                          )
                        )
                         (
                          loop18
                        )
                      )
                    )
                  )
                   (
                    ret17 total
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
          grid (
            generate_large_matrix
          )
        )
      )
       (
        begin (
          let (
            (
              test_grids (
                _list (
                  _list (
                    _list 4 3 2 (
                      - 1
                    )
                  )
                   (
                    _list 3 2 1 (
                      - 1
                    )
                  )
                   (
                    _list 1 1 (
                      - 1
                    )
                     (
                      - 2
                    )
                  )
                   (
                    _list (
                      - 1
                    )
                     (
                      - 1
                    )
                     (
                      - 2
                    )
                     (
                      - 3
                    )
                  )
                )
                 (
                  _list (
                    _list 3 2
                  )
                   (
                    _list 1 0
                  )
                )
                 (
                  _list (
                    _list 7 7 6
                  )
                )
                 (
                  _list (
                    _list 7 7 6
                  )
                   (
                    _list (
                      - 1
                    )
                     (
                      - 2
                    )
                     (
                      - 3
                    )
                  )
                )
                 grid
              )
            )
          )
           (
            begin (
              let (
                (
                  results_bin (
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
                                    < i (
                                      _len test_grids
                                    )
                                  )
                                   (
                                    begin (
                                      set! results_bin (
                                        append results_bin (
                                          _list (
                                            count_negatives_binary_search (
                                              list-ref-safe test_grids i
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
                                      loop22
                                    )
                                  )
                                   '(
                                    
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
                      _display (
                        if (
                          string? (
                            to-str-space results_bin
                          )
                        )
                         (
                          to-str-space results_bin
                        )
                         (
                          to-str (
                            to-str-space results_bin
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
                          results_brute (
                            _list
                          )
                        )
                      )
                       (
                        begin (
                          set! i 0
                        )
                         (
                          call/cc (
                            lambda (
                              break25
                            )
                             (
                              letrec (
                                (
                                  loop24 (
                                    lambda (
                                      
                                    )
                                     (
                                      if (
                                        < i (
                                          _len test_grids
                                        )
                                      )
                                       (
                                        begin (
                                          set! results_brute (
                                            append results_brute (
                                              _list (
                                                count_negatives_brute_force (
                                                  list-ref-safe test_grids i
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
                                          loop24
                                        )
                                      )
                                       '(
                                        
                                      )
                                    )
                                  )
                                )
                              )
                               (
                                loop24
                              )
                            )
                          )
                        )
                         (
                          _display (
                            if (
                              string? (
                                to-str-space results_brute
                              )
                            )
                             (
                              to-str-space results_brute
                            )
                             (
                              to-str (
                                to-str-space results_brute
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
                              results_break (
                                _list
                              )
                            )
                          )
                           (
                            begin (
                              set! i 0
                            )
                             (
                              call/cc (
                                lambda (
                                  break27
                                )
                                 (
                                  letrec (
                                    (
                                      loop26 (
                                        lambda (
                                          
                                        )
                                         (
                                          if (
                                            < i (
                                              _len test_grids
                                            )
                                          )
                                           (
                                            begin (
                                              set! results_break (
                                                append results_break (
                                                  _list (
                                                    count_negatives_brute_force_with_break (
                                                      list-ref-safe test_grids i
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
                                              loop26
                                            )
                                          )
                                           '(
                                            
                                          )
                                        )
                                      )
                                    )
                                  )
                                   (
                                    loop26
                                  )
                                )
                              )
                            )
                             (
                              _display (
                                if (
                                  string? (
                                    to-str-space results_break
                                  )
                                )
                                 (
                                  to-str-space results_break
                                )
                                 (
                                  to-str (
                                    to-str-space results_break
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
              )
            )
          )
        )
      )
    )
     (
      let (
        (
          end29 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur30 (
              quotient (
                * (
                  - end29 start28
                )
                 1000000
              )
               jps31
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur30
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
