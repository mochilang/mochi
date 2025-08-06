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
      start27 (
        current-jiffy
      )
    )
     (
      jps30 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        string_to_grid s
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                grid (
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
                                  < i 9
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
                                            j 0
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
                                                          < j 9
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                ch (
                                                                  _substring s (
                                                                    + (
                                                                      * i 9
                                                                    )
                                                                     j
                                                                  )
                                                                   (
                                                                    + (
                                                                      + (
                                                                        * i 9
                                                                      )
                                                                       j
                                                                    )
                                                                     1
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                let (
                                                                  (
                                                                    val 0
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    if (
                                                                      and (
                                                                        not (
                                                                          string=? ch "0"
                                                                        )
                                                                      )
                                                                       (
                                                                        not (
                                                                          string=? ch "."
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        set! val (
                                                                          let (
                                                                            (
                                                                              v6 ch
                                                                            )
                                                                          )
                                                                           (
                                                                            cond (
                                                                              (
                                                                                string? v6
                                                                              )
                                                                               (
                                                                                exact (
                                                                                  floor (
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
                                                                                  floor v6
                                                                                )
                                                                              )
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
                                                                    set! row (
                                                                      append row (
                                                                        _list val
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
                                                           (
                                                            loop4
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
                                                  loop4
                                                )
                                              )
                                            )
                                          )
                                           (
                                            set! grid (
                                              append grid (
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
                                 (
                                  quote (
                                    
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
                    )
                  )
                   (
                    ret1 grid
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
        print_grid grid
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            call/cc (
              lambda (
                break9
              )
               (
                letrec (
                  (
                    loop8 (
                      lambda (
                        r
                      )
                       (
                        if (
                          < r 9
                        )
                         (
                          begin (
                            begin (
                              let (
                                (
                                  line ""
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
                                              c
                                            )
                                             (
                                              if (
                                                < c 9
                                              )
                                               (
                                                begin (
                                                  begin (
                                                    set! line (
                                                      string-append line (
                                                        to-str-space (
                                                          cond (
                                                            (
                                                              string? (
                                                                list-ref grid r
                                                              )
                                                            )
                                                             (
                                                              _substring (
                                                                list-ref grid r
                                                              )
                                                               c (
                                                                + c 1
                                                              )
                                                            )
                                                          )
                                                           (
                                                            (
                                                              hash-table? (
                                                                list-ref grid r
                                                              )
                                                            )
                                                             (
                                                              hash-table-ref (
                                                                list-ref grid r
                                                              )
                                                               c
                                                            )
                                                          )
                                                           (
                                                            else (
                                                              list-ref (
                                                                list-ref grid r
                                                              )
                                                               c
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    if (
                                                      _lt c 8
                                                    )
                                                     (
                                                      begin (
                                                        set! line (
                                                          string-append line " "
                                                        )
                                                      )
                                                    )
                                                     (
                                                      quote (
                                                        
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  loop10 (
                                                    + c 1
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
                                       (
                                        loop10 0
                                      )
                                    )
                                  )
                                )
                                 (
                                  _display (
                                    if (
                                      string? line
                                    )
                                     line (
                                      to-str line
                                    )
                                  )
                                )
                                 (
                                  newline
                                )
                              )
                            )
                          )
                           (
                            loop8 (
                              + r 1
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
                 (
                  loop8 0
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        is_safe grid row column n
      )
       (
        call/cc (
          lambda (
            ret12
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
                          i
                        )
                         (
                          if (
                            < i 9
                          )
                           (
                            begin (
                              begin (
                                if (
                                  or (
                                    equal? (
                                      cond (
                                        (
                                          string? (
                                            list-ref grid row
                                          )
                                        )
                                         (
                                          _substring (
                                            list-ref grid row
                                          )
                                           i (
                                            + i 1
                                          )
                                        )
                                      )
                                       (
                                        (
                                          hash-table? (
                                            list-ref grid row
                                          )
                                        )
                                         (
                                          hash-table-ref (
                                            list-ref grid row
                                          )
                                           i
                                        )
                                      )
                                       (
                                        else (
                                          list-ref (
                                            list-ref grid row
                                          )
                                           i
                                        )
                                      )
                                    )
                                     n
                                  )
                                   (
                                    equal? (
                                      cond (
                                        (
                                          string? (
                                            list-ref grid i
                                          )
                                        )
                                         (
                                          _substring (
                                            list-ref grid i
                                          )
                                           column (
                                            + column 1
                                          )
                                        )
                                      )
                                       (
                                        (
                                          hash-table? (
                                            list-ref grid i
                                          )
                                        )
                                         (
                                          hash-table-ref (
                                            list-ref grid i
                                          )
                                           column
                                        )
                                      )
                                       (
                                        else (
                                          list-ref (
                                            list-ref grid i
                                          )
                                           column
                                        )
                                      )
                                    )
                                     n
                                  )
                                )
                                 (
                                  begin (
                                    ret12 #f
                                  )
                                )
                                 (
                                  quote (
                                    
                                  )
                                )
                              )
                            )
                             (
                              loop13 (
                                + i 1
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
                   (
                    loop13 0
                  )
                )
              )
            )
             (
              call/cc (
                lambda (
                  break16
                )
                 (
                  letrec (
                    (
                      loop15 (
                        lambda (
                          i
                        )
                         (
                          if (
                            < i 3
                          )
                           (
                            begin (
                              begin (
                                call/cc (
                                  lambda (
                                    break18
                                  )
                                   (
                                    letrec (
                                      (
                                        loop17 (
                                          lambda (
                                            j
                                          )
                                           (
                                            if (
                                              < j 3
                                            )
                                             (
                                              begin (
                                                begin (
                                                  if (
                                                    equal? (
                                                      cond (
                                                        (
                                                          string? (
                                                            list-ref grid (
                                                              _add (
                                                                - row (
                                                                  _mod row 3
                                                                )
                                                              )
                                                               i
                                                            )
                                                          )
                                                        )
                                                         (
                                                          _substring (
                                                            list-ref grid (
                                                              _add (
                                                                - row (
                                                                  _mod row 3
                                                                )
                                                              )
                                                               i
                                                            )
                                                          )
                                                           (
                                                            _add (
                                                              - column (
                                                                _mod column 3
                                                              )
                                                            )
                                                             j
                                                          )
                                                           (
                                                            + (
                                                              _add (
                                                                - column (
                                                                  _mod column 3
                                                                )
                                                              )
                                                               j
                                                            )
                                                             1
                                                          )
                                                        )
                                                      )
                                                       (
                                                        (
                                                          hash-table? (
                                                            list-ref grid (
                                                              _add (
                                                                - row (
                                                                  _mod row 3
                                                                )
                                                              )
                                                               i
                                                            )
                                                          )
                                                        )
                                                         (
                                                          hash-table-ref (
                                                            list-ref grid (
                                                              _add (
                                                                - row (
                                                                  _mod row 3
                                                                )
                                                              )
                                                               i
                                                            )
                                                          )
                                                           (
                                                            _add (
                                                              - column (
                                                                _mod column 3
                                                              )
                                                            )
                                                             j
                                                          )
                                                        )
                                                      )
                                                       (
                                                        else (
                                                          list-ref (
                                                            list-ref grid (
                                                              _add (
                                                                - row (
                                                                  _mod row 3
                                                                )
                                                              )
                                                               i
                                                            )
                                                          )
                                                           (
                                                            _add (
                                                              - column (
                                                                _mod column 3
                                                              )
                                                            )
                                                             j
                                                          )
                                                        )
                                                      )
                                                    )
                                                     n
                                                  )
                                                   (
                                                    begin (
                                                      ret12 #f
                                                    )
                                                  )
                                                   (
                                                    quote (
                                                      
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                loop17 (
                                                  + j 1
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
                                     (
                                      loop17 0
                                    )
                                  )
                                )
                              )
                            )
                             (
                              loop15 (
                                + i 1
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
                   (
                    loop15 0
                  )
                )
              )
            )
             (
              ret12 #t
            )
          )
        )
      )
    )
     (
      define (
        find_empty grid
      )
       (
        call/cc (
          lambda (
            ret19
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
                          i
                        )
                         (
                          if (
                            < i 9
                          )
                           (
                            begin (
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
                                            j
                                          )
                                           (
                                            if (
                                              < j 9
                                            )
                                             (
                                              begin (
                                                begin (
                                                  if (
                                                    equal? (
                                                      cond (
                                                        (
                                                          string? (
                                                            list-ref grid i
                                                          )
                                                        )
                                                         (
                                                          _substring (
                                                            list-ref grid i
                                                          )
                                                           j (
                                                            + j 1
                                                          )
                                                        )
                                                      )
                                                       (
                                                        (
                                                          hash-table? (
                                                            list-ref grid i
                                                          )
                                                        )
                                                         (
                                                          hash-table-ref (
                                                            list-ref grid i
                                                          )
                                                           j
                                                        )
                                                      )
                                                       (
                                                        else (
                                                          list-ref (
                                                            list-ref grid i
                                                          )
                                                           j
                                                        )
                                                      )
                                                    )
                                                     0
                                                  )
                                                   (
                                                    begin (
                                                      ret19 (
                                                        _list i j
                                                      )
                                                    )
                                                  )
                                                   (
                                                    quote (
                                                      
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                loop22 (
                                                  + j 1
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
                                     (
                                      loop22 0
                                    )
                                  )
                                )
                              )
                            )
                             (
                              loop20 (
                                + i 1
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
                   (
                    loop20 0
                  )
                )
              )
            )
             (
              ret19 (
                _list
              )
            )
          )
        )
      )
    )
     (
      define (
        solve grid
      )
       (
        call/cc (
          lambda (
            ret24
          )
           (
            let (
              (
                loc (
                  find_empty grid
                )
              )
            )
             (
              begin (
                if (
                  equal? (
                    _len loc
                  )
                   0
                )
                 (
                  begin (
                    ret24 #t
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
                    row (
                      cond (
                        (
                          string? loc
                        )
                         (
                          _substring loc 0 (
                            + 0 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? loc
                        )
                         (
                          hash-table-ref loc 0
                        )
                      )
                       (
                        else (
                          list-ref loc 0
                        )
                      )
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        column (
                          cond (
                            (
                              string? loc
                            )
                             (
                              _substring loc 1 (
                                + 1 1
                              )
                            )
                          )
                           (
                            (
                              hash-table? loc
                            )
                             (
                              hash-table-ref loc 1
                            )
                          )
                           (
                            else (
                              list-ref loc 1
                            )
                          )
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
                                    digit
                                  )
                                   (
                                    if (
                                      < digit 10
                                    )
                                     (
                                      begin (
                                        begin (
                                          if (
                                            is_safe grid row column digit
                                          )
                                           (
                                            begin (
                                              list-set! (
                                                list-ref grid row
                                              )
                                               column digit
                                            )
                                             (
                                              if (
                                                solve grid
                                              )
                                               (
                                                begin (
                                                  ret24 #t
                                                )
                                              )
                                               (
                                                quote (
                                                  
                                                )
                                              )
                                            )
                                             (
                                              list-set! (
                                                list-ref grid row
                                              )
                                               column 0
                                            )
                                          )
                                           (
                                            quote (
                                              
                                            )
                                          )
                                        )
                                      )
                                       (
                                        loop25 (
                                          + digit 1
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
                             (
                              loop25 1
                            )
                          )
                        )
                      )
                       (
                        ret24 #f
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
          puzzle "003020600900305001001806400008102900700000008006708200002609500800203009005010300"
        )
      )
       (
        begin (
          let (
            (
              grid (
                string_to_grid puzzle
              )
            )
          )
           (
            begin (
              _display (
                if (
                  string? "Original grid:"
                )
                 "Original grid:" (
                  to-str "Original grid:"
                )
              )
            )
             (
              newline
            )
             (
              print_grid grid
            )
             (
              if (
                solve grid
              )
               (
                begin (
                  _display (
                    if (
                      string? "\nSolved grid:"
                    )
                     "\nSolved grid:" (
                      to-str "\nSolved grid:"
                    )
                  )
                )
                 (
                  newline
                )
                 (
                  print_grid grid
                )
              )
               (
                begin (
                  _display (
                    if (
                      string? "\nNo solution found"
                    )
                     "\nNo solution found" (
                      to-str "\nNo solution found"
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
          end28 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur29 (
              quotient (
                * (
                  - end28 start27
                )
                 1000000
              )
               jps30
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur29
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
