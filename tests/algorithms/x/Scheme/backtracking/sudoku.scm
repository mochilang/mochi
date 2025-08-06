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
      start23 (
        current-jiffy
      )
    )
     (
      jps26 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        is_safe grid row column n
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
                                    ret1 #f
                                  )
                                )
                                 (
                                  quote (
                                    
                                  )
                                )
                              )
                            )
                             (
                              loop2 (
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
                    loop2 0
                  )
                )
              )
            )
             (
              call/cc (
                lambda (
                  break5
                )
                 (
                  letrec (
                    (
                      loop4 (
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
                                    break7
                                  )
                                   (
                                    letrec (
                                      (
                                        loop6 (
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
                                                                  modulo row 3
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
                                                                  modulo row 3
                                                                )
                                                              )
                                                               i
                                                            )
                                                          )
                                                           (
                                                            _add (
                                                              - column (
                                                                modulo column 3
                                                              )
                                                            )
                                                             j
                                                          )
                                                           (
                                                            + (
                                                              _add (
                                                                - column (
                                                                  modulo column 3
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
                                                                  modulo row 3
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
                                                                  modulo row 3
                                                                )
                                                              )
                                                               i
                                                            )
                                                          )
                                                           (
                                                            _add (
                                                              - column (
                                                                modulo column 3
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
                                                                  modulo row 3
                                                                )
                                                              )
                                                               i
                                                            )
                                                          )
                                                           (
                                                            _add (
                                                              - column (
                                                                modulo column 3
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
                                                      ret1 #f
                                                    )
                                                  )
                                                   (
                                                    quote (
                                                      
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                loop6 (
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
                                      loop6 0
                                    )
                                  )
                                )
                              )
                            )
                             (
                              loop4 (
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
                    loop4 0
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
        find_empty_location grid
      )
       (
        call/cc (
          lambda (
            ret8
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
                                    break12
                                  )
                                   (
                                    letrec (
                                      (
                                        loop11 (
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
                                                      ret8 (
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
                                                loop11 (
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
                                      loop11 0
                                    )
                                  )
                                )
                              )
                            )
                             (
                              loop9 (
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
                    loop9 0
                  )
                )
              )
            )
             (
              ret8 (
                _list
              )
            )
          )
        )
      )
    )
     (
      define (
        sudoku grid
      )
       (
        call/cc (
          lambda (
            ret13
          )
           (
            let (
              (
                loc (
                  find_empty_location grid
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
                    ret13 #t
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
                            break15
                          )
                           (
                            letrec (
                              (
                                loop14 (
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
                                                sudoku grid
                                              )
                                               (
                                                begin (
                                                  ret13 #t
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
                                        loop14 (
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
                              loop14 1
                            )
                          )
                        )
                      )
                       (
                        ret13 #f
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
        print_solution grid
      )
       (
        call/cc (
          lambda (
            ret16
          )
           (
            call/cc (
              lambda (
                break18
              )
               (
                letrec (
                  (
                    loop17 (
                      lambda (
                        r
                      )
                       (
                        if (
                          < r (
                            _len grid
                          )
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
                                      break20
                                    )
                                     (
                                      letrec (
                                        (
                                          loop19 (
                                            lambda (
                                              c
                                            )
                                             (
                                              if (
                                                < c (
                                                  _len (
                                                    list-ref grid r
                                                  )
                                                )
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
                                                      _lt c (
                                                        - (
                                                          _len (
                                                            list-ref grid r
                                                          )
                                                        )
                                                         1
                                                      )
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
                                                  loop19 (
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
                                        loop19 0
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
                            loop17 (
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
                  loop17 0
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
          initial_grid (
            _list (
              _list 3 0 6 5 0 8 4 0 0
            )
             (
              _list 5 2 0 0 0 0 0 0 0
            )
             (
              _list 0 8 7 0 0 0 0 3 1
            )
             (
              _list 0 0 3 0 1 0 0 8 0
            )
             (
              _list 9 0 0 8 6 3 0 0 5
            )
             (
              _list 0 5 0 0 9 0 6 0 0
            )
             (
              _list 1 3 0 0 0 0 2 5 0
            )
             (
              _list 0 0 0 0 0 0 0 7 4
            )
             (
              _list 0 0 5 2 0 6 3 0 0
            )
          )
        )
      )
       (
        begin (
          let (
            (
              no_solution (
                _list (
                  _list 5 0 6 5 0 8 4 0 3
                )
                 (
                  _list 5 2 0 0 0 0 0 0 2
                )
                 (
                  _list 1 8 7 0 0 0 0 3 1
                )
                 (
                  _list 0 0 3 0 1 0 0 8 0
                )
                 (
                  _list 9 0 0 8 6 3 0 0 5
                )
                 (
                  _list 0 5 0 0 9 0 6 0 0
                )
                 (
                  _list 1 3 0 0 0 0 2 5 0
                )
                 (
                  _list 0 0 0 0 0 0 0 7 4
                )
                 (
                  _list 0 0 5 2 0 6 3 0 0
                )
              )
            )
          )
           (
            begin (
              let (
                (
                  examples (
                    _list initial_grid no_solution
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      idx 0
                    )
                  )
                   (
                    begin (
                      call/cc (
                        lambda (
                          break22
                        )
                         (
                          letrec (
                            (
                              loop21 (
                                lambda (
                                  
                                )
                                 (
                                  if (
                                    < idx (
                                      _len examples
                                    )
                                  )
                                   (
                                    begin (
                                      _display (
                                        if (
                                          string? "\nExample grid:\n===================="
                                        )
                                         "\nExample grid:\n====================" (
                                          to-str "\nExample grid:\n===================="
                                        )
                                      )
                                    )
                                     (
                                      newline
                                    )
                                     (
                                      print_solution (
                                        list-ref examples idx
                                      )
                                    )
                                     (
                                      _display (
                                        if (
                                          string? "\nExample grid solution:"
                                        )
                                         "\nExample grid solution:" (
                                          to-str "\nExample grid solution:"
                                        )
                                      )
                                    )
                                     (
                                      newline
                                    )
                                     (
                                      if (
                                        sudoku (
                                          list-ref examples idx
                                        )
                                      )
                                       (
                                        begin (
                                          print_solution (
                                            list-ref examples idx
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          _display (
                                            if (
                                              string? "Cannot find a solution."
                                            )
                                             "Cannot find a solution." (
                                              to-str "Cannot find a solution."
                                            )
                                          )
                                        )
                                         (
                                          newline
                                        )
                                      )
                                    )
                                     (
                                      set! idx (
                                        + idx 1
                                      )
                                    )
                                     (
                                      loop21
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
                            loop21
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
          end24 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur25 (
              quotient (
                * (
                  - end24 start23
                )
                 1000000
              )
               jps26
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur25
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
