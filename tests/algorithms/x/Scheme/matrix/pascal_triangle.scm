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
      start16 (
        current-jiffy
      )
    )
     (
      jps19 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        populate_current_row triangle current_row_idx
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
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
                                  <= i current_row_idx
                                )
                                 (
                                  begin (
                                    if (
                                      or (
                                        equal? i 0
                                      )
                                       (
                                        equal? i current_row_idx
                                      )
                                    )
                                     (
                                      begin (
                                        set! row (
                                          append row (
                                            _list 1
                                          )
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            left (
                                              cond (
                                                (
                                                  string? (
                                                    list-ref-safe triangle (
                                                      - current_row_idx 1
                                                    )
                                                  )
                                                )
                                                 (
                                                  _substring (
                                                    list-ref-safe triangle (
                                                      - current_row_idx 1
                                                    )
                                                  )
                                                   (
                                                    - i 1
                                                  )
                                                   (
                                                    + (
                                                      - i 1
                                                    )
                                                     1
                                                  )
                                                )
                                              )
                                               (
                                                (
                                                  hash-table? (
                                                    list-ref-safe triangle (
                                                      - current_row_idx 1
                                                    )
                                                  )
                                                )
                                                 (
                                                  hash-table-ref (
                                                    list-ref-safe triangle (
                                                      - current_row_idx 1
                                                    )
                                                  )
                                                   (
                                                    - i 1
                                                  )
                                                )
                                              )
                                               (
                                                else (
                                                  list-ref-safe (
                                                    list-ref-safe triangle (
                                                      - current_row_idx 1
                                                    )
                                                  )
                                                   (
                                                    - i 1
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                right (
                                                  cond (
                                                    (
                                                      string? (
                                                        list-ref-safe triangle (
                                                          - current_row_idx 1
                                                        )
                                                      )
                                                    )
                                                     (
                                                      _substring (
                                                        list-ref-safe triangle (
                                                          - current_row_idx 1
                                                        )
                                                      )
                                                       i (
                                                        + i 1
                                                      )
                                                    )
                                                  )
                                                   (
                                                    (
                                                      hash-table? (
                                                        list-ref-safe triangle (
                                                          - current_row_idx 1
                                                        )
                                                      )
                                                    )
                                                     (
                                                      hash-table-ref (
                                                        list-ref-safe triangle (
                                                          - current_row_idx 1
                                                        )
                                                      )
                                                       i
                                                    )
                                                  )
                                                   (
                                                    else (
                                                      list-ref-safe (
                                                        list-ref-safe triangle (
                                                          - current_row_idx 1
                                                        )
                                                      )
                                                       i
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                set! row (
                                                  append row (
                                                    _list (
                                                      + left right
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
                                    set! i (
                                      + i 1
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
                    ret1 row
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
        generate_pascal_triangle num_rows
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            begin (
              if (
                <= num_rows 0
              )
               (
                begin (
                  ret4 (
                    _list
                  )
                )
              )
               '(
                
              )
            )
             (
              let (
                (
                  triangle (
                    _list
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      row_idx 0
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
                                    < row_idx num_rows
                                  )
                                   (
                                    begin (
                                      let (
                                        (
                                          row (
                                            populate_current_row triangle row_idx
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          set! triangle (
                                            append triangle (
                                              _list row
                                            )
                                          )
                                        )
                                         (
                                          set! row_idx (
                                            + row_idx 1
                                          )
                                        )
                                      )
                                    )
                                     (
                                      loop5
                                    )
                                  )
                                   '(
                                    
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
                      ret4 triangle
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
        row_to_string row total_rows row_idx
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            let (
              (
                line ""
              )
            )
             (
              begin (
                let (
                  (
                    spaces (
                      - (
                        - total_rows row_idx
                      )
                       1
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        s 0
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
                                      < s spaces
                                    )
                                     (
                                      begin (
                                        set! line (
                                          string-append line " "
                                        )
                                      )
                                       (
                                        set! s (
                                          + s 1
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
                        let (
                          (
                            c 0
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
                                          <= c row_idx
                                        )
                                         (
                                          begin (
                                            set! line (
                                              string-append line (
                                                to-str-space (
                                                  list-ref-safe row c
                                                )
                                              )
                                            )
                                          )
                                           (
                                            if (
                                              not (
                                                equal? c row_idx
                                              )
                                            )
                                             (
                                              begin (
                                                set! line (
                                                  string-append line " "
                                                )
                                              )
                                            )
                                             '(
                                              
                                            )
                                          )
                                           (
                                            set! c (
                                              + c 1
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
                            ret7 line
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
        print_pascal_triangle num_rows
      )
       (
        call/cc (
          lambda (
            ret12
          )
           (
            let (
              (
                triangle (
                  generate_pascal_triangle num_rows
                )
              )
            )
             (
              begin (
                let (
                  (
                    r 0
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
                                  < r num_rows
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        line (
                                          row_to_string (
                                            cond (
                                              (
                                                string? triangle
                                              )
                                               (
                                                _substring triangle r (
                                                  + r 1
                                                )
                                              )
                                            )
                                             (
                                              (
                                                hash-table? triangle
                                              )
                                               (
                                                hash-table-ref triangle r
                                              )
                                            )
                                             (
                                              else (
                                                list-ref-safe triangle r
                                              )
                                            )
                                          )
                                           num_rows r
                                        )
                                      )
                                    )
                                     (
                                      begin (
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
                                       (
                                        set! r (
                                          + r 1
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
            ret15
          )
           (
            begin (
              print_pascal_triangle 5
            )
             (
              _display (
                if (
                  string? (
                    to-str-space (
                      generate_pascal_triangle 5
                    )
                  )
                )
                 (
                  to-str-space (
                    generate_pascal_triangle 5
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      generate_pascal_triangle 5
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
      main
    )
     (
      let (
        (
          end17 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur18 (
              quotient (
                * (
                  - end17 start16
                )
                 1000000
              )
               jps19
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur18
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
