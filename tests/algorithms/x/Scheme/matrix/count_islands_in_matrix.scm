;; Generated on 2025-08-07 14:14 +0700
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
      define (
        is_safe grid visited row col
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                rows (
                  _len grid
                )
              )
            )
             (
              begin (
                let (
                  (
                    cols (
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
                        visited_cell (
                          cond (
                            (
                              string? (
                                list-ref-safe visited row
                              )
                            )
                             (
                              _substring (
                                list-ref-safe visited row
                              )
                               col (
                                + col 1
                              )
                            )
                          )
                           (
                            (
                              hash-table? (
                                list-ref-safe visited row
                              )
                            )
                             (
                              hash-table-ref (
                                list-ref-safe visited row
                              )
                               col
                            )
                          )
                           (
                            else (
                              list-ref-safe (
                                list-ref-safe visited row
                              )
                               col
                            )
                          )
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            within_bounds (
                              and (
                                and (
                                  and (
                                    >= row 0
                                  )
                                   (
                                    < row rows
                                  )
                                )
                                 (
                                  >= col 0
                                )
                              )
                               (
                                < col cols
                              )
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                not_visited (
                                  eq? visited_cell #f
                                )
                              )
                            )
                             (
                              begin (
                                ret1 (
                                  and (
                                    and within_bounds not_visited
                                  )
                                   (
                                    equal? (
                                      cond (
                                        (
                                          string? (
                                            list-ref-safe grid row
                                          )
                                        )
                                         (
                                          _substring (
                                            list-ref-safe grid row
                                          )
                                           col (
                                            + col 1
                                          )
                                        )
                                      )
                                       (
                                        (
                                          hash-table? (
                                            list-ref-safe grid row
                                          )
                                        )
                                         (
                                          hash-table-ref (
                                            list-ref-safe grid row
                                          )
                                           col
                                        )
                                      )
                                       (
                                        else (
                                          list-ref-safe (
                                            list-ref-safe grid row
                                          )
                                           col
                                        )
                                      )
                                    )
                                     1
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
        dfs grid visited row col
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            let (
              (
                row_nbr (
                  _list (
                    - 1
                  )
                   (
                    - 1
                  )
                   (
                    - 1
                  )
                   0 0 1 1 1
                )
              )
            )
             (
              begin (
                let (
                  (
                    col_nbr (
                      _list (
                        - 1
                      )
                       0 1 (
                        - 1
                      )
                       1 (
                        - 1
                      )
                       0 1
                    )
                  )
                )
                 (
                  begin (
                    list-set! (
                      list-ref-safe visited row
                    )
                     col #t
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
                            break4
                          )
                           (
                            letrec (
                              (
                                loop3 (
                                  lambda (
                                    
                                  )
                                   (
                                    if (
                                      < k 8
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            new_row (
                                              + row (
                                                list-ref-safe row_nbr k
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                new_col (
                                                  + col (
                                                    list-ref-safe col_nbr k
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                if (
                                                  is_safe grid visited new_row new_col
                                                )
                                                 (
                                                  begin (
                                                    dfs grid visited new_row new_col
                                                  )
                                                )
                                                 (
                                                  quote (
                                                    
                                                  )
                                                )
                                              )
                                               (
                                                set! k (
                                                  + k 1
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                       (
                                        loop3
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
                              loop3
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
        count_islands grid
      )
       (
        call/cc (
          lambda (
            ret5
          )
           (
            let (
              (
                rows (
                  _len grid
                )
              )
            )
             (
              begin (
                let (
                  (
                    cols (
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
                        visited (
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
                                break7
                              )
                               (
                                letrec (
                                  (
                                    loop6 (
                                      lambda (
                                        
                                      )
                                       (
                                        if (
                                          < i rows
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                row_list (
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
                                                                  < j cols
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! row_list (
                                                                      append row_list (
                                                                        _list #f
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    set! j (
                                                                      + j 1
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
                                                    set! visited (
                                                      append visited (
                                                        _list row_list
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
                                            loop6
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
                                  loop6
                                )
                              )
                            )
                          )
                           (
                            let (
                              (
                                count 0
                              )
                            )
                             (
                              begin (
                                set! i 0
                              )
                               (
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
                                              < i rows
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
                                                                  < j cols
                                                                )
                                                                 (
                                                                  begin (
                                                                    if (
                                                                      and (
                                                                        not (
                                                                          cond (
                                                                            (
                                                                              string? (
                                                                                list-ref-safe visited i
                                                                              )
                                                                            )
                                                                             (
                                                                              _substring (
                                                                                list-ref-safe visited i
                                                                              )
                                                                               j (
                                                                                + j 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            (
                                                                              hash-table? (
                                                                                list-ref-safe visited i
                                                                              )
                                                                            )
                                                                             (
                                                                              hash-table-ref (
                                                                                list-ref-safe visited i
                                                                              )
                                                                               j
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref-safe (
                                                                                list-ref-safe visited i
                                                                              )
                                                                               j
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        equal? (
                                                                          cond (
                                                                            (
                                                                              string? (
                                                                                list-ref-safe grid i
                                                                              )
                                                                            )
                                                                             (
                                                                              _substring (
                                                                                list-ref-safe grid i
                                                                              )
                                                                               j (
                                                                                + j 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            (
                                                                              hash-table? (
                                                                                list-ref-safe grid i
                                                                              )
                                                                            )
                                                                             (
                                                                              hash-table-ref (
                                                                                list-ref-safe grid i
                                                                              )
                                                                               j
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref-safe (
                                                                                list-ref-safe grid i
                                                                              )
                                                                               j
                                                                            )
                                                                          )
                                                                        )
                                                                         1
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        dfs grid visited i j
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
                                ret5 count
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
          grid (
            _list (
              _list 1 1 0 0 0
            )
             (
              _list 0 1 0 0 1
            )
             (
              _list 1 0 0 1 1
            )
             (
              _list 0 0 0 0 0
            )
             (
              _list 1 0 1 0 1
            )
          )
        )
      )
       (
        begin (
          _display (
            if (
              string? (
                count_islands grid
              )
            )
             (
              count_islands grid
            )
             (
              to-str (
                count_islands grid
              )
            )
          )
        )
         (
          newline
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
