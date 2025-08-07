;; Generated on 2025-08-07 11:54 +0700
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
(define (_div a b) (/ a b))
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
      start29 (
        current-jiffy
      )
    )
     (
      jps32 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        update_area_of_max_square row col rows cols mat largest_square_area
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            begin (
              if (
                or (
                  >= row rows
                )
                 (
                  >= col cols
                )
              )
               (
                begin (
                  ret1 0
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
                  right (
                    update_area_of_max_square row (
                      + col 1
                    )
                     rows cols mat largest_square_area
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      diagonal (
                        update_area_of_max_square (
                          + row 1
                        )
                         (
                          + col 1
                        )
                         rows cols mat largest_square_area
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          down (
                            update_area_of_max_square (
                              + row 1
                            )
                             col rows cols mat largest_square_area
                          )
                        )
                      )
                       (
                        begin (
                          if (
                            equal? (
                              cond (
                                (
                                  string? (
                                    list-ref mat row
                                  )
                                )
                                 (
                                  _substring (
                                    list-ref mat row
                                  )
                                   col (
                                    + col 1
                                  )
                                )
                              )
                               (
                                (
                                  hash-table? (
                                    list-ref mat row
                                  )
                                )
                                 (
                                  hash-table-ref (
                                    list-ref mat row
                                  )
                                   col
                                )
                              )
                               (
                                else (
                                  list-ref (
                                    list-ref mat row
                                  )
                                   col
                                )
                              )
                            )
                             1
                          )
                           (
                            begin (
                              let (
                                (
                                  sub (
                                    _add 1 (
                                      apply min (
                                        _list right diagonal down
                                      )
                                    )
                                  )
                                )
                              )
                               (
                                begin (
                                  if (
                                    _gt sub (
                                      list-ref largest_square_area 0
                                    )
                                  )
                                   (
                                    begin (
                                      list-set! largest_square_area 0 sub
                                    )
                                  )
                                   (
                                    quote (
                                      
                                    )
                                  )
                                )
                                 (
                                  ret1 sub
                                )
                              )
                            )
                          )
                           (
                            begin (
                              ret1 0
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
        largest_square_area_in_matrix_top_down rows cols mat
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            let (
              (
                largest (
                  _list 0
                )
              )
            )
             (
              begin (
                update_area_of_max_square 0 0 rows cols mat largest
              )
               (
                ret2 (
                  list-ref largest 0
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        update_area_of_max_square_with_dp row col rows cols mat dp_array largest_square_area
      )
       (
        call/cc (
          lambda (
            ret3
          )
           (
            begin (
              if (
                or (
                  >= row rows
                )
                 (
                  >= col cols
                )
              )
               (
                begin (
                  ret3 0
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                not (
                  equal? (
                    cond (
                      (
                        string? (
                          list-ref dp_array row
                        )
                      )
                       (
                        _substring (
                          list-ref dp_array row
                        )
                         col (
                          + col 1
                        )
                      )
                    )
                     (
                      (
                        hash-table? (
                          list-ref dp_array row
                        )
                      )
                       (
                        hash-table-ref (
                          list-ref dp_array row
                        )
                         col
                      )
                    )
                     (
                      else (
                        list-ref (
                          list-ref dp_array row
                        )
                         col
                      )
                    )
                  )
                   (
                    - 1
                  )
                )
              )
               (
                begin (
                  ret3 (
                    cond (
                      (
                        string? (
                          list-ref dp_array row
                        )
                      )
                       (
                        _substring (
                          list-ref dp_array row
                        )
                         col (
                          + col 1
                        )
                      )
                    )
                     (
                      (
                        hash-table? (
                          list-ref dp_array row
                        )
                      )
                       (
                        hash-table-ref (
                          list-ref dp_array row
                        )
                         col
                      )
                    )
                     (
                      else (
                        list-ref (
                          list-ref dp_array row
                        )
                         col
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
              let (
                (
                  right (
                    update_area_of_max_square_with_dp row (
                      + col 1
                    )
                     rows cols mat dp_array largest_square_area
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      diagonal (
                        update_area_of_max_square_with_dp (
                          + row 1
                        )
                         (
                          + col 1
                        )
                         rows cols mat dp_array largest_square_area
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          down (
                            update_area_of_max_square_with_dp (
                              + row 1
                            )
                             col rows cols mat dp_array largest_square_area
                          )
                        )
                      )
                       (
                        begin (
                          if (
                            equal? (
                              cond (
                                (
                                  string? (
                                    list-ref mat row
                                  )
                                )
                                 (
                                  _substring (
                                    list-ref mat row
                                  )
                                   col (
                                    + col 1
                                  )
                                )
                              )
                               (
                                (
                                  hash-table? (
                                    list-ref mat row
                                  )
                                )
                                 (
                                  hash-table-ref (
                                    list-ref mat row
                                  )
                                   col
                                )
                              )
                               (
                                else (
                                  list-ref (
                                    list-ref mat row
                                  )
                                   col
                                )
                              )
                            )
                             1
                          )
                           (
                            begin (
                              let (
                                (
                                  sub (
                                    _add 1 (
                                      apply min (
                                        _list right diagonal down
                                      )
                                    )
                                  )
                                )
                              )
                               (
                                begin (
                                  if (
                                    _gt sub (
                                      list-ref largest_square_area 0
                                    )
                                  )
                                   (
                                    begin (
                                      list-set! largest_square_area 0 sub
                                    )
                                  )
                                   (
                                    quote (
                                      
                                    )
                                  )
                                )
                                 (
                                  list-set! (
                                    list-ref dp_array row
                                  )
                                   col sub
                                )
                                 (
                                  ret3 sub
                                )
                              )
                            )
                          )
                           (
                            begin (
                              list-set! (
                                list-ref dp_array row
                              )
                               col 0
                            )
                             (
                              ret3 0
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
        largest_square_area_in_matrix_top_down_with_dp rows cols mat
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            let (
              (
                largest (
                  _list 0
                )
              )
            )
             (
              begin (
                let (
                  (
                    dp_array (
                      _list
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
                                      < r rows
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
                                                c 0
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
                                                              < c cols
                                                            )
                                                             (
                                                              begin (
                                                                set! row_list (
                                                                  append row_list (
                                                                    _list (
                                                                      - 1
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                set! c (
                                                                  + c 1
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
                                                set! dp_array (
                                                  append dp_array (
                                                    _list row_list
                                                  )
                                                )
                                              )
                                               (
                                                set! r (
                                                  + r 1
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
                        update_area_of_max_square_with_dp 0 0 rows cols mat dp_array largest
                      )
                       (
                        ret4 (
                          list-ref largest 0
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
        largest_square_area_in_matrix_bottom_up rows cols mat
      )
       (
        call/cc (
          lambda (
            ret9
          )
           (
            let (
              (
                dp_array (
                  _list
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
                                  <= r rows
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
                                            c 0
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
                                                          <= c cols
                                                        )
                                                         (
                                                          begin (
                                                            set! row_list (
                                                              append row_list (
                                                                _list 0
                                                              )
                                                            )
                                                          )
                                                           (
                                                            set! c (
                                                              + c 1
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
                                            set! dp_array (
                                              append dp_array (
                                                _list row_list
                                              )
                                            )
                                          )
                                           (
                                            set! r (
                                              + r 1
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
                    let (
                      (
                        largest 0
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            row (
                              - rows 1
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
                                        
                                      )
                                       (
                                        if (
                                          >= row 0
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                col (
                                                  - cols 1
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                call/cc (
                                                  lambda (
                                                    break17
                                                  )
                                                   (
                                                    letrec (
                                                      (
                                                        loop16 (
                                                          lambda (
                                                            
                                                          )
                                                           (
                                                            if (
                                                              >= col 0
                                                            )
                                                             (
                                                              begin (
                                                                let (
                                                                  (
                                                                    right (
                                                                      cond (
                                                                        (
                                                                          string? (
                                                                            list-ref dp_array row
                                                                          )
                                                                        )
                                                                         (
                                                                          _substring (
                                                                            list-ref dp_array row
                                                                          )
                                                                           (
                                                                            + col 1
                                                                          )
                                                                           (
                                                                            + (
                                                                              + col 1
                                                                            )
                                                                             1
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        (
                                                                          hash-table? (
                                                                            list-ref dp_array row
                                                                          )
                                                                        )
                                                                         (
                                                                          hash-table-ref (
                                                                            list-ref dp_array row
                                                                          )
                                                                           (
                                                                            + col 1
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        else (
                                                                          list-ref (
                                                                            list-ref dp_array row
                                                                          )
                                                                           (
                                                                            + col 1
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
                                                                        diagonal (
                                                                          cond (
                                                                            (
                                                                              string? (
                                                                                list-ref dp_array (
                                                                                  + row 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              _substring (
                                                                                list-ref dp_array (
                                                                                  + row 1
                                                                                )
                                                                              )
                                                                               (
                                                                                + col 1
                                                                              )
                                                                               (
                                                                                + (
                                                                                  + col 1
                                                                                )
                                                                                 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            (
                                                                              hash-table? (
                                                                                list-ref dp_array (
                                                                                  + row 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              hash-table-ref (
                                                                                list-ref dp_array (
                                                                                  + row 1
                                                                                )
                                                                              )
                                                                               (
                                                                                + col 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref (
                                                                                list-ref dp_array (
                                                                                  + row 1
                                                                                )
                                                                              )
                                                                               (
                                                                                + col 1
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
                                                                            bottom (
                                                                              cond (
                                                                                (
                                                                                  string? (
                                                                                    list-ref dp_array (
                                                                                      + row 1
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  _substring (
                                                                                    list-ref dp_array (
                                                                                      + row 1
                                                                                    )
                                                                                  )
                                                                                   col (
                                                                                    + col 1
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                (
                                                                                  hash-table? (
                                                                                    list-ref dp_array (
                                                                                      + row 1
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  hash-table-ref (
                                                                                    list-ref dp_array (
                                                                                      + row 1
                                                                                    )
                                                                                  )
                                                                                   col
                                                                                )
                                                                              )
                                                                               (
                                                                                else (
                                                                                  list-ref (
                                                                                    list-ref dp_array (
                                                                                      + row 1
                                                                                    )
                                                                                  )
                                                                                   col
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            if (
                                                                              equal? (
                                                                                cond (
                                                                                  (
                                                                                    string? (
                                                                                      list-ref mat row
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    _substring (
                                                                                      list-ref mat row
                                                                                    )
                                                                                     col (
                                                                                      + col 1
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  (
                                                                                    hash-table? (
                                                                                      list-ref mat row
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    hash-table-ref (
                                                                                      list-ref mat row
                                                                                    )
                                                                                     col
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  else (
                                                                                    list-ref (
                                                                                      list-ref mat row
                                                                                    )
                                                                                     col
                                                                                  )
                                                                                )
                                                                              )
                                                                               1
                                                                            )
                                                                             (
                                                                              begin (
                                                                                let (
                                                                                  (
                                                                                    value (
                                                                                      + 1 (
                                                                                        apply min (
                                                                                          _list right diagonal bottom
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    list-set! (
                                                                                      list-ref dp_array row
                                                                                    )
                                                                                     col value
                                                                                  )
                                                                                   (
                                                                                    if (
                                                                                      > value largest
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        set! largest value
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
                                                                              begin (
                                                                                list-set! (
                                                                                  list-ref dp_array row
                                                                                )
                                                                                 col 0
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            set! col (
                                                                              - col 1
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                loop16
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
                                                      loop16
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                set! row (
                                                  - row 1
                                                )
                                              )
                                            )
                                          )
                                           (
                                            loop14
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
                                  loop14
                                )
                              )
                            )
                          )
                           (
                            ret9 largest
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
        largest_square_area_in_matrix_bottom_up_space_optimization rows cols mat
      )
       (
        call/cc (
          lambda (
            ret18
          )
           (
            let (
              (
                current_row (
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
                        break20
                      )
                       (
                        letrec (
                          (
                            loop19 (
                              lambda (
                                
                              )
                               (
                                if (
                                  <= i cols
                                )
                                 (
                                  begin (
                                    set! current_row (
                                      append current_row (
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
                                    loop19
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
                          loop19
                        )
                      )
                    )
                  )
                   (
                    let (
                      (
                        next_row (
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
                                          <= j cols
                                        )
                                         (
                                          begin (
                                            set! next_row (
                                              append next_row (
                                                _list 0
                                              )
                                            )
                                          )
                                           (
                                            set! j (
                                              + j 1
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
                           (
                            let (
                              (
                                largest 0
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    row (
                                      - rows 1
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    call/cc (
                                      lambda (
                                        break24
                                      )
                                       (
                                        letrec (
                                          (
                                            loop23 (
                                              lambda (
                                                
                                              )
                                               (
                                                if (
                                                  >= row 0
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        col (
                                                          - cols 1
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
                                                                    
                                                                  )
                                                                   (
                                                                    if (
                                                                      >= col 0
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            right (
                                                                              list-ref current_row (
                                                                                + col 1
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                diagonal (
                                                                                  list-ref next_row (
                                                                                    + col 1
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                let (
                                                                                  (
                                                                                    bottom (
                                                                                      list-ref next_row col
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    if (
                                                                                      equal? (
                                                                                        cond (
                                                                                          (
                                                                                            string? (
                                                                                              list-ref mat row
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            _substring (
                                                                                              list-ref mat row
                                                                                            )
                                                                                             col (
                                                                                              + col 1
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          (
                                                                                            hash-table? (
                                                                                              list-ref mat row
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            hash-table-ref (
                                                                                              list-ref mat row
                                                                                            )
                                                                                             col
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          else (
                                                                                            list-ref (
                                                                                              list-ref mat row
                                                                                            )
                                                                                             col
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       1
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        let (
                                                                                          (
                                                                                            value (
                                                                                              + 1 (
                                                                                                apply min (
                                                                                                  _list right diagonal bottom
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            list-set! current_row col value
                                                                                          )
                                                                                           (
                                                                                            if (
                                                                                              > value largest
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                set! largest value
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
                                                                                      begin (
                                                                                        list-set! current_row col 0
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    set! col (
                                                                                      - col 1
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        loop25
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
                                                              loop25
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        set! next_row current_row
                                                      )
                                                       (
                                                        set! current_row (
                                                          _list
                                                        )
                                                      )
                                                       (
                                                        let (
                                                          (
                                                            t 0
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            call/cc (
                                                              lambda (
                                                                break28
                                                              )
                                                               (
                                                                letrec (
                                                                  (
                                                                    loop27 (
                                                                      lambda (
                                                                        
                                                                      )
                                                                       (
                                                                        if (
                                                                          <= t cols
                                                                        )
                                                                         (
                                                                          begin (
                                                                            set! current_row (
                                                                              append current_row (
                                                                                _list 0
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            set! t (
                                                                              + t 1
                                                                            )
                                                                          )
                                                                           (
                                                                            loop27
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
                                                                  loop27
                                                                )
                                                              )
                                                            )
                                                          )
                                                           (
                                                            set! row (
                                                              - row 1
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    loop23
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
                                          loop23
                                        )
                                      )
                                    )
                                  )
                                   (
                                    ret18 largest
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
          sample (
            _list (
              _list 1 1
            )
             (
              _list 1 1
            )
          )
        )
      )
       (
        begin (
          _display (
            if (
              string? (
                largest_square_area_in_matrix_top_down 2 2 sample
              )
            )
             (
              largest_square_area_in_matrix_top_down 2 2 sample
            )
             (
              to-str (
                largest_square_area_in_matrix_top_down 2 2 sample
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
                largest_square_area_in_matrix_top_down_with_dp 2 2 sample
              )
            )
             (
              largest_square_area_in_matrix_top_down_with_dp 2 2 sample
            )
             (
              to-str (
                largest_square_area_in_matrix_top_down_with_dp 2 2 sample
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
                largest_square_area_in_matrix_bottom_up 2 2 sample
              )
            )
             (
              largest_square_area_in_matrix_bottom_up 2 2 sample
            )
             (
              to-str (
                largest_square_area_in_matrix_bottom_up 2 2 sample
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
                largest_square_area_in_matrix_bottom_up_space_optimization 2 2 sample
              )
            )
             (
              largest_square_area_in_matrix_bottom_up_space_optimization 2 2 sample
            )
             (
              to-str (
                largest_square_area_in_matrix_bottom_up_space_optimization 2 2 sample
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
          end30 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur31 (
              quotient (
                * (
                  - end30 start29
                )
                 1000000
              )
               jps32
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur31
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
