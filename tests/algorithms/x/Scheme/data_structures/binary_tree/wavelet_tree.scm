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
      start17 (
        current-jiffy
      )
    )
     (
      jps20 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      let (
        (
          nodes (
            _list
          )
        )
      )
       (
        begin (
          define (
            make_list length value
          )
           (
            call/cc (
              lambda (
                ret1
              )
               (
                let (
                  (
                    lst (
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
                                      < i length
                                    )
                                     (
                                      begin (
                                        set! lst (
                                          append lst (
                                            _list value
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
                        ret1 lst
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
            min_list arr
          )
           (
            call/cc (
              lambda (
                ret4
              )
               (
                let (
                  (
                    m (
                      list-ref arr 0
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        i 1
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
                                      < i (
                                        _len arr
                                      )
                                    )
                                     (
                                      begin (
                                        if (
                                          < (
                                            list-ref arr i
                                          )
                                           m
                                        )
                                         (
                                          begin (
                                            set! m (
                                              list-ref arr i
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
                        ret4 m
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
            max_list arr
          )
           (
            call/cc (
              lambda (
                ret7
              )
               (
                let (
                  (
                    m (
                      list-ref arr 0
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        i 1
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
                                        _len arr
                                      )
                                    )
                                     (
                                      begin (
                                        if (
                                          > (
                                            list-ref arr i
                                          )
                                           m
                                        )
                                         (
                                          begin (
                                            set! m (
                                              list-ref arr i
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
                        ret7 m
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
            build_tree arr
          )
           (
            call/cc (
              lambda (
                ret10
              )
               (
                let (
                  (
                    n (
                      alist->hash-table (
                        _list (
                          cons "minn" (
                            min_list arr
                          )
                        )
                         (
                          cons "maxx" (
                            max_list arr
                          )
                        )
                         (
                          cons "map_left" (
                            make_list (
                              _len arr
                            )
                             0
                          )
                        )
                         (
                          cons "left" (
                            - 1
                          )
                        )
                         (
                          cons "right" (
                            - 1
                          )
                        )
                      )
                    )
                  )
                )
                 (
                  begin (
                    if (
                      equal? (
                        hash-table-ref n "minn"
                      )
                       (
                        hash-table-ref n "maxx"
                      )
                    )
                     (
                      begin (
                        set! nodes (
                          append nodes (
                            _list n
                          )
                        )
                      )
                       (
                        ret10 (
                          - (
                            _len nodes
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
                        pivot (
                          _div (
                            + (
                              hash-table-ref n "minn"
                            )
                             (
                              hash-table-ref n "maxx"
                            )
                          )
                           2
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            left_arr (
                              _list
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                right_arr (
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
                                        break12
                                      )
                                       (
                                        letrec (
                                          (
                                            loop11 (
                                              lambda (
                                                
                                              )
                                               (
                                                if (
                                                  < i (
                                                    _len arr
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        num (
                                                          list-ref arr i
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        if (
                                                          <= num pivot
                                                        )
                                                         (
                                                          begin (
                                                            set! left_arr (
                                                              append left_arr (
                                                                _list num
                                                              )
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            set! right_arr (
                                                              append right_arr (
                                                                _list num
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        let (
                                                          (
                                                            ml (
                                                              hash-table-ref n "map_left"
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            list-set! ml i (
                                                              _len left_arr
                                                            )
                                                          )
                                                           (
                                                            hash-table-set! n "map_left" ml
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
                                                    loop11
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
                                          loop11
                                        )
                                      )
                                    )
                                  )
                                   (
                                    if (
                                      > (
                                        _len left_arr
                                      )
                                       0
                                    )
                                     (
                                      begin (
                                        hash-table-set! n "left" (
                                          build_tree left_arr
                                        )
                                      )
                                    )
                                     (
                                      quote (
                                        
                                      )
                                    )
                                  )
                                   (
                                    if (
                                      > (
                                        _len right_arr
                                      )
                                       0
                                    )
                                     (
                                      begin (
                                        hash-table-set! n "right" (
                                          build_tree right_arr
                                        )
                                      )
                                    )
                                     (
                                      quote (
                                        
                                      )
                                    )
                                  )
                                   (
                                    set! nodes (
                                      append nodes (
                                        _list n
                                      )
                                    )
                                  )
                                   (
                                    ret10 (
                                      - (
                                        _len nodes
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
         (
          define (
            rank_till_index node_idx num index
          )
           (
            call/cc (
              lambda (
                ret13
              )
               (
                begin (
                  if (
                    or (
                      < index 0
                    )
                     (
                      < node_idx 0
                    )
                  )
                   (
                    begin (
                      ret13 0
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
                      node (
                        list-ref nodes node_idx
                      )
                    )
                  )
                   (
                    begin (
                      if (
                        equal? (
                          hash-table-ref node "minn"
                        )
                         (
                          hash-table-ref node "maxx"
                        )
                      )
                       (
                        begin (
                          if (
                            equal? (
                              hash-table-ref node "minn"
                            )
                             num
                          )
                           (
                            begin (
                              ret13 (
                                + index 1
                              )
                            )
                          )
                           (
                            begin (
                              ret13 0
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
                          pivot (
                            _div (
                              + (
                                hash-table-ref node "minn"
                              )
                               (
                                hash-table-ref node "maxx"
                              )
                            )
                             2
                          )
                        )
                      )
                       (
                        begin (
                          if (
                            <= num pivot
                          )
                           (
                            begin (
                              ret13 (
                                rank_till_index (
                                  hash-table-ref node "left"
                                )
                                 num (
                                  - (
                                    list-ref (
                                      hash-table-ref node "map_left"
                                    )
                                     index
                                  )
                                   1
                                )
                              )
                            )
                          )
                           (
                            begin (
                              ret13 (
                                rank_till_index (
                                  hash-table-ref node "right"
                                )
                                 num (
                                  - index (
                                    list-ref (
                                      hash-table-ref node "map_left"
                                    )
                                     index
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
            rank node_idx num start end
          )
           (
            call/cc (
              lambda (
                ret14
              )
               (
                begin (
                  if (
                    > start end
                  )
                   (
                    begin (
                      ret14 0
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
                      rank_till_end (
                        rank_till_index node_idx num end
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          rank_before_start (
                            rank_till_index node_idx num (
                              - start 1
                            )
                          )
                        )
                      )
                       (
                        begin (
                          ret14 (
                            - rank_till_end rank_before_start
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
            quantile node_idx index start end
          )
           (
            call/cc (
              lambda (
                ret15
              )
               (
                begin (
                  if (
                    or (
                      or (
                        > index (
                          - end start
                        )
                      )
                       (
                        > start end
                      )
                    )
                     (
                      < node_idx 0
                    )
                  )
                   (
                    begin (
                      ret15 (
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
                  let (
                    (
                      node (
                        list-ref nodes node_idx
                      )
                    )
                  )
                   (
                    begin (
                      if (
                        equal? (
                          hash-table-ref node "minn"
                        )
                         (
                          hash-table-ref node "maxx"
                        )
                      )
                       (
                        begin (
                          ret15 (
                            hash-table-ref node "minn"
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
                          left_start (
                            if (
                              equal? start 0
                            )
                             0 (
                              list-ref (
                                hash-table-ref node "map_left"
                              )
                               (
                                - start 1
                              )
                            )
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              num_left (
                                - (
                                  list-ref (
                                    hash-table-ref node "map_left"
                                  )
                                   end
                                )
                                 left_start
                              )
                            )
                          )
                           (
                            begin (
                              if (
                                > num_left index
                              )
                               (
                                begin (
                                  ret15 (
                                    quantile (
                                      hash-table-ref node "left"
                                    )
                                     index left_start (
                                      - (
                                        list-ref (
                                          hash-table-ref node "map_left"
                                        )
                                         end
                                      )
                                       1
                                    )
                                  )
                                )
                              )
                               (
                                begin (
                                  ret15 (
                                    quantile (
                                      hash-table-ref node "right"
                                    )
                                     (
                                      - index num_left
                                    )
                                     (
                                      - start left_start
                                    )
                                     (
                                      - end (
                                        list-ref (
                                          hash-table-ref node "map_left"
                                        )
                                         end
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
            range_counting node_idx start end start_num end_num
          )
           (
            call/cc (
              lambda (
                ret16
              )
               (
                begin (
                  if (
                    or (
                      or (
                        > start end
                      )
                       (
                        < node_idx 0
                      )
                    )
                     (
                      > start_num end_num
                    )
                  )
                   (
                    begin (
                      ret16 0
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
                      node (
                        list-ref nodes node_idx
                      )
                    )
                  )
                   (
                    begin (
                      if (
                        or (
                          > (
                            hash-table-ref node "minn"
                          )
                           end_num
                        )
                         (
                          < (
                            hash-table-ref node "maxx"
                          )
                           start_num
                        )
                      )
                       (
                        begin (
                          ret16 0
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
                          <= start_num (
                            hash-table-ref node "minn"
                          )
                        )
                         (
                          <= (
                            hash-table-ref node "maxx"
                          )
                           end_num
                        )
                      )
                       (
                        begin (
                          ret16 (
                            + (
                              - end start
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
                          left (
                            range_counting (
                              hash-table-ref node "left"
                            )
                             (
                              if (
                                equal? start 0
                              )
                               0 (
                                list-ref (
                                  hash-table-ref node "map_left"
                                )
                                 (
                                  - start 1
                                )
                              )
                            )
                             (
                              - (
                                list-ref (
                                  hash-table-ref node "map_left"
                                )
                                 end
                              )
                               1
                            )
                             start_num end_num
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              right (
                                range_counting (
                                  hash-table-ref node "right"
                                )
                                 (
                                  - start (
                                    if (
                                      equal? start 0
                                    )
                                     0 (
                                      list-ref (
                                        hash-table-ref node "map_left"
                                      )
                                       (
                                        - start 1
                                      )
                                    )
                                  )
                                )
                                 (
                                  - end (
                                    list-ref (
                                      hash-table-ref node "map_left"
                                    )
                                     end
                                  )
                                )
                                 start_num end_num
                              )
                            )
                          )
                           (
                            begin (
                              ret16 (
                                _add left right
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
              test_array (
                _list 2 1 4 5 6 0 8 9 1 2 0 6 4 2 0 6 5 3 2 7
              )
            )
          )
           (
            begin (
              let (
                (
                  root (
                    build_tree test_array
                  )
                )
              )
               (
                begin (
                  _display (
                    if (
                      string? (
                        string-append "rank_till_index 6 at 6 -> " (
                          to-str-space (
                            rank_till_index root 6 6
                          )
                        )
                      )
                    )
                     (
                      string-append "rank_till_index 6 at 6 -> " (
                        to-str-space (
                          rank_till_index root 6 6
                        )
                      )
                    )
                     (
                      to-str (
                        string-append "rank_till_index 6 at 6 -> " (
                          to-str-space (
                            rank_till_index root 6 6
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
                  _display (
                    if (
                      string? (
                        string-append "rank 6 in [3,13] -> " (
                          to-str-space (
                            rank root 6 3 13
                          )
                        )
                      )
                    )
                     (
                      string-append "rank 6 in [3,13] -> " (
                        to-str-space (
                          rank root 6 3 13
                        )
                      )
                    )
                     (
                      to-str (
                        string-append "rank 6 in [3,13] -> " (
                          to-str-space (
                            rank root 6 3 13
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
                  _display (
                    if (
                      string? (
                        string-append "quantile index 2 in [2,5] -> " (
                          to-str-space (
                            quantile root 2 2 5
                          )
                        )
                      )
                    )
                     (
                      string-append "quantile index 2 in [2,5] -> " (
                        to-str-space (
                          quantile root 2 2 5
                        )
                      )
                    )
                     (
                      to-str (
                        string-append "quantile index 2 in [2,5] -> " (
                          to-str-space (
                            quantile root 2 2 5
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
                  _display (
                    if (
                      string? (
                        string-append "range_counting [3,7] in [1,10] -> " (
                          to-str-space (
                            range_counting root 1 10 3 7
                          )
                        )
                      )
                    )
                     (
                      string-append "range_counting [3,7] in [1,10] -> " (
                        to-str-space (
                          range_counting root 1 10 3 7
                        )
                      )
                    )
                     (
                      to-str (
                        string-append "range_counting [3,7] in [1,10] -> " (
                          to-str-space (
                            range_counting root 1 10 3 7
                          )
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
      )
    )
     (
      let (
        (
          end18 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur19 (
              quotient (
                * (
                  - end18 start17
                )
                 1000000
              )
               jps20
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur19
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
