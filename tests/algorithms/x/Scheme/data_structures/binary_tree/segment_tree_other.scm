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
      define (
        combine a b op
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            begin (
              if (
                equal? op 0
              )
               (
                begin (
                  ret1 (
                    + a b
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
                equal? op 1
              )
               (
                begin (
                  if (
                    > a b
                  )
                   (
                    begin (
                      ret1 a
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  ret1 b
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                < a b
              )
               (
                begin (
                  ret1 a
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              ret1 b
            )
          )
        )
      )
    )
     (
      define (
        build_tree nodes arr start end op
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            begin (
              if (
                equal? start end
              )
               (
                begin (
                  let (
                    (
                      node (
                        alist->hash-table (
                          _list (
                            cons "start" start
                          )
                           (
                            cons "end" end
                          )
                           (
                            cons "val" (
                              list-ref arr start
                            )
                          )
                           (
                            cons "mid" start
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
                      let (
                        (
                          new_nodes (
                            append nodes (
                              _list node
                            )
                          )
                        )
                      )
                       (
                        begin (
                          ret2 (
                            alist->hash-table (
                              _list (
                                cons "nodes" new_nodes
                              )
                               (
                                cons "idx" (
                                  - (
                                    _len new_nodes
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
                      + start end
                    )
                     2
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      left_res (
                        build_tree nodes arr start mid op
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          right_res (
                            build_tree (
                              hash-table-ref left_res "nodes"
                            )
                             arr (
                              + mid 1
                            )
                             end op
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              left_node (
                                cond (
                                  (
                                    string? (
                                      hash-table-ref right_res "nodes"
                                    )
                                  )
                                   (
                                    _substring (
                                      hash-table-ref right_res "nodes"
                                    )
                                     (
                                      hash-table-ref left_res "idx"
                                    )
                                     (
                                      + (
                                        hash-table-ref left_res "idx"
                                      )
                                       1
                                    )
                                  )
                                )
                                 (
                                  (
                                    hash-table? (
                                      hash-table-ref right_res "nodes"
                                    )
                                  )
                                   (
                                    hash-table-ref (
                                      hash-table-ref right_res "nodes"
                                    )
                                     (
                                      hash-table-ref left_res "idx"
                                    )
                                  )
                                )
                                 (
                                  else (
                                    list-ref (
                                      hash-table-ref right_res "nodes"
                                    )
                                     (
                                      hash-table-ref left_res "idx"
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
                                  right_node (
                                    cond (
                                      (
                                        string? (
                                          hash-table-ref right_res "nodes"
                                        )
                                      )
                                       (
                                        _substring (
                                          hash-table-ref right_res "nodes"
                                        )
                                         (
                                          hash-table-ref right_res "idx"
                                        )
                                         (
                                          + (
                                            hash-table-ref right_res "idx"
                                          )
                                           1
                                        )
                                      )
                                    )
                                     (
                                      (
                                        hash-table? (
                                          hash-table-ref right_res "nodes"
                                        )
                                      )
                                       (
                                        hash-table-ref (
                                          hash-table-ref right_res "nodes"
                                        )
                                         (
                                          hash-table-ref right_res "idx"
                                        )
                                      )
                                    )
                                     (
                                      else (
                                        list-ref (
                                          hash-table-ref right_res "nodes"
                                        )
                                         (
                                          hash-table-ref right_res "idx"
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
                                      val (
                                        combine (
                                          hash-table-ref left_node "val"
                                        )
                                         (
                                          hash-table-ref right_node "val"
                                        )
                                         op
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      let (
                                        (
                                          parent (
                                            alist->hash-table (
                                              _list (
                                                cons "start" start
                                              )
                                               (
                                                cons "end" end
                                              )
                                               (
                                                cons "val" val
                                              )
                                               (
                                                cons "mid" mid
                                              )
                                               (
                                                cons "left" (
                                                  hash-table-ref left_res "idx"
                                                )
                                              )
                                               (
                                                cons "right" (
                                                  hash-table-ref right_res "idx"
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
                                              new_nodes (
                                                append (
                                                  hash-table-ref right_res "nodes"
                                                )
                                                 (
                                                  _list parent
                                                )
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              ret2 (
                                                alist->hash-table (
                                                  _list (
                                                    cons "nodes" new_nodes
                                                  )
                                                   (
                                                    cons "idx" (
                                                      - (
                                                        _len new_nodes
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
        new_segment_tree collection op
      )
       (
        call/cc (
          lambda (
            ret3
          )
           (
            ret3 (
              alist->hash-table (
                _list (
                  cons "arr" collection
                )
                 (
                  cons "op" op
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        update tree i val
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            let (
              (
                new_arr (
                  _list
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
                                  < idx (
                                    _len (
                                      hash-table-ref tree "arr"
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      equal? idx i
                                    )
                                     (
                                      begin (
                                        set! new_arr (
                                          append new_arr (
                                            _list val
                                          )
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        set! new_arr (
                                          append new_arr (
                                            _list (
                                              list-ref (
                                                hash-table-ref tree "arr"
                                              )
                                               idx
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                   (
                                    set! idx (
                                      + idx 1
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
                    ret4 (
                      alist->hash-table (
                        _list (
                          cons "arr" new_arr
                        )
                         (
                          cons "op" (
                            hash-table-ref tree "op"
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
        query_range tree i j
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            let (
              (
                result (
                  list-ref (
                    hash-table-ref tree "arr"
                  )
                   i
                )
              )
            )
             (
              begin (
                let (
                  (
                    idx (
                      + i 1
                    )
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
                                  <= idx j
                                )
                                 (
                                  begin (
                                    set! result (
                                      combine result (
                                        list-ref (
                                          hash-table-ref tree "arr"
                                        )
                                         idx
                                      )
                                       (
                                        hash-table-ref tree "op"
                                      )
                                    )
                                  )
                                   (
                                    set! idx (
                                      + idx 1
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
                    ret7 result
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
        traverse tree
      )
       (
        call/cc (
          lambda (
            ret10
          )
           (
            begin (
              if (
                equal? (
                  _len (
                    hash-table-ref tree "arr"
                  )
                )
                 0
              )
               (
                begin (
                  ret10 (
                    _list
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
                  res (
                    build_tree (
                      _list
                    )
                     (
                      hash-table-ref tree "arr"
                    )
                     0 (
                      - (
                        _len (
                          hash-table-ref tree "arr"
                        )
                      )
                       1
                    )
                     (
                      hash-table-ref tree "op"
                    )
                  )
                )
              )
               (
                begin (
                  ret10 (
                    hash-table-ref res "nodes"
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
        node_to_string node
      )
       (
        call/cc (
          lambda (
            ret11
          )
           (
            ret11 (
              string-append (
                string-append (
                  string-append (
                    string-append (
                      string-append (
                        string-append "SegmentTreeNode(start=" (
                          to-str-space (
                            hash-table-ref node "start"
                          )
                        )
                      )
                       ", end="
                    )
                     (
                      to-str-space (
                        hash-table-ref node "end"
                      )
                    )
                  )
                   ", val="
                )
                 (
                  to-str-space (
                    hash-table-ref node "val"
                  )
                )
              )
               ")"
            )
          )
        )
      )
    )
     (
      define (
        print_traverse tree
      )
       (
        call/cc (
          lambda (
            ret12
          )
           (
            let (
              (
                nodes (
                  traverse tree
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
                                    _len nodes
                                  )
                                )
                                 (
                                  begin (
                                    _display (
                                      if (
                                        string? (
                                          node_to_string (
                                            cond (
                                              (
                                                string? nodes
                                              )
                                               (
                                                _substring nodes i (
                                                  + i 1
                                                )
                                              )
                                            )
                                             (
                                              (
                                                hash-table? nodes
                                              )
                                               (
                                                hash-table-ref nodes i
                                              )
                                            )
                                             (
                                              else (
                                                list-ref nodes i
                                              )
                                            )
                                          )
                                        )
                                      )
                                       (
                                        node_to_string (
                                          cond (
                                            (
                                              string? nodes
                                            )
                                             (
                                              _substring nodes i (
                                                + i 1
                                              )
                                            )
                                          )
                                           (
                                            (
                                              hash-table? nodes
                                            )
                                             (
                                              hash-table-ref nodes i
                                            )
                                          )
                                           (
                                            else (
                                              list-ref nodes i
                                            )
                                          )
                                        )
                                      )
                                       (
                                        to-str (
                                          node_to_string (
                                            cond (
                                              (
                                                string? nodes
                                              )
                                               (
                                                _substring nodes i (
                                                  + i 1
                                                )
                                              )
                                            )
                                             (
                                              (
                                                hash-table? nodes
                                              )
                                               (
                                                hash-table-ref nodes i
                                              )
                                            )
                                             (
                                              else (
                                                list-ref nodes i
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
                                    set! i (
                                      + i 1
                                    )
                                  )
                                   (
                                    loop13
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
                          loop13
                        )
                      )
                    )
                  )
                   (
                    _display (
                      if (
                        string? ""
                      )
                       "" (
                        to-str ""
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
      let (
        (
          arr (
            _list 2 1 5 3 4
          )
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
                              op (
                                car xs
                              )
                            )
                          )
                           (
                            begin (
                              _display (
                                if (
                                  string? "**************************************************"
                                )
                                 "**************************************************" (
                                  to-str "**************************************************"
                                )
                              )
                            )
                             (
                              newline
                            )
                             (
                              let (
                                (
                                  tree (
                                    new_segment_tree arr op
                                  )
                                )
                              )
                               (
                                begin (
                                  print_traverse tree
                                )
                                 (
                                  set! tree (
                                    update tree 1 5
                                  )
                                )
                                 (
                                  print_traverse tree
                                )
                                 (
                                  _display (
                                    if (
                                      string? (
                                        query_range tree 3 4
                                      )
                                    )
                                     (
                                      query_range tree 3 4
                                    )
                                     (
                                      to-str (
                                        query_range tree 3 4
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
                                        query_range tree 2 2
                                      )
                                    )
                                     (
                                      query_range tree 2 2
                                    )
                                     (
                                      to-str (
                                        query_range tree 2 2
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
                                        query_range tree 1 3
                                      )
                                    )
                                     (
                                      query_range tree 1 3
                                    )
                                     (
                                      to-str (
                                        query_range tree 1 3
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
                                      string? ""
                                    )
                                     "" (
                                      to-str ""
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
                          loop15 (
                            cdr xs
                          )
                        )
                      )
                    )
                  )
                )
              )
               (
                loop15 (
                  _list 0 1 2
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
