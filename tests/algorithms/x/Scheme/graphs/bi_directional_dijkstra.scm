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
      start13 (
        current-jiffy
      )
    )
     (
      jps16 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        get_min_index q
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                idx 0
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
                                  < i (
                                    _len q
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      < (
                                        hash-table-ref (
                                          list-ref q i
                                        )
                                         "cost"
                                      )
                                       (
                                        hash-table-ref (
                                          list-ref q idx
                                        )
                                         "cost"
                                      )
                                    )
                                     (
                                      begin (
                                        set! idx i
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
                    ret1 idx
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
        remove_at q idx
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            let (
              (
                res (
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
                                    _len q
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      not (
                                        equal? i idx
                                      )
                                    )
                                     (
                                      begin (
                                        set! res (
                                          append res (
                                            _list (
                                              list-ref q i
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
                    ret4 res
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
        pass_and_relaxation graph v visited_forward visited_backward cst_fwd cst_bwd queue parent shortest_distance
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            let (
              (
                q queue
              )
            )
             (
              begin (
                let (
                  (
                    sd shortest_distance
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
                                            nxt (
                                              hash-table-ref e "to"
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                d (
                                                  hash-table-ref e "cost"
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                if (
                                                  cond (
                                                    (
                                                      string? visited_forward
                                                    )
                                                     (
                                                      if (
                                                        string-contains visited_forward nxt
                                                      )
                                                       #t #f
                                                    )
                                                  )
                                                   (
                                                    (
                                                      hash-table? visited_forward
                                                    )
                                                     (
                                                      if (
                                                        hash-table-exists? visited_forward nxt
                                                      )
                                                       #t #f
                                                    )
                                                  )
                                                   (
                                                    else (
                                                      if (
                                                        member nxt visited_forward
                                                      )
                                                       #t #f
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    loop8 (
                                                      cdr xs
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
                                                    old_cost (
                                                      if (
                                                        cond (
                                                          (
                                                            string? cst_fwd
                                                          )
                                                           (
                                                            if (
                                                              string-contains cst_fwd nxt
                                                            )
                                                             #t #f
                                                          )
                                                        )
                                                         (
                                                          (
                                                            hash-table? cst_fwd
                                                          )
                                                           (
                                                            if (
                                                              hash-table-exists? cst_fwd nxt
                                                            )
                                                             #t #f
                                                          )
                                                        )
                                                         (
                                                          else (
                                                            if (
                                                              member nxt cst_fwd
                                                            )
                                                             #t #f
                                                          )
                                                        )
                                                      )
                                                       (
                                                        hash-table-ref/default cst_fwd nxt (
                                                          quote (
                                                            
                                                          )
                                                        )
                                                      )
                                                       2147483647
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        new_cost (
                                                          _add (
                                                            hash-table-ref/default cst_fwd v (
                                                              quote (
                                                                
                                                              )
                                                            )
                                                          )
                                                           d
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        if (
                                                          _lt new_cost old_cost
                                                        )
                                                         (
                                                          begin (
                                                            set! q (
                                                              append q (
                                                                _list (
                                                                  alist->hash-table (
                                                                    _list (
                                                                      cons "node" nxt
                                                                    )
                                                                     (
                                                                      cons "cost" new_cost
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                           (
                                                            hash-table-set! cst_fwd nxt new_cost
                                                          )
                                                           (
                                                            hash-table-set! parent nxt v
                                                          )
                                                        )
                                                         (
                                                          quote (
                                                            
                                                          )
                                                        )
                                                      )
                                                       (
                                                        if (
                                                          cond (
                                                            (
                                                              string? visited_backward
                                                            )
                                                             (
                                                              if (
                                                                string-contains visited_backward nxt
                                                              )
                                                               #t #f
                                                            )
                                                          )
                                                           (
                                                            (
                                                              hash-table? visited_backward
                                                            )
                                                             (
                                                              if (
                                                                hash-table-exists? visited_backward nxt
                                                              )
                                                               #t #f
                                                            )
                                                          )
                                                           (
                                                            else (
                                                              if (
                                                                member nxt visited_backward
                                                              )
                                                               #t #f
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                alt (
                                                                  _add (
                                                                    _add (
                                                                      hash-table-ref/default cst_fwd v (
                                                                        quote (
                                                                          
                                                                        )
                                                                      )
                                                                    )
                                                                     d
                                                                  )
                                                                   (
                                                                    hash-table-ref/default cst_bwd nxt (
                                                                      quote (
                                                                        
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                if (
                                                                  _lt alt sd
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! sd alt
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
                                  )
                                   (
                                    loop8 (
                                      cdr xs
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                         (
                          loop8 (
                            hash-table-ref/default graph v (
                              quote (
                                
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                   (
                    ret7 (
                      alist->hash-table (
                        _list (
                          cons "queue" q
                        )
                         (
                          cons "dist" sd
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
        bidirectional_dij source destination graph_forward graph_backward
      )
       (
        call/cc (
          lambda (
            ret10
          )
           (
            let (
              (
                shortest_path_distance (
                  - 1
                )
              )
            )
             (
              begin (
                let (
                  (
                    visited_forward (
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
                        visited_backward (
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
                            cst_fwd (
                              alist->hash-table (
                                _list
                              )
                            )
                          )
                        )
                         (
                          begin (
                            hash-table-set! cst_fwd source 0
                          )
                           (
                            let (
                              (
                                cst_bwd (
                                  alist->hash-table (
                                    _list
                                  )
                                )
                              )
                            )
                             (
                              begin (
                                hash-table-set! cst_bwd destination 0
                              )
                               (
                                let (
                                  (
                                    parent_forward (
                                      alist->hash-table (
                                        _list
                                      )
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    hash-table-set! parent_forward source ""
                                  )
                                   (
                                    let (
                                      (
                                        parent_backward (
                                          alist->hash-table (
                                            _list
                                          )
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        hash-table-set! parent_backward destination ""
                                      )
                                       (
                                        let (
                                          (
                                            queue_forward (
                                              _list
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            set! queue_forward (
                                              append queue_forward (
                                                _list (
                                                  alist->hash-table (
                                                    _list (
                                                      cons "node" source
                                                    )
                                                     (
                                                      cons "cost" 0
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                           (
                                            let (
                                              (
                                                queue_backward (
                                                  _list
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                set! queue_backward (
                                                  append queue_backward (
                                                    _list (
                                                      alist->hash-table (
                                                        _list (
                                                          cons "node" destination
                                                        )
                                                         (
                                                          cons "cost" 0
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                let (
                                                  (
                                                    shortest_distance 2147483647
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    if (
                                                      string=? source destination
                                                    )
                                                     (
                                                      begin (
                                                        ret10 0
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
                                                                  and (
                                                                    > (
                                                                      _len queue_forward
                                                                    )
                                                                     0
                                                                  )
                                                                   (
                                                                    > (
                                                                      _len queue_backward
                                                                    )
                                                                     0
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        idx_f (
                                                                          get_min_index queue_forward
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            item_f (
                                                                              list-ref queue_forward idx_f
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            set! queue_forward (
                                                                              remove_at queue_forward idx_f
                                                                            )
                                                                          )
                                                                           (
                                                                            let (
                                                                              (
                                                                                v_fwd (
                                                                                  hash-table-ref item_f "node"
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                hash-table-set! visited_forward v_fwd #t
                                                                              )
                                                                               (
                                                                                let (
                                                                                  (
                                                                                    idx_b (
                                                                                      get_min_index queue_backward
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    let (
                                                                                      (
                                                                                        item_b (
                                                                                          list-ref queue_backward idx_b
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        set! queue_backward (
                                                                                          remove_at queue_backward idx_b
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        let (
                                                                                          (
                                                                                            v_bwd (
                                                                                              hash-table-ref item_b "node"
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            hash-table-set! visited_backward v_bwd #t
                                                                                          )
                                                                                           (
                                                                                            let (
                                                                                              (
                                                                                                res_f (
                                                                                                  pass_and_relaxation graph_forward v_fwd visited_forward visited_backward cst_fwd cst_bwd queue_forward parent_forward shortest_distance
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                set! queue_forward (
                                                                                                  hash-table-ref res_f "queue"
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                set! shortest_distance (
                                                                                                  hash-table-ref res_f "dist"
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                let (
                                                                                                  (
                                                                                                    res_b (
                                                                                                      pass_and_relaxation graph_backward v_bwd visited_backward visited_forward cst_bwd cst_fwd queue_backward parent_backward shortest_distance
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  begin (
                                                                                                    set! queue_backward (
                                                                                                      hash-table-ref res_b "queue"
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    set! shortest_distance (
                                                                                                      hash-table-ref res_b "dist"
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    if (
                                                                                                      >= (
                                                                                                        + (
                                                                                                          hash-table-ref/default cst_fwd v_fwd (
                                                                                                            quote (
                                                                                                              
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref/default cst_bwd v_bwd (
                                                                                                            quote (
                                                                                                              
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       shortest_distance
                                                                                                    )
                                                                                                     (
                                                                                                      begin (
                                                                                                        break12 (
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
                                                      not (
                                                        equal? shortest_distance 2147483647
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        set! shortest_path_distance shortest_distance
                                                      )
                                                    )
                                                     (
                                                      quote (
                                                        
                                                      )
                                                    )
                                                  )
                                                   (
                                                    ret10 shortest_path_distance
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
      let (
        (
          graph_fwd (
            alist->hash-table (
              _list (
                cons "B" (
                  _list (
                    alist->hash-table (
                      _list (
                        cons "to" "C"
                      )
                       (
                        cons "cost" 1
                      )
                    )
                  )
                )
              )
               (
                cons "C" (
                  _list (
                    alist->hash-table (
                      _list (
                        cons "to" "D"
                      )
                       (
                        cons "cost" 1
                      )
                    )
                  )
                )
              )
               (
                cons "D" (
                  _list (
                    alist->hash-table (
                      _list (
                        cons "to" "F"
                      )
                       (
                        cons "cost" 1
                      )
                    )
                  )
                )
              )
               (
                cons "E" (
                  _list (
                    alist->hash-table (
                      _list (
                        cons "to" "B"
                      )
                       (
                        cons "cost" 1
                      )
                    )
                  )
                   (
                    alist->hash-table (
                      _list (
                        cons "to" "G"
                      )
                       (
                        cons "cost" 2
                      )
                    )
                  )
                )
              )
               (
                cons "F" (
                  _list
                )
              )
               (
                cons "G" (
                  _list (
                    alist->hash-table (
                      _list (
                        cons "to" "F"
                      )
                       (
                        cons "cost" 1
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
        begin (
          let (
            (
              graph_bwd (
                alist->hash-table (
                  _list (
                    cons "B" (
                      _list (
                        alist->hash-table (
                          _list (
                            cons "to" "E"
                          )
                           (
                            cons "cost" 1
                          )
                        )
                      )
                    )
                  )
                   (
                    cons "C" (
                      _list (
                        alist->hash-table (
                          _list (
                            cons "to" "B"
                          )
                           (
                            cons "cost" 1
                          )
                        )
                      )
                    )
                  )
                   (
                    cons "D" (
                      _list (
                        alist->hash-table (
                          _list (
                            cons "to" "C"
                          )
                           (
                            cons "cost" 1
                          )
                        )
                      )
                    )
                  )
                   (
                    cons "F" (
                      _list (
                        alist->hash-table (
                          _list (
                            cons "to" "D"
                          )
                           (
                            cons "cost" 1
                          )
                        )
                      )
                       (
                        alist->hash-table (
                          _list (
                            cons "to" "G"
                          )
                           (
                            cons "cost" 1
                          )
                        )
                      )
                    )
                  )
                   (
                    cons "E" (
                      _list
                    )
                  )
                   (
                    cons "G" (
                      _list (
                        alist->hash-table (
                          _list (
                            cons "to" "E"
                          )
                           (
                            cons "cost" 2
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
            begin (
              _display (
                if (
                  string? (
                    to-str-space (
                      bidirectional_dij "E" "F" graph_fwd graph_bwd
                    )
                  )
                )
                 (
                  to-str-space (
                    bidirectional_dij "E" "F" graph_fwd graph_bwd
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      bidirectional_dij "E" "F" graph_fwd graph_bwd
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
      let (
        (
          end14 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur15 (
              quotient (
                * (
                  - end14 start13
                )
                 1000000
              )
               jps16
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur15
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
