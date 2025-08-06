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
        empty_list
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            ret1 (
              alist->hash-table (
                _list (
                  cons "nodes" (
                    _list
                  )
                )
                 (
                  cons "head_idx" (
                    - 1
                  )
                )
                 (
                  cons "tail_idx" (
                    - 1
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
        get_head_data ll
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            begin (
              if (
                equal? (
                  hash-table-ref ll "head_idx"
                )
                 (
                  - 1
                )
              )
               (
                begin (
                  ret2 (
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
                    list-ref (
                      hash-table-ref ll "nodes"
                    )
                     (
                      hash-table-ref ll "head_idx"
                    )
                  )
                )
              )
               (
                begin (
                  ret2 (
                    hash-table-ref node "data"
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
        get_tail_data ll
      )
       (
        call/cc (
          lambda (
            ret3
          )
           (
            begin (
              if (
                equal? (
                  hash-table-ref ll "tail_idx"
                )
                 (
                  - 1
                )
              )
               (
                begin (
                  ret3 (
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
                    list-ref (
                      hash-table-ref ll "nodes"
                    )
                     (
                      hash-table-ref ll "tail_idx"
                    )
                  )
                )
              )
               (
                begin (
                  ret3 (
                    hash-table-ref node "data"
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
        insert_before_node ll idx new_idx
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            let (
              (
                nodes (
                  hash-table-ref ll "nodes"
                )
              )
            )
             (
              begin (
                let (
                  (
                    new_node (
                      list-ref nodes new_idx
                    )
                  )
                )
                 (
                  begin (
                    hash-table-set! new_node "next_index" idx
                  )
                   (
                    let (
                      (
                        node (
                          list-ref nodes idx
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            p (
                              hash-table-ref node "prev_index"
                            )
                          )
                        )
                         (
                          begin (
                            hash-table-set! new_node "prev_index" p
                          )
                           (
                            list-set! nodes new_idx new_node
                          )
                           (
                            if (
                              equal? p (
                                - 1
                              )
                            )
                             (
                              begin (
                                hash-table-set! ll "head_idx" new_idx
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    prev_node (
                                      list-ref nodes p
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    hash-table-set! prev_node "next_index" new_idx
                                  )
                                   (
                                    list-set! nodes p prev_node
                                  )
                                )
                              )
                            )
                          )
                           (
                            hash-table-set! node "prev_index" new_idx
                          )
                           (
                            list-set! nodes idx node
                          )
                           (
                            hash-table-set! ll "nodes" nodes
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
        insert_after_node ll idx new_idx
      )
       (
        call/cc (
          lambda (
            ret5
          )
           (
            let (
              (
                nodes (
                  hash-table-ref ll "nodes"
                )
              )
            )
             (
              begin (
                let (
                  (
                    new_node (
                      list-ref nodes new_idx
                    )
                  )
                )
                 (
                  begin (
                    hash-table-set! new_node "prev_index" idx
                  )
                   (
                    let (
                      (
                        node (
                          list-ref nodes idx
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            nxt (
                              hash-table-ref node "next_index"
                            )
                          )
                        )
                         (
                          begin (
                            hash-table-set! new_node "next_index" nxt
                          )
                           (
                            list-set! nodes new_idx new_node
                          )
                           (
                            if (
                              equal? nxt (
                                - 1
                              )
                            )
                             (
                              begin (
                                hash-table-set! ll "tail_idx" new_idx
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    next_node (
                                      list-ref nodes nxt
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    hash-table-set! next_node "prev_index" new_idx
                                  )
                                   (
                                    list-set! nodes nxt next_node
                                  )
                                )
                              )
                            )
                          )
                           (
                            hash-table-set! node "next_index" new_idx
                          )
                           (
                            list-set! nodes idx node
                          )
                           (
                            hash-table-set! ll "nodes" nodes
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
        set_head ll idx
      )
       (
        call/cc (
          lambda (
            ret6
          )
           (
            if (
              equal? (
                hash-table-ref ll "head_idx"
              )
               (
                - 1
              )
            )
             (
              begin (
                hash-table-set! ll "head_idx" idx
              )
               (
                hash-table-set! ll "tail_idx" idx
              )
            )
             (
              begin (
                insert_before_node ll (
                  hash-table-ref ll "head_idx"
                )
                 idx
              )
            )
          )
        )
      )
    )
     (
      define (
        set_tail ll idx
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            if (
              equal? (
                hash-table-ref ll "tail_idx"
              )
               (
                - 1
              )
            )
             (
              begin (
                hash-table-set! ll "head_idx" idx
              )
               (
                hash-table-set! ll "tail_idx" idx
              )
            )
             (
              begin (
                insert_after_node ll (
                  hash-table-ref ll "tail_idx"
                )
                 idx
              )
            )
          )
        )
      )
    )
     (
      define (
        insert ll value
      )
       (
        call/cc (
          lambda (
            ret8
          )
           (
            let (
              (
                nodes (
                  hash-table-ref ll "nodes"
                )
              )
            )
             (
              begin (
                set! nodes (
                  append nodes (
                    _list (
                      alist->hash-table (
                        _list (
                          cons "data" value
                        )
                         (
                          cons "prev_index" (
                            - 1
                          )
                        )
                         (
                          cons "next_index" (
                            - 1
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
                    idx (
                      - (
                        _len nodes
                      )
                       1
                    )
                  )
                )
                 (
                  begin (
                    hash-table-set! ll "nodes" nodes
                  )
                   (
                    if (
                      equal? (
                        hash-table-ref ll "head_idx"
                      )
                       (
                        - 1
                      )
                    )
                     (
                      begin (
                        hash-table-set! ll "head_idx" idx
                      )
                       (
                        hash-table-set! ll "tail_idx" idx
                      )
                    )
                     (
                      begin (
                        insert_after_node ll (
                          hash-table-ref ll "tail_idx"
                        )
                         idx
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
        insert_at_position ll position value
      )
       (
        call/cc (
          lambda (
            ret9
          )
           (
            let (
              (
                current (
                  hash-table-ref ll "head_idx"
                )
              )
            )
             (
              begin (
                let (
                  (
                    current_pos 1
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
                                  not (
                                    equal? current (
                                      - 1
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      equal? current_pos position
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            nodes (
                                              hash-table-ref ll "nodes"
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            set! nodes (
                                              append nodes (
                                                _list (
                                                  alist->hash-table (
                                                    _list (
                                                      cons "data" value
                                                    )
                                                     (
                                                      cons "prev_index" (
                                                        - 1
                                                      )
                                                    )
                                                     (
                                                      cons "next_index" (
                                                        - 1
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
                                                new_idx (
                                                  - (
                                                    _len nodes
                                                  )
                                                   1
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                hash-table-set! ll "nodes" nodes
                                              )
                                               (
                                                insert_before_node ll current new_idx
                                              )
                                               (
                                                ret9 (
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
                                      quote (
                                        
                                      )
                                    )
                                  )
                                   (
                                    let (
                                      (
                                        node (
                                          list-ref (
                                            hash-table-ref ll "nodes"
                                          )
                                           current
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        set! current (
                                          hash-table-ref node "next_index"
                                        )
                                      )
                                       (
                                        set! current_pos (
                                          + current_pos 1
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
                    insert ll value
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
        get_node ll item
      )
       (
        call/cc (
          lambda (
            ret12
          )
           (
            let (
              (
                current (
                  hash-table-ref ll "head_idx"
                )
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
                              not (
                                equal? current (
                                  - 1
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    node (
                                      list-ref (
                                        hash-table-ref ll "nodes"
                                      )
                                       current
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      equal? (
                                        hash-table-ref node "data"
                                      )
                                       item
                                    )
                                     (
                                      begin (
                                        ret12 current
                                      )
                                    )
                                     (
                                      quote (
                                        
                                      )
                                    )
                                  )
                                   (
                                    set! current (
                                      hash-table-ref node "next_index"
                                    )
                                  )
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
                ret12 (
                  - 1
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        remove_node_pointers ll idx
      )
       (
        call/cc (
          lambda (
            ret15
          )
           (
            let (
              (
                nodes (
                  hash-table-ref ll "nodes"
                )
              )
            )
             (
              begin (
                let (
                  (
                    node (
                      list-ref nodes idx
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        nxt (
                          hash-table-ref node "next_index"
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            p (
                              hash-table-ref node "prev_index"
                            )
                          )
                        )
                         (
                          begin (
                            if (
                              not (
                                equal? nxt (
                                  - 1
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    nxt_node (
                                      list-ref nodes nxt
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    hash-table-set! nxt_node "prev_index" p
                                  )
                                   (
                                    list-set! nodes nxt nxt_node
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
                            if (
                              not (
                                equal? p (
                                  - 1
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    prev_node (
                                      list-ref nodes p
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    hash-table-set! prev_node "next_index" nxt
                                  )
                                   (
                                    list-set! nodes p prev_node
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
                            hash-table-set! node "next_index" (
                              - 1
                            )
                          )
                           (
                            hash-table-set! node "prev_index" (
                              - 1
                            )
                          )
                           (
                            list-set! nodes idx node
                          )
                           (
                            hash-table-set! ll "nodes" nodes
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
        delete_value ll value
      )
       (
        call/cc (
          lambda (
            ret16
          )
           (
            let (
              (
                idx (
                  get_node ll value
                )
              )
            )
             (
              begin (
                if (
                  equal? idx (
                    - 1
                  )
                )
                 (
                  begin (
                    ret16 (
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
                if (
                  equal? idx (
                    hash-table-ref ll "head_idx"
                  )
                )
                 (
                  begin (
                    let (
                      (
                        node (
                          list-ref (
                            hash-table-ref ll "nodes"
                          )
                           idx
                        )
                      )
                    )
                     (
                      begin (
                        hash-table-set! ll "head_idx" (
                          hash-table-ref node "next_index"
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
                if (
                  equal? idx (
                    hash-table-ref ll "tail_idx"
                  )
                )
                 (
                  begin (
                    let (
                      (
                        node (
                          list-ref (
                            hash-table-ref ll "nodes"
                          )
                           idx
                        )
                      )
                    )
                     (
                      begin (
                        hash-table-set! ll "tail_idx" (
                          hash-table-ref node "prev_index"
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
                remove_node_pointers ll idx
              )
            )
          )
        )
      )
    )
     (
      define (
        contains ll value
      )
       (
        call/cc (
          lambda (
            ret17
          )
           (
            ret17 (
              not (
                equal? (
                  get_node ll value
                )
                 (
                  - 1
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        is_empty ll
      )
       (
        call/cc (
          lambda (
            ret18
          )
           (
            ret18 (
              equal? (
                hash-table-ref ll "head_idx"
              )
               (
                - 1
              )
            )
          )
        )
      )
    )
     (
      define (
        to_string ll
      )
       (
        call/cc (
          lambda (
            ret19
          )
           (
            let (
              (
                res ""
              )
            )
             (
              begin (
                let (
                  (
                    first #t
                  )
                )
                 (
                  begin (
                    let (
                      (
                        current (
                          hash-table-ref ll "head_idx"
                        )
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
                                      not (
                                        equal? current (
                                          - 1
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            node (
                                              list-ref (
                                                hash-table-ref ll "nodes"
                                              )
                                               current
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                val (
                                                  to-str-space (
                                                    hash-table-ref node "data"
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                if first (
                                                  begin (
                                                    set! res val
                                                  )
                                                   (
                                                    set! first #f
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    set! res (
                                                      string-append (
                                                        string-append res " "
                                                      )
                                                       val
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                set! current (
                                                  hash-table-ref node "next_index"
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
                        ret19 res
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
        print_list ll
      )
       (
        call/cc (
          lambda (
            ret22
          )
           (
            let (
              (
                current (
                  hash-table-ref ll "head_idx"
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
                              not (
                                equal? current (
                                  - 1
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    node (
                                      list-ref (
                                        hash-table-ref ll "nodes"
                                      )
                                       current
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    _display (
                                      if (
                                        string? (
                                          to-str-space (
                                            hash-table-ref node "data"
                                          )
                                        )
                                      )
                                       (
                                        to-str-space (
                                          hash-table-ref node "data"
                                        )
                                      )
                                       (
                                        to-str (
                                          to-str-space (
                                            hash-table-ref node "data"
                                          )
                                        )
                                      )
                                    )
                                  )
                                   (
                                    newline
                                  )
                                   (
                                    set! current (
                                      hash-table-ref node "next_index"
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
            ret25
          )
           (
            let (
              (
                ll (
                  empty_list
                )
              )
            )
             (
              begin (
                _display (
                  if (
                    string? (
                      to-str-space (
                        get_head_data ll
                      )
                    )
                  )
                   (
                    to-str-space (
                      get_head_data ll
                    )
                  )
                   (
                    to-str (
                      to-str-space (
                        get_head_data ll
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
                      to-str-space (
                        get_tail_data ll
                      )
                    )
                  )
                   (
                    to-str-space (
                      get_tail_data ll
                    )
                  )
                   (
                    to-str (
                      to-str-space (
                        get_tail_data ll
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
                      to-str-space (
                        is_empty ll
                      )
                    )
                  )
                   (
                    to-str-space (
                      is_empty ll
                    )
                  )
                   (
                    to-str (
                      to-str-space (
                        is_empty ll
                      )
                    )
                  )
                )
              )
               (
                newline
              )
               (
                insert ll 10
              )
               (
                _display (
                  if (
                    string? (
                      to-str-space (
                        get_head_data ll
                      )
                    )
                  )
                   (
                    to-str-space (
                      get_head_data ll
                    )
                  )
                   (
                    to-str (
                      to-str-space (
                        get_head_data ll
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
                      to-str-space (
                        get_tail_data ll
                      )
                    )
                  )
                   (
                    to-str-space (
                      get_tail_data ll
                    )
                  )
                   (
                    to-str (
                      to-str-space (
                        get_tail_data ll
                      )
                    )
                  )
                )
              )
               (
                newline
              )
               (
                insert_at_position ll 3 20
              )
               (
                _display (
                  if (
                    string? (
                      to-str-space (
                        get_head_data ll
                      )
                    )
                  )
                   (
                    to-str-space (
                      get_head_data ll
                    )
                  )
                   (
                    to-str (
                      to-str-space (
                        get_head_data ll
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
                      to-str-space (
                        get_tail_data ll
                      )
                    )
                  )
                   (
                    to-str-space (
                      get_tail_data ll
                    )
                  )
                   (
                    to-str (
                      to-str-space (
                        get_tail_data ll
                      )
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
                    nodes (
                      hash-table-ref ll "nodes"
                    )
                  )
                )
                 (
                  begin (
                    set! nodes (
                      append nodes (
                        _list (
                          alist->hash-table (
                            _list (
                              cons "data" 1000
                            )
                             (
                              cons "prev_index" (
                                - 1
                              )
                            )
                             (
                              cons "next_index" (
                                - 1
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
                        idx_head (
                          - (
                            _len nodes
                          )
                           1
                        )
                      )
                    )
                     (
                      begin (
                        hash-table-set! ll "nodes" nodes
                      )
                       (
                        set_head ll idx_head
                      )
                       (
                        set! nodes (
                          hash-table-ref ll "nodes"
                        )
                      )
                       (
                        set! nodes (
                          append nodes (
                            _list (
                              alist->hash-table (
                                _list (
                                  cons "data" 2000
                                )
                                 (
                                  cons "prev_index" (
                                    - 1
                                  )
                                )
                                 (
                                  cons "next_index" (
                                    - 1
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
                            idx_tail (
                              - (
                                _len nodes
                              )
                               1
                            )
                          )
                        )
                         (
                          begin (
                            hash-table-set! ll "nodes" nodes
                          )
                           (
                            set_tail ll idx_tail
                          )
                           (
                            print_list ll
                          )
                           (
                            _display (
                              if (
                                string? (
                                  to-str-space (
                                    is_empty ll
                                  )
                                )
                              )
                               (
                                to-str-space (
                                  is_empty ll
                                )
                              )
                               (
                                to-str (
                                  to-str-space (
                                    is_empty ll
                                  )
                                )
                              )
                            )
                          )
                           (
                            newline
                          )
                           (
                            print_list ll
                          )
                           (
                            _display (
                              if (
                                string? (
                                  to-str-space (
                                    contains ll 10
                                  )
                                )
                              )
                               (
                                to-str-space (
                                  contains ll 10
                                )
                              )
                               (
                                to-str (
                                  to-str-space (
                                    contains ll 10
                                  )
                                )
                              )
                            )
                          )
                           (
                            newline
                          )
                           (
                            delete_value ll 10
                          )
                           (
                            _display (
                              if (
                                string? (
                                  to-str-space (
                                    contains ll 10
                                  )
                                )
                              )
                               (
                                to-str-space (
                                  contains ll 10
                                )
                              )
                               (
                                to-str (
                                  to-str-space (
                                    contains ll 10
                                  )
                                )
                              )
                            )
                          )
                           (
                            newline
                          )
                           (
                            delete_value ll 2000
                          )
                           (
                            _display (
                              if (
                                string? (
                                  to-str-space (
                                    get_tail_data ll
                                  )
                                )
                              )
                               (
                                to-str-space (
                                  get_tail_data ll
                                )
                              )
                               (
                                to-str (
                                  to-str-space (
                                    get_tail_data ll
                                  )
                                )
                              )
                            )
                          )
                           (
                            newline
                          )
                           (
                            delete_value ll 1000
                          )
                           (
                            _display (
                              if (
                                string? (
                                  to-str-space (
                                    get_tail_data ll
                                  )
                                )
                              )
                               (
                                to-str-space (
                                  get_tail_data ll
                                )
                              )
                               (
                                to-str (
                                  to-str-space (
                                    get_tail_data ll
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
                                  to-str-space (
                                    get_head_data ll
                                  )
                                )
                              )
                               (
                                to-str-space (
                                  get_head_data ll
                                )
                              )
                               (
                                to-str (
                                  to-str-space (
                                    get_head_data ll
                                  )
                                )
                              )
                            )
                          )
                           (
                            newline
                          )
                           (
                            print_list ll
                          )
                           (
                            delete_value ll 20
                          )
                           (
                            print_list ll
                          )
                           (
                            let (
                              (
                                i 1
                              )
                            )
                             (
                              begin (
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
                                              < i 10
                                            )
                                             (
                                              begin (
                                                insert ll i
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
                                             (
                                              quote (
                                                
                                              )
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
                                print_list ll
                              )
                               (
                                let (
                                  (
                                    ll2 (
                                      empty_list
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    insert_at_position ll2 1 10
                                  )
                                   (
                                    _display (
                                      if (
                                        string? (
                                          to_string ll2
                                        )
                                      )
                                       (
                                        to_string ll2
                                      )
                                       (
                                        to-str (
                                          to_string ll2
                                        )
                                      )
                                    )
                                  )
                                   (
                                    newline
                                  )
                                   (
                                    insert_at_position ll2 2 20
                                  )
                                   (
                                    _display (
                                      if (
                                        string? (
                                          to_string ll2
                                        )
                                      )
                                       (
                                        to_string ll2
                                      )
                                       (
                                        to-str (
                                          to_string ll2
                                        )
                                      )
                                    )
                                  )
                                   (
                                    newline
                                  )
                                   (
                                    insert_at_position ll2 1 30
                                  )
                                   (
                                    _display (
                                      if (
                                        string? (
                                          to_string ll2
                                        )
                                      )
                                       (
                                        to_string ll2
                                      )
                                       (
                                        to-str (
                                          to_string ll2
                                        )
                                      )
                                    )
                                  )
                                   (
                                    newline
                                  )
                                   (
                                    insert_at_position ll2 3 40
                                  )
                                   (
                                    _display (
                                      if (
                                        string? (
                                          to_string ll2
                                        )
                                      )
                                       (
                                        to_string ll2
                                      )
                                       (
                                        to-str (
                                          to_string ll2
                                        )
                                      )
                                    )
                                  )
                                   (
                                    newline
                                  )
                                   (
                                    insert_at_position ll2 5 50
                                  )
                                   (
                                    _display (
                                      if (
                                        string? (
                                          to_string ll2
                                        )
                                      )
                                       (
                                        to_string ll2
                                      )
                                       (
                                        to-str (
                                          to_string ll2
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
        )
      )
    )
     (
      main
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
