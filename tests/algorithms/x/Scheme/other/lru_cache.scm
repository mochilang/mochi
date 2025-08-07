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
      start10 (
        current-jiffy
      )
    )
     (
      jps13 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        new_list
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                nodes (
                  _list
                )
              )
            )
             (
              begin (
                let (
                  (
                    head (
                      alist->hash-table (
                        _list (
                          cons "key" 0
                        )
                         (
                          cons "value" 0
                        )
                         (
                          cons "prev" (
                            - 0 1
                          )
                        )
                         (
                          cons "next" 1
                        )
                      )
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        tail (
                          alist->hash-table (
                            _list (
                              cons "key" 0
                            )
                             (
                              cons "value" 0
                            )
                             (
                              cons "prev" 0
                            )
                             (
                              cons "next" (
                                - 0 1
                              )
                            )
                          )
                        )
                      )
                    )
                     (
                      begin (
                        set! nodes (
                          append nodes (
                            _list head
                          )
                        )
                      )
                       (
                        set! nodes (
                          append nodes (
                            _list tail
                          )
                        )
                      )
                       (
                        ret1 (
                          alist->hash-table (
                            _list (
                              cons "nodes" nodes
                            )
                             (
                              cons "head" 0
                            )
                             (
                              cons "tail" 1
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
        dll_add lst idx
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            let (
              (
                nodes (
                  hash-table-ref lst "nodes"
                )
              )
            )
             (
              begin (
                let (
                  (
                    tail_idx (
                      hash-table-ref lst "tail"
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        tail_node (
                          list-ref-safe nodes tail_idx
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            prev_idx (
                              hash-table-ref tail_node "prev"
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                node (
                                  list-ref-safe nodes idx
                                )
                              )
                            )
                             (
                              begin (
                                hash-table-set! node "prev" prev_idx
                              )
                               (
                                hash-table-set! node "next" tail_idx
                              )
                               (
                                list-set! nodes idx node
                              )
                               (
                                let (
                                  (
                                    prev_node (
                                      list-ref-safe nodes prev_idx
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    hash-table-set! prev_node "next" idx
                                  )
                                   (
                                    list-set! nodes prev_idx prev_node
                                  )
                                   (
                                    hash-table-set! tail_node "prev" idx
                                  )
                                   (
                                    list-set! nodes tail_idx tail_node
                                  )
                                   (
                                    hash-table-set! lst "nodes" nodes
                                  )
                                   (
                                    ret2 lst
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
        dll_remove lst idx
      )
       (
        call/cc (
          lambda (
            ret3
          )
           (
            let (
              (
                nodes (
                  hash-table-ref lst "nodes"
                )
              )
            )
             (
              begin (
                let (
                  (
                    node (
                      list-ref-safe nodes idx
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        prev_idx (
                          hash-table-ref node "prev"
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            next_idx (
                              hash-table-ref node "next"
                            )
                          )
                        )
                         (
                          begin (
                            if (
                              or (
                                equal? prev_idx (
                                  - 0 1
                                )
                              )
                               (
                                equal? next_idx (
                                  - 0 1
                                )
                              )
                            )
                             (
                              begin (
                                ret3 lst
                              )
                            )
                             '(
                              
                            )
                          )
                           (
                            let (
                              (
                                prev_node (
                                  list-ref-safe nodes prev_idx
                                )
                              )
                            )
                             (
                              begin (
                                hash-table-set! prev_node "next" next_idx
                              )
                               (
                                list-set! nodes prev_idx prev_node
                              )
                               (
                                let (
                                  (
                                    next_node (
                                      list-ref-safe nodes next_idx
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    hash-table-set! next_node "prev" prev_idx
                                  )
                                   (
                                    list-set! nodes next_idx next_node
                                  )
                                   (
                                    hash-table-set! node "prev" (
                                      - 0 1
                                    )
                                  )
                                   (
                                    hash-table-set! node "next" (
                                      - 0 1
                                    )
                                  )
                                   (
                                    list-set! nodes idx node
                                  )
                                   (
                                    hash-table-set! lst "nodes" nodes
                                  )
                                   (
                                    ret3 lst
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
        new_cache cap
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            let (
              (
                empty_map (
                  alist->hash-table (
                    _list
                  )
                )
              )
            )
             (
              begin (
                ret4 (
                  alist->hash-table (
                    _list (
                      cons "list" (
                        new_list
                      )
                    )
                     (
                      cons "capacity" cap
                    )
                     (
                      cons "num_keys" 0
                    )
                     (
                      cons "hits" 0
                    )
                     (
                      cons "misses" 0
                    )
                     (
                      cons "cache" empty_map
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
        lru_get c key
      )
       (
        call/cc (
          lambda (
            ret5
          )
           (
            let (
              (
                cache c
              )
            )
             (
              begin (
                let (
                  (
                    key_str (
                      to-str-space key
                    )
                  )
                )
                 (
                  begin (
                    if (
                      cond (
                        (
                          string? (
                            hash-table-ref cache "cache"
                          )
                        )
                         (
                          if (
                            string-contains (
                              hash-table-ref cache "cache"
                            )
                             key_str
                          )
                           #t #f
                        )
                      )
                       (
                        (
                          hash-table? (
                            hash-table-ref cache "cache"
                          )
                        )
                         (
                          if (
                            hash-table-exists? (
                              hash-table-ref cache "cache"
                            )
                             key_str
                          )
                           #t #f
                        )
                      )
                       (
                        else (
                          if (
                            member key_str (
                              hash-table-ref cache "cache"
                            )
                          )
                           #t #f
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            idx (
                              hash-table-ref/default (
                                hash-table-ref cache "cache"
                              )
                               key_str '(
                                
                              )
                            )
                          )
                        )
                         (
                          begin (
                            if (
                              not (
                                equal? idx (
                                  - 0 1
                                )
                              )
                            )
                             (
                              begin (
                                hash-table-set! cache "hits" (
                                  + (
                                    hash-table-ref cache "hits"
                                  )
                                   1
                                )
                              )
                               (
                                let (
                                  (
                                    node (
                                      list-ref-safe (
                                        hash-table-ref (
                                          hash-table-ref cache "list"
                                        )
                                         "nodes"
                                      )
                                       idx
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        value (
                                          hash-table-ref node "value"
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        hash-table-set! cache "list" (
                                          dll_remove (
                                            hash-table-ref cache "list"
                                          )
                                           idx
                                        )
                                      )
                                       (
                                        hash-table-set! cache "list" (
                                          dll_add (
                                            hash-table-ref cache "list"
                                          )
                                           idx
                                        )
                                      )
                                       (
                                        ret5 (
                                          alist->hash-table (
                                            _list (
                                              cons "cache" cache
                                            )
                                             (
                                              cons "value" value
                                            )
                                             (
                                              cons "ok" #t
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                             '(
                              
                            )
                          )
                        )
                      )
                    )
                     '(
                      
                    )
                  )
                   (
                    hash-table-set! cache "misses" (
                      + (
                        hash-table-ref cache "misses"
                      )
                       1
                    )
                  )
                   (
                    ret5 (
                      alist->hash-table (
                        _list (
                          cons "cache" cache
                        )
                         (
                          cons "value" 0
                        )
                         (
                          cons "ok" #f
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
        lru_put c key value
      )
       (
        call/cc (
          lambda (
            ret6
          )
           (
            let (
              (
                cache c
              )
            )
             (
              begin (
                let (
                  (
                    key_str (
                      to-str-space key
                    )
                  )
                )
                 (
                  begin (
                    if (
                      not (
                        cond (
                          (
                            string? (
                              hash-table-ref cache "cache"
                            )
                          )
                           (
                            if (
                              string-contains (
                                hash-table-ref cache "cache"
                              )
                               key_str
                            )
                             #t #f
                          )
                        )
                         (
                          (
                            hash-table? (
                              hash-table-ref cache "cache"
                            )
                          )
                           (
                            if (
                              hash-table-exists? (
                                hash-table-ref cache "cache"
                              )
                               key_str
                            )
                             #t #f
                          )
                        )
                         (
                          else (
                            if (
                              member key_str (
                                hash-table-ref cache "cache"
                              )
                            )
                             #t #f
                          )
                        )
                      )
                    )
                     (
                      begin (
                        if (
                          >= (
                            hash-table-ref cache "num_keys"
                          )
                           (
                            hash-table-ref cache "capacity"
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                head_node (
                                  list-ref-safe (
                                    hash-table-ref (
                                      hash-table-ref cache "list"
                                    )
                                     "nodes"
                                  )
                                   (
                                    hash-table-ref (
                                      hash-table-ref cache "list"
                                    )
                                     "head"
                                  )
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    first_idx (
                                      hash-table-ref head_node "next"
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        first_node (
                                          list-ref-safe (
                                            hash-table-ref (
                                              hash-table-ref cache "list"
                                            )
                                             "nodes"
                                          )
                                           first_idx
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            old_key (
                                              hash-table-ref first_node "key"
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            hash-table-set! cache "list" (
                                              dll_remove (
                                                hash-table-ref cache "list"
                                              )
                                               first_idx
                                            )
                                          )
                                           (
                                            let (
                                              (
                                                mdel (
                                                  hash-table-ref cache "cache"
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                hash-table-set! mdel (
                                                  to-str-space old_key
                                                )
                                                 (
                                                  - 0 1
                                                )
                                              )
                                               (
                                                hash-table-set! cache "cache" mdel
                                              )
                                               (
                                                hash-table-set! cache "num_keys" (
                                                  - (
                                                    hash-table-ref cache "num_keys"
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
                         '(
                          
                        )
                      )
                       (
                        let (
                          (
                            nodes (
                              hash-table-ref (
                                hash-table-ref cache "list"
                              )
                               "nodes"
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                new_node (
                                  alist->hash-table (
                                    _list (
                                      cons "key" key
                                    )
                                     (
                                      cons "value" value
                                    )
                                     (
                                      cons "prev" (
                                        - 0 1
                                      )
                                    )
                                     (
                                      cons "next" (
                                        - 0 1
                                      )
                                    )
                                  )
                                )
                              )
                            )
                             (
                              begin (
                                set! nodes (
                                  append nodes (
                                    _list new_node
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
                                    hash-table-set! (
                                      hash-table-ref cache "list"
                                    )
                                     "nodes" nodes
                                  )
                                   (
                                    hash-table-set! cache "list" (
                                      dll_add (
                                        hash-table-ref cache "list"
                                      )
                                       idx
                                    )
                                  )
                                   (
                                    let (
                                      (
                                        m (
                                          hash-table-ref cache "cache"
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        hash-table-set! m key_str idx
                                      )
                                       (
                                        hash-table-set! cache "cache" m
                                      )
                                       (
                                        hash-table-set! cache "num_keys" (
                                          + (
                                            hash-table-ref cache "num_keys"
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
                     (
                      begin (
                        let (
                          (
                            m (
                              hash-table-ref cache "cache"
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                idx (
                                  hash-table-ref/default m key_str '(
                                    
                                  )
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    nodes (
                                      hash-table-ref (
                                        hash-table-ref cache "list"
                                      )
                                       "nodes"
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        node (
                                          list-ref-safe nodes idx
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        hash-table-set! node "value" value
                                      )
                                       (
                                        list-set! nodes idx node
                                      )
                                       (
                                        hash-table-set! (
                                          hash-table-ref cache "list"
                                        )
                                         "nodes" nodes
                                      )
                                       (
                                        hash-table-set! cache "list" (
                                          dll_remove (
                                            hash-table-ref cache "list"
                                          )
                                           idx
                                        )
                                      )
                                       (
                                        hash-table-set! cache "list" (
                                          dll_add (
                                            hash-table-ref cache "list"
                                          )
                                           idx
                                        )
                                      )
                                       (
                                        hash-table-set! cache "cache" m
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
                    ret6 cache
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
        cache_info cache
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            ret7 (
              string-append (
                string-append (
                  string-append (
                    string-append (
                      string-append (
                        string-append (
                          string-append (
                            string-append "CacheInfo(hits=" (
                              to-str-space (
                                hash-table-ref cache "hits"
                              )
                            )
                          )
                           ", misses="
                        )
                         (
                          to-str-space (
                            hash-table-ref cache "misses"
                          )
                        )
                      )
                       ", capacity="
                    )
                     (
                      to-str-space (
                        hash-table-ref cache "capacity"
                      )
                    )
                  )
                   ", current size="
                )
                 (
                  to-str-space (
                    hash-table-ref cache "num_keys"
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
        print_result res
      )
       (
        call/cc (
          lambda (
            ret8
          )
           (
            if (
              hash-table-ref res "ok"
            )
             (
              begin (
                _display (
                  if (
                    string? (
                      to-str-space (
                        hash-table-ref res "value"
                      )
                    )
                  )
                   (
                    to-str-space (
                      hash-table-ref res "value"
                    )
                  )
                   (
                    to-str (
                      to-str-space (
                        hash-table-ref res "value"
                      )
                    )
                  )
                )
              )
               (
                newline
              )
            )
             (
              begin (
                _display (
                  if (
                    string? "None"
                  )
                   "None" (
                    to-str "None"
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
     (
      define (
        main
      )
       (
        call/cc (
          lambda (
            ret9
          )
           (
            let (
              (
                cache (
                  new_cache 2
                )
              )
            )
             (
              begin (
                set! cache (
                  lru_put cache 1 1
                )
              )
               (
                set! cache (
                  lru_put cache 2 2
                )
              )
               (
                let (
                  (
                    r1 (
                      lru_get cache 1
                    )
                  )
                )
                 (
                  begin (
                    set! cache (
                      hash-table-ref r1 "cache"
                    )
                  )
                   (
                    print_result r1
                  )
                   (
                    set! cache (
                      lru_put cache 3 3
                    )
                  )
                   (
                    let (
                      (
                        r2 (
                          lru_get cache 2
                        )
                      )
                    )
                     (
                      begin (
                        set! cache (
                          hash-table-ref r2 "cache"
                        )
                      )
                       (
                        print_result r2
                      )
                       (
                        set! cache (
                          lru_put cache 4 4
                        )
                      )
                       (
                        let (
                          (
                            r3 (
                              lru_get cache 1
                            )
                          )
                        )
                         (
                          begin (
                            set! cache (
                              hash-table-ref r3 "cache"
                            )
                          )
                           (
                            print_result r3
                          )
                           (
                            let (
                              (
                                r4 (
                                  lru_get cache 3
                                )
                              )
                            )
                             (
                              begin (
                                set! cache (
                                  hash-table-ref r4 "cache"
                                )
                              )
                               (
                                print_result r4
                              )
                               (
                                let (
                                  (
                                    r5 (
                                      lru_get cache 4
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    set! cache (
                                      hash-table-ref r5 "cache"
                                    )
                                  )
                                   (
                                    print_result r5
                                  )
                                   (
                                    _display (
                                      if (
                                        string? (
                                          cache_info cache
                                        )
                                      )
                                       (
                                        cache_info cache
                                      )
                                       (
                                        to-str (
                                          cache_info cache
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
          end11 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur12 (
              quotient (
                * (
                  - end11 start10
                )
                 1000000
              )
               jps13
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur12
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
