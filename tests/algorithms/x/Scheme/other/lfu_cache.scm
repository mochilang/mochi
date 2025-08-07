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
        lfu_new cap
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
                  cons "entries" (
                    _list
                  )
                )
                 (
                  cons "capacity" cap
                )
                 (
                  cons "hits" 0
                )
                 (
                  cons "miss" 0
                )
                 (
                  cons "tick" 0
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        find_entry entries key
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            let (
              (
                i 0
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
                              < i (
                                _len entries
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    e (
                                      list-ref-safe entries i
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      equal? (
                                        hash-table-ref e "key"
                                      )
                                       key
                                    )
                                     (
                                      begin (
                                        ret2 i
                                      )
                                    )
                                     '(
                                      
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
                                loop3
                              )
                            )
                             '(
                              
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
               (
                ret2 (
                  - 0 1
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        lfu_get cache key
      )
       (
        call/cc (
          lambda (
            ret5
          )
           (
            let (
              (
                idx (
                  find_entry (
                    hash-table-ref cache "entries"
                  )
                   key
                )
              )
            )
             (
              begin (
                if (
                  equal? idx (
                    - 0 1
                  )
                )
                 (
                  begin (
                    let (
                      (
                        new_cache (
                          alist->hash-table (
                            _list (
                              cons "entries" (
                                hash-table-ref cache "entries"
                              )
                            )
                             (
                              cons "capacity" (
                                hash-table-ref cache "capacity"
                              )
                            )
                             (
                              cons "hits" (
                                hash-table-ref cache "hits"
                              )
                            )
                             (
                              cons "miss" (
                                + (
                                  hash-table-ref cache "miss"
                                )
                                 1
                              )
                            )
                             (
                              cons "tick" (
                                hash-table-ref cache "tick"
                              )
                            )
                          )
                        )
                      )
                    )
                     (
                      begin (
                        ret5 (
                          alist->hash-table (
                            _list (
                              cons "cache" new_cache
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
                 '(
                  
                )
              )
               (
                let (
                  (
                    entries (
                      hash-table-ref cache "entries"
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        e (
                          list-ref-safe entries idx
                        )
                      )
                    )
                     (
                      begin (
                        hash-table-set! e "freq" (
                          + (
                            hash-table-ref e "freq"
                          )
                           1
                        )
                      )
                       (
                        let (
                          (
                            new_tick (
                              + (
                                hash-table-ref cache "tick"
                              )
                               1
                            )
                          )
                        )
                         (
                          begin (
                            hash-table-set! e "order" new_tick
                          )
                           (
                            list-set! entries idx e
                          )
                           (
                            let (
                              (
                                new_cache (
                                  alist->hash-table (
                                    _list (
                                      cons "entries" entries
                                    )
                                     (
                                      cons "capacity" (
                                        hash-table-ref cache "capacity"
                                      )
                                    )
                                     (
                                      cons "hits" (
                                        + (
                                          hash-table-ref cache "hits"
                                        )
                                         1
                                      )
                                    )
                                     (
                                      cons "miss" (
                                        hash-table-ref cache "miss"
                                      )
                                    )
                                     (
                                      cons "tick" new_tick
                                    )
                                  )
                                )
                              )
                            )
                             (
                              begin (
                                ret5 (
                                  alist->hash-table (
                                    _list (
                                      cons "cache" new_cache
                                    )
                                     (
                                      cons "value" (
                                        hash-table-ref e "val"
                                      )
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
        remove_lfu entries
      )
       (
        call/cc (
          lambda (
            ret6
          )
           (
            begin (
              if (
                equal? (
                  _len entries
                )
                 0
              )
               (
                begin (
                  ret6 entries
                )
              )
               '(
                
              )
            )
             (
              let (
                (
                  min_idx 0
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
                                    < i (
                                      _len entries
                                    )
                                  )
                                   (
                                    begin (
                                      let (
                                        (
                                          e (
                                            list-ref-safe entries i
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          let (
                                            (
                                              m (
                                                list-ref-safe entries min_idx
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              if (
                                                or (
                                                  < (
                                                    hash-table-ref e "freq"
                                                  )
                                                   (
                                                    hash-table-ref m "freq"
                                                  )
                                                )
                                                 (
                                                  and (
                                                    equal? (
                                                      hash-table-ref e "freq"
                                                    )
                                                     (
                                                      hash-table-ref m "freq"
                                                    )
                                                  )
                                                   (
                                                    < (
                                                      hash-table-ref e "order"
                                                    )
                                                     (
                                                      hash-table-ref m "order"
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                begin (
                                                  set! min_idx i
                                                )
                                              )
                                               '(
                                                
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
                                      loop7
                                    )
                                  )
                                   '(
                                    
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
                              j 0
                            )
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
                                          
                                        )
                                         (
                                          if (
                                            < j (
                                              _len entries
                                            )
                                          )
                                           (
                                            begin (
                                              if (
                                                not (
                                                  equal? j min_idx
                                                )
                                              )
                                               (
                                                begin (
                                                  set! res (
                                                    append res (
                                                      _list (
                                                        list-ref-safe entries j
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                               '(
                                                
                                              )
                                            )
                                             (
                                              set! j (
                                                + j 1
                                              )
                                            )
                                             (
                                              loop9
                                            )
                                          )
                                           '(
                                            
                                          )
                                        )
                                      )
                                    )
                                  )
                                   (
                                    loop9
                                  )
                                )
                              )
                            )
                             (
                              ret6 res
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
        lfu_put cache key value
      )
       (
        call/cc (
          lambda (
            ret11
          )
           (
            let (
              (
                entries (
                  hash-table-ref cache "entries"
                )
              )
            )
             (
              begin (
                let (
                  (
                    idx (
                      find_entry entries key
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
                        let (
                          (
                            e (
                              list-ref-safe entries idx
                            )
                          )
                        )
                         (
                          begin (
                            hash-table-set! e "val" value
                          )
                           (
                            hash-table-set! e "freq" (
                              + (
                                hash-table-ref e "freq"
                              )
                               1
                            )
                          )
                           (
                            let (
                              (
                                new_tick (
                                  + (
                                    hash-table-ref cache "tick"
                                  )
                                   1
                                )
                              )
                            )
                             (
                              begin (
                                hash-table-set! e "order" new_tick
                              )
                               (
                                list-set! entries idx e
                              )
                               (
                                ret11 (
                                  alist->hash-table (
                                    _list (
                                      cons "entries" entries
                                    )
                                     (
                                      cons "capacity" (
                                        hash-table-ref cache "capacity"
                                      )
                                    )
                                     (
                                      cons "hits" (
                                        hash-table-ref cache "hits"
                                      )
                                    )
                                     (
                                      cons "miss" (
                                        hash-table-ref cache "miss"
                                      )
                                    )
                                     (
                                      cons "tick" new_tick
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
                    if (
                      >= (
                        _len entries
                      )
                       (
                        hash-table-ref cache "capacity"
                      )
                    )
                     (
                      begin (
                        set! entries (
                          remove_lfu entries
                        )
                      )
                    )
                     '(
                      
                    )
                  )
                   (
                    let (
                      (
                        new_tick (
                          + (
                            hash-table-ref cache "tick"
                          )
                           1
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            new_entry (
                              alist->hash-table (
                                _list (
                                  cons "key" key
                                )
                                 (
                                  cons "val" value
                                )
                                 (
                                  cons "freq" 1
                                )
                                 (
                                  cons "order" new_tick
                                )
                              )
                            )
                          )
                        )
                         (
                          begin (
                            set! entries (
                              append entries (
                                _list new_entry
                              )
                            )
                          )
                           (
                            ret11 (
                              alist->hash-table (
                                _list (
                                  cons "entries" entries
                                )
                                 (
                                  cons "capacity" (
                                    hash-table-ref cache "capacity"
                                  )
                                )
                                 (
                                  cons "hits" (
                                    hash-table-ref cache "hits"
                                  )
                                )
                                 (
                                  cons "miss" (
                                    hash-table-ref cache "miss"
                                  )
                                )
                                 (
                                  cons "tick" new_tick
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
        cache_info cache
      )
       (
        call/cc (
          lambda (
            ret12
          )
           (
            ret12 (
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
                            hash-table-ref cache "miss"
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
                   ", current_size="
                )
                 (
                  to-str-space (
                    _len (
                      hash-table-ref cache "entries"
                    )
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
        main
      )
       (
        call/cc (
          lambda (
            ret13
          )
           (
            let (
              (
                cache (
                  lfu_new 2
                )
              )
            )
             (
              begin (
                set! cache (
                  lfu_put cache 1 1
                )
              )
               (
                set! cache (
                  lfu_put cache 2 2
                )
              )
               (
                let (
                  (
                    r (
                      lfu_get cache 1
                    )
                  )
                )
                 (
                  begin (
                    set! cache (
                      hash-table-ref r "cache"
                    )
                  )
                   (
                    if (
                      hash-table-ref r "ok"
                    )
                     (
                      begin (
                        _display (
                          if (
                            string? (
                              to-str-space (
                                hash-table-ref r "value"
                              )
                            )
                          )
                           (
                            to-str-space (
                              hash-table-ref r "value"
                            )
                          )
                           (
                            to-str (
                              to-str-space (
                                hash-table-ref r "value"
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
                   (
                    set! cache (
                      lfu_put cache 3 3
                    )
                  )
                   (
                    set! r (
                      lfu_get cache 2
                    )
                  )
                   (
                    set! cache (
                      hash-table-ref r "cache"
                    )
                  )
                   (
                    if (
                      hash-table-ref r "ok"
                    )
                     (
                      begin (
                        _display (
                          if (
                            string? (
                              to-str-space (
                                hash-table-ref r "value"
                              )
                            )
                          )
                           (
                            to-str-space (
                              hash-table-ref r "value"
                            )
                          )
                           (
                            to-str (
                              to-str-space (
                                hash-table-ref r "value"
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
                   (
                    set! cache (
                      lfu_put cache 4 4
                    )
                  )
                   (
                    set! r (
                      lfu_get cache 1
                    )
                  )
                   (
                    set! cache (
                      hash-table-ref r "cache"
                    )
                  )
                   (
                    if (
                      hash-table-ref r "ok"
                    )
                     (
                      begin (
                        _display (
                          if (
                            string? (
                              to-str-space (
                                hash-table-ref r "value"
                              )
                            )
                          )
                           (
                            to-str-space (
                              hash-table-ref r "value"
                            )
                          )
                           (
                            to-str (
                              to-str-space (
                                hash-table-ref r "value"
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
                   (
                    set! r (
                      lfu_get cache 3
                    )
                  )
                   (
                    set! cache (
                      hash-table-ref r "cache"
                    )
                  )
                   (
                    if (
                      hash-table-ref r "ok"
                    )
                     (
                      begin (
                        _display (
                          if (
                            string? (
                              to-str-space (
                                hash-table-ref r "value"
                              )
                            )
                          )
                           (
                            to-str-space (
                              hash-table-ref r "value"
                            )
                          )
                           (
                            to-str (
                              to-str-space (
                                hash-table-ref r "value"
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
                   (
                    set! r (
                      lfu_get cache 4
                    )
                  )
                   (
                    set! cache (
                      hash-table-ref r "cache"
                    )
                  )
                   (
                    if (
                      hash-table-ref r "ok"
                    )
                     (
                      begin (
                        _display (
                          if (
                            string? (
                              to-str-space (
                                hash-table-ref r "value"
                              )
                            )
                          )
                           (
                            to-str-space (
                              hash-table-ref r "value"
                            )
                          )
                           (
                            to-str (
                              to-str-space (
                                hash-table-ref r "value"
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
     (
      main
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
