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
      start24 (
        current-jiffy
      )
    )
     (
      jps27 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        make_hash_map
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
              )
            )
          )
        )
      )
    )
     (
      define (
        hm_len m
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            ret2 (
              _len (
                hash-table-ref m "entries"
              )
            )
          )
        )
      )
    )
     (
      define (
        hm_set m key value
      )
       (
        call/cc (
          lambda (
            ret3
          )
           (
            let (
              (
                entries (
                  hash-table-ref m "entries"
                )
              )
            )
             (
              begin (
                let (
                  (
                    updated #f
                  )
                )
                 (
                  begin (
                    let (
                      (
                        new_entries (
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
                                break5
                              )
                               (
                                letrec (
                                  (
                                    loop4 (
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
                                                  list-ref entries i
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                if (
                                                  string=? (
                                                    hash-table-ref e "key"
                                                  )
                                                   key
                                                )
                                                 (
                                                  begin (
                                                    set! new_entries (
                                                      append new_entries (
                                                        _list (
                                                          alist->hash-table (
                                                            _list (
                                                              cons "key" key
                                                            )
                                                             (
                                                              cons "value" value
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    set! updated #t
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    set! new_entries (
                                                      append new_entries (
                                                        _list e
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
                                            )
                                          )
                                           (
                                            loop4
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
                                  loop4
                                )
                              )
                            )
                          )
                           (
                            if (
                              not updated
                            )
                             (
                              begin (
                                set! new_entries (
                                  append new_entries (
                                    _list (
                                      alist->hash-table (
                                        _list (
                                          cons "key" key
                                        )
                                         (
                                          cons "value" value
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
                            ret3 (
                              alist->hash-table (
                                _list (
                                  cons "entries" new_entries
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
        hm_get m key
      )
       (
        call/cc (
          lambda (
            ret6
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
                                _len (
                                  hash-table-ref m "entries"
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    e (
                                      list-ref (
                                        hash-table-ref m "entries"
                                      )
                                       i
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      string=? (
                                        hash-table-ref e "key"
                                      )
                                       key
                                    )
                                     (
                                      begin (
                                        ret6 (
                                          alist->hash-table (
                                            _list (
                                              cons "found" #t
                                            )
                                             (
                                              cons "value" (
                                                hash-table-ref e "value"
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
                                    set! i (
                                      + i 1
                                    )
                                  )
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
                ret6 (
                  alist->hash-table (
                    _list (
                      cons "found" #f
                    )
                     (
                      cons "value" ""
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
        hm_del m key
      )
       (
        call/cc (
          lambda (
            ret9
          )
           (
            let (
              (
                entries (
                  hash-table-ref m "entries"
                )
              )
            )
             (
              begin (
                let (
                  (
                    new_entries (
                      _list
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        removed #f
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
                                          < i (
                                            _len entries
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                e (
                                                  list-ref entries i
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                if (
                                                  string=? (
                                                    hash-table-ref e "key"
                                                  )
                                                   key
                                                )
                                                 (
                                                  begin (
                                                    set! removed #t
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    set! new_entries (
                                                      append new_entries (
                                                        _list e
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
                            if removed (
                              begin (
                                ret9 (
                                  alist->hash-table (
                                    _list (
                                      cons "map" (
                                        alist->hash-table (
                                          _list (
                                            cons "entries" new_entries
                                          )
                                        )
                                      )
                                    )
                                     (
                                      cons "ok" #t
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
                            ret9 (
                              alist->hash-table (
                                _list (
                                  cons "map" m
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
          )
        )
      )
    )
     (
      define (
        test_add_items
      )
       (
        call/cc (
          lambda (
            ret12
          )
           (
            let (
              (
                h (
                  make_hash_map
                )
              )
            )
             (
              begin (
                set! h (
                  hm_set h "key_a" "val_a"
                )
              )
               (
                set! h (
                  hm_set h "key_b" "val_b"
                )
              )
               (
                let (
                  (
                    a (
                      hm_get h "key_a"
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        b (
                          hm_get h "key_b"
                        )
                      )
                    )
                     (
                      begin (
                        ret12 (
                          and (
                            and (
                              and (
                                and (
                                  equal? (
                                    hm_len h
                                  )
                                   2
                                )
                                 (
                                  hash-table-ref a "found"
                                )
                              )
                               (
                                hash-table-ref b "found"
                              )
                            )
                             (
                              string=? (
                                hash-table-ref a "value"
                              )
                               "val_a"
                            )
                          )
                           (
                            string=? (
                              hash-table-ref b "value"
                            )
                             "val_b"
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
        test_overwrite_items
      )
       (
        call/cc (
          lambda (
            ret13
          )
           (
            let (
              (
                h (
                  make_hash_map
                )
              )
            )
             (
              begin (
                set! h (
                  hm_set h "key_a" "val_a"
                )
              )
               (
                set! h (
                  hm_set h "key_a" "val_b"
                )
              )
               (
                let (
                  (
                    a (
                      hm_get h "key_a"
                    )
                  )
                )
                 (
                  begin (
                    ret13 (
                      and (
                        and (
                          equal? (
                            hm_len h
                          )
                           1
                        )
                         (
                          hash-table-ref a "found"
                        )
                      )
                       (
                        string=? (
                          hash-table-ref a "value"
                        )
                         "val_b"
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
        test_delete_items
      )
       (
        call/cc (
          lambda (
            ret14
          )
           (
            let (
              (
                h (
                  make_hash_map
                )
              )
            )
             (
              begin (
                set! h (
                  hm_set h "key_a" "val_a"
                )
              )
               (
                set! h (
                  hm_set h "key_b" "val_b"
                )
              )
               (
                let (
                  (
                    d1 (
                      hm_del h "key_a"
                    )
                  )
                )
                 (
                  begin (
                    set! h (
                      hash-table-ref d1 "map"
                    )
                  )
                   (
                    let (
                      (
                        d2 (
                          hm_del h "key_b"
                        )
                      )
                    )
                     (
                      begin (
                        set! h (
                          hash-table-ref d2 "map"
                        )
                      )
                       (
                        set! h (
                          hm_set h "key_a" "val_a"
                        )
                      )
                       (
                        let (
                          (
                            d3 (
                              hm_del h "key_a"
                            )
                          )
                        )
                         (
                          begin (
                            set! h (
                              hash-table-ref d3 "map"
                            )
                          )
                           (
                            ret14 (
                              equal? (
                                hm_len h
                              )
                               0
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
        test_access_absent_items
      )
       (
        call/cc (
          lambda (
            ret15
          )
           (
            let (
              (
                h (
                  make_hash_map
                )
              )
            )
             (
              begin (
                let (
                  (
                    g1 (
                      hm_get h "key_a"
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        d1 (
                          hm_del h "key_a"
                        )
                      )
                    )
                     (
                      begin (
                        set! h (
                          hash-table-ref d1 "map"
                        )
                      )
                       (
                        set! h (
                          hm_set h "key_a" "val_a"
                        )
                      )
                       (
                        let (
                          (
                            d2 (
                              hm_del h "key_a"
                            )
                          )
                        )
                         (
                          begin (
                            set! h (
                              hash-table-ref d2 "map"
                            )
                          )
                           (
                            let (
                              (
                                d3 (
                                  hm_del h "key_a"
                                )
                              )
                            )
                             (
                              begin (
                                set! h (
                                  hash-table-ref d3 "map"
                                )
                              )
                               (
                                let (
                                  (
                                    g2 (
                                      hm_get h "key_a"
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    ret15 (
                                      and (
                                        and (
                                          and (
                                            and (
                                              and (
                                                eq? (
                                                  hash-table-ref g1 "found"
                                                )
                                                 #f
                                              )
                                               (
                                                eq? (
                                                  hash-table-ref d1 "ok"
                                                )
                                                 #f
                                              )
                                            )
                                             (
                                              hash-table-ref d2 "ok"
                                            )
                                          )
                                           (
                                            eq? (
                                              hash-table-ref d3 "ok"
                                            )
                                             #f
                                          )
                                        )
                                         (
                                          eq? (
                                            hash-table-ref g2 "found"
                                          )
                                           #f
                                        )
                                      )
                                       (
                                        equal? (
                                          hm_len h
                                        )
                                         0
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
        test_add_with_resize_up
      )
       (
        call/cc (
          lambda (
            ret16
          )
           (
            let (
              (
                h (
                  make_hash_map
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
                        break18
                      )
                       (
                        letrec (
                          (
                            loop17 (
                              lambda (
                                
                              )
                               (
                                if (
                                  < i 5
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        s (
                                          to-str-space i
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        set! h (
                                          hm_set h s s
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
                                    loop17
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
                          loop17
                        )
                      )
                    )
                  )
                   (
                    ret16 (
                      equal? (
                        hm_len h
                      )
                       5
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
        test_add_with_resize_down
      )
       (
        call/cc (
          lambda (
            ret19
          )
           (
            let (
              (
                h (
                  make_hash_map
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
                                  < i 5
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        s (
                                          to-str-space i
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        set! h (
                                          hm_set h s s
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
                    let (
                      (
                        j 0
                      )
                    )
                     (
                      begin (
                        call/cc (
                          lambda (
                            break23
                          )
                           (
                            letrec (
                              (
                                loop22 (
                                  lambda (
                                    
                                  )
                                   (
                                    if (
                                      < j 5
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            s (
                                              to-str-space j
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                d (
                                                  hm_del h s
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                set! h (
                                                  hash-table-ref d "map"
                                                )
                                              )
                                               (
                                                set! j (
                                                  + j 1
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                       (
                                        loop22
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
                              loop22
                            )
                          )
                        )
                      )
                       (
                        set! h (
                          hm_set h "key_a" "val_b"
                        )
                      )
                       (
                        let (
                          (
                            a (
                              hm_get h "key_a"
                            )
                          )
                        )
                         (
                          begin (
                            ret19 (
                              and (
                                and (
                                  equal? (
                                    hm_len h
                                  )
                                   1
                                )
                                 (
                                  hash-table-ref a "found"
                                )
                              )
                               (
                                string=? (
                                  hash-table-ref a "value"
                                )
                                 "val_b"
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
      _display (
        if (
          string? (
            test_add_items
          )
        )
         (
          test_add_items
        )
         (
          to-str (
            test_add_items
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
            test_overwrite_items
          )
        )
         (
          test_overwrite_items
        )
         (
          to-str (
            test_overwrite_items
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
            test_delete_items
          )
        )
         (
          test_delete_items
        )
         (
          to-str (
            test_delete_items
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
            test_access_absent_items
          )
        )
         (
          test_access_absent_items
        )
         (
          to-str (
            test_access_absent_items
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
            test_add_with_resize_up
          )
        )
         (
          test_add_with_resize_up
        )
         (
          to-str (
            test_add_with_resize_up
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
            test_add_with_resize_down
          )
        )
         (
          test_add_with_resize_down
        )
         (
          to-str (
            test_add_with_resize_down
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
            if #t #t #f
          )
        )
         (
          if #t #t #f
        )
         (
          to-str (
            if #t #t #f
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
          end25 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur26 (
              quotient (
                * (
                  - end25 start24
                )
                 1000000
              )
               jps27
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur26
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
