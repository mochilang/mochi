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
        make_buckets n
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                buckets (
                  quote (
                    
                  )
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
                                  < i n
                                )
                                 (
                                  begin (
                                    set! buckets (
                                      append buckets (
                                        _list (
                                          alist->hash-table (
                                            _list (
                                              cons "state" 0
                                            )
                                             (
                                              cons "key" 0
                                            )
                                             (
                                              cons "val" 0
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
                    ret1 buckets
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
        hashmap_new initial_size
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            ret4 (
              alist->hash-table (
                _list (
                  cons "buckets" (
                    make_buckets initial_size
                  )
                )
                 (
                  cons "len" 0
                )
                 (
                  cons "cap_num" 3
                )
                 (
                  cons "cap_den" 4
                )
                 (
                  cons "initial_size" initial_size
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        bucket_index hm key
      )
       (
        call/cc (
          lambda (
            ret5
          )
           (
            let (
              (
                ind (
                  _mod key (
                    _len (
                      hash-table-ref hm "buckets"
                    )
                  )
                )
              )
            )
             (
              begin (
                if (
                  < ind 0
                )
                 (
                  begin (
                    set! ind (
                      + ind (
                        _len (
                          hash-table-ref hm "buckets"
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
                ret5 ind
              )
            )
          )
        )
      )
    )
     (
      define (
        next_index hm ind
      )
       (
        call/cc (
          lambda (
            ret6
          )
           (
            ret6 (
              _mod (
                + ind 1
              )
               (
                _len (
                  hash-table-ref hm "buckets"
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        try_set hm ind key val
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            let (
              (
                buckets (
                  hash-table-ref hm "buckets"
                )
              )
            )
             (
              begin (
                let (
                  (
                    b (
                      list-ref buckets ind
                    )
                  )
                )
                 (
                  begin (
                    if (
                      or (
                        equal? (
                          hash-table-ref b "state"
                        )
                         0
                      )
                       (
                        equal? (
                          hash-table-ref b "state"
                        )
                         2
                      )
                    )
                     (
                      begin (
                        list-set! buckets ind (
                          alist->hash-table (
                            _list (
                              cons "state" 1
                            )
                             (
                              cons "key" key
                            )
                             (
                              cons "val" val
                            )
                          )
                        )
                      )
                       (
                        hash-table-set! hm "buckets" buckets
                      )
                       (
                        hash-table-set! hm "len" (
                          + (
                            hash-table-ref hm "len"
                          )
                           1
                        )
                      )
                       (
                        ret7 #t
                      )
                    )
                     (
                      quote (
                        
                      )
                    )
                  )
                   (
                    if (
                      equal? (
                        hash-table-ref b "key"
                      )
                       key
                    )
                     (
                      begin (
                        list-set! buckets ind (
                          alist->hash-table (
                            _list (
                              cons "state" 1
                            )
                             (
                              cons "key" key
                            )
                             (
                              cons "val" val
                            )
                          )
                        )
                      )
                       (
                        hash-table-set! hm "buckets" buckets
                      )
                       (
                        ret7 #t
                      )
                    )
                     (
                      quote (
                        
                      )
                    )
                  )
                   (
                    ret7 #f
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
        is_full hm
      )
       (
        call/cc (
          lambda (
            ret8
          )
           (
            let (
              (
                limit (
                  _div (
                    * (
                      _len (
                        hash-table-ref hm "buckets"
                      )
                    )
                     (
                      hash-table-ref hm "cap_num"
                    )
                  )
                   (
                    hash-table-ref hm "cap_den"
                  )
                )
              )
            )
             (
              begin (
                ret8 (
                  >= (
                    hash-table-ref hm "len"
                  )
                   limit
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        is_sparse hm
      )
       (
        call/cc (
          lambda (
            ret9
          )
           (
            begin (
              if (
                <= (
                  _len (
                    hash-table-ref hm "buckets"
                  )
                )
                 (
                  hash-table-ref hm "initial_size"
                )
              )
               (
                begin (
                  ret9 #f
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
                  limit (
                    _div (
                      * (
                        _len (
                          hash-table-ref hm "buckets"
                        )
                      )
                       (
                        hash-table-ref hm "cap_num"
                      )
                    )
                     (
                      * 2 (
                        hash-table-ref hm "cap_den"
                      )
                    )
                  )
                )
              )
               (
                begin (
                  ret9 (
                    < (
                      hash-table-ref hm "len"
                    )
                     limit
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
        resize hm new_size
      )
       (
        call/cc (
          lambda (
            ret10
          )
           (
            let (
              (
                old (
                  hash-table-ref hm "buckets"
                )
              )
            )
             (
              begin (
                hash-table-set! hm "buckets" (
                  make_buckets new_size
                )
              )
               (
                hash-table-set! hm "len" 0
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
                                    _len old
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        it (
                                          list-ref old i
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        if (
                                          equal? (
                                            hash-table-ref it "state"
                                          )
                                           1
                                        )
                                         (
                                          begin (
                                            add_item hm (
                                              hash-table-ref it "key"
                                            )
                                             (
                                              hash-table-ref it "val"
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
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        size_up hm
      )
       (
        call/cc (
          lambda (
            ret13
          )
           (
            resize hm (
              * (
                _len (
                  hash-table-ref hm "buckets"
                )
              )
               2
            )
          )
        )
      )
    )
     (
      define (
        size_down hm
      )
       (
        call/cc (
          lambda (
            ret14
          )
           (
            resize hm (
              _div (
                _len (
                  hash-table-ref hm "buckets"
                )
              )
               2
            )
          )
        )
      )
    )
     (
      define (
        add_item hm key val
      )
       (
        call/cc (
          lambda (
            ret15
          )
           (
            let (
              (
                ind (
                  bucket_index hm key
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
                                  < i (
                                    _len (
                                      hash-table-ref hm "buckets"
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      try_set hm ind key val
                                    )
                                     (
                                      begin (
                                        break17 (
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
                                    set! ind (
                                      next_index hm ind
                                    )
                                  )
                                   (
                                    set! i (
                                      + i 1
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
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        hashmap_set hm key val
      )
       (
        call/cc (
          lambda (
            ret18
          )
           (
            begin (
              if (
                is_full hm
              )
               (
                begin (
                  size_up hm
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              add_item hm key val
            )
          )
        )
      )
    )
     (
      define (
        hashmap_get hm key
      )
       (
        call/cc (
          lambda (
            ret19
          )
           (
            let (
              (
                buckets (
                  hash-table-ref hm "buckets"
                )
              )
            )
             (
              begin (
                let (
                  (
                    ind (
                      bucket_index hm key
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
                                      < i (
                                        _len buckets
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            it (
                                              list-ref buckets ind
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              equal? (
                                                hash-table-ref it "state"
                                              )
                                               0
                                            )
                                             (
                                              begin (
                                                break21 (
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
                                              and (
                                                equal? (
                                                  hash-table-ref it "state"
                                                )
                                                 1
                                              )
                                               (
                                                equal? (
                                                  hash-table-ref it "key"
                                                )
                                                 key
                                              )
                                            )
                                             (
                                              begin (
                                                ret19 (
                                                  hash-table-ref it "val"
                                                )
                                              )
                                            )
                                             (
                                              quote (
                                                
                                              )
                                            )
                                          )
                                           (
                                            set! ind (
                                              next_index hm ind
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
                        ret19 0
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
        hashmap_del hm key
      )
       (
        call/cc (
          lambda (
            ret22
          )
           (
            let (
              (
                buckets (
                  hash-table-ref hm "buckets"
                )
              )
            )
             (
              begin (
                let (
                  (
                    ind (
                      bucket_index hm key
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
                                      < i (
                                        _len buckets
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            it (
                                              list-ref buckets ind
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              equal? (
                                                hash-table-ref it "state"
                                              )
                                               0
                                            )
                                             (
                                              begin (
                                                _display (
                                                  if (
                                                    string? (
                                                      string-append "KeyError: " (
                                                        to-str-space key
                                                      )
                                                    )
                                                  )
                                                   (
                                                    string-append "KeyError: " (
                                                      to-str-space key
                                                    )
                                                  )
                                                   (
                                                    to-str (
                                                      string-append "KeyError: " (
                                                        to-str-space key
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                newline
                                              )
                                               (
                                                ret22 (
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
                                              and (
                                                equal? (
                                                  hash-table-ref it "state"
                                                )
                                                 1
                                              )
                                               (
                                                equal? (
                                                  hash-table-ref it "key"
                                                )
                                                 key
                                              )
                                            )
                                             (
                                              begin (
                                                list-set! buckets ind (
                                                  alist->hash-table (
                                                    _list (
                                                      cons "state" 2
                                                    )
                                                     (
                                                      cons "key" 0
                                                    )
                                                     (
                                                      cons "val" 0
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                hash-table-set! hm "buckets" buckets
                                              )
                                               (
                                                hash-table-set! hm "len" (
                                                  - (
                                                    hash-table-ref hm "len"
                                                  )
                                                   1
                                                )
                                              )
                                               (
                                                break24 (
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
                                            set! ind (
                                              next_index hm ind
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
                        if (
                          is_sparse hm
                        )
                         (
                          begin (
                            size_down hm
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
     (
      define (
        hashmap_len hm
      )
       (
        call/cc (
          lambda (
            ret25
          )
           (
            ret25 (
              hash-table-ref hm "len"
            )
          )
        )
      )
    )
     (
      define (
        hashmap_repr hm
      )
       (
        call/cc (
          lambda (
            ret26
          )
           (
            let (
              (
                out "HashMap("
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
                        i 0
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
                                      < i (
                                        _len (
                                          hash-table-ref hm "buckets"
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            b (
                                              list-ref (
                                                hash-table-ref hm "buckets"
                                              )
                                               i
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              equal? (
                                                hash-table-ref b "state"
                                              )
                                               1
                                            )
                                             (
                                              begin (
                                                if (
                                                  not first
                                                )
                                                 (
                                                  begin (
                                                    set! out (
                                                      string-append out ", "
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    set! first #f
                                                  )
                                                )
                                              )
                                               (
                                                set! out (
                                                  string-append (
                                                    string-append (
                                                      string-append out (
                                                        to-str-space (
                                                          hash-table-ref b "key"
                                                        )
                                                      )
                                                    )
                                                     ": "
                                                  )
                                                   (
                                                    to-str-space (
                                                      hash-table-ref b "val"
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
                        set! out (
                          string-append out ")"
                        )
                      )
                       (
                        ret26 out
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
          hm (
            hashmap_new 5
          )
        )
      )
       (
        begin (
          hashmap_set hm 1 10
        )
         (
          hashmap_set hm 2 20
        )
         (
          hashmap_set hm 3 30
        )
         (
          _display (
            if (
              string? (
                hashmap_repr hm
              )
            )
             (
              hashmap_repr hm
            )
             (
              to-str (
                hashmap_repr hm
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
                  hashmap_get hm 2
                )
              )
            )
             (
              to-str-space (
                hashmap_get hm 2
              )
            )
             (
              to-str (
                to-str-space (
                  hashmap_get hm 2
                )
              )
            )
          )
        )
         (
          newline
        )
         (
          hashmap_del hm 1
        )
         (
          _display (
            if (
              string? (
                hashmap_repr hm
              )
            )
             (
              hashmap_repr hm
            )
             (
              to-str (
                hashmap_repr hm
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
                  hashmap_len hm
                )
              )
            )
             (
              to-str-space (
                hashmap_len hm
              )
            )
             (
              to-str (
                to-str-space (
                  hashmap_len hm
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
