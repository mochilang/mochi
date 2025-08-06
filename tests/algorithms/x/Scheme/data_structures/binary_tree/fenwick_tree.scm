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
      start36 (
        current-jiffy
      )
    )
     (
      jps39 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        fenwick_from_list arr
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                size (
                  _len arr
                )
              )
            )
             (
              begin (
                let (
                  (
                    tree (
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
                                      < i size
                                    )
                                     (
                                      begin (
                                        set! tree (
                                          append tree (
                                            _list (
                                              list-ref arr i
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
                        set! i 1
                      )
                       (
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
                                      < i size
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            j (
                                              fenwick_next i
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              < j size
                                            )
                                             (
                                              begin (
                                                list-set! tree j (
                                                  + (
                                                    list-ref tree j
                                                  )
                                                   (
                                                    list-ref tree i
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
                        ret1 (
                          alist->hash-table (
                            _list (
                              cons "size" size
                            )
                             (
                              cons "tree" tree
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
        fenwick_empty size
      )
       (
        call/cc (
          lambda (
            ret6
          )
           (
            let (
              (
                tree (
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
                                  < i size
                                )
                                 (
                                  begin (
                                    set! tree (
                                      append tree (
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
                          cons "size" size
                        )
                         (
                          cons "tree" tree
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
        fenwick_get_array f
      )
       (
        call/cc (
          lambda (
            ret9
          )
           (
            let (
              (
                arr (
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
                                    hash-table-ref f "size"
                                  )
                                )
                                 (
                                  begin (
                                    set! arr (
                                      append arr (
                                        _list (
                                          list-ref (
                                            hash-table-ref f "tree"
                                          )
                                           i
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
                    set! i (
                      - (
                        hash-table-ref f "size"
                      )
                       1
                    )
                  )
                   (
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
                                  > i 0
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        j (
                                          fenwick_next i
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        if (
                                          < j (
                                            hash-table-ref f "size"
                                          )
                                        )
                                         (
                                          begin (
                                            list-set! arr j (
                                              - (
                                                list-ref arr j
                                              )
                                               (
                                                list-ref arr i
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
                                          - i 1
                                        )
                                      )
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
                    ret9 arr
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
        bit_and a b
      )
       (
        call/cc (
          lambda (
            ret14
          )
           (
            let (
              (
                ua a
              )
            )
             (
              begin (
                let (
                  (
                    ub b
                  )
                )
                 (
                  begin (
                    let (
                      (
                        res 0
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            bit 1
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
                                        
                                      )
                                       (
                                        if (
                                          or (
                                            not (
                                              equal? ua 0
                                            )
                                          )
                                           (
                                            not (
                                              equal? ub 0
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              and (
                                                equal? (
                                                  _mod ua 2
                                                )
                                                 1
                                              )
                                               (
                                                equal? (
                                                  _mod ub 2
                                                )
                                                 1
                                              )
                                            )
                                             (
                                              begin (
                                                set! res (
                                                  + res bit
                                                )
                                              )
                                            )
                                             (
                                              quote (
                                                
                                              )
                                            )
                                          )
                                           (
                                            set! ua (
                                              let (
                                                (
                                                  v17 (
                                                    _div ua 2
                                                  )
                                                )
                                              )
                                               (
                                                cond (
                                                  (
                                                    string? v17
                                                  )
                                                   (
                                                    inexact->exact (
                                                      floor (
                                                        string->number v17
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  (
                                                    boolean? v17
                                                  )
                                                   (
                                                    if v17 1 0
                                                  )
                                                )
                                                 (
                                                  else (
                                                    inexact->exact (
                                                      floor v17
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                           (
                                            set! ub (
                                              let (
                                                (
                                                  v18 (
                                                    _div ub 2
                                                  )
                                                )
                                              )
                                               (
                                                cond (
                                                  (
                                                    string? v18
                                                  )
                                                   (
                                                    inexact->exact (
                                                      floor (
                                                        string->number v18
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  (
                                                    boolean? v18
                                                  )
                                                   (
                                                    if v18 1 0
                                                  )
                                                )
                                                 (
                                                  else (
                                                    inexact->exact (
                                                      floor v18
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                           (
                                            set! bit (
                                              * bit 2
                                            )
                                          )
                                           (
                                            loop15
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
                                  loop15
                                )
                              )
                            )
                          )
                           (
                            ret14 res
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
        low_bit x
      )
       (
        call/cc (
          lambda (
            ret19
          )
           (
            begin (
              if (
                equal? x 0
              )
               (
                begin (
                  ret19 0
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              ret19 (
                - x (
                  bit_and x (
                    - x 1
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
        fenwick_next index
      )
       (
        call/cc (
          lambda (
            ret20
          )
           (
            ret20 (
              _add index (
                low_bit index
              )
            )
          )
        )
      )
    )
     (
      define (
        fenwick_prev index
      )
       (
        call/cc (
          lambda (
            ret21
          )
           (
            ret21 (
              - index (
                low_bit index
              )
            )
          )
        )
      )
    )
     (
      define (
        fenwick_add f index value
      )
       (
        call/cc (
          lambda (
            ret22
          )
           (
            let (
              (
                tree (
                  hash-table-ref f "tree"
                )
              )
            )
             (
              begin (
                if (
                  equal? index 0
                )
                 (
                  begin (
                    list-set! tree 0 (
                      + (
                        list-ref tree 0
                      )
                       value
                    )
                  )
                   (
                    ret22 (
                      alist->hash-table (
                        _list (
                          cons "size" (
                            hash-table-ref f "size"
                          )
                        )
                         (
                          cons "tree" tree
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
                    i index
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
                                    hash-table-ref f "size"
                                  )
                                )
                                 (
                                  begin (
                                    list-set! tree i (
                                      + (
                                        list-ref tree i
                                      )
                                       value
                                    )
                                  )
                                   (
                                    set! i (
                                      fenwick_next i
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
                    ret22 (
                      alist->hash-table (
                        _list (
                          cons "size" (
                            hash-table-ref f "size"
                          )
                        )
                         (
                          cons "tree" tree
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
        fenwick_update f index value
      )
       (
        call/cc (
          lambda (
            ret25
          )
           (
            let (
              (
                current (
                  fenwick_get f index
                )
              )
            )
             (
              begin (
                ret25 (
                  fenwick_add f index (
                    - value current
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
        fenwick_prefix f right
      )
       (
        call/cc (
          lambda (
            ret26
          )
           (
            begin (
              if (
                equal? right 0
              )
               (
                begin (
                  ret26 0
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
                  result (
                    list-ref (
                      hash-table-ref f "tree"
                    )
                     0
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      r (
                        - right 1
                      )
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
                                    > r 0
                                  )
                                   (
                                    begin (
                                      set! result (
                                        + result (
                                          list-ref (
                                            hash-table-ref f "tree"
                                          )
                                           r
                                        )
                                      )
                                    )
                                     (
                                      set! r (
                                        fenwick_prev r
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
                      ret26 result
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
        fenwick_query f left right
      )
       (
        call/cc (
          lambda (
            ret29
          )
           (
            ret29 (
              - (
                fenwick_prefix f right
              )
               (
                fenwick_prefix f left
              )
            )
          )
        )
      )
    )
     (
      define (
        fenwick_get f index
      )
       (
        call/cc (
          lambda (
            ret30
          )
           (
            ret30 (
              fenwick_query f index (
                + index 1
              )
            )
          )
        )
      )
    )
     (
      define (
        fenwick_rank_query f value
      )
       (
        call/cc (
          lambda (
            ret31
          )
           (
            let (
              (
                v (
                  - value (
                    list-ref (
                      hash-table-ref f "tree"
                    )
                     0
                  )
                )
              )
            )
             (
              begin (
                if (
                  < v 0
                )
                 (
                  begin (
                    ret31 (
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
                    j 1
                  )
                )
                 (
                  begin (
                    call/cc (
                      lambda (
                        break33
                      )
                       (
                        letrec (
                          (
                            loop32 (
                              lambda (
                                
                              )
                               (
                                if (
                                  < (
                                    * j 2
                                  )
                                   (
                                    hash-table-ref f "size"
                                  )
                                )
                                 (
                                  begin (
                                    set! j (
                                      * j 2
                                    )
                                  )
                                   (
                                    loop32
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
                          loop32
                        )
                      )
                    )
                  )
                   (
                    let (
                      (
                        i 0
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            jj j
                          )
                        )
                         (
                          begin (
                            call/cc (
                              lambda (
                                break35
                              )
                               (
                                letrec (
                                  (
                                    loop34 (
                                      lambda (
                                        
                                      )
                                       (
                                        if (
                                          > jj 0
                                        )
                                         (
                                          begin (
                                            if (
                                              and (
                                                < (
                                                  + i jj
                                                )
                                                 (
                                                  hash-table-ref f "size"
                                                )
                                              )
                                               (
                                                <= (
                                                  list-ref (
                                                    hash-table-ref f "tree"
                                                  )
                                                   (
                                                    + i jj
                                                  )
                                                )
                                                 v
                                              )
                                            )
                                             (
                                              begin (
                                                set! v (
                                                  - v (
                                                    list-ref (
                                                      hash-table-ref f "tree"
                                                    )
                                                     (
                                                      + i jj
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                set! i (
                                                  + i jj
                                                )
                                              )
                                            )
                                             (
                                              quote (
                                                
                                              )
                                            )
                                          )
                                           (
                                            set! jj (
                                              _div jj 2
                                            )
                                          )
                                           (
                                            loop34
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
                                  loop34
                                )
                              )
                            )
                          )
                           (
                            ret31 i
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
          f_base (
            fenwick_from_list (
              _list 1 2 3 4 5
            )
          )
        )
      )
       (
        begin (
          _display (
            if (
              string? (
                fenwick_get_array f_base
              )
            )
             (
              fenwick_get_array f_base
            )
             (
              to-str (
                fenwick_get_array f_base
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
              f (
                fenwick_from_list (
                  _list 1 2 3 4 5
                )
              )
            )
          )
           (
            begin (
              set! f (
                fenwick_add f 0 1
              )
            )
             (
              set! f (
                fenwick_add f 1 2
              )
            )
             (
              set! f (
                fenwick_add f 2 3
              )
            )
             (
              set! f (
                fenwick_add f 3 4
              )
            )
             (
              set! f (
                fenwick_add f 4 5
              )
            )
             (
              _display (
                if (
                  string? (
                    fenwick_get_array f
                  )
                )
                 (
                  fenwick_get_array f
                )
                 (
                  to-str (
                    fenwick_get_array f
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
                  f2 (
                    fenwick_from_list (
                      _list 1 2 3 4 5
                    )
                  )
                )
              )
               (
                begin (
                  _display (
                    if (
                      string? (
                        fenwick_prefix f2 3
                      )
                    )
                     (
                      fenwick_prefix f2 3
                    )
                     (
                      to-str (
                        fenwick_prefix f2 3
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
                        fenwick_query f2 1 4
                      )
                    )
                     (
                      fenwick_query f2 1 4
                    )
                     (
                      to-str (
                        fenwick_query f2 1 4
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
                      f3 (
                        fenwick_from_list (
                          _list 1 2 0 3 0 5
                        )
                      )
                    )
                  )
                   (
                    begin (
                      _display (
                        if (
                          string? (
                            fenwick_rank_query f3 0
                          )
                        )
                         (
                          fenwick_rank_query f3 0
                        )
                         (
                          to-str (
                            fenwick_rank_query f3 0
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
                            fenwick_rank_query f3 2
                          )
                        )
                         (
                          fenwick_rank_query f3 2
                        )
                         (
                          to-str (
                            fenwick_rank_query f3 2
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
                            fenwick_rank_query f3 1
                          )
                        )
                         (
                          fenwick_rank_query f3 1
                        )
                         (
                          to-str (
                            fenwick_rank_query f3 1
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
                            fenwick_rank_query f3 3
                          )
                        )
                         (
                          fenwick_rank_query f3 3
                        )
                         (
                          to-str (
                            fenwick_rank_query f3 3
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
                            fenwick_rank_query f3 5
                          )
                        )
                         (
                          fenwick_rank_query f3 5
                        )
                         (
                          to-str (
                            fenwick_rank_query f3 5
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
                            fenwick_rank_query f3 6
                          )
                        )
                         (
                          fenwick_rank_query f3 6
                        )
                         (
                          to-str (
                            fenwick_rank_query f3 6
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
                            fenwick_rank_query f3 11
                          )
                        )
                         (
                          fenwick_rank_query f3 11
                        )
                         (
                          to-str (
                            fenwick_rank_query f3 11
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
     (
      let (
        (
          end37 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur38 (
              quotient (
                * (
                  - end37 start36
                )
                 1000000
              )
               jps39
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur38
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
