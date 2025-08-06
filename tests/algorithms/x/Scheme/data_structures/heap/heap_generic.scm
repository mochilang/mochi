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
      start21 (
        current-jiffy
      )
    )
     (
      jps24 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        new_heap key
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
                  cons "arr" (
                    _list
                  )
                )
                 (
                  cons "pos_map" (
                    alist->hash-table (
                      _list
                    )
                  )
                )
                 (
                  cons "size" 0
                )
                 (
                  cons "key" key
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        parent i
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            begin (
              if (
                > i 0
              )
               (
                begin (
                  ret2 (
                    _div (
                      - i 1
                    )
                     2
                  )
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              ret2 (
                - 1
              )
            )
          )
        )
      )
    )
     (
      define (
        left i size
      )
       (
        call/cc (
          lambda (
            ret3
          )
           (
            let (
              (
                l (
                  + (
                    * 2 i
                  )
                   1
                )
              )
            )
             (
              begin (
                if (
                  < l size
                )
                 (
                  begin (
                    ret3 l
                  )
                )
                 (
                  quote (
                    
                  )
                )
              )
               (
                ret3 (
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
        right i size
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            let (
              (
                r (
                  + (
                    * 2 i
                  )
                   2
                )
              )
            )
             (
              begin (
                if (
                  < r size
                )
                 (
                  begin (
                    ret4 r
                  )
                )
                 (
                  quote (
                    
                  )
                )
              )
               (
                ret4 (
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
        swap h i j
      )
       (
        call/cc (
          lambda (
            ret5
          )
           (
            let (
              (
                arr (
                  hash-table-ref h "arr"
                )
              )
            )
             (
              begin (
                let (
                  (
                    item_i (
                      cond (
                        (
                          string? (
                            list-ref arr i
                          )
                        )
                         (
                          _substring (
                            list-ref arr i
                          )
                           0 (
                            + 0 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? (
                            list-ref arr i
                          )
                        )
                         (
                          hash-table-ref (
                            list-ref arr i
                          )
                           0
                        )
                      )
                       (
                        else (
                          list-ref (
                            list-ref arr i
                          )
                           0
                        )
                      )
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        item_j (
                          cond (
                            (
                              string? (
                                list-ref arr j
                              )
                            )
                             (
                              _substring (
                                list-ref arr j
                              )
                               0 (
                                + 0 1
                              )
                            )
                          )
                           (
                            (
                              hash-table? (
                                list-ref arr j
                              )
                            )
                             (
                              hash-table-ref (
                                list-ref arr j
                              )
                               0
                            )
                          )
                           (
                            else (
                              list-ref (
                                list-ref arr j
                              )
                               0
                            )
                          )
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            pm (
                              hash-table-ref h "pos_map"
                            )
                          )
                        )
                         (
                          begin (
                            hash-table-set! pm item_i (
                              + j 1
                            )
                          )
                           (
                            hash-table-set! pm item_j (
                              + i 1
                            )
                          )
                           (
                            hash-table-set! h "pos_map" pm
                          )
                           (
                            let (
                              (
                                tmp (
                                  list-ref arr i
                                )
                              )
                            )
                             (
                              begin (
                                list-set! arr i (
                                  list-ref arr j
                                )
                              )
                               (
                                list-set! arr j tmp
                              )
                               (
                                hash-table-set! h "arr" arr
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
        cmp h i j
      )
       (
        call/cc (
          lambda (
            ret6
          )
           (
            let (
              (
                arr (
                  hash-table-ref h "arr"
                )
              )
            )
             (
              begin (
                ret6 (
                  < (
                    cond (
                      (
                        string? (
                          list-ref arr i
                        )
                      )
                       (
                        _substring (
                          list-ref arr i
                        )
                         1 (
                          + 1 1
                        )
                      )
                    )
                     (
                      (
                        hash-table? (
                          list-ref arr i
                        )
                      )
                       (
                        hash-table-ref (
                          list-ref arr i
                        )
                         1
                      )
                    )
                     (
                      else (
                        list-ref (
                          list-ref arr i
                        )
                         1
                      )
                    )
                  )
                   (
                    cond (
                      (
                        string? (
                          list-ref arr j
                        )
                      )
                       (
                        _substring (
                          list-ref arr j
                        )
                         1 (
                          + 1 1
                        )
                      )
                    )
                     (
                      (
                        hash-table? (
                          list-ref arr j
                        )
                      )
                       (
                        hash-table-ref (
                          list-ref arr j
                        )
                         1
                      )
                    )
                     (
                      else (
                        list-ref (
                          list-ref arr j
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
      define (
        get_valid_parent h i
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            let (
              (
                vp i
              )
            )
             (
              begin (
                let (
                  (
                    l (
                      left i (
                        hash-table-ref h "size"
                      )
                    )
                  )
                )
                 (
                  begin (
                    if (
                      and (
                        not (
                          equal? l (
                            - 0 1
                          )
                        )
                      )
                       (
                        eq? (
                          cmp h l vp
                        )
                         #f
                      )
                    )
                     (
                      begin (
                        set! vp l
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
                        r (
                          right i (
                            hash-table-ref h "size"
                          )
                        )
                      )
                    )
                     (
                      begin (
                        if (
                          and (
                            not (
                              equal? r (
                                - 0 1
                              )
                            )
                          )
                           (
                            eq? (
                              cmp h r vp
                            )
                             #f
                          )
                        )
                         (
                          begin (
                            set! vp r
                          )
                        )
                         (
                          quote (
                            
                          )
                        )
                      )
                       (
                        ret7 vp
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
        heapify_up h index
      )
       (
        call/cc (
          lambda (
            ret8
          )
           (
            let (
              (
                idx index
              )
            )
             (
              begin (
                let (
                  (
                    p (
                      parent idx
                    )
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
                                  and (
                                    not (
                                      equal? p (
                                        - 0 1
                                      )
                                    )
                                  )
                                   (
                                    eq? (
                                      cmp h idx p
                                    )
                                     #f
                                  )
                                )
                                 (
                                  begin (
                                    swap h idx p
                                  )
                                   (
                                    set! idx p
                                  )
                                   (
                                    set! p (
                                      parent p
                                    )
                                  )
                                   (
                                    loop9
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
                          loop9
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
        heapify_down h index
      )
       (
        call/cc (
          lambda (
            ret11
          )
           (
            let (
              (
                idx index
              )
            )
             (
              begin (
                let (
                  (
                    vp (
                      get_valid_parent h idx
                    )
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
                                  not (
                                    equal? vp idx
                                  )
                                )
                                 (
                                  begin (
                                    swap h idx vp
                                  )
                                   (
                                    set! idx vp
                                  )
                                   (
                                    set! vp (
                                      get_valid_parent h idx
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
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        update_item h item item_value
      )
       (
        call/cc (
          lambda (
            ret14
          )
           (
            let (
              (
                pm (
                  hash-table-ref h "pos_map"
                )
              )
            )
             (
              begin (
                if (
                  equal? (
                    hash-table-ref/default pm item (
                      quote (
                        
                      )
                    )
                  )
                   0
                )
                 (
                  begin (
                    ret14 (
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
                let (
                  (
                    index (
                      - (
                        hash-table-ref/default pm item (
                          quote (
                            
                          )
                        )
                      )
                       1
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        arr (
                          hash-table-ref h "arr"
                        )
                      )
                    )
                     (
                      begin (
                        list-set! arr index (
                          _list item (
                            (
                              hash-table-ref h "key"
                            )
                             item_value
                          )
                        )
                      )
                       (
                        hash-table-set! h "arr" arr
                      )
                       (
                        hash-table-set! h "pos_map" pm
                      )
                       (
                        heapify_up h index
                      )
                       (
                        heapify_down h index
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
        delete_item h item
      )
       (
        call/cc (
          lambda (
            ret15
          )
           (
            let (
              (
                pm (
                  hash-table-ref h "pos_map"
                )
              )
            )
             (
              begin (
                if (
                  equal? (
                    hash-table-ref/default pm item (
                      quote (
                        
                      )
                    )
                  )
                   0
                )
                 (
                  begin (
                    ret15 (
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
                let (
                  (
                    index (
                      - (
                        hash-table-ref/default pm item (
                          quote (
                            
                          )
                        )
                      )
                       1
                    )
                  )
                )
                 (
                  begin (
                    hash-table-set! pm item 0
                  )
                   (
                    let (
                      (
                        arr (
                          hash-table-ref h "arr"
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            last_index (
                              - (
                                hash-table-ref h "size"
                              )
                               1
                            )
                          )
                        )
                         (
                          begin (
                            if (
                              not (
                                equal? index last_index
                              )
                            )
                             (
                              begin (
                                list-set! arr index (
                                  list-ref arr last_index
                                )
                              )
                               (
                                let (
                                  (
                                    moved (
                                      cond (
                                        (
                                          string? (
                                            list-ref arr index
                                          )
                                        )
                                         (
                                          _substring (
                                            list-ref arr index
                                          )
                                           0 (
                                            + 0 1
                                          )
                                        )
                                      )
                                       (
                                        (
                                          hash-table? (
                                            list-ref arr index
                                          )
                                        )
                                         (
                                          hash-table-ref (
                                            list-ref arr index
                                          )
                                           0
                                        )
                                      )
                                       (
                                        else (
                                          list-ref (
                                            list-ref arr index
                                          )
                                           0
                                        )
                                      )
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    hash-table-set! pm moved (
                                      + index 1
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
                            hash-table-set! h "size" (
                              - (
                                hash-table-ref h "size"
                              )
                               1
                            )
                          )
                           (
                            hash-table-set! h "arr" arr
                          )
                           (
                            hash-table-set! h "pos_map" pm
                          )
                           (
                            if (
                              > (
                                hash-table-ref h "size"
                              )
                               index
                            )
                             (
                              begin (
                                heapify_up h index
                              )
                               (
                                heapify_down h index
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
     (
      define (
        insert_item h item item_value
      )
       (
        call/cc (
          lambda (
            ret16
          )
           (
            let (
              (
                arr (
                  hash-table-ref h "arr"
                )
              )
            )
             (
              begin (
                let (
                  (
                    arr_len (
                      _len arr
                    )
                  )
                )
                 (
                  begin (
                    if (
                      equal? arr_len (
                        hash-table-ref h "size"
                      )
                    )
                     (
                      begin (
                        set! arr (
                          append arr (
                            _list (
                              _list item (
                                (
                                  hash-table-ref h "key"
                                )
                                 item_value
                              )
                            )
                          )
                        )
                      )
                    )
                     (
                      begin (
                        list-set! arr (
                          hash-table-ref h "size"
                        )
                         (
                          _list item (
                            (
                              hash-table-ref h "key"
                            )
                             item_value
                          )
                        )
                      )
                    )
                  )
                   (
                    let (
                      (
                        pm (
                          hash-table-ref h "pos_map"
                        )
                      )
                    )
                     (
                      begin (
                        hash-table-set! pm item (
                          + (
                            hash-table-ref h "size"
                          )
                           1
                        )
                      )
                       (
                        hash-table-set! h "size" (
                          + (
                            hash-table-ref h "size"
                          )
                           1
                        )
                      )
                       (
                        hash-table-set! h "arr" arr
                      )
                       (
                        hash-table-set! h "pos_map" pm
                      )
                       (
                        heapify_up h (
                          - (
                            hash-table-ref h "size"
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
      define (
        get_top h
      )
       (
        call/cc (
          lambda (
            ret17
          )
           (
            let (
              (
                arr (
                  hash-table-ref h "arr"
                )
              )
            )
             (
              begin (
                if (
                  > (
                    hash-table-ref h "size"
                  )
                   0
                )
                 (
                  begin (
                    ret17 (
                      list-ref arr 0
                    )
                  )
                )
                 (
                  quote (
                    
                  )
                )
              )
               (
                ret17 (
                  _list
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        extract_top h
      )
       (
        call/cc (
          lambda (
            ret18
          )
           (
            let (
              (
                top (
                  get_top h
                )
              )
            )
             (
              begin (
                if (
                  > (
                    _len top
                  )
                   0
                )
                 (
                  begin (
                    delete_item h (
                      cond (
                        (
                          string? top
                        )
                         (
                          _substring top 0 (
                            + 0 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? top
                        )
                         (
                          hash-table-ref top 0
                        )
                      )
                       (
                        else (
                          list-ref top 0
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
                ret18 top
              )
            )
          )
        )
      )
    )
     (
      define (
        identity x
      )
       (
        call/cc (
          lambda (
            ret19
          )
           (
            ret19 x
          )
        )
      )
    )
     (
      define (
        negate x
      )
       (
        call/cc (
          lambda (
            ret20
          )
           (
            ret20 (
              - 0 x
            )
          )
        )
      )
    )
     (
      let (
        (
          h (
            new_heap identity
          )
        )
      )
       (
        begin (
          insert_item h 5 34
        )
         (
          insert_item h 6 31
        )
         (
          insert_item h 7 37
        )
         (
          _display (
            if (
              string? (
                to-str-space (
                  get_top h
                )
              )
            )
             (
              to-str-space (
                get_top h
              )
            )
             (
              to-str (
                to-str-space (
                  get_top h
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
                  extract_top h
                )
              )
            )
             (
              to-str-space (
                extract_top h
              )
            )
             (
              to-str (
                to-str-space (
                  extract_top h
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
                  extract_top h
                )
              )
            )
             (
              to-str-space (
                extract_top h
              )
            )
             (
              to-str (
                to-str-space (
                  extract_top h
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
                  extract_top h
                )
              )
            )
             (
              to-str-space (
                extract_top h
              )
            )
             (
              to-str (
                to-str-space (
                  extract_top h
                )
              )
            )
          )
        )
         (
          newline
        )
         (
          set! h (
            new_heap negate
          )
        )
         (
          insert_item h 5 34
        )
         (
          insert_item h 6 31
        )
         (
          insert_item h 7 37
        )
         (
          _display (
            if (
              string? (
                to-str-space (
                  get_top h
                )
              )
            )
             (
              to-str-space (
                get_top h
              )
            )
             (
              to-str (
                to-str-space (
                  get_top h
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
                  extract_top h
                )
              )
            )
             (
              to-str-space (
                extract_top h
              )
            )
             (
              to-str (
                to-str-space (
                  extract_top h
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
                  extract_top h
                )
              )
            )
             (
              to-str-space (
                extract_top h
              )
            )
             (
              to-str (
                to-str-space (
                  extract_top h
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
                  extract_top h
                )
              )
            )
             (
              to-str-space (
                extract_top h
              )
            )
             (
              to-str (
                to-str-space (
                  extract_top h
                )
              )
            )
          )
        )
         (
          newline
        )
         (
          insert_item h 8 45
        )
         (
          insert_item h 9 40
        )
         (
          insert_item h 10 50
        )
         (
          _display (
            if (
              string? (
                to-str-space (
                  get_top h
                )
              )
            )
             (
              to-str-space (
                get_top h
              )
            )
             (
              to-str (
                to-str-space (
                  get_top h
                )
              )
            )
          )
        )
         (
          newline
        )
         (
          update_item h 10 30
        )
         (
          _display (
            if (
              string? (
                to-str-space (
                  get_top h
                )
              )
            )
             (
              to-str-space (
                get_top h
              )
            )
             (
              to-str (
                to-str-space (
                  get_top h
                )
              )
            )
          )
        )
         (
          newline
        )
         (
          delete_item h 10
        )
         (
          _display (
            if (
              string? (
                to-str-space (
                  get_top h
                )
              )
            )
             (
              to-str-space (
                get_top h
              )
            )
             (
              to-str (
                to-str-space (
                  get_top h
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
          end22 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur23 (
              quotient (
                * (
                  - end22 start21
                )
                 1000000
              )
               jps24
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur23
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
