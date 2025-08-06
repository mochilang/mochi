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
      start20 (
        current-jiffy
      )
    )
     (
      jps23 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        make_table size_table charge_factor
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                vals (
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
                                  < i size_table
                                )
                                 (
                                  begin (
                                    set! vals (
                                      append vals (
                                        _list (
                                          _list
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
                    ret1 (
                      alist->hash-table (
                        _list (
                          cons "size_table" size_table
                        )
                         (
                          cons "charge_factor" charge_factor
                        )
                         (
                          cons "values" vals
                        )
                         (
                          cons "keys" (
                            alist->hash-table (
                              _list
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
        hash_function ht key
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
                  _mod key (
                    hash-table-ref ht "size_table"
                  )
                )
              )
            )
             (
              begin (
                if (
                  < res 0
                )
                 (
                  begin (
                    set! res (
                      + res (
                        hash-table-ref ht "size_table"
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
                ret4 res
              )
            )
          )
        )
      )
    )
     (
      define (
        prepend lst value
      )
       (
        call/cc (
          lambda (
            ret5
          )
           (
            let (
              (
                result (
                  _list value
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
                        break7
                      )
                       (
                        letrec (
                          (
                            loop6 (
                              lambda (
                                
                              )
                               (
                                if (
                                  < i (
                                    _len lst
                                  )
                                )
                                 (
                                  begin (
                                    set! result (
                                      append result (
                                        _list (
                                          list-ref lst i
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
                                    loop6
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
                          loop6
                        )
                      )
                    )
                  )
                   (
                    ret5 result
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
        set_value ht key data
      )
       (
        call/cc (
          lambda (
            ret8
          )
           (
            let (
              (
                current (
                  list-ref (
                    hash-table-ref ht "values"
                  )
                   key
                )
              )
            )
             (
              begin (
                let (
                  (
                    updated (
                      prepend current data
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        vals (
                          hash-table-ref ht "values"
                        )
                      )
                    )
                     (
                      begin (
                        list-set! vals key updated
                      )
                       (
                        hash-table-set! ht "values" vals
                      )
                       (
                        let (
                          (
                            ks (
                              hash-table-ref ht "keys"
                            )
                          )
                        )
                         (
                          begin (
                            hash-table-set! ks key updated
                          )
                           (
                            hash-table-set! ht "keys" ks
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
        count_empty ht
      )
       (
        call/cc (
          lambda (
            ret9
          )
           (
            let (
              (
                count 0
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
                                    _len (
                                      hash-table-ref ht "values"
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      equal? (
                                        _len (
                                          list-ref (
                                            hash-table-ref ht "values"
                                          )
                                           i
                                        )
                                      )
                                       0
                                    )
                                     (
                                      begin (
                                        set! count (
                                          + count 1
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
                    ret9 count
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
        balanced_factor ht
      )
       (
        call/cc (
          lambda (
            ret12
          )
           (
            let (
              (
                total 0
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
                                    _len (
                                      hash-table-ref ht "values"
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    set! total (
                                      + total (
                                        - (
                                          hash-table-ref ht "charge_factor"
                                        )
                                         (
                                          _len (
                                            list-ref (
                                              hash-table-ref ht "values"
                                            )
                                             i
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
                      * (
                        _div (
                          + 0.0 total
                        )
                         (
                          + 0.0 (
                            hash-table-ref ht "size_table"
                          )
                        )
                      )
                       (
                        + 0.0 (
                          hash-table-ref ht "charge_factor"
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
        collision_resolution ht key
      )
       (
        call/cc (
          lambda (
            ret15
          )
           (
            begin (
              if (
                not (
                  and (
                    equal? (
                      _len (
                        list-ref (
                          hash-table-ref ht "values"
                        )
                         key
                      )
                    )
                     (
                      hash-table-ref ht "charge_factor"
                    )
                  )
                   (
                    equal? (
                      count_empty ht
                    )
                     0
                  )
                )
              )
               (
                begin (
                  ret15 key
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
                  new_key (
                    _mod (
                      + key 1
                    )
                     (
                      hash-table-ref ht "size_table"
                    )
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      steps 0
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
                                    and (
                                      equal? (
                                        _len (
                                          list-ref (
                                            hash-table-ref ht "values"
                                          )
                                           new_key
                                        )
                                      )
                                       (
                                        hash-table-ref ht "charge_factor"
                                      )
                                    )
                                     (
                                      < steps (
                                        - (
                                          hash-table-ref ht "size_table"
                                        )
                                         1
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      set! new_key (
                                        _mod (
                                          + new_key 1
                                        )
                                         (
                                          hash-table-ref ht "size_table"
                                        )
                                      )
                                    )
                                     (
                                      set! steps (
                                        + steps 1
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
                     (
                      if (
                        < (
                          _len (
                            list-ref (
                              hash-table-ref ht "values"
                            )
                             new_key
                          )
                        )
                         (
                          hash-table-ref ht "charge_factor"
                        )
                      )
                       (
                        begin (
                          ret15 new_key
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      ret15 (
                        - 1
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
        insert ht data
      )
       (
        call/cc (
          lambda (
            ret18
          )
           (
            let (
              (
                key (
                  hash_function ht data
                )
              )
            )
             (
              begin (
                if (
                  or (
                    equal? (
                      _len (
                        list-ref (
                          hash-table-ref ht "values"
                        )
                         key
                      )
                    )
                     0
                  )
                   (
                    < (
                      _len (
                        list-ref (
                          hash-table-ref ht "values"
                        )
                         key
                      )
                    )
                     (
                      hash-table-ref ht "charge_factor"
                    )
                  )
                )
                 (
                  begin (
                    set_value ht key data
                  )
                   (
                    ret18 (
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
                    dest (
                      collision_resolution ht key
                    )
                  )
                )
                 (
                  begin (
                    if (
                      _ge dest 0
                    )
                     (
                      begin (
                        set_value ht dest data
                      )
                    )
                     (
                      begin (
                        _display (
                          if (
                            string? "table full"
                          )
                           "table full" (
                            to-str "table full"
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
     (
      define (
        main
      )
       (
        call/cc (
          lambda (
            ret19
          )
           (
            let (
              (
                ht (
                  make_table 3 2
                )
              )
            )
             (
              begin (
                insert ht 10
              )
               (
                insert ht 20
              )
               (
                insert ht 30
              )
               (
                insert ht 40
              )
               (
                insert ht 50
              )
               (
                _display (
                  if (
                    string? (
                      to-str-space (
                        hash-table-ref ht "values"
                      )
                    )
                  )
                   (
                    to-str-space (
                      hash-table-ref ht "values"
                    )
                  )
                   (
                    to-str (
                      to-str-space (
                        hash-table-ref ht "values"
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
                        balanced_factor ht
                      )
                    )
                  )
                   (
                    to-str-space (
                      balanced_factor ht
                    )
                  )
                   (
                    to-str (
                      to-str-space (
                        balanced_factor ht
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
     (
      main
    )
     (
      let (
        (
          end21 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur22 (
              quotient (
                * (
                  - end21 start20
                )
                 1000000
              )
               jps23
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur22
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
