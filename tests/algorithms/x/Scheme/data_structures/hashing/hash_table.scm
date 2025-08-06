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
      start35 (
        current-jiffy
      )
    )
     (
      jps38 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        repeat_int n val
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                res (
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
                                    set! res (
                                      append res (
                                        _list val
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
                    ret1 res
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
        repeat_bool n val
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
                                  < i n
                                )
                                 (
                                  begin (
                                    set! res (
                                      append res (
                                        _list val
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
        set_int xs idx value
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            let (
              (
                res (
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
                                  < i (
                                    _len xs
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      equal? i idx
                                    )
                                     (
                                      begin (
                                        set! res (
                                          append res (
                                            _list value
                                          )
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        set! res (
                                          append res (
                                            _list (
                                              list-ref xs i
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
                    ret7 res
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
        set_bool xs idx value
      )
       (
        call/cc (
          lambda (
            ret10
          )
           (
            let (
              (
                res (
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
                                    _len xs
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      equal? i idx
                                    )
                                     (
                                      begin (
                                        set! res (
                                          append res (
                                            _list value
                                          )
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        set! res (
                                          append res (
                                            _list (
                                              list-ref xs i
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
                    ret10 res
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
        create_table size_table charge_factor lim_charge
      )
       (
        call/cc (
          lambda (
            ret13
          )
           (
            ret13 (
              alist->hash-table (
                _list (
                  cons "size_table" size_table
                )
                 (
                  cons "values" (
                    repeat_int size_table 0
                  )
                )
                 (
                  cons "filled" (
                    repeat_bool size_table #f
                  )
                )
                 (
                  cons "charge_factor" charge_factor
                )
                 (
                  cons "lim_charge" lim_charge
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
            ret14
          )
           (
            let (
              (
                k (
                  _mod key (
                    hash-table-ref ht "size_table"
                  )
                )
              )
            )
             (
              begin (
                if (
                  < k 0
                )
                 (
                  begin (
                    set! k (
                      + k (
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
                ret14 k
              )
            )
          )
        )
      )
    )
     (
      define (
        is_prime n
      )
       (
        call/cc (
          lambda (
            ret15
          )
           (
            begin (
              if (
                < n 2
              )
               (
                begin (
                  ret15 #f
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
                  _mod n 2
                )
                 0
              )
               (
                begin (
                  ret15 (
                    equal? n 2
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
                  i 3
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
                                <= (
                                  * i i
                                )
                                 n
                              )
                               (
                                begin (
                                  if (
                                    equal? (
                                      _mod n i
                                    )
                                     0
                                  )
                                   (
                                    begin (
                                      ret15 #f
                                    )
                                  )
                                   (
                                    quote (
                                      
                                    )
                                  )
                                )
                                 (
                                  set! i (
                                    + i 2
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
                  ret15 #t
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        next_prime value factor
      )
       (
        call/cc (
          lambda (
            ret18
          )
           (
            let (
              (
                candidate (
                  + (
                    * value factor
                  )
                   1
                )
              )
            )
             (
              begin (
                call/cc (
                  lambda (
                    break20
                  )
                   (
                    letrec (
                      (
                        loop19 (
                          lambda (
                            
                          )
                           (
                            if (
                              not (
                                is_prime candidate
                              )
                            )
                             (
                              begin (
                                set! candidate (
                                  + candidate 1
                                )
                              )
                               (
                                loop19
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
                      loop19
                    )
                  )
                )
              )
               (
                ret18 candidate
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
            ret21
          )
           (
            let (
              (
                new_values (
                  set_int (
                    hash-table-ref ht "values"
                  )
                   key data
                )
              )
            )
             (
              begin (
                let (
                  (
                    new_filled (
                      set_bool (
                        hash-table-ref ht "filled"
                      )
                       key #t
                    )
                  )
                )
                 (
                  begin (
                    ret21 (
                      alist->hash-table (
                        _list (
                          cons "size_table" (
                            hash-table-ref ht "size_table"
                          )
                        )
                         (
                          cons "values" new_values
                        )
                         (
                          cons "filled" new_filled
                        )
                         (
                          cons "charge_factor" (
                            hash-table-ref ht "charge_factor"
                          )
                        )
                         (
                          cons "lim_charge" (
                            hash-table-ref ht "lim_charge"
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
        collision_resolution ht key
      )
       (
        call/cc (
          lambda (
            ret22
          )
           (
            let (
              (
                new_key (
                  hash_function ht (
                    + key 1
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
                                  list-ref (
                                    hash-table-ref ht "filled"
                                  )
                                   new_key
                                )
                                 (
                                  begin (
                                    set! new_key (
                                      hash_function ht (
                                        _add new_key 1
                                      )
                                    )
                                  )
                                   (
                                    set! steps (
                                      + steps 1
                                    )
                                  )
                                   (
                                    if (
                                      >= steps (
                                        hash-table-ref ht "size_table"
                                      )
                                    )
                                     (
                                      begin (
                                        ret22 (
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
                    ret22 new_key
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
        rehashing ht
      )
       (
        call/cc (
          lambda (
            ret25
          )
           (
            let (
              (
                survivors (
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
                                  < i (
                                    _len (
                                      hash-table-ref ht "values"
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      list-ref (
                                        hash-table-ref ht "filled"
                                      )
                                       i
                                    )
                                     (
                                      begin (
                                        set! survivors (
                                          append survivors (
                                            _list (
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
                    let (
                      (
                        new_size (
                          next_prime (
                            hash-table-ref ht "size_table"
                          )
                           2
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            new_ht (
                              create_table new_size (
                                hash-table-ref ht "charge_factor"
                              )
                               (
                                hash-table-ref ht "lim_charge"
                              )
                            )
                          )
                        )
                         (
                          begin (
                            set! i 0
                          )
                           (
                            call/cc (
                              lambda (
                                break29
                              )
                               (
                                letrec (
                                  (
                                    loop28 (
                                      lambda (
                                        
                                      )
                                       (
                                        if (
                                          < i (
                                            _len survivors
                                          )
                                        )
                                         (
                                          begin (
                                            set! new_ht (
                                              insert_data new_ht (
                                                list-ref survivors i
                                              )
                                            )
                                          )
                                           (
                                            set! i (
                                              + i 1
                                            )
                                          )
                                           (
                                            loop28
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
                                  loop28
                                )
                              )
                            )
                          )
                           (
                            ret25 new_ht
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
        insert_data ht data
      )
       (
        call/cc (
          lambda (
            ret30
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
                  not (
                    list-ref (
                      hash-table-ref ht "filled"
                    )
                     key
                  )
                )
                 (
                  begin (
                    ret30 (
                      set_value ht key data
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
                  equal? (
                    list-ref (
                      hash-table-ref ht "values"
                    )
                     key
                  )
                   data
                )
                 (
                  begin (
                    ret30 ht
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
                      collision_resolution ht key
                    )
                  )
                )
                 (
                  begin (
                    if (
                      _ge new_key 0
                    )
                     (
                      begin (
                        ret30 (
                          set_value ht new_key data
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
                        resized (
                          rehashing ht
                        )
                      )
                    )
                     (
                      begin (
                        ret30 (
                          insert_data resized data
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
        keys ht
      )
       (
        call/cc (
          lambda (
            ret31
          )
           (
            let (
              (
                res (
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
                                  < i (
                                    _len (
                                      hash-table-ref ht "values"
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      list-ref (
                                        hash-table-ref ht "filled"
                                      )
                                       i
                                    )
                                     (
                                      begin (
                                        set! res (
                                          append res (
                                            _list (
                                              _list i (
                                                list-ref (
                                                  hash-table-ref ht "values"
                                                )
                                                 i
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
                    ret31 res
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
            ret34
          )
           (
            let (
              (
                ht (
                  create_table 3 1 0.75
                )
              )
            )
             (
              begin (
                set! ht (
                  insert_data ht 17
                )
              )
               (
                set! ht (
                  insert_data ht 18
                )
              )
               (
                set! ht (
                  insert_data ht 99
                )
              )
               (
                _display (
                  if (
                    string? (
                      keys ht
                    )
                  )
                   (
                    keys ht
                  )
                   (
                    to-str (
                      keys ht
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
          end36 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur37 (
              quotient (
                * (
                  - end36 start35
                )
                 1000000
              )
               jps38
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur37
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
