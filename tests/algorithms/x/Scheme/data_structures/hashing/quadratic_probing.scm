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
      start18 (
        current-jiffy
      )
    )
     (
      jps21 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        create_hash_table size
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
                                  < i size
                                )
                                 (
                                  begin (
                                    set! vals (
                                      append vals (
                                        _list (
                                          quote (
                                            
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
                    ret1 (
                      alist->hash-table (
                        _list (
                          cons "size_table" size
                        )
                         (
                          cons "values" vals
                        )
                         (
                          cons "lim_charge" 0.75
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
        hash_function table key
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            ret4 (
              _mod key (
                hash-table-ref table "size_table"
              )
            )
          )
        )
      )
    )
     (
      define (
        balanced_factor table
      )
       (
        call/cc (
          lambda (
            ret5
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
                                    _len (
                                      hash-table-ref table "values"
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      not (
                                        equal? (
                                          list-ref (
                                            hash-table-ref table "values"
                                          )
                                           i
                                        )
                                         (
                                          quote (
                                            
                                          )
                                        )
                                      )
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
                    ret5 (
                      _div (
                        + 0.0 count
                      )
                       (
                        + 0.0 (
                          hash-table-ref table "size_table"
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
        collision_resolution table key
      )
       (
        call/cc (
          lambda (
            ret8
          )
           (
            let (
              (
                i 1
              )
            )
             (
              begin (
                let (
                  (
                    new_key (
                      hash_function table (
                        + key (
                          * i i
                        )
                      )
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
                                      equal? (
                                        list-ref (
                                          hash-table-ref table "values"
                                        )
                                         new_key
                                      )
                                       (
                                        quote (
                                          
                                        )
                                      )
                                    )
                                  )
                                   (
                                    not (
                                      equal? (
                                        list-ref (
                                          hash-table-ref table "values"
                                        )
                                         new_key
                                      )
                                       key
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    set! i (
                                      + i 1
                                    )
                                  )
                                   (
                                    if (
                                      _ge (
                                        balanced_factor table
                                      )
                                       (
                                        hash-table-ref table "lim_charge"
                                      )
                                    )
                                     (
                                      begin (
                                        ret8 (
                                          hash-table-ref table "size_table"
                                        )
                                      )
                                    )
                                     (
                                      quote (
                                        
                                      )
                                    )
                                  )
                                   (
                                    set! new_key (
                                      hash_function table (
                                        + key (
                                          * i i
                                        )
                                      )
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
                   (
                    ret8 new_key
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
        insert_data table data
      )
       (
        call/cc (
          lambda (
            ret11
          )
           (
            let (
              (
                key (
                  hash_function table data
                )
              )
            )
             (
              begin (
                let (
                  (
                    vals (
                      hash-table-ref table "values"
                    )
                  )
                )
                 (
                  begin (
                    if (
                      equal? (
                        list-ref vals key
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      begin (
                        list-set! vals key data
                      )
                    )
                     (
                      if (
                        equal? (
                          list-ref vals key
                        )
                         data
                      )
                       (
                        begin (
                          hash-table-set! table "values" vals
                        )
                         (
                          ret11 (
                            quote (
                              
                            )
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              new_key (
                                collision_resolution table key
                              )
                            )
                          )
                           (
                            begin (
                              if (
                                and (
                                  _lt new_key (
                                    _len vals
                                  )
                                )
                                 (
                                  equal? (
                                    list-ref vals new_key
                                  )
                                   (
                                    quote (
                                      
                                    )
                                  )
                                )
                              )
                               (
                                begin (
                                  list-set! vals new_key data
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
                   (
                    hash-table-set! table "values" vals
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
        int_to_string n
      )
       (
        call/cc (
          lambda (
            ret12
          )
           (
            begin (
              if (
                equal? n 0
              )
               (
                begin (
                  ret12 "0"
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
                  num n
                )
              )
               (
                begin (
                  let (
                    (
                      neg #f
                    )
                  )
                   (
                    begin (
                      if (
                        < num 0
                      )
                       (
                        begin (
                          set! neg #t
                        )
                         (
                          set! num (
                            - num
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
                          res ""
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
                                        > num 0
                                      )
                                       (
                                        begin (
                                          let (
                                            (
                                              digit (
                                                _mod num 10
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              let (
                                                (
                                                  ch (
                                                    _substring "0123456789" digit (
                                                      + digit 1
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                begin (
                                                  set! res (
                                                    string-append ch res
                                                  )
                                                )
                                                 (
                                                  set! num (
                                                    _div num 10
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
                          if neg (
                            begin (
                              set! res (
                                string-append "-" res
                              )
                            )
                          )
                           (
                            quote (
                              
                            )
                          )
                        )
                         (
                          ret12 res
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
        keys_to_string table
      )
       (
        call/cc (
          lambda (
            ret15
          )
           (
            let (
              (
                result "{"
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
                                          hash-table-ref table "values"
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            v (
                                              list-ref (
                                                hash-table-ref table "values"
                                              )
                                               i
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              not (
                                                equal? v (
                                                  quote (
                                                    
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                if (
                                                  not first
                                                )
                                                 (
                                                  begin (
                                                    set! result (
                                                      string-append result ", "
                                                    )
                                                  )
                                                )
                                                 (
                                                  quote (
                                                    
                                                  )
                                                )
                                              )
                                               (
                                                set! result (
                                                  string-append (
                                                    string-append (
                                                      string-append result (
                                                        int_to_string i
                                                      )
                                                    )
                                                     ": "
                                                  )
                                                   (
                                                    int_to_string v
                                                  )
                                                )
                                              )
                                               (
                                                set! first #f
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
                        set! result (
                          string-append result "}"
                        )
                      )
                       (
                        ret15 result
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
          qp (
            create_hash_table 8
          )
        )
      )
       (
        begin (
          insert_data qp 0
        )
         (
          insert_data qp 999
        )
         (
          insert_data qp 111
        )
         (
          _display (
            if (
              string? (
                keys_to_string qp
              )
            )
             (
              keys_to_string qp
            )
             (
              to-str (
                keys_to_string qp
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
          end19 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur20 (
              quotient (
                * (
                  - end19 start18
                )
                 1000000
              )
               jps21
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur20
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
