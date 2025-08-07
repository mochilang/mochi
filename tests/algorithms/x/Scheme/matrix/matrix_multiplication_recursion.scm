;; Generated on 2025-08-07 11:54 +0700
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
(define (_div a b) (/ a b))
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
(
  let (
    (
      start17 (
        current-jiffy
      )
    )
     (
      jps20 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        is_square matrix
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                n (
                  _len matrix
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
                                    if (
                                      not (
                                        equal? (
                                          _len (
                                            list-ref matrix i
                                          )
                                        )
                                         n
                                      )
                                    )
                                     (
                                      begin (
                                        ret1 #f
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
                    ret1 #t
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
        matrix_multiply a b
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            let (
              (
                rows (
                  _len a
                )
              )
            )
             (
              begin (
                let (
                  (
                    cols (
                      _len (
                        list-ref b 0
                      )
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        inner (
                          _len b
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            result (
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
                                              < i rows
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    row (
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
                                                                      < j cols
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            sum 0
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                k 0
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
                                                                                              < k inner
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                set! sum (
                                                                                                  + sum (
                                                                                                    * (
                                                                                                      cond (
                                                                                                        (
                                                                                                          string? (
                                                                                                            list-ref a i
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          _substring (
                                                                                                            list-ref a i
                                                                                                          )
                                                                                                           k (
                                                                                                            + k 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? (
                                                                                                            list-ref a i
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref (
                                                                                                            list-ref a i
                                                                                                          )
                                                                                                           k
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref (
                                                                                                            list-ref a i
                                                                                                          )
                                                                                                           k
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      cond (
                                                                                                        (
                                                                                                          string? (
                                                                                                            list-ref b k
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          _substring (
                                                                                                            list-ref b k
                                                                                                          )
                                                                                                           j (
                                                                                                            + j 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? (
                                                                                                            list-ref b k
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref (
                                                                                                            list-ref b k
                                                                                                          )
                                                                                                           j
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref (
                                                                                                            list-ref b k
                                                                                                          )
                                                                                                           j
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                set! k (
                                                                                                  + k 1
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
                                                                                set! row (
                                                                                  append row (
                                                                                    _list sum
                                                                                  )
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
                                                        set! result (
                                                          append result (
                                                            _list row
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
                                ret4 result
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
        multiply i j k a b result n m
      )
       (
        call/cc (
          lambda (
            ret11
          )
           (
            begin (
              if (
                >= i n
              )
               (
                begin (
                  ret11 (
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
                >= j m
              )
               (
                begin (
                  multiply (
                    + i 1
                  )
                   0 0 a b result n m
                )
                 (
                  ret11 (
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
                >= k (
                  _len b
                )
              )
               (
                begin (
                  multiply i (
                    + j 1
                  )
                   0 a b result n m
                )
                 (
                  ret11 (
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
              list-set! (
                list-ref result i
              )
               j (
                + (
                  cond (
                    (
                      string? (
                        list-ref result i
                      )
                    )
                     (
                      _substring (
                        list-ref result i
                      )
                       j (
                        + j 1
                      )
                    )
                  )
                   (
                    (
                      hash-table? (
                        list-ref result i
                      )
                    )
                     (
                      hash-table-ref (
                        list-ref result i
                      )
                       j
                    )
                  )
                   (
                    else (
                      list-ref (
                        list-ref result i
                      )
                       j
                    )
                  )
                )
                 (
                  * (
                    cond (
                      (
                        string? (
                          list-ref a i
                        )
                      )
                       (
                        _substring (
                          list-ref a i
                        )
                         k (
                          + k 1
                        )
                      )
                    )
                     (
                      (
                        hash-table? (
                          list-ref a i
                        )
                      )
                       (
                        hash-table-ref (
                          list-ref a i
                        )
                         k
                      )
                    )
                     (
                      else (
                        list-ref (
                          list-ref a i
                        )
                         k
                      )
                    )
                  )
                   (
                    cond (
                      (
                        string? (
                          list-ref b k
                        )
                      )
                       (
                        _substring (
                          list-ref b k
                        )
                         j (
                          + j 1
                        )
                      )
                    )
                     (
                      (
                        hash-table? (
                          list-ref b k
                        )
                      )
                       (
                        hash-table-ref (
                          list-ref b k
                        )
                         j
                      )
                    )
                     (
                      else (
                        list-ref (
                          list-ref b k
                        )
                         j
                      )
                    )
                  )
                )
              )
            )
             (
              multiply i j (
                + k 1
              )
               a b result n m
            )
          )
        )
      )
    )
     (
      define (
        matrix_multiply_recursive a b
      )
       (
        call/cc (
          lambda (
            ret12
          )
           (
            begin (
              if (
                or (
                  equal? (
                    _len a
                  )
                   0
                )
                 (
                  equal? (
                    _len b
                  )
                   0
                )
              )
               (
                begin (
                  ret12 (
                    _list
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
                or (
                  or (
                    not (
                      equal? (
                        _len a
                      )
                       (
                        _len b
                      )
                    )
                  )
                   (
                    not (
                      is_square a
                    )
                  )
                )
                 (
                  not (
                    is_square b
                  )
                )
              )
               (
                begin (
                  panic "Invalid matrix dimensions"
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
                  n (
                    _len a
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      m (
                        _len (
                          list-ref b 0
                        )
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          result (
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
                                            < i n
                                          )
                                           (
                                            begin (
                                              let (
                                                (
                                                  row (
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
                                                                    < j m
                                                                  )
                                                                   (
                                                                    begin (
                                                                      set! row (
                                                                        append row (
                                                                          _list 0
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      set! j (
                                                                        + j 1
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
                                                      set! result (
                                                        append result (
                                                          _list row
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
                              multiply 0 0 0 a b result n m
                            )
                             (
                              ret12 result
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
      let (
        (
          matrix_1_to_4 (
            _list (
              _list 1 2
            )
             (
              _list 3 4
            )
          )
        )
      )
       (
        begin (
          let (
            (
              matrix_5_to_8 (
                _list (
                  _list 5 6
                )
                 (
                  _list 7 8
                )
              )
            )
          )
           (
            begin (
              let (
                (
                  matrix_count_up (
                    _list (
                      _list 1 2 3 4
                    )
                     (
                      _list 5 6 7 8
                    )
                     (
                      _list 9 10 11 12
                    )
                     (
                      _list 13 14 15 16
                    )
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      matrix_unordered (
                        _list (
                          _list 5 8 1 2
                        )
                         (
                          _list 6 7 3 0
                        )
                         (
                          _list 4 5 9 1
                        )
                         (
                          _list 2 6 10 14
                        )
                      )
                    )
                  )
                   (
                    begin (
                      _display (
                        if (
                          string? (
                            matrix_multiply_recursive matrix_1_to_4 matrix_5_to_8
                          )
                        )
                         (
                          matrix_multiply_recursive matrix_1_to_4 matrix_5_to_8
                        )
                         (
                          to-str (
                            matrix_multiply_recursive matrix_1_to_4 matrix_5_to_8
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
                            matrix_multiply_recursive matrix_count_up matrix_unordered
                          )
                        )
                         (
                          matrix_multiply_recursive matrix_count_up matrix_unordered
                        )
                         (
                          to-str (
                            matrix_multiply_recursive matrix_count_up matrix_unordered
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
          end18 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur19 (
              quotient (
                * (
                  - end18 start17
                )
                 1000000
              )
               jps20
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur19
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
