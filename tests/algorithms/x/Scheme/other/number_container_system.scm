;; Generated on 2025-08-08 15:49 +0700
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
      start15 (
        current-jiffy
      )
    )
     (
      jps18 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        remove_at xs idx
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
                                  < i (
                                    _len xs
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      not (
                                        equal? i idx
                                      )
                                    )
                                     (
                                      begin (
                                        set! res (
                                          append res (
                                            _list (
                                              list-ref-safe xs i
                                            )
                                          )
                                        )
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
                                   (
                                    loop2
                                  )
                                )
                                 '(
                                  
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
        insert_at xs idx val
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
                                            _list val
                                          )
                                        )
                                      )
                                    )
                                     '(
                                      
                                    )
                                  )
                                   (
                                    set! res (
                                      append res (
                                        _list (
                                          list-ref-safe xs i
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
                                    loop5
                                  )
                                )
                                 '(
                                  
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
                    if (
                      equal? idx (
                        _len xs
                      )
                    )
                     (
                      begin (
                        set! res (
                          append res (
                            _list val
                          )
                        )
                      )
                    )
                     '(
                      
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
        binary_search_delete array item
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            let (
              (
                low 0
              )
            )
             (
              begin (
                let (
                  (
                    high (
                      - (
                        _len array
                      )
                       1
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        arr array
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
                                      <= low high
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            mid (
                                              _div (
                                                + low high
                                              )
                                               2
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              equal? (
                                                list-ref-safe arr mid
                                              )
                                               item
                                            )
                                             (
                                              begin (
                                                set! arr (
                                                  remove_at arr mid
                                                )
                                              )
                                               (
                                                ret7 arr
                                              )
                                            )
                                             (
                                              if (
                                                < (
                                                  list-ref-safe arr mid
                                                )
                                                 item
                                              )
                                               (
                                                begin (
                                                  set! low (
                                                    + mid 1
                                                  )
                                                )
                                              )
                                               (
                                                begin (
                                                  set! high (
                                                    - mid 1
                                                  )
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
                                     '(
                                      
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
                        _display (
                          if (
                            string? "ValueError: Either the item is not in the array or the array was unsorted"
                          )
                           "ValueError: Either the item is not in the array or the array was unsorted" (
                            to-str "ValueError: Either the item is not in the array or the array was unsorted"
                          )
                        )
                      )
                       (
                        newline
                      )
                       (
                        ret7 arr
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
        binary_search_insert array index
      )
       (
        call/cc (
          lambda (
            ret10
          )
           (
            let (
              (
                low 0
              )
            )
             (
              begin (
                let (
                  (
                    high (
                      - (
                        _len array
                      )
                       1
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        arr array
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
                                      <= low high
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            mid (
                                              _div (
                                                + low high
                                              )
                                               2
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              equal? (
                                                list-ref-safe arr mid
                                              )
                                               index
                                            )
                                             (
                                              begin (
                                                set! arr (
                                                  insert_at arr (
                                                    + mid 1
                                                  )
                                                   index
                                                )
                                              )
                                               (
                                                ret10 arr
                                              )
                                            )
                                             (
                                              if (
                                                < (
                                                  list-ref-safe arr mid
                                                )
                                                 index
                                              )
                                               (
                                                begin (
                                                  set! low (
                                                    + mid 1
                                                  )
                                                )
                                              )
                                               (
                                                begin (
                                                  set! high (
                                                    - mid 1
                                                  )
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
                                     '(
                                      
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
                        set! arr (
                          insert_at arr low index
                        )
                      )
                       (
                        ret10 arr
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
        change cont idx num
      )
       (
        call/cc (
          lambda (
            ret13
          )
           (
            let (
              (
                numbermap (
                  hash-table-ref cont "numbermap"
                )
              )
            )
             (
              begin (
                let (
                  (
                    indexmap (
                      hash-table-ref cont "indexmap"
                    )
                  )
                )
                 (
                  begin (
                    if (
                      cond (
                        (
                          string? indexmap
                        )
                         (
                          if (
                            string-contains indexmap idx
                          )
                           #t #f
                        )
                      )
                       (
                        (
                          hash-table? indexmap
                        )
                         (
                          if (
                            hash-table-exists? indexmap idx
                          )
                           #t #f
                        )
                      )
                       (
                        else (
                          if (
                            member idx indexmap
                          )
                           #t #f
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            old (
                              hash-table-ref/default indexmap idx '(
                                
                              )
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                indexes (
                                  hash-table-ref/default numbermap old '(
                                    
                                  )
                                )
                              )
                            )
                             (
                              begin (
                                if (
                                  equal? (
                                    _len indexes
                                  )
                                   1
                                )
                                 (
                                  begin (
                                    hash-table-set! numbermap old (
                                      _list
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    hash-table-set! numbermap old (
                                      binary_search_delete indexes idx
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
                    hash-table-set! indexmap idx num
                  )
                   (
                    if (
                      cond (
                        (
                          string? numbermap
                        )
                         (
                          if (
                            string-contains numbermap num
                          )
                           #t #f
                        )
                      )
                       (
                        (
                          hash-table? numbermap
                        )
                         (
                          if (
                            hash-table-exists? numbermap num
                          )
                           #t #f
                        )
                      )
                       (
                        else (
                          if (
                            member num numbermap
                          )
                           #t #f
                        )
                      )
                    )
                     (
                      begin (
                        hash-table-set! numbermap num (
                          binary_search_insert (
                            hash-table-ref/default numbermap num '(
                              
                            )
                          )
                           idx
                        )
                      )
                    )
                     (
                      begin (
                        hash-table-set! numbermap num (
                          _list idx
                        )
                      )
                    )
                  )
                   (
                    ret13 (
                      alist->hash-table (
                        _list (
                          cons "numbermap" numbermap
                        )
                         (
                          cons "indexmap" indexmap
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
        find cont num
      )
       (
        call/cc (
          lambda (
            ret14
          )
           (
            let (
              (
                numbermap (
                  hash-table-ref cont "numbermap"
                )
              )
            )
             (
              begin (
                if (
                  cond (
                    (
                      string? numbermap
                    )
                     (
                      if (
                        string-contains numbermap num
                      )
                       #t #f
                    )
                  )
                   (
                    (
                      hash-table? numbermap
                    )
                     (
                      if (
                        hash-table-exists? numbermap num
                      )
                       #t #f
                    )
                  )
                   (
                    else (
                      if (
                        member num numbermap
                      )
                       #t #f
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        arr (
                          hash-table-ref/default numbermap num '(
                            
                          )
                        )
                      )
                    )
                     (
                      begin (
                        if (
                          > (
                            _len arr
                          )
                           0
                        )
                         (
                          begin (
                            ret14 (
                              list-ref-safe arr 0
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
                ret14 (
                  - 1
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
          nm (
            alist->hash-table (
              _list
            )
          )
        )
      )
       (
        begin (
          let (
            (
              im (
                alist->hash-table (
                  _list
                )
              )
            )
          )
           (
            begin (
              let (
                (
                  cont (
                    alist->hash-table (
                      _list (
                        cons "numbermap" nm
                      )
                       (
                        cons "indexmap" im
                      )
                    )
                  )
                )
              )
               (
                begin (
                  _display (
                    if (
                      string? (
                        find cont 10
                      )
                    )
                     (
                      find cont 10
                    )
                     (
                      to-str (
                        find cont 10
                      )
                    )
                  )
                )
                 (
                  newline
                )
                 (
                  set! cont (
                    change cont 0 10
                  )
                )
                 (
                  _display (
                    if (
                      string? (
                        find cont 10
                      )
                    )
                     (
                      find cont 10
                    )
                     (
                      to-str (
                        find cont 10
                      )
                    )
                  )
                )
                 (
                  newline
                )
                 (
                  set! cont (
                    change cont 0 20
                  )
                )
                 (
                  _display (
                    if (
                      string? (
                        find cont 10
                      )
                    )
                     (
                      find cont 10
                    )
                     (
                      to-str (
                        find cont 10
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
                        find cont 20
                      )
                    )
                     (
                      find cont 20
                    )
                     (
                      to-str (
                        find cont 20
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
     (
      let (
        (
          end16 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur17 (
              quotient (
                * (
                  - end16 start15
                )
                 1000000
              )
               jps18
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur17
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
