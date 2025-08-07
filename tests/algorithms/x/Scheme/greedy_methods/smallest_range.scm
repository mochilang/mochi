;; Generated on 2025-08-07 14:57 +0700
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
      let (
        (
          INF 1000000000
        )
      )
       (
        begin (
          define (
            smallest_range nums
          )
           (
            call/cc (
              lambda (
                ret1
              )
               (
                let (
                  (
                    heap (
                      _list
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        current_max (
                          - INF
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
                                            _len nums
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                first_val (
                                                  cond (
                                                    (
                                                      string? (
                                                        list-ref-safe nums i
                                                      )
                                                    )
                                                     (
                                                      _substring (
                                                        list-ref-safe nums i
                                                      )
                                                       0 (
                                                        + 0 1
                                                      )
                                                    )
                                                  )
                                                   (
                                                    (
                                                      hash-table? (
                                                        list-ref-safe nums i
                                                      )
                                                    )
                                                     (
                                                      hash-table-ref (
                                                        list-ref-safe nums i
                                                      )
                                                       0
                                                    )
                                                  )
                                                   (
                                                    else (
                                                      list-ref-safe (
                                                        list-ref-safe nums i
                                                      )
                                                       0
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                set! heap (
                                                  append heap (
                                                    _list (
                                                      alist->hash-table (
                                                        _list (
                                                          cons "value" first_val
                                                        )
                                                         (
                                                          cons "list_idx" i
                                                        )
                                                         (
                                                          cons "elem_idx" 0
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                if (
                                                  > first_val current_max
                                                )
                                                 (
                                                  begin (
                                                    set! current_max first_val
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
                            let (
                              (
                                best (
                                  _list (
                                    - INF
                                  )
                                   INF
                                )
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
                                              > (
                                                _len heap
                                              )
                                               0
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    min_idx 0
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        j 1
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
                                                                      < j (
                                                                        _len heap
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            hj (
                                                                              list-ref-safe heap j
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                hmin (
                                                                                  list-ref-safe heap min_idx
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                if (
                                                                                  < (
                                                                                    hash-table-ref hj "value"
                                                                                  )
                                                                                   (
                                                                                    hash-table-ref hmin "value"
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    set! min_idx j
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  quote (
                                                                                    
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
                                                        let (
                                                          (
                                                            item (
                                                              list-ref-safe heap min_idx
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                new_heap (
                                                                  _list
                                                                )
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
                                                                                  < k (
                                                                                    _len heap
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    if (
                                                                                      not (
                                                                                        equal? k min_idx
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        set! new_heap (
                                                                                          append new_heap (
                                                                                            _list (
                                                                                              list-ref-safe heap k
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
                                                                                    set! k (
                                                                                      + k 1
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
                                                                    set! heap new_heap
                                                                  )
                                                                   (
                                                                    let (
                                                                      (
                                                                        current_min (
                                                                          hash-table-ref item "value"
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        if (
                                                                          < (
                                                                            - current_max current_min
                                                                          )
                                                                           (
                                                                            - (
                                                                              list-ref-safe best 1
                                                                            )
                                                                             (
                                                                              list-ref-safe best 0
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            set! best (
                                                                              _list current_min current_max
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
                                                                            hash-table-ref item "elem_idx"
                                                                          )
                                                                           (
                                                                            - (
                                                                              _len (
                                                                                list-ref-safe nums (
                                                                                  hash-table-ref item "list_idx"
                                                                                )
                                                                              )
                                                                            )
                                                                             1
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            break5 (
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
                                                                            next_val (
                                                                              cond (
                                                                                (
                                                                                  string? (
                                                                                    list-ref-safe nums (
                                                                                      hash-table-ref item "list_idx"
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  _substring (
                                                                                    list-ref-safe nums (
                                                                                      hash-table-ref item "list_idx"
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    + (
                                                                                      hash-table-ref item "elem_idx"
                                                                                    )
                                                                                     1
                                                                                  )
                                                                                   (
                                                                                    + (
                                                                                      + (
                                                                                        hash-table-ref item "elem_idx"
                                                                                      )
                                                                                       1
                                                                                    )
                                                                                     1
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                (
                                                                                  hash-table? (
                                                                                    list-ref-safe nums (
                                                                                      hash-table-ref item "list_idx"
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  hash-table-ref (
                                                                                    list-ref-safe nums (
                                                                                      hash-table-ref item "list_idx"
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    + (
                                                                                      hash-table-ref item "elem_idx"
                                                                                    )
                                                                                     1
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                else (
                                                                                  list-ref-safe (
                                                                                    list-ref-safe nums (
                                                                                      hash-table-ref item "list_idx"
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    + (
                                                                                      hash-table-ref item "elem_idx"
                                                                                    )
                                                                                     1
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            set! heap (
                                                                              append heap (
                                                                                _list (
                                                                                  alist->hash-table (
                                                                                    _list (
                                                                                      cons "value" next_val
                                                                                    )
                                                                                     (
                                                                                      cons "list_idx" (
                                                                                        hash-table-ref item "list_idx"
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      cons "elem_idx" (
                                                                                        + (
                                                                                          hash-table-ref item "elem_idx"
                                                                                        )
                                                                                         1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            if (
                                                                              > next_val current_max
                                                                            )
                                                                             (
                                                                              begin (
                                                                                set! current_max next_val
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
                                ret1 best
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
            list_to_string arr
          )
           (
            call/cc (
              lambda (
                ret10
              )
               (
                let (
                  (
                    s "["
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
                                        _len arr
                                      )
                                    )
                                     (
                                      begin (
                                        set! s (
                                          string-append s (
                                            to-str-space (
                                              list-ref-safe arr i
                                            )
                                          )
                                        )
                                      )
                                       (
                                        if (
                                          < i (
                                            - (
                                              _len arr
                                            )
                                             1
                                          )
                                        )
                                         (
                                          begin (
                                            set! s (
                                              string-append s ", "
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
                        ret10 (
                          string-append s "]"
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
                ret13
              )
               (
                let (
                  (
                    result1 (
                      smallest_range (
                        _list (
                          _list 4 10 15 24 26
                        )
                         (
                          _list 0 9 12 20
                        )
                         (
                          _list 5 18 22 30
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
                          list_to_string result1
                        )
                      )
                       (
                        list_to_string result1
                      )
                       (
                        to-str (
                          list_to_string result1
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
                        result2 (
                          smallest_range (
                            _list (
                              _list 1 2 3
                            )
                             (
                              _list 1 2 3
                            )
                             (
                              _list 1 2 3
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
                              list_to_string result2
                            )
                          )
                           (
                            list_to_string result2
                          )
                           (
                            to-str (
                              list_to_string result2
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
      )
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
