;; Generated on 2025-08-07 16:11 +0700
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
        subarray xs start end
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
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
                    k start
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
                                  < k end
                                )
                                 (
                                  begin (
                                    set! result (
                                      append result (
                                        _list (
                                          list-ref-safe xs k
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
                    ret1 result
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
        merge left_half right_half
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
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
                    let (
                      (
                        j 0
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
                                      and (
                                        < i (
                                          _len left_half
                                        )
                                      )
                                       (
                                        < j (
                                          _len right_half
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        if (
                                          < (
                                            list-ref-safe left_half i
                                          )
                                           (
                                            list-ref-safe right_half j
                                          )
                                        )
                                         (
                                          begin (
                                            set! result (
                                              append result (
                                                _list (
                                                  list-ref-safe left_half i
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
                                         (
                                          begin (
                                            set! result (
                                              append result (
                                                _list (
                                                  list-ref-safe right_half j
                                                )
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
                                        _len left_half
                                      )
                                    )
                                     (
                                      begin (
                                        set! result (
                                          append result (
                                            _list (
                                              list-ref-safe left_half i
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
                                        loop7
                                      )
                                    )
                                     '(
                                      
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
                                      < j (
                                        _len right_half
                                      )
                                    )
                                     (
                                      begin (
                                        set! result (
                                          append result (
                                            _list (
                                              list-ref-safe right_half j
                                            )
                                          )
                                        )
                                      )
                                       (
                                        set! j (
                                          + j 1
                                        )
                                      )
                                       (
                                        loop9
                                      )
                                    )
                                     '(
                                      
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
     (
      define (
        merge_sort array
      )
       (
        call/cc (
          lambda (
            ret11
          )
           (
            begin (
              if (
                <= (
                  _len array
                )
                 1
              )
               (
                begin (
                  ret11 array
                )
              )
               '(
                
              )
            )
             (
              let (
                (
                  middle (
                    _div (
                      _len array
                    )
                     2
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      left_half (
                        subarray array 0 middle
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          right_half (
                            subarray array middle (
                              _len array
                            )
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              sorted_left (
                                merge_sort left_half
                              )
                            )
                          )
                           (
                            begin (
                              let (
                                (
                                  sorted_right (
                                    merge_sort right_half
                                  )
                                )
                              )
                               (
                                begin (
                                  ret11 (
                                    merge sorted_left sorted_right
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
        h_index citations
      )
       (
        call/cc (
          lambda (
            ret12
          )
           (
            let (
              (
                idx 0
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
                              < idx (
                                _len citations
                              )
                            )
                             (
                              begin (
                                if (
                                  < (
                                    list-ref-safe citations idx
                                  )
                                   0
                                )
                                 (
                                  begin (
                                    panic "The citations should be a list of non negative integers."
                                  )
                                )
                                 '(
                                  
                                )
                              )
                               (
                                set! idx (
                                  + idx 1
                                )
                              )
                               (
                                loop13
                              )
                            )
                             '(
                              
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
                let (
                  (
                    sorted (
                      merge_sort citations
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        n (
                          _len sorted
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
                                          < i n
                                        )
                                         (
                                          begin (
                                            if (
                                              _le (
                                                cond (
                                                  (
                                                    string? sorted
                                                  )
                                                   (
                                                    _substring sorted (
                                                      - (
                                                        - n 1
                                                      )
                                                       i
                                                    )
                                                     (
                                                      + (
                                                        - (
                                                          - n 1
                                                        )
                                                         i
                                                      )
                                                       1
                                                    )
                                                  )
                                                )
                                                 (
                                                  (
                                                    hash-table? sorted
                                                  )
                                                   (
                                                    hash-table-ref sorted (
                                                      - (
                                                        - n 1
                                                      )
                                                       i
                                                    )
                                                  )
                                                )
                                                 (
                                                  else (
                                                    list-ref-safe sorted (
                                                      - (
                                                        - n 1
                                                      )
                                                       i
                                                    )
                                                  )
                                                )
                                              )
                                               i
                                            )
                                             (
                                              begin (
                                                ret12 i
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
                                            loop15
                                          )
                                        )
                                         '(
                                          
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
                            ret12 n
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
            to-str-space (
              h_index (
                _list 3 0 6 1 5
              )
            )
          )
        )
         (
          to-str-space (
            h_index (
              _list 3 0 6 1 5
            )
          )
        )
         (
          to-str (
            to-str-space (
              h_index (
                _list 3 0 6 1 5
              )
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
              h_index (
                _list 1 3 1
              )
            )
          )
        )
         (
          to-str-space (
            h_index (
              _list 1 3 1
            )
          )
        )
         (
          to-str (
            to-str-space (
              h_index (
                _list 1 3 1
              )
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
              h_index (
                _list 1 2 3
              )
            )
          )
        )
         (
          to-str-space (
            h_index (
              _list 1 2 3
            )
          )
        )
         (
          to-str (
            to-str-space (
              h_index (
                _list 1 2 3
              )
            )
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
