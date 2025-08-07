;; Generated on 2025-08-07 08:20 +0700
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
      start9 (
        current-jiffy
      )
    )
     (
      jps12 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        max_cross_sum arr low mid high
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                left_sum (
                  - 1000000000000000000.0
                )
              )
            )
             (
              begin (
                let (
                  (
                    max_left (
                      - 1
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        sum 0.0
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            i mid
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
                                          >= i low
                                        )
                                         (
                                          begin (
                                            set! sum (
                                              + sum (
                                                list-ref arr i
                                              )
                                            )
                                          )
                                           (
                                            if (
                                              > sum left_sum
                                            )
                                             (
                                              begin (
                                                set! left_sum sum
                                              )
                                               (
                                                set! max_left i
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
                                right_sum (
                                  - 1000000000000000000.0
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    max_right (
                                      - 1
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    set! sum 0.0
                                  )
                                   (
                                    set! i (
                                      + mid 1
                                    )
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
                                                  <= i high
                                                )
                                                 (
                                                  begin (
                                                    set! sum (
                                                      + sum (
                                                        list-ref arr i
                                                      )
                                                    )
                                                  )
                                                   (
                                                    if (
                                                      > sum right_sum
                                                    )
                                                     (
                                                      begin (
                                                        set! right_sum sum
                                                      )
                                                       (
                                                        set! max_right i
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
                                          cons "start" max_left
                                        )
                                         (
                                          cons "end" max_right
                                        )
                                         (
                                          cons "sum" (
                                            + left_sum right_sum
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
        )
      )
    )
     (
      define (
        max_subarray arr low high
      )
       (
        call/cc (
          lambda (
            ret6
          )
           (
            begin (
              if (
                equal? (
                  _len arr
                )
                 0
              )
               (
                begin (
                  ret6 (
                    alist->hash-table (
                      _list (
                        cons "start" (
                          - 1
                        )
                      )
                       (
                        cons "end" (
                          - 1
                        )
                      )
                       (
                        cons "sum" 0.0
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
              if (
                equal? low high
              )
               (
                begin (
                  ret6 (
                    alist->hash-table (
                      _list (
                        cons "start" low
                      )
                       (
                        cons "end" high
                      )
                       (
                        cons "sum" (
                          list-ref arr low
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
                  let (
                    (
                      left (
                        max_subarray arr low mid
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          right (
                            max_subarray arr (
                              + mid 1
                            )
                             high
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              cross (
                                max_cross_sum arr low mid high
                              )
                            )
                          )
                           (
                            begin (
                              if (
                                and (
                                  _ge (
                                    hash-table-ref left "sum"
                                  )
                                   (
                                    hash-table-ref right "sum"
                                  )
                                )
                                 (
                                  _ge (
                                    hash-table-ref left "sum"
                                  )
                                   (
                                    hash-table-ref cross "sum"
                                  )
                                )
                              )
                               (
                                begin (
                                  ret6 left
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
                                  _ge (
                                    hash-table-ref right "sum"
                                  )
                                   (
                                    hash-table-ref left "sum"
                                  )
                                )
                                 (
                                  _ge (
                                    hash-table-ref right "sum"
                                  )
                                   (
                                    hash-table-ref cross "sum"
                                  )
                                )
                              )
                               (
                                begin (
                                  ret6 right
                                )
                              )
                               (
                                quote (
                                  
                                )
                              )
                            )
                             (
                              ret6 cross
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
        show res
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            begin (
              _display (
                if (
                  string? (
                    string-append (
                      string-append (
                        string-append (
                          string-append (
                            string-append (
                              string-append "[" (
                                to-str-space (
                                  hash-table-ref res "start"
                                )
                              )
                            )
                             ", "
                          )
                           (
                            to-str-space (
                              hash-table-ref res "end"
                            )
                          )
                        )
                         ", "
                      )
                       (
                        to-str-space (
                          hash-table-ref res "sum"
                        )
                      )
                    )
                     "]"
                  )
                )
                 (
                  string-append (
                    string-append (
                      string-append (
                        string-append (
                          string-append (
                            string-append "[" (
                              to-str-space (
                                hash-table-ref res "start"
                              )
                            )
                          )
                           ", "
                        )
                         (
                          to-str-space (
                            hash-table-ref res "end"
                          )
                        )
                      )
                       ", "
                    )
                     (
                      to-str-space (
                        hash-table-ref res "sum"
                      )
                    )
                  )
                   "]"
                )
                 (
                  to-str (
                    string-append (
                      string-append (
                        string-append (
                          string-append (
                            string-append (
                              string-append "[" (
                                to-str-space (
                                  hash-table-ref res "start"
                                )
                              )
                            )
                             ", "
                          )
                           (
                            to-str-space (
                              hash-table-ref res "end"
                            )
                          )
                        )
                         ", "
                      )
                       (
                        to-str-space (
                          hash-table-ref res "sum"
                        )
                      )
                    )
                     "]"
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
     (
      define (
        main
      )
       (
        call/cc (
          lambda (
            ret8
          )
           (
            let (
              (
                nums1 (
                  _list (
                    - 2.0
                  )
                   1.0 (
                    - 3.0
                  )
                   4.0 (
                    - 1.0
                  )
                   2.0 1.0 (
                    - 5.0
                  )
                   4.0
                )
              )
            )
             (
              begin (
                let (
                  (
                    res1 (
                      max_subarray nums1 0 (
                        - (
                          _len nums1
                        )
                         1
                      )
                    )
                  )
                )
                 (
                  begin (
                    show res1
                  )
                   (
                    let (
                      (
                        nums2 (
                          _list 2.0 8.0 9.0
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            res2 (
                              max_subarray nums2 0 (
                                - (
                                  _len nums2
                                )
                                 1
                              )
                            )
                          )
                        )
                         (
                          begin (
                            show res2
                          )
                           (
                            let (
                              (
                                nums3 (
                                  _list 0.0 0.0
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    res3 (
                                      max_subarray nums3 0 (
                                        - (
                                          _len nums3
                                        )
                                         1
                                      )
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    show res3
                                  )
                                   (
                                    let (
                                      (
                                        nums4 (
                                          _list (
                                            - 1.0
                                          )
                                           0.0 1.0
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            res4 (
                                              max_subarray nums4 0 (
                                                - (
                                                  _len nums4
                                                )
                                                 1
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            show res4
                                          )
                                           (
                                            let (
                                              (
                                                nums5 (
                                                  _list (
                                                    - 2.0
                                                  )
                                                   (
                                                    - 3.0
                                                  )
                                                   (
                                                    - 1.0
                                                  )
                                                   (
                                                    - 4.0
                                                  )
                                                   (
                                                    - 6.0
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    res5 (
                                                      max_subarray nums5 0 (
                                                        - (
                                                          _len nums5
                                                        )
                                                         1
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    show res5
                                                  )
                                                   (
                                                    let (
                                                      (
                                                        nums6 (
                                                          _list
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            res6 (
                                                              max_subarray nums6 0 0
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            show res6
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
      main
    )
     (
      let (
        (
          end10 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur11 (
              quotient (
                * (
                  - end10 start9
                )
                 1000000
              )
               jps12
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur11
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
