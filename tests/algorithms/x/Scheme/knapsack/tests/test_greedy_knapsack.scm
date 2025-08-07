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
      start16 (
        current-jiffy
      )
    )
     (
      jps19 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        calc_profit profit weight max_weight
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            begin (
              if (
                not (
                  equal? (
                    _len profit
                  )
                   (
                    _len weight
                  )
                )
              )
               (
                begin (
                  ret1 (
                    alist->hash-table (
                      _list (
                        cons "ok" #f
                      )
                       (
                        cons "value" 0.0
                      )
                       (
                        cons "error" "The length of profit and weight must be same."
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
                <= max_weight 0
              )
               (
                begin (
                  ret1 (
                    alist->hash-table (
                      _list (
                        cons "ok" #f
                      )
                       (
                        cons "value" 0.0
                      )
                       (
                        cons "error" "max_weight must greater than zero."
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
                                  _len profit
                                )
                              )
                               (
                                begin (
                                  if (
                                    < (
                                      list-ref-safe profit i
                                    )
                                     0
                                  )
                                   (
                                    begin (
                                      ret1 (
                                        alist->hash-table (
                                          _list (
                                            cons "ok" #f
                                          )
                                           (
                                            cons "value" 0.0
                                          )
                                           (
                                            cons "error" "Profit can not be negative."
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
                                    < (
                                      list-ref-safe weight i
                                    )
                                     0
                                  )
                                   (
                                    begin (
                                      ret1 (
                                        alist->hash-table (
                                          _list (
                                            cons "ok" #f
                                          )
                                           (
                                            cons "value" 0.0
                                          )
                                           (
                                            cons "error" "Weight can not be negative."
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
                      used (
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
                                        < j (
                                          _len profit
                                        )
                                      )
                                       (
                                        begin (
                                          set! used (
                                            append used (
                                              _list #f
                                            )
                                          )
                                        )
                                         (
                                          set! j (
                                            + j 1
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
                          let (
                            (
                              limit 0
                            )
                          )
                           (
                            begin (
                              let (
                                (
                                  gain 0.0
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
                                                < limit max_weight
                                              )
                                               (
                                                begin (
                                                  let (
                                                    (
                                                      max_ratio (
                                                        - 1.0
                                                      )
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      let (
                                                        (
                                                          idx (
                                                            - 0 1
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
                                                                              _len profit
                                                                            )
                                                                          )
                                                                           (
                                                                            begin (
                                                                              if (
                                                                                not (
                                                                                  list-ref-safe used k
                                                                                )
                                                                              )
                                                                               (
                                                                                begin (
                                                                                  let (
                                                                                    (
                                                                                      ratio (
                                                                                        _div (
                                                                                          + 0.0 (
                                                                                            list-ref-safe profit k
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          + 0.0 (
                                                                                            list-ref-safe weight k
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    begin (
                                                                                      if (
                                                                                        > ratio max_ratio
                                                                                      )
                                                                                       (
                                                                                        begin (
                                                                                          set! max_ratio ratio
                                                                                        )
                                                                                         (
                                                                                          set! idx k
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
                                                              if (
                                                                equal? idx (
                                                                  - 0 1
                                                                )
                                                              )
                                                               (
                                                                begin (
                                                                  break7 (
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
                                                              list-set! used idx #t
                                                            )
                                                             (
                                                              if (
                                                                >= (
                                                                  - max_weight limit
                                                                )
                                                                 (
                                                                  list-ref-safe weight idx
                                                                )
                                                              )
                                                               (
                                                                begin (
                                                                  set! limit (
                                                                    + limit (
                                                                      list-ref-safe weight idx
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  set! gain (
                                                                    + gain (
                                                                      + 0.0 (
                                                                        list-ref-safe profit idx
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                begin (
                                                                  set! gain (
                                                                    _add gain (
                                                                      * (
                                                                        _div (
                                                                          + 0.0 (
                                                                            - max_weight limit
                                                                          )
                                                                        )
                                                                         (
                                                                          + 0.0 (
                                                                            list-ref-safe weight idx
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        + 0.0 (
                                                                          list-ref-safe profit idx
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  break7 (
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
                                  ret1 (
                                    alist->hash-table (
                                      _list (
                                        cons "ok" #t
                                      )
                                       (
                                        cons "value" gain
                                      )
                                       (
                                        cons "error" ""
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
        test_sorted
      )
       (
        call/cc (
          lambda (
            ret10
          )
           (
            let (
              (
                profit (
                  _list 10 20 30 40 50 60
                )
              )
            )
             (
              begin (
                let (
                  (
                    weight (
                      _list 2 4 6 8 10 12
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        res (
                          calc_profit profit weight 100
                        )
                      )
                    )
                     (
                      begin (
                        ret10 (
                          and (
                            hash-table-ref res "ok"
                          )
                           (
                            equal? (
                              hash-table-ref res "value"
                            )
                             210.0
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
        test_negative_max_weight
      )
       (
        call/cc (
          lambda (
            ret11
          )
           (
            let (
              (
                profit (
                  _list 10 20 30 40 50 60
                )
              )
            )
             (
              begin (
                let (
                  (
                    weight (
                      _list 2 4 6 8 10 12
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        res (
                          calc_profit profit weight (
                            - 15
                          )
                        )
                      )
                    )
                     (
                      begin (
                        ret11 (
                          and (
                            not (
                              hash-table-ref res "ok"
                            )
                          )
                           (
                            string=? (
                              hash-table-ref res "error"
                            )
                             "max_weight must greater than zero."
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
        test_negative_profit_value
      )
       (
        call/cc (
          lambda (
            ret12
          )
           (
            let (
              (
                profit (
                  _list 10 (
                    - 20
                  )
                   30 40 50 60
                )
              )
            )
             (
              begin (
                let (
                  (
                    weight (
                      _list 2 4 6 8 10 12
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        res (
                          calc_profit profit weight 15
                        )
                      )
                    )
                     (
                      begin (
                        ret12 (
                          and (
                            not (
                              hash-table-ref res "ok"
                            )
                          )
                           (
                            string=? (
                              hash-table-ref res "error"
                            )
                             "Profit can not be negative."
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
        test_negative_weight_value
      )
       (
        call/cc (
          lambda (
            ret13
          )
           (
            let (
              (
                profit (
                  _list 10 20 30 40 50 60
                )
              )
            )
             (
              begin (
                let (
                  (
                    weight (
                      _list 2 (
                        - 4
                      )
                       6 (
                        - 8
                      )
                       10 12
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        res (
                          calc_profit profit weight 15
                        )
                      )
                    )
                     (
                      begin (
                        ret13 (
                          and (
                            not (
                              hash-table-ref res "ok"
                            )
                          )
                           (
                            string=? (
                              hash-table-ref res "error"
                            )
                             "Weight can not be negative."
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
        test_null_max_weight
      )
       (
        call/cc (
          lambda (
            ret14
          )
           (
            let (
              (
                profit (
                  _list 10 20 30 40 50 60
                )
              )
            )
             (
              begin (
                let (
                  (
                    weight (
                      _list 2 4 6 8 10 12
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        res (
                          calc_profit profit weight 0
                        )
                      )
                    )
                     (
                      begin (
                        ret14 (
                          and (
                            not (
                              hash-table-ref res "ok"
                            )
                          )
                           (
                            string=? (
                              hash-table-ref res "error"
                            )
                             "max_weight must greater than zero."
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
        test_unequal_list_length
      )
       (
        call/cc (
          lambda (
            ret15
          )
           (
            let (
              (
                profit (
                  _list 10 20 30 40 50
                )
              )
            )
             (
              begin (
                let (
                  (
                    weight (
                      _list 2 4 6 8 10 12
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        res (
                          calc_profit profit weight 100
                        )
                      )
                    )
                     (
                      begin (
                        ret15 (
                          and (
                            not (
                              hash-table-ref res "ok"
                            )
                          )
                           (
                            string=? (
                              hash-table-ref res "error"
                            )
                             "The length of profit and weight must be same."
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
            test_sorted
          )
        )
         (
          test_sorted
        )
         (
          to-str (
            test_sorted
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
            test_negative_max_weight
          )
        )
         (
          test_negative_max_weight
        )
         (
          to-str (
            test_negative_max_weight
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
            test_negative_profit_value
          )
        )
         (
          test_negative_profit_value
        )
         (
          to-str (
            test_negative_profit_value
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
            test_negative_weight_value
          )
        )
         (
          test_negative_weight_value
        )
         (
          to-str (
            test_negative_weight_value
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
            test_null_max_weight
          )
        )
         (
          test_null_max_weight
        )
         (
          to-str (
            test_null_max_weight
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
            test_unequal_list_length
          )
        )
         (
          test_unequal_list_length
        )
         (
          to-str (
            test_unequal_list_length
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
            if #t #t #f
          )
        )
         (
          if #t #t #f
        )
         (
          to-str (
            if #t #t #f
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
          end17 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur18 (
              quotient (
                * (
                  - end17 start16
                )
                 1000000
              )
               jps19
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur18
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
