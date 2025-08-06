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
      start22 (
        current-jiffy
      )
    )
     (
      jps25 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        sort_triplet a b c
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                x a
              )
            )
             (
              begin (
                let (
                  (
                    y b
                  )
                )
                 (
                  begin (
                    let (
                      (
                        z c
                      )
                    )
                     (
                      begin (
                        if (
                          > x y
                        )
                         (
                          begin (
                            let (
                              (
                                t x
                              )
                            )
                             (
                              begin (
                                set! x y
                              )
                               (
                                set! y t
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
                          > y z
                        )
                         (
                          begin (
                            let (
                              (
                                t y
                              )
                            )
                             (
                              begin (
                                set! y z
                              )
                               (
                                set! z t
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
                          > x y
                        )
                         (
                          begin (
                            let (
                              (
                                t x
                              )
                            )
                             (
                              begin (
                                set! x y
                              )
                               (
                                set! y t
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
                        ret1 (
                          _list x y z
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
        contains_triplet arr target
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            begin (
              call/cc (
                lambda (
                  break4
                )
                 (
                  letrec (
                    (
                      loop3 (
                        lambda (
                          i
                        )
                         (
                          if (
                            < i (
                              _len arr
                            )
                          )
                           (
                            begin (
                              begin (
                                let (
                                  (
                                    item (
                                      list-ref arr i
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        same #t
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
                                                    j
                                                  )
                                                   (
                                                    if (
                                                      < j (
                                                        _len target
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        begin (
                                                          if (
                                                            not (
                                                              equal? (
                                                                list-ref item j
                                                              )
                                                               (
                                                                list-ref target j
                                                              )
                                                            )
                                                          )
                                                           (
                                                            begin (
                                                              set! same #f
                                                            )
                                                             (
                                                              break6 (
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
                                                      )
                                                       (
                                                        loop5 (
                                                          + j 1
                                                        )
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
                                              loop5 0
                                            )
                                          )
                                        )
                                      )
                                       (
                                        if same (
                                          begin (
                                            ret2 #t
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
                              loop3 (
                                + i 1
                              )
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
                    loop3 0
                  )
                )
              )
            )
             (
              ret2 #f
            )
          )
        )
      )
    )
     (
      define (
        contains_int arr value
      )
       (
        call/cc (
          lambda (
            ret7
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
                          i
                        )
                         (
                          if (
                            < i (
                              _len arr
                            )
                          )
                           (
                            begin (
                              begin (
                                if (
                                  equal? (
                                    list-ref arr i
                                  )
                                   value
                                )
                                 (
                                  begin (
                                    ret7 #t
                                  )
                                )
                                 (
                                  quote (
                                    
                                  )
                                )
                              )
                            )
                             (
                              loop8 (
                                + i 1
                              )
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
                    loop8 0
                  )
                )
              )
            )
             (
              ret7 #f
            )
          )
        )
      )
    )
     (
      define (
        find_triplets_with_0_sum nums
      )
       (
        call/cc (
          lambda (
            ret10
          )
           (
            let (
              (
                n (
                  _len nums
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
                    call/cc (
                      lambda (
                        break12
                      )
                       (
                        letrec (
                          (
                            loop11 (
                              lambda (
                                i
                              )
                               (
                                if (
                                  < i n
                                )
                                 (
                                  begin (
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
                                                  j
                                                )
                                                 (
                                                  if (
                                                    < j n
                                                  )
                                                   (
                                                    begin (
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
                                                                    k
                                                                  )
                                                                   (
                                                                    if (
                                                                      < k n
                                                                    )
                                                                     (
                                                                      begin (
                                                                        begin (
                                                                          let (
                                                                            (
                                                                              a (
                                                                                list-ref nums i
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            begin (
                                                                              let (
                                                                                (
                                                                                  b (
                                                                                    list-ref nums j
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                begin (
                                                                                  let (
                                                                                    (
                                                                                      c (
                                                                                        list-ref nums k
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    begin (
                                                                                      if (
                                                                                        equal? (
                                                                                          + (
                                                                                            + a b
                                                                                          )
                                                                                           c
                                                                                        )
                                                                                         0
                                                                                      )
                                                                                       (
                                                                                        begin (
                                                                                          let (
                                                                                            (
                                                                                              trip (
                                                                                                sort_triplet a b c
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            begin (
                                                                                              if (
                                                                                                not (
                                                                                                  contains_triplet result trip
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                begin (
                                                                                                  set! result (
                                                                                                    append result (
                                                                                                      _list trip
                                                                                                    )
                                                                                                  )
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
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        loop15 (
                                                                          + k 1
                                                                        )
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
                                                              loop15 (
                                                                _add j 1
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      loop13 (
                                                        + j 1
                                                      )
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
                                            loop13 (
                                              _add i 1
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                   (
                                    loop11 (
                                      + i 1
                                    )
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
                          loop11 0
                        )
                      )
                    )
                  )
                   (
                    ret10 result
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
        find_triplets_with_0_sum_hashing arr
      )
       (
        call/cc (
          lambda (
            ret17
          )
           (
            let (
              (
                target_sum 0
              )
            )
             (
              begin (
                let (
                  (
                    output (
                      _list
                    )
                  )
                )
                 (
                  begin (
                    call/cc (
                      lambda (
                        break19
                      )
                       (
                        letrec (
                          (
                            loop18 (
                              lambda (
                                i
                              )
                               (
                                if (
                                  < i (
                                    _len arr
                                  )
                                )
                                 (
                                  begin (
                                    begin (
                                      let (
                                        (
                                          seen (
                                            _list
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          let (
                                            (
                                              current_sum (
                                                - target_sum (
                                                  list-ref arr i
                                                )
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              call/cc (
                                                lambda (
                                                  break21
                                                )
                                                 (
                                                  letrec (
                                                    (
                                                      loop20 (
                                                        lambda (
                                                          j
                                                        )
                                                         (
                                                          if (
                                                            < j (
                                                              _len arr
                                                            )
                                                          )
                                                           (
                                                            begin (
                                                              begin (
                                                                let (
                                                                  (
                                                                    other (
                                                                      list-ref arr j
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        required (
                                                                          - current_sum other
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        if (
                                                                          contains_int seen required
                                                                        )
                                                                         (
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                trip (
                                                                                  sort_triplet (
                                                                                    list-ref arr i
                                                                                  )
                                                                                   other required
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                if (
                                                                                  not (
                                                                                    contains_triplet output trip
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    set! output (
                                                                                      append output (
                                                                                        _list trip
                                                                                      )
                                                                                    )
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
                                                                        set! seen (
                                                                          append seen (
                                                                            _list other
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             (
                                                              loop20 (
                                                                + j 1
                                                              )
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
                                                    loop20 (
                                                      _add i 1
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
                                    loop18 (
                                      + i 1
                                    )
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
                          loop18 0
                        )
                      )
                    )
                  )
                   (
                    ret17 output
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
              find_triplets_with_0_sum (
                _list (
                  - 1
                )
                 0 1 2 (
                  - 1
                )
                 (
                  - 4
                )
              )
            )
          )
        )
         (
          to-str-space (
            find_triplets_with_0_sum (
              _list (
                - 1
              )
               0 1 2 (
                - 1
              )
               (
                - 4
              )
            )
          )
        )
         (
          to-str (
            to-str-space (
              find_triplets_with_0_sum (
                _list (
                  - 1
                )
                 0 1 2 (
                  - 1
                )
                 (
                  - 4
                )
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
              find_triplets_with_0_sum (
                _list
              )
            )
          )
        )
         (
          to-str-space (
            find_triplets_with_0_sum (
              _list
            )
          )
        )
         (
          to-str (
            to-str-space (
              find_triplets_with_0_sum (
                _list
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
              find_triplets_with_0_sum (
                _list 0 0 0
              )
            )
          )
        )
         (
          to-str-space (
            find_triplets_with_0_sum (
              _list 0 0 0
            )
          )
        )
         (
          to-str (
            to-str-space (
              find_triplets_with_0_sum (
                _list 0 0 0
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
              find_triplets_with_0_sum (
                _list 1 2 3 0 (
                  - 1
                )
                 (
                  - 2
                )
                 (
                  - 3
                )
              )
            )
          )
        )
         (
          to-str-space (
            find_triplets_with_0_sum (
              _list 1 2 3 0 (
                - 1
              )
               (
                - 2
              )
               (
                - 3
              )
            )
          )
        )
         (
          to-str (
            to-str-space (
              find_triplets_with_0_sum (
                _list 1 2 3 0 (
                  - 1
                )
                 (
                  - 2
                )
                 (
                  - 3
                )
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
              find_triplets_with_0_sum_hashing (
                _list (
                  - 1
                )
                 0 1 2 (
                  - 1
                )
                 (
                  - 4
                )
              )
            )
          )
        )
         (
          to-str-space (
            find_triplets_with_0_sum_hashing (
              _list (
                - 1
              )
               0 1 2 (
                - 1
              )
               (
                - 4
              )
            )
          )
        )
         (
          to-str (
            to-str-space (
              find_triplets_with_0_sum_hashing (
                _list (
                  - 1
                )
                 0 1 2 (
                  - 1
                )
                 (
                  - 4
                )
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
              find_triplets_with_0_sum_hashing (
                _list
              )
            )
          )
        )
         (
          to-str-space (
            find_triplets_with_0_sum_hashing (
              _list
            )
          )
        )
         (
          to-str (
            to-str-space (
              find_triplets_with_0_sum_hashing (
                _list
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
              find_triplets_with_0_sum_hashing (
                _list 0 0 0
              )
            )
          )
        )
         (
          to-str-space (
            find_triplets_with_0_sum_hashing (
              _list 0 0 0
            )
          )
        )
         (
          to-str (
            to-str-space (
              find_triplets_with_0_sum_hashing (
                _list 0 0 0
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
              find_triplets_with_0_sum_hashing (
                _list 1 2 3 0 (
                  - 1
                )
                 (
                  - 2
                )
                 (
                  - 3
                )
              )
            )
          )
        )
         (
          to-str-space (
            find_triplets_with_0_sum_hashing (
              _list 1 2 3 0 (
                - 1
              )
               (
                - 2
              )
               (
                - 3
              )
            )
          )
        )
         (
          to-str (
            to-str-space (
              find_triplets_with_0_sum_hashing (
                _list 1 2 3 0 (
                  - 1
                )
                 (
                  - 2
                )
                 (
                  - 3
                )
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
          end23 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur24 (
              quotient (
                * (
                  - end23 start22
                )
                 1000000
              )
               jps25
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur24
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
