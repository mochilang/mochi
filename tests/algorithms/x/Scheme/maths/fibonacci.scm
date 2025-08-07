;; Generated on 2025-08-07 10:06 +0700
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
        sqrt x
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            begin (
              if (
                <= x 0.0
              )
               (
                begin (
                  ret1 0.0
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
                  guess x
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
                                    < i 10
                                  )
                                   (
                                    begin (
                                      set! guess (
                                        _div (
                                          _add guess (
                                            _div x guess
                                          )
                                        )
                                         2.0
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
                      ret1 guess
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
        powf x n
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            let (
              (
                res 1.0
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
                                      * res x
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
        roundf x
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            begin (
              if (
                >= x 0.0
              )
               (
                begin (
                  ret7 (
                    let (
                      (
                        v8 (
                          + x 0.5
                        )
                      )
                    )
                     (
                      cond (
                        (
                          string? v8
                        )
                         (
                          inexact->exact (
                            floor (
                              string->number v8
                            )
                          )
                        )
                      )
                       (
                        (
                          boolean? v8
                        )
                         (
                          if v8 1 0
                        )
                      )
                       (
                        else (
                          inexact->exact (
                            floor v8
                          )
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
              ret7 (
                let (
                  (
                    v9 (
                      - x 0.5
                    )
                  )
                )
                 (
                  cond (
                    (
                      string? v9
                    )
                     (
                      inexact->exact (
                        floor (
                          string->number v9
                        )
                      )
                    )
                  )
                   (
                    (
                      boolean? v9
                    )
                     (
                      if v9 1 0
                    )
                  )
                   (
                    else (
                      inexact->exact (
                        floor v9
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
        fib_iterative n
      )
       (
        call/cc (
          lambda (
            ret10
          )
           (
            begin (
              if (
                < n 0
              )
               (
                begin (
                  panic "n is negative"
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                equal? n 0
              )
               (
                begin (
                  ret10 (
                    _list 0
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
                  fib (
                    _list 0 1
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      i 2
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
                                    <= i n
                                  )
                                   (
                                    begin (
                                      set! fib (
                                        append fib (
                                          _list (
                                            + (
                                              list-ref fib (
                                                - i 1
                                              )
                                            )
                                             (
                                              list-ref fib (
                                                - i 2
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
                      ret10 fib
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
        fib_recursive_term i
      )
       (
        call/cc (
          lambda (
            ret13
          )
           (
            begin (
              if (
                < i 0
              )
               (
                begin (
                  panic "n is negative"
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                < i 2
              )
               (
                begin (
                  ret13 i
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              ret13 (
                _add (
                  fib_recursive_term (
                    - i 1
                  )
                )
                 (
                  fib_recursive_term (
                    - i 2
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
        fib_recursive n
      )
       (
        call/cc (
          lambda (
            ret14
          )
           (
            begin (
              if (
                < n 0
              )
               (
                begin (
                  panic "n is negative"
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
                                    <= i n
                                  )
                                   (
                                    begin (
                                      set! res (
                                        append res (
                                          _list (
                                            fib_recursive_term i
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
                      ret14 res
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
          fib_cache_global (
            alist->hash-table (
              _list
            )
          )
        )
      )
       (
        begin (
          define (
            fib_recursive_cached_term i
          )
           (
            call/cc (
              lambda (
                ret17
              )
               (
                begin (
                  if (
                    < i 0
                  )
                   (
                    begin (
                      panic "n is negative"
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  if (
                    < i 2
                  )
                   (
                    begin (
                      ret17 i
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  if (
                    cond (
                      (
                        string? fib_cache_global
                      )
                       (
                        if (
                          string-contains fib_cache_global i
                        )
                         #t #f
                      )
                    )
                     (
                      (
                        hash-table? fib_cache_global
                      )
                       (
                        if (
                          hash-table-exists? fib_cache_global i
                        )
                         #t #f
                      )
                    )
                     (
                      else (
                        if (
                          member i fib_cache_global
                        )
                         #t #f
                      )
                    )
                  )
                   (
                    begin (
                      ret17 (
                        hash-table-ref/default fib_cache_global i (
                          quote (
                            
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
                      val (
                        _add (
                          fib_recursive_cached_term (
                            - i 1
                          )
                        )
                         (
                          fib_recursive_cached_term (
                            - i 2
                          )
                        )
                      )
                    )
                  )
                   (
                    begin (
                      hash-table-set! fib_cache_global i val
                    )
                     (
                      ret17 val
                    )
                  )
                )
              )
            )
          )
        )
         (
          define (
            fib_recursive_cached n
          )
           (
            call/cc (
              lambda (
                ret18
              )
               (
                begin (
                  if (
                    < n 0
                  )
                   (
                    begin (
                      panic "n is negative"
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
                      res (
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
                                        <= j n
                                      )
                                       (
                                        begin (
                                          set! res (
                                            append res (
                                              _list (
                                                fib_recursive_cached_term j
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
                          ret18 res
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
              fib_memo_cache (
                alist->hash-table (
                  _list (
                    cons 0 0
                  )
                   (
                    cons 1 1
                  )
                   (
                    cons 2 1
                  )
                )
              )
            )
          )
           (
            begin (
              define (
                fib_memoization_term num
              )
               (
                call/cc (
                  lambda (
                    ret21
                  )
                   (
                    begin (
                      if (
                        cond (
                          (
                            string? fib_memo_cache
                          )
                           (
                            if (
                              string-contains fib_memo_cache num
                            )
                             #t #f
                          )
                        )
                         (
                          (
                            hash-table? fib_memo_cache
                          )
                           (
                            if (
                              hash-table-exists? fib_memo_cache num
                            )
                             #t #f
                          )
                        )
                         (
                          else (
                            if (
                              member num fib_memo_cache
                            )
                             #t #f
                          )
                        )
                      )
                       (
                        begin (
                          ret21 (
                            hash-table-ref/default fib_memo_cache num (
                              quote (
                                
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
                          value (
                            _add (
                              fib_memoization_term (
                                - num 1
                              )
                            )
                             (
                              fib_memoization_term (
                                - num 2
                              )
                            )
                          )
                        )
                      )
                       (
                        begin (
                          hash-table-set! fib_memo_cache num value
                        )
                         (
                          ret21 value
                        )
                      )
                    )
                  )
                )
              )
            )
             (
              define (
                fib_memoization n
              )
               (
                call/cc (
                  lambda (
                    ret22
                  )
                   (
                    begin (
                      if (
                        < n 0
                      )
                       (
                        begin (
                          panic "n is negative"
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
                          out (
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
                                            <= i n
                                          )
                                           (
                                            begin (
                                              set! out (
                                                append out (
                                                  _list (
                                                    fib_memoization_term i
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
                              ret22 out
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
                fib_binet n
              )
               (
                call/cc (
                  lambda (
                    ret25
                  )
                   (
                    begin (
                      if (
                        < n 0
                      )
                       (
                        begin (
                          panic "n is negative"
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      if (
                        >= n 1475
                      )
                       (
                        begin (
                          panic "n is too large"
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
                          sqrt5 (
                            sqrt 5.0
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              phi (
                                _div (
                                  _add 1.0 sqrt5
                                )
                                 2.0
                              )
                            )
                          )
                           (
                            begin (
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
                                                    <= i n
                                                  )
                                                   (
                                                    begin (
                                                      let (
                                                        (
                                                          val (
                                                            roundf (
                                                              _div (
                                                                powf phi i
                                                              )
                                                               sqrt5
                                                            )
                                                          )
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
                                                         (
                                                          set! i (
                                                            + i 1
                                                          )
                                                        )
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
                                      ret25 res
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
                matrix_mul a b
              )
               (
                call/cc (
                  lambda (
                    ret28
                  )
                   (
                    let (
                      (
                        a00 (
                          + (
                            * (
                              cond (
                                (
                                  string? (
                                    list-ref a 0
                                  )
                                )
                                 (
                                  _substring (
                                    list-ref a 0
                                  )
                                   0 (
                                    + 0 1
                                  )
                                )
                              )
                               (
                                (
                                  hash-table? (
                                    list-ref a 0
                                  )
                                )
                                 (
                                  hash-table-ref (
                                    list-ref a 0
                                  )
                                   0
                                )
                              )
                               (
                                else (
                                  list-ref (
                                    list-ref a 0
                                  )
                                   0
                                )
                              )
                            )
                             (
                              cond (
                                (
                                  string? (
                                    list-ref b 0
                                  )
                                )
                                 (
                                  _substring (
                                    list-ref b 0
                                  )
                                   0 (
                                    + 0 1
                                  )
                                )
                              )
                               (
                                (
                                  hash-table? (
                                    list-ref b 0
                                  )
                                )
                                 (
                                  hash-table-ref (
                                    list-ref b 0
                                  )
                                   0
                                )
                              )
                               (
                                else (
                                  list-ref (
                                    list-ref b 0
                                  )
                                   0
                                )
                              )
                            )
                          )
                           (
                            * (
                              cond (
                                (
                                  string? (
                                    list-ref a 0
                                  )
                                )
                                 (
                                  _substring (
                                    list-ref a 0
                                  )
                                   1 (
                                    + 1 1
                                  )
                                )
                              )
                               (
                                (
                                  hash-table? (
                                    list-ref a 0
                                  )
                                )
                                 (
                                  hash-table-ref (
                                    list-ref a 0
                                  )
                                   1
                                )
                              )
                               (
                                else (
                                  list-ref (
                                    list-ref a 0
                                  )
                                   1
                                )
                              )
                            )
                             (
                              cond (
                                (
                                  string? (
                                    list-ref b 1
                                  )
                                )
                                 (
                                  _substring (
                                    list-ref b 1
                                  )
                                   0 (
                                    + 0 1
                                  )
                                )
                              )
                               (
                                (
                                  hash-table? (
                                    list-ref b 1
                                  )
                                )
                                 (
                                  hash-table-ref (
                                    list-ref b 1
                                  )
                                   0
                                )
                              )
                               (
                                else (
                                  list-ref (
                                    list-ref b 1
                                  )
                                   0
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            a01 (
                              + (
                                * (
                                  cond (
                                    (
                                      string? (
                                        list-ref a 0
                                      )
                                    )
                                     (
                                      _substring (
                                        list-ref a 0
                                      )
                                       0 (
                                        + 0 1
                                      )
                                    )
                                  )
                                   (
                                    (
                                      hash-table? (
                                        list-ref a 0
                                      )
                                    )
                                     (
                                      hash-table-ref (
                                        list-ref a 0
                                      )
                                       0
                                    )
                                  )
                                   (
                                    else (
                                      list-ref (
                                        list-ref a 0
                                      )
                                       0
                                    )
                                  )
                                )
                                 (
                                  cond (
                                    (
                                      string? (
                                        list-ref b 0
                                      )
                                    )
                                     (
                                      _substring (
                                        list-ref b 0
                                      )
                                       1 (
                                        + 1 1
                                      )
                                    )
                                  )
                                   (
                                    (
                                      hash-table? (
                                        list-ref b 0
                                      )
                                    )
                                     (
                                      hash-table-ref (
                                        list-ref b 0
                                      )
                                       1
                                    )
                                  )
                                   (
                                    else (
                                      list-ref (
                                        list-ref b 0
                                      )
                                       1
                                    )
                                  )
                                )
                              )
                               (
                                * (
                                  cond (
                                    (
                                      string? (
                                        list-ref a 0
                                      )
                                    )
                                     (
                                      _substring (
                                        list-ref a 0
                                      )
                                       1 (
                                        + 1 1
                                      )
                                    )
                                  )
                                   (
                                    (
                                      hash-table? (
                                        list-ref a 0
                                      )
                                    )
                                     (
                                      hash-table-ref (
                                        list-ref a 0
                                      )
                                       1
                                    )
                                  )
                                   (
                                    else (
                                      list-ref (
                                        list-ref a 0
                                      )
                                       1
                                    )
                                  )
                                )
                                 (
                                  cond (
                                    (
                                      string? (
                                        list-ref b 1
                                      )
                                    )
                                     (
                                      _substring (
                                        list-ref b 1
                                      )
                                       1 (
                                        + 1 1
                                      )
                                    )
                                  )
                                   (
                                    (
                                      hash-table? (
                                        list-ref b 1
                                      )
                                    )
                                     (
                                      hash-table-ref (
                                        list-ref b 1
                                      )
                                       1
                                    )
                                  )
                                   (
                                    else (
                                      list-ref (
                                        list-ref b 1
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
                          begin (
                            let (
                              (
                                a10 (
                                  + (
                                    * (
                                      cond (
                                        (
                                          string? (
                                            list-ref a 1
                                          )
                                        )
                                         (
                                          _substring (
                                            list-ref a 1
                                          )
                                           0 (
                                            + 0 1
                                          )
                                        )
                                      )
                                       (
                                        (
                                          hash-table? (
                                            list-ref a 1
                                          )
                                        )
                                         (
                                          hash-table-ref (
                                            list-ref a 1
                                          )
                                           0
                                        )
                                      )
                                       (
                                        else (
                                          list-ref (
                                            list-ref a 1
                                          )
                                           0
                                        )
                                      )
                                    )
                                     (
                                      cond (
                                        (
                                          string? (
                                            list-ref b 0
                                          )
                                        )
                                         (
                                          _substring (
                                            list-ref b 0
                                          )
                                           0 (
                                            + 0 1
                                          )
                                        )
                                      )
                                       (
                                        (
                                          hash-table? (
                                            list-ref b 0
                                          )
                                        )
                                         (
                                          hash-table-ref (
                                            list-ref b 0
                                          )
                                           0
                                        )
                                      )
                                       (
                                        else (
                                          list-ref (
                                            list-ref b 0
                                          )
                                           0
                                        )
                                      )
                                    )
                                  )
                                   (
                                    * (
                                      cond (
                                        (
                                          string? (
                                            list-ref a 1
                                          )
                                        )
                                         (
                                          _substring (
                                            list-ref a 1
                                          )
                                           1 (
                                            + 1 1
                                          )
                                        )
                                      )
                                       (
                                        (
                                          hash-table? (
                                            list-ref a 1
                                          )
                                        )
                                         (
                                          hash-table-ref (
                                            list-ref a 1
                                          )
                                           1
                                        )
                                      )
                                       (
                                        else (
                                          list-ref (
                                            list-ref a 1
                                          )
                                           1
                                        )
                                      )
                                    )
                                     (
                                      cond (
                                        (
                                          string? (
                                            list-ref b 1
                                          )
                                        )
                                         (
                                          _substring (
                                            list-ref b 1
                                          )
                                           0 (
                                            + 0 1
                                          )
                                        )
                                      )
                                       (
                                        (
                                          hash-table? (
                                            list-ref b 1
                                          )
                                        )
                                         (
                                          hash-table-ref (
                                            list-ref b 1
                                          )
                                           0
                                        )
                                      )
                                       (
                                        else (
                                          list-ref (
                                            list-ref b 1
                                          )
                                           0
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    a11 (
                                      + (
                                        * (
                                          cond (
                                            (
                                              string? (
                                                list-ref a 1
                                              )
                                            )
                                             (
                                              _substring (
                                                list-ref a 1
                                              )
                                               0 (
                                                + 0 1
                                              )
                                            )
                                          )
                                           (
                                            (
                                              hash-table? (
                                                list-ref a 1
                                              )
                                            )
                                             (
                                              hash-table-ref (
                                                list-ref a 1
                                              )
                                               0
                                            )
                                          )
                                           (
                                            else (
                                              list-ref (
                                                list-ref a 1
                                              )
                                               0
                                            )
                                          )
                                        )
                                         (
                                          cond (
                                            (
                                              string? (
                                                list-ref b 0
                                              )
                                            )
                                             (
                                              _substring (
                                                list-ref b 0
                                              )
                                               1 (
                                                + 1 1
                                              )
                                            )
                                          )
                                           (
                                            (
                                              hash-table? (
                                                list-ref b 0
                                              )
                                            )
                                             (
                                              hash-table-ref (
                                                list-ref b 0
                                              )
                                               1
                                            )
                                          )
                                           (
                                            else (
                                              list-ref (
                                                list-ref b 0
                                              )
                                               1
                                            )
                                          )
                                        )
                                      )
                                       (
                                        * (
                                          cond (
                                            (
                                              string? (
                                                list-ref a 1
                                              )
                                            )
                                             (
                                              _substring (
                                                list-ref a 1
                                              )
                                               1 (
                                                + 1 1
                                              )
                                            )
                                          )
                                           (
                                            (
                                              hash-table? (
                                                list-ref a 1
                                              )
                                            )
                                             (
                                              hash-table-ref (
                                                list-ref a 1
                                              )
                                               1
                                            )
                                          )
                                           (
                                            else (
                                              list-ref (
                                                list-ref a 1
                                              )
                                               1
                                            )
                                          )
                                        )
                                         (
                                          cond (
                                            (
                                              string? (
                                                list-ref b 1
                                              )
                                            )
                                             (
                                              _substring (
                                                list-ref b 1
                                              )
                                               1 (
                                                + 1 1
                                              )
                                            )
                                          )
                                           (
                                            (
                                              hash-table? (
                                                list-ref b 1
                                              )
                                            )
                                             (
                                              hash-table-ref (
                                                list-ref b 1
                                              )
                                               1
                                            )
                                          )
                                           (
                                            else (
                                              list-ref (
                                                list-ref b 1
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
                                  begin (
                                    ret28 (
                                      _list (
                                        _list a00 a01
                                      )
                                       (
                                        _list a10 a11
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
                matrix_pow m power
              )
               (
                call/cc (
                  lambda (
                    ret29
                  )
                   (
                    begin (
                      if (
                        < power 0
                      )
                       (
                        begin (
                          panic "power is negative"
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
                          result (
                            _list (
                              _list 1 0
                            )
                             (
                              _list 0 1
                            )
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              base m
                            )
                          )
                           (
                            begin (
                              let (
                                (
                                  p power
                                )
                              )
                               (
                                begin (
                                  call/cc (
                                    lambda (
                                      break31
                                    )
                                     (
                                      letrec (
                                        (
                                          loop30 (
                                            lambda (
                                              
                                            )
                                             (
                                              if (
                                                > p 0
                                              )
                                               (
                                                begin (
                                                  if (
                                                    equal? (
                                                      _mod p 2
                                                    )
                                                     1
                                                  )
                                                   (
                                                    begin (
                                                      set! result (
                                                        matrix_mul result base
                                                      )
                                                    )
                                                  )
                                                   (
                                                    quote (
                                                      
                                                    )
                                                  )
                                                )
                                                 (
                                                  set! base (
                                                    matrix_mul base base
                                                  )
                                                )
                                                 (
                                                  set! p (
                                                    let (
                                                      (
                                                        v32 (
                                                          _div p 2
                                                        )
                                                      )
                                                    )
                                                     (
                                                      cond (
                                                        (
                                                          string? v32
                                                        )
                                                         (
                                                          inexact->exact (
                                                            floor (
                                                              string->number v32
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        (
                                                          boolean? v32
                                                        )
                                                         (
                                                          if v32 1 0
                                                        )
                                                      )
                                                       (
                                                        else (
                                                          inexact->exact (
                                                            floor v32
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  loop30
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
                                        loop30
                                      )
                                    )
                                  )
                                )
                                 (
                                  ret29 result
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
                fib_matrix n
              )
               (
                call/cc (
                  lambda (
                    ret33
                  )
                   (
                    begin (
                      if (
                        < n 0
                      )
                       (
                        begin (
                          panic "n is negative"
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      if (
                        equal? n 0
                      )
                       (
                        begin (
                          ret33 0
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
                          m (
                            _list (
                              _list 1 1
                            )
                             (
                              _list 1 0
                            )
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              res (
                                matrix_pow m (
                                  - n 1
                                )
                              )
                            )
                          )
                           (
                            begin (
                              ret33 (
                                cond (
                                  (
                                    string? (
                                      cond (
                                        (
                                          string? res
                                        )
                                         (
                                          _substring res 0 (
                                            + 0 1
                                          )
                                        )
                                      )
                                       (
                                        (
                                          hash-table? res
                                        )
                                         (
                                          hash-table-ref res 0
                                        )
                                      )
                                       (
                                        else (
                                          list-ref res 0
                                        )
                                      )
                                    )
                                  )
                                   (
                                    _substring (
                                      cond (
                                        (
                                          string? res
                                        )
                                         (
                                          _substring res 0 (
                                            + 0 1
                                          )
                                        )
                                      )
                                       (
                                        (
                                          hash-table? res
                                        )
                                         (
                                          hash-table-ref res 0
                                        )
                                      )
                                       (
                                        else (
                                          list-ref res 0
                                        )
                                      )
                                    )
                                     0 (
                                      + 0 1
                                    )
                                  )
                                )
                                 (
                                  (
                                    hash-table? (
                                      cond (
                                        (
                                          string? res
                                        )
                                         (
                                          _substring res 0 (
                                            + 0 1
                                          )
                                        )
                                      )
                                       (
                                        (
                                          hash-table? res
                                        )
                                         (
                                          hash-table-ref res 0
                                        )
                                      )
                                       (
                                        else (
                                          list-ref res 0
                                        )
                                      )
                                    )
                                  )
                                   (
                                    hash-table-ref (
                                      cond (
                                        (
                                          string? res
                                        )
                                         (
                                          _substring res 0 (
                                            + 0 1
                                          )
                                        )
                                      )
                                       (
                                        (
                                          hash-table? res
                                        )
                                         (
                                          hash-table-ref res 0
                                        )
                                      )
                                       (
                                        else (
                                          list-ref res 0
                                        )
                                      )
                                    )
                                     0
                                  )
                                )
                                 (
                                  else (
                                    list-ref (
                                      cond (
                                        (
                                          string? res
                                        )
                                         (
                                          _substring res 0 (
                                            + 0 1
                                          )
                                        )
                                      )
                                       (
                                        (
                                          hash-table? res
                                        )
                                         (
                                          hash-table-ref res 0
                                        )
                                      )
                                       (
                                        else (
                                          list-ref res 0
                                        )
                                      )
                                    )
                                     0
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
                run_tests
              )
               (
                call/cc (
                  lambda (
                    ret34
                  )
                   (
                    let (
                      (
                        expected (
                          _list 0 1 1 2 3 5 8 13 21 34 55
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            it (
                              fib_iterative 10
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                rec (
                                  fib_recursive 10
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    cache (
                                      fib_recursive_cached 10
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        memo (
                                          fib_memoization 10
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            bin (
                                              fib_binet 10
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                m (
                                                  fib_matrix 10
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                if (
                                                  not (
                                                    equal? it expected
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    panic "iterative failed"
                                                  )
                                                )
                                                 (
                                                  quote (
                                                    
                                                  )
                                                )
                                              )
                                               (
                                                if (
                                                  not (
                                                    equal? rec expected
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    panic "recursive failed"
                                                  )
                                                )
                                                 (
                                                  quote (
                                                    
                                                  )
                                                )
                                              )
                                               (
                                                if (
                                                  not (
                                                    equal? cache expected
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    panic "cached failed"
                                                  )
                                                )
                                                 (
                                                  quote (
                                                    
                                                  )
                                                )
                                              )
                                               (
                                                if (
                                                  not (
                                                    equal? memo expected
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    panic "memoization failed"
                                                  )
                                                )
                                                 (
                                                  quote (
                                                    
                                                  )
                                                )
                                              )
                                               (
                                                if (
                                                  not (
                                                    equal? bin expected
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    panic "binet failed"
                                                  )
                                                )
                                                 (
                                                  quote (
                                                    
                                                  )
                                                )
                                              )
                                               (
                                                if (
                                                  not (
                                                    equal? m 55
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    panic "matrix failed"
                                                  )
                                                )
                                                 (
                                                  quote (
                                                    
                                                  )
                                                )
                                              )
                                               (
                                                ret34 m
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
              _display (
                if (
                  string? (
                    to-str-space (
                      run_tests
                    )
                  )
                )
                 (
                  to-str-space (
                    run_tests
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      run_tests
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
