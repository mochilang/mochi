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
      start19 (
        current-jiffy
      )
    )
     (
      jps22 (
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
                                              list-ref xs i
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
        kth_permutation k n
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            begin (
              if (
                <= n 0
              )
               (
                begin (
                  panic "n must be positive"
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
                  factorials (
                    _list 1
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
                                      set! factorials (
                                        append factorials (
                                          _list (
                                            * (
                                              list-ref factorials (
                                                - (
                                                  _len factorials
                                                )
                                                 1
                                              )
                                            )
                                             i
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
                      let (
                        (
                          total (
                            * (
                              list-ref factorials (
                                - (
                                  _len factorials
                                )
                                 1
                              )
                            )
                             n
                          )
                        )
                      )
                       (
                        begin (
                          if (
                            or (
                              < k 0
                            )
                             (
                              >= k total
                            )
                          )
                           (
                            begin (
                              panic "k out of bounds"
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
                              elements (
                                _list
                              )
                            )
                          )
                           (
                            begin (
                              let (
                                (
                                  e 0
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
                                                < e n
                                              )
                                               (
                                                begin (
                                                  set! elements (
                                                    append elements (
                                                      _list e
                                                    )
                                                  )
                                                )
                                                 (
                                                  set! e (
                                                    + e 1
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
                                  let (
                                    (
                                      permutation (
                                        _list
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      let (
                                        (
                                          idx (
                                            - (
                                              _len factorials
                                            )
                                             1
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
                                                        >= idx 0
                                                      )
                                                       (
                                                        begin (
                                                          let (
                                                            (
                                                              factorial (
                                                                list-ref factorials idx
                                                              )
                                                            )
                                                          )
                                                           (
                                                            begin (
                                                              let (
                                                                (
                                                                  number (
                                                                    _div k factorial
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                begin (
                                                                  set! k (
                                                                    _mod k factorial
                                                                  )
                                                                )
                                                                 (
                                                                  set! permutation (
                                                                    append permutation (
                                                                      _list (
                                                                        list-ref elements number
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  set! elements (
                                                                    remove_at elements number
                                                                  )
                                                                )
                                                                 (
                                                                  set! idx (
                                                                    - idx 1
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
                                          set! permutation (
                                            append permutation (
                                              _list (
                                                list-ref elements 0
                                              )
                                            )
                                          )
                                        )
                                         (
                                          ret4 permutation
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
        list_equal a b
      )
       (
        call/cc (
          lambda (
            ret11
          )
           (
            begin (
              if (
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
                begin (
                  ret11 #f
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
                      break13
                    )
                     (
                      letrec (
                        (
                          loop12 (
                            lambda (
                              
                            )
                             (
                              if (
                                < i (
                                  _len a
                                )
                              )
                               (
                                begin (
                                  if (
                                    not (
                                      equal? (
                                        list-ref a i
                                      )
                                       (
                                        list-ref b i
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      ret11 #f
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
                                  loop12
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
                        loop12
                      )
                    )
                  )
                )
                 (
                  ret11 #t
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        list_to_string xs
      )
       (
        call/cc (
          lambda (
            ret14
          )
           (
            begin (
              if (
                equal? (
                  _len xs
                )
                 0
              )
               (
                begin (
                  ret14 "[]"
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
                  s (
                    string-append "[" (
                      to-str-space (
                        list-ref xs 0
                      )
                    )
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      i 1
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
                                    < i (
                                      _len xs
                                    )
                                  )
                                   (
                                    begin (
                                      set! s (
                                        string-append (
                                          string-append s ", "
                                        )
                                         (
                                          to-str-space (
                                            list-ref xs i
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
                      set! s (
                        string-append s "]"
                      )
                    )
                     (
                      ret14 s
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
        test_kth_permutation
      )
       (
        call/cc (
          lambda (
            ret17
          )
           (
            let (
              (
                expected1 (
                  _list 0 1 2 3 4
                )
              )
            )
             (
              begin (
                let (
                  (
                    res1 (
                      kth_permutation 0 5
                    )
                  )
                )
                 (
                  begin (
                    if (
                      not (
                        list_equal res1 expected1
                      )
                    )
                     (
                      begin (
                        panic "test case 1 failed"
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
                        expected2 (
                          _list 1 3 0 2
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            res2 (
                              kth_permutation 10 4
                            )
                          )
                        )
                         (
                          begin (
                            if (
                              not (
                                list_equal res2 expected2
                              )
                            )
                             (
                              begin (
                                panic "test case 2 failed"
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
     (
      define (
        main
      )
       (
        call/cc (
          lambda (
            ret18
          )
           (
            begin (
              test_kth_permutation
            )
             (
              let (
                (
                  res (
                    kth_permutation 10 4
                  )
                )
              )
               (
                begin (
                  _display (
                    if (
                      string? (
                        list_to_string res
                      )
                    )
                     (
                      list_to_string res
                    )
                     (
                      to-str (
                        list_to_string res
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
      main
    )
     (
      let (
        (
          end20 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur21 (
              quotient (
                * (
                  - end20 start19
                )
                 1000000
              )
               jps22
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur21
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
