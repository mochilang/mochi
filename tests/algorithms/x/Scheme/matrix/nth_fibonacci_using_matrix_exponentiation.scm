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
      start26 (
        current-jiffy
      )
    )
     (
      jps29 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        multiply matrix_a matrix_b
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
                  _len matrix_a
                )
              )
            )
             (
              begin (
                let (
                  (
                    matrix_c (
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
                                                              < j n
                                                            )
                                                             (
                                                              begin (
                                                                let (
                                                                  (
                                                                    val 0
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
                                                                                      < k n
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        set! val (
                                                                                          + val (
                                                                                            * (
                                                                                              cond (
                                                                                                (
                                                                                                  string? (
                                                                                                    list-ref-safe matrix_a i
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  _substring (
                                                                                                    list-ref-safe matrix_a i
                                                                                                  )
                                                                                                   k (
                                                                                                    + k 1
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                (
                                                                                                  hash-table? (
                                                                                                    list-ref-safe matrix_a i
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  hash-table-ref (
                                                                                                    list-ref-safe matrix_a i
                                                                                                  )
                                                                                                   k
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                else (
                                                                                                  list-ref-safe (
                                                                                                    list-ref-safe matrix_a i
                                                                                                  )
                                                                                                   k
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              cond (
                                                                                                (
                                                                                                  string? (
                                                                                                    list-ref-safe matrix_b k
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  _substring (
                                                                                                    list-ref-safe matrix_b k
                                                                                                  )
                                                                                                   j (
                                                                                                    + j 1
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                (
                                                                                                  hash-table? (
                                                                                                    list-ref-safe matrix_b k
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  hash-table-ref (
                                                                                                    list-ref-safe matrix_b k
                                                                                                  )
                                                                                                   j
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                else (
                                                                                                  list-ref-safe (
                                                                                                    list-ref-safe matrix_b k
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
                                                                                        loop6
                                                                                      )
                                                                                    )
                                                                                     '(
                                                                                      
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
                                                                        set! row (
                                                                          append row (
                                                                            _list val
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
                                                                loop4
                                                              )
                                                            )
                                                             '(
                                                              
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
                                                set! matrix_c (
                                                  append matrix_c (
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
                        ret1 matrix_c
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
        identity n
      )
       (
        call/cc (
          lambda (
            ret8
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
                                                          < j n
                                                        )
                                                         (
                                                          begin (
                                                            if (
                                                              equal? i j
                                                            )
                                                             (
                                                              begin (
                                                                set! row (
                                                                  append row (
                                                                    _list 1
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                set! row (
                                                                  append row (
                                                                    _list 0
                                                                  )
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
                                            set! res (
                                              append res (
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
                    ret8 res
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
        nth_fibonacci_matrix n
      )
       (
        call/cc (
          lambda (
            ret13
          )
           (
            begin (
              if (
                <= n 1
              )
               (
                begin (
                  ret13 n
                )
              )
               '(
                
              )
            )
             (
              let (
                (
                  res_matrix (
                    identity 2
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      fib_matrix (
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
                          m (
                            - n 1
                          )
                        )
                      )
                       (
                        begin (
                          call/cc (
                            lambda (
                              break15
                            )
                             (
                              letrec (
                                (
                                  loop14 (
                                    lambda (
                                      
                                    )
                                     (
                                      if (
                                        > m 0
                                      )
                                       (
                                        begin (
                                          if (
                                            equal? (
                                              _mod m 2
                                            )
                                             1
                                          )
                                           (
                                            begin (
                                              set! res_matrix (
                                                multiply res_matrix fib_matrix
                                              )
                                            )
                                          )
                                           '(
                                            
                                          )
                                        )
                                         (
                                          set! fib_matrix (
                                            multiply fib_matrix fib_matrix
                                          )
                                        )
                                         (
                                          set! m (
                                            _div m 2
                                          )
                                        )
                                         (
                                          loop14
                                        )
                                      )
                                       '(
                                        
                                      )
                                    )
                                  )
                                )
                              )
                               (
                                loop14
                              )
                            )
                          )
                        )
                         (
                          ret13 (
                            cond (
                              (
                                string? (
                                  cond (
                                    (
                                      string? res_matrix
                                    )
                                     (
                                      _substring res_matrix 0 (
                                        + 0 1
                                      )
                                    )
                                  )
                                   (
                                    (
                                      hash-table? res_matrix
                                    )
                                     (
                                      hash-table-ref res_matrix 0
                                    )
                                  )
                                   (
                                    else (
                                      list-ref-safe res_matrix 0
                                    )
                                  )
                                )
                              )
                               (
                                _substring (
                                  cond (
                                    (
                                      string? res_matrix
                                    )
                                     (
                                      _substring res_matrix 0 (
                                        + 0 1
                                      )
                                    )
                                  )
                                   (
                                    (
                                      hash-table? res_matrix
                                    )
                                     (
                                      hash-table-ref res_matrix 0
                                    )
                                  )
                                   (
                                    else (
                                      list-ref-safe res_matrix 0
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
                                      string? res_matrix
                                    )
                                     (
                                      _substring res_matrix 0 (
                                        + 0 1
                                      )
                                    )
                                  )
                                   (
                                    (
                                      hash-table? res_matrix
                                    )
                                     (
                                      hash-table-ref res_matrix 0
                                    )
                                  )
                                   (
                                    else (
                                      list-ref-safe res_matrix 0
                                    )
                                  )
                                )
                              )
                               (
                                hash-table-ref (
                                  cond (
                                    (
                                      string? res_matrix
                                    )
                                     (
                                      _substring res_matrix 0 (
                                        + 0 1
                                      )
                                    )
                                  )
                                   (
                                    (
                                      hash-table? res_matrix
                                    )
                                     (
                                      hash-table-ref res_matrix 0
                                    )
                                  )
                                   (
                                    else (
                                      list-ref-safe res_matrix 0
                                    )
                                  )
                                )
                                 0
                              )
                            )
                             (
                              else (
                                list-ref-safe (
                                  cond (
                                    (
                                      string? res_matrix
                                    )
                                     (
                                      _substring res_matrix 0 (
                                        + 0 1
                                      )
                                    )
                                  )
                                   (
                                    (
                                      hash-table? res_matrix
                                    )
                                     (
                                      hash-table-ref res_matrix 0
                                    )
                                  )
                                   (
                                    else (
                                      list-ref-safe res_matrix 0
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
      )
    )
     (
      define (
        nth_fibonacci_bruteforce n
      )
       (
        call/cc (
          lambda (
            ret16
          )
           (
            begin (
              if (
                <= n 1
              )
               (
                begin (
                  ret16 n
                )
              )
               '(
                
              )
            )
             (
              let (
                (
                  fib0 0
                )
              )
               (
                begin (
                  let (
                    (
                      fib1 1
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
                              break18
                            )
                             (
                              letrec (
                                (
                                  loop17 (
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
                                              next (
                                                + fib0 fib1
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              set! fib0 fib1
                                            )
                                             (
                                              set! fib1 next
                                            )
                                             (
                                              set! i (
                                                + i 1
                                              )
                                            )
                                          )
                                        )
                                         (
                                          loop17
                                        )
                                      )
                                       '(
                                        
                                      )
                                    )
                                  )
                                )
                              )
                               (
                                loop17
                              )
                            )
                          )
                        )
                         (
                          ret16 fib1
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
        parse_number s
      )
       (
        call/cc (
          lambda (
            ret19
          )
           (
            let (
              (
                result 0
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
                        break21
                      )
                       (
                        letrec (
                          (
                            loop20 (
                              lambda (
                                
                              )
                               (
                                if (
                                  < i (
                                    _len s
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        ch (
                                          _substring s i (
                                            + i 1
                                          )
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        if (
                                          and (
                                            string>=? ch "0"
                                          )
                                           (
                                            string<=? ch "9"
                                          )
                                        )
                                         (
                                          begin (
                                            set! result (
                                              + (
                                                * result 10
                                              )
                                               (
                                                let (
                                                  (
                                                    v22 ch
                                                  )
                                                )
                                                 (
                                                  cond (
                                                    (
                                                      string? v22
                                                    )
                                                     (
                                                      inexact->exact (
                                                        floor (
                                                          string->number v22
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    (
                                                      boolean? v22
                                                    )
                                                     (
                                                      if v22 1 0
                                                    )
                                                  )
                                                   (
                                                    else (
                                                      inexact->exact (
                                                        floor v22
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
                                        set! i (
                                          + i 1
                                        )
                                      )
                                    )
                                  )
                                   (
                                    loop20
                                  )
                                )
                                 '(
                                  
                                )
                              )
                            )
                          )
                        )
                         (
                          loop20
                        )
                      )
                    )
                  )
                   (
                    ret19 result
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
            ret23
          )
           (
            let (
              (
                ordinals (
                  _list "0th" "1st" "2nd" "3rd" "10th" "100th" "1000th"
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
                        break25
                      )
                       (
                        letrec (
                          (
                            loop24 (
                              lambda (
                                
                              )
                               (
                                if (
                                  < i (
                                    _len ordinals
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        ordinal (
                                          list-ref-safe ordinals i
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            n (
                                              parse_number ordinal
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                msg (
                                                  string-append (
                                                    string-append (
                                                      string-append (
                                                        string-append ordinal " fibonacci number using matrix exponentiation is "
                                                      )
                                                       (
                                                        to-str-space (
                                                          nth_fibonacci_matrix n
                                                        )
                                                      )
                                                    )
                                                     " and using bruteforce is "
                                                  )
                                                   (
                                                    to-str-space (
                                                      nth_fibonacci_bruteforce n
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                _display (
                                                  if (
                                                    string? msg
                                                  )
                                                   msg (
                                                    to-str msg
                                                  )
                                                )
                                              )
                                               (
                                                newline
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
                                    )
                                  )
                                   (
                                    loop24
                                  )
                                )
                                 '(
                                  
                                )
                              )
                            )
                          )
                        )
                         (
                          loop24
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
          end27 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur28 (
              quotient (
                * (
                  - end27 start26
                )
                 1000000
              )
               jps29
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur28
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
