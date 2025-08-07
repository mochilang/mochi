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
      start62 (
        current-jiffy
      )
    )
     (
      jps65 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        abs_int x
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            begin (
              if (
                < x 0
              )
               (
                begin (
                  ret1 (
                    - x
                  )
                )
              )
               '(
                
              )
            )
             (
              ret1 x
            )
          )
        )
      )
    )
     (
      define (
        gcd_iter a b
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            let (
              (
                x (
                  abs_int a
                )
              )
            )
             (
              begin (
                let (
                  (
                    y (
                      abs_int b
                    )
                  )
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
                                
                              )
                               (
                                if (
                                  not (
                                    equal? y 0
                                  )
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
                                        set! y (
                                          _mod x y
                                        )
                                      )
                                       (
                                        set! x t
                                      )
                                    )
                                  )
                                   (
                                    loop3
                                  )
                                )
                                 '(
                                  
                                )
                              )
                            )
                          )
                        )
                         (
                          loop3
                        )
                      )
                    )
                  )
                   (
                    ret2 x
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
        is_prime n
      )
       (
        call/cc (
          lambda (
            ret5
          )
           (
            begin (
              if (
                <= n 1
              )
               (
                begin (
                  ret5 #f
                )
              )
               '(
                
              )
            )
             (
              let (
                (
                  d 2
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
                                <= (
                                  * d d
                                )
                                 n
                              )
                               (
                                begin (
                                  if (
                                    equal? (
                                      _mod n d
                                    )
                                     0
                                  )
                                   (
                                    begin (
                                      ret5 #f
                                    )
                                  )
                                   '(
                                    
                                  )
                                )
                                 (
                                  set! d (
                                    + d 1
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
                  ret5 #t
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        sieve_er n
      )
       (
        call/cc (
          lambda (
            ret8
          )
           (
            let (
              (
                nums (
                  _list
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
                                  <= i n
                                )
                                 (
                                  begin (
                                    set! nums (
                                      append nums (
                                        _list i
                                      )
                                    )
                                  )
                                   (
                                    set! i (
                                      + i 1
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
                    let (
                      (
                        idx 0
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
                                      < idx (
                                        _len nums
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            j (
                                              + idx 1
                                            )
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
                                                          < j (
                                                            _len nums
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            if (
                                                              not (
                                                                equal? (
                                                                  list-ref-safe nums idx
                                                                )
                                                                 0
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                if (
                                                                  equal? (
                                                                    _mod (
                                                                      list-ref-safe nums j
                                                                    )
                                                                     (
                                                                      list-ref-safe nums idx
                                                                    )
                                                                  )
                                                                   0
                                                                )
                                                                 (
                                                                  begin (
                                                                    list-set! nums j 0
                                                                  )
                                                                )
                                                                 '(
                                                                  
                                                                )
                                                              )
                                                            )
                                                             '(
                                                              
                                                            )
                                                          )
                                                           (
                                                            set! j (
                                                              + j 1
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
                                            set! idx (
                                              + idx 1
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
                                k 0
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
                                              < k (
                                                _len nums
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    v (
                                                      list-ref-safe nums k
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    if (
                                                      not (
                                                        equal? v 0
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        set! res (
                                                          append res (
                                                            _list v
                                                          )
                                                        )
                                                      )
                                                    )
                                                     '(
                                                      
                                                    )
                                                  )
                                                   (
                                                    set! k (
                                                      + k 1
                                                    )
                                                  )
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
                                ret8 res
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
        get_prime_numbers n
      )
       (
        call/cc (
          lambda (
            ret17
          )
           (
            let (
              (
                ans (
                  _list
                )
              )
            )
             (
              begin (
                let (
                  (
                    num 2
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
                                
                              )
                               (
                                if (
                                  <= num n
                                )
                                 (
                                  begin (
                                    if (
                                      is_prime num
                                    )
                                     (
                                      begin (
                                        set! ans (
                                          append ans (
                                            _list num
                                          )
                                        )
                                      )
                                    )
                                     '(
                                      
                                    )
                                  )
                                   (
                                    set! num (
                                      + num 1
                                    )
                                  )
                                   (
                                    loop18
                                  )
                                )
                                 '(
                                  
                                )
                              )
                            )
                          )
                        )
                         (
                          loop18
                        )
                      )
                    )
                  )
                   (
                    ret17 ans
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
        prime_factorization number
      )
       (
        call/cc (
          lambda (
            ret20
          )
           (
            begin (
              if (
                equal? number 0
              )
               (
                begin (
                  ret20 (
                    _list 0
                  )
                )
              )
               '(
                
              )
            )
             (
              if (
                equal? number 1
              )
               (
                begin (
                  ret20 (
                    _list 1
                  )
                )
              )
               '(
                
              )
            )
             (
              let (
                (
                  ans (
                    _list
                  )
                )
              )
               (
                begin (
                  if (
                    is_prime number
                  )
                   (
                    begin (
                      set! ans (
                        append ans (
                          _list number
                        )
                      )
                    )
                     (
                      ret20 ans
                    )
                  )
                   '(
                    
                  )
                )
                 (
                  let (
                    (
                      quotient number
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          factor 2
                        )
                      )
                       (
                        begin (
                          call/cc (
                            lambda (
                              break22
                            )
                             (
                              letrec (
                                (
                                  loop21 (
                                    lambda (
                                      
                                    )
                                     (
                                      if (
                                        not (
                                          equal? quotient 1
                                        )
                                      )
                                       (
                                        begin (
                                          if (
                                            and (
                                              is_prime factor
                                            )
                                             (
                                              equal? (
                                                _mod quotient factor
                                              )
                                               0
                                            )
                                          )
                                           (
                                            begin (
                                              set! ans (
                                                append ans (
                                                  _list factor
                                                )
                                              )
                                            )
                                             (
                                              set! quotient (
                                                _div quotient factor
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              set! factor (
                                                + factor 1
                                              )
                                            )
                                          )
                                        )
                                         (
                                          loop21
                                        )
                                      )
                                       '(
                                        
                                      )
                                    )
                                  )
                                )
                              )
                               (
                                loop21
                              )
                            )
                          )
                        )
                         (
                          ret20 ans
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
        greatest_prime_factor number
      )
       (
        call/cc (
          lambda (
            ret23
          )
           (
            let (
              (
                factors (
                  prime_factorization number
                )
              )
            )
             (
              begin (
                let (
                  (
                    m (
                      cond (
                        (
                          string? factors
                        )
                         (
                          _substring factors 0 (
                            + 0 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? factors
                        )
                         (
                          hash-table-ref factors 0
                        )
                      )
                       (
                        else (
                          list-ref-safe factors 0
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
                                        _len factors
                                      )
                                    )
                                     (
                                      begin (
                                        if (
                                          _gt (
                                            cond (
                                              (
                                                string? factors
                                              )
                                               (
                                                _substring factors i (
                                                  + i 1
                                                )
                                              )
                                            )
                                             (
                                              (
                                                hash-table? factors
                                              )
                                               (
                                                hash-table-ref factors i
                                              )
                                            )
                                             (
                                              else (
                                                list-ref-safe factors i
                                              )
                                            )
                                          )
                                           m
                                        )
                                         (
                                          begin (
                                            set! m (
                                              cond (
                                                (
                                                  string? factors
                                                )
                                                 (
                                                  _substring factors i (
                                                    + i 1
                                                  )
                                                )
                                              )
                                               (
                                                (
                                                  hash-table? factors
                                                )
                                                 (
                                                  hash-table-ref factors i
                                                )
                                              )
                                               (
                                                else (
                                                  list-ref-safe factors i
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
                       (
                        ret23 m
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
        smallest_prime_factor number
      )
       (
        call/cc (
          lambda (
            ret26
          )
           (
            let (
              (
                factors (
                  prime_factorization number
                )
              )
            )
             (
              begin (
                let (
                  (
                    m (
                      cond (
                        (
                          string? factors
                        )
                         (
                          _substring factors 0 (
                            + 0 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? factors
                        )
                         (
                          hash-table-ref factors 0
                        )
                      )
                       (
                        else (
                          list-ref-safe factors 0
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
                            break28
                          )
                           (
                            letrec (
                              (
                                loop27 (
                                  lambda (
                                    
                                  )
                                   (
                                    if (
                                      < i (
                                        _len factors
                                      )
                                    )
                                     (
                                      begin (
                                        if (
                                          _lt (
                                            cond (
                                              (
                                                string? factors
                                              )
                                               (
                                                _substring factors i (
                                                  + i 1
                                                )
                                              )
                                            )
                                             (
                                              (
                                                hash-table? factors
                                              )
                                               (
                                                hash-table-ref factors i
                                              )
                                            )
                                             (
                                              else (
                                                list-ref-safe factors i
                                              )
                                            )
                                          )
                                           m
                                        )
                                         (
                                          begin (
                                            set! m (
                                              cond (
                                                (
                                                  string? factors
                                                )
                                                 (
                                                  _substring factors i (
                                                    + i 1
                                                  )
                                                )
                                              )
                                               (
                                                (
                                                  hash-table? factors
                                                )
                                                 (
                                                  hash-table-ref factors i
                                                )
                                              )
                                               (
                                                else (
                                                  list-ref-safe factors i
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
                                        loop27
                                      )
                                    )
                                     '(
                                      
                                    )
                                  )
                                )
                              )
                            )
                             (
                              loop27
                            )
                          )
                        )
                      )
                       (
                        ret26 m
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
        kg_v number1 number2
      )
       (
        call/cc (
          lambda (
            ret29
          )
           (
            begin (
              if (
                or (
                  < number1 1
                )
                 (
                  < number2 1
                )
              )
               (
                begin (
                  panic "numbers must be positive"
                )
              )
               '(
                
              )
            )
             (
              let (
                (
                  g (
                    gcd_iter number1 number2
                  )
                )
              )
               (
                begin (
                  ret29 (
                    * (
                      _div number1 g
                    )
                     number2
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
        is_even number
      )
       (
        call/cc (
          lambda (
            ret30
          )
           (
            ret30 (
              equal? (
                _mod number 2
              )
               0
            )
          )
        )
      )
    )
     (
      define (
        is_odd number
      )
       (
        call/cc (
          lambda (
            ret31
          )
           (
            ret31 (
              not (
                equal? (
                  _mod number 2
                )
                 0
              )
            )
          )
        )
      )
    )
     (
      define (
        goldbach number
      )
       (
        call/cc (
          lambda (
            ret32
          )
           (
            begin (
              if (
                or (
                  not (
                    is_even number
                  )
                )
                 (
                  <= number 2
                )
              )
               (
                begin (
                  panic "number must be even and > 2"
                )
              )
               '(
                
              )
            )
             (
              let (
                (
                  primes (
                    get_prime_numbers number
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
                          break34
                        )
                         (
                          letrec (
                            (
                              loop33 (
                                lambda (
                                  
                                )
                                 (
                                  if (
                                    < i (
                                      _len primes
                                    )
                                  )
                                   (
                                    begin (
                                      let (
                                        (
                                          j (
                                            + i 1
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          call/cc (
                                            lambda (
                                              break36
                                            )
                                             (
                                              letrec (
                                                (
                                                  loop35 (
                                                    lambda (
                                                      
                                                    )
                                                     (
                                                      if (
                                                        < j (
                                                          _len primes
                                                        )
                                                      )
                                                       (
                                                        begin (
                                                          if (
                                                            equal? (
                                                              _add (
                                                                cond (
                                                                  (
                                                                    string? primes
                                                                  )
                                                                   (
                                                                    _substring primes i (
                                                                      + i 1
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  (
                                                                    hash-table? primes
                                                                  )
                                                                   (
                                                                    hash-table-ref primes i
                                                                  )
                                                                )
                                                                 (
                                                                  else (
                                                                    list-ref-safe primes i
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                cond (
                                                                  (
                                                                    string? primes
                                                                  )
                                                                   (
                                                                    _substring primes j (
                                                                      + j 1
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  (
                                                                    hash-table? primes
                                                                  )
                                                                   (
                                                                    hash-table-ref primes j
                                                                  )
                                                                )
                                                                 (
                                                                  else (
                                                                    list-ref-safe primes j
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             number
                                                          )
                                                           (
                                                            begin (
                                                              ret32 (
                                                                _list (
                                                                  cond (
                                                                    (
                                                                      string? primes
                                                                    )
                                                                     (
                                                                      _substring primes i (
                                                                        + i 1
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    (
                                                                      hash-table? primes
                                                                    )
                                                                     (
                                                                      hash-table-ref primes i
                                                                    )
                                                                  )
                                                                   (
                                                                    else (
                                                                      list-ref-safe primes i
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  cond (
                                                                    (
                                                                      string? primes
                                                                    )
                                                                     (
                                                                      _substring primes j (
                                                                        + j 1
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    (
                                                                      hash-table? primes
                                                                    )
                                                                     (
                                                                      hash-table-ref primes j
                                                                    )
                                                                  )
                                                                   (
                                                                    else (
                                                                      list-ref-safe primes j
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
                                                          set! j (
                                                            + j 1
                                                          )
                                                        )
                                                         (
                                                          loop35
                                                        )
                                                      )
                                                       '(
                                                        
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                loop35
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
                                    )
                                     (
                                      loop33
                                    )
                                  )
                                   '(
                                    
                                  )
                                )
                              )
                            )
                          )
                           (
                            loop33
                          )
                        )
                      )
                    )
                     (
                      ret32 (
                        _list
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
        get_prime n
      )
       (
        call/cc (
          lambda (
            ret37
          )
           (
            begin (
              if (
                < n 0
              )
               (
                begin (
                  panic "n must be non-negative"
                )
              )
               '(
                
              )
            )
             (
              let (
                (
                  index 0
                )
              )
               (
                begin (
                  let (
                    (
                      ans 2
                    )
                  )
                   (
                    begin (
                      call/cc (
                        lambda (
                          break39
                        )
                         (
                          letrec (
                            (
                              loop38 (
                                lambda (
                                  
                                )
                                 (
                                  if (
                                    < index n
                                  )
                                   (
                                    begin (
                                      set! index (
                                        + index 1
                                      )
                                    )
                                     (
                                      set! ans (
                                        + ans 1
                                      )
                                    )
                                     (
                                      call/cc (
                                        lambda (
                                          break41
                                        )
                                         (
                                          letrec (
                                            (
                                              loop40 (
                                                lambda (
                                                  
                                                )
                                                 (
                                                  if (
                                                    not (
                                                      is_prime ans
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      set! ans (
                                                        + ans 1
                                                      )
                                                    )
                                                     (
                                                      loop40
                                                    )
                                                  )
                                                   '(
                                                    
                                                  )
                                                )
                                              )
                                            )
                                          )
                                           (
                                            loop40
                                          )
                                        )
                                      )
                                    )
                                     (
                                      loop38
                                    )
                                  )
                                   '(
                                    
                                  )
                                )
                              )
                            )
                          )
                           (
                            loop38
                          )
                        )
                      )
                    )
                     (
                      ret37 ans
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
        get_primes_between p1 p2
      )
       (
        call/cc (
          lambda (
            ret42
          )
           (
            let (
              (
                bad1 (
                  not (
                    is_prime p1
                  )
                )
              )
            )
             (
              begin (
                let (
                  (
                    bad2 (
                      not (
                        is_prime p2
                      )
                    )
                  )
                )
                 (
                  begin (
                    if (
                      or (
                        or bad1 bad2
                      )
                       (
                        >= p1 p2
                      )
                    )
                     (
                      begin (
                        panic "arguments must be prime and p1 < p2"
                      )
                    )
                     '(
                      
                    )
                  )
                   (
                    let (
                      (
                        num (
                          + p1 1
                        )
                      )
                    )
                     (
                      begin (
                        call/cc (
                          lambda (
                            break44
                          )
                           (
                            letrec (
                              (
                                loop43 (
                                  lambda (
                                    
                                  )
                                   (
                                    if (
                                      < num p2
                                    )
                                     (
                                      begin (
                                        if (
                                          is_prime num
                                        )
                                         (
                                          begin (
                                            break44 '(
                                              
                                            )
                                          )
                                        )
                                         '(
                                          
                                        )
                                      )
                                       (
                                        set! num (
                                          + num 1
                                        )
                                      )
                                       (
                                        loop43
                                      )
                                    )
                                     '(
                                      
                                    )
                                  )
                                )
                              )
                            )
                             (
                              loop43
                            )
                          )
                        )
                      )
                       (
                        let (
                          (
                            ans (
                              _list
                            )
                          )
                        )
                         (
                          begin (
                            call/cc (
                              lambda (
                                break46
                              )
                               (
                                letrec (
                                  (
                                    loop45 (
                                      lambda (
                                        
                                      )
                                       (
                                        if (
                                          < num p2
                                        )
                                         (
                                          begin (
                                            set! ans (
                                              append ans (
                                                _list num
                                              )
                                            )
                                          )
                                           (
                                            set! num (
                                              + num 1
                                            )
                                          )
                                           (
                                            call/cc (
                                              lambda (
                                                break48
                                              )
                                               (
                                                letrec (
                                                  (
                                                    loop47 (
                                                      lambda (
                                                        
                                                      )
                                                       (
                                                        if (
                                                          < num p2
                                                        )
                                                         (
                                                          begin (
                                                            if (
                                                              is_prime num
                                                            )
                                                             (
                                                              begin (
                                                                break48 '(
                                                                  
                                                                )
                                                              )
                                                            )
                                                             '(
                                                              
                                                            )
                                                          )
                                                           (
                                                            set! num (
                                                              + num 1
                                                            )
                                                          )
                                                           (
                                                            loop47
                                                          )
                                                        )
                                                         '(
                                                          
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  loop47
                                                )
                                              )
                                            )
                                          )
                                           (
                                            loop45
                                          )
                                        )
                                         '(
                                          
                                        )
                                      )
                                    )
                                  )
                                )
                                 (
                                  loop45
                                )
                              )
                            )
                          )
                           (
                            ret42 ans
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
        get_divisors n
      )
       (
        call/cc (
          lambda (
            ret49
          )
           (
            begin (
              if (
                < n 1
              )
               (
                begin (
                  panic "n must be >= 1"
                )
              )
               '(
                
              )
            )
             (
              let (
                (
                  ans (
                    _list
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      d 1
                    )
                  )
                   (
                    begin (
                      call/cc (
                        lambda (
                          break51
                        )
                         (
                          letrec (
                            (
                              loop50 (
                                lambda (
                                  
                                )
                                 (
                                  if (
                                    <= d n
                                  )
                                   (
                                    begin (
                                      if (
                                        equal? (
                                          _mod n d
                                        )
                                         0
                                      )
                                       (
                                        begin (
                                          set! ans (
                                            append ans (
                                              _list d
                                            )
                                          )
                                        )
                                      )
                                       '(
                                        
                                      )
                                    )
                                     (
                                      set! d (
                                        + d 1
                                      )
                                    )
                                     (
                                      loop50
                                    )
                                  )
                                   '(
                                    
                                  )
                                )
                              )
                            )
                          )
                           (
                            loop50
                          )
                        )
                      )
                    )
                     (
                      ret49 ans
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
        is_perfect_number number
      )
       (
        call/cc (
          lambda (
            ret52
          )
           (
            begin (
              if (
                <= number 1
              )
               (
                begin (
                  panic "number must be > 1"
                )
              )
               '(
                
              )
            )
             (
              let (
                (
                  divisors (
                    get_divisors number
                  )
                )
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
                          i 0
                        )
                      )
                       (
                        begin (
                          call/cc (
                            lambda (
                              break54
                            )
                             (
                              letrec (
                                (
                                  loop53 (
                                    lambda (
                                      
                                    )
                                     (
                                      if (
                                        < i (
                                          - (
                                            _len divisors
                                          )
                                           1
                                        )
                                      )
                                       (
                                        begin (
                                          set! sum (
                                            _add sum (
                                              cond (
                                                (
                                                  string? divisors
                                                )
                                                 (
                                                  _substring divisors i (
                                                    + i 1
                                                  )
                                                )
                                              )
                                               (
                                                (
                                                  hash-table? divisors
                                                )
                                                 (
                                                  hash-table-ref divisors i
                                                )
                                              )
                                               (
                                                else (
                                                  list-ref-safe divisors i
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
                                          loop53
                                        )
                                      )
                                       '(
                                        
                                      )
                                    )
                                  )
                                )
                              )
                               (
                                loop53
                              )
                            )
                          )
                        )
                         (
                          ret52 (
                            equal? sum number
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
        simplify_fraction numerator denominator
      )
       (
        call/cc (
          lambda (
            ret55
          )
           (
            begin (
              if (
                equal? denominator 0
              )
               (
                begin (
                  panic "denominator cannot be zero"
                )
              )
               '(
                
              )
            )
             (
              let (
                (
                  g (
                    gcd_iter (
                      abs_int numerator
                    )
                     (
                      abs_int denominator
                    )
                  )
                )
              )
               (
                begin (
                  ret55 (
                    _list (
                      _div numerator g
                    )
                     (
                      _div denominator g
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
        factorial n
      )
       (
        call/cc (
          lambda (
            ret56
          )
           (
            begin (
              if (
                < n 0
              )
               (
                begin (
                  panic "n must be >= 0"
                )
              )
               '(
                
              )
            )
             (
              let (
                (
                  ans 1
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
                          break58
                        )
                         (
                          letrec (
                            (
                              loop57 (
                                lambda (
                                  
                                )
                                 (
                                  if (
                                    <= i n
                                  )
                                   (
                                    begin (
                                      set! ans (
                                        * ans i
                                      )
                                    )
                                     (
                                      set! i (
                                        + i 1
                                      )
                                    )
                                     (
                                      loop57
                                    )
                                  )
                                   '(
                                    
                                  )
                                )
                              )
                            )
                          )
                           (
                            loop57
                          )
                        )
                      )
                    )
                     (
                      ret56 ans
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
        fib n
      )
       (
        call/cc (
          lambda (
            ret59
          )
           (
            begin (
              if (
                < n 0
              )
               (
                begin (
                  panic "n must be >= 0"
                )
              )
               '(
                
              )
            )
             (
              if (
                <= n 1
              )
               (
                begin (
                  ret59 1
                )
              )
               '(
                
              )
            )
             (
              let (
                (
                  tmp 0
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
                          ans 1
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
                                  break61
                                )
                                 (
                                  letrec (
                                    (
                                      loop60 (
                                        lambda (
                                          
                                        )
                                         (
                                          if (
                                            < i (
                                              - n 1
                                            )
                                          )
                                           (
                                            begin (
                                              set! tmp ans
                                            )
                                             (
                                              set! ans (
                                                + ans fib1
                                              )
                                            )
                                             (
                                              set! fib1 tmp
                                            )
                                             (
                                              set! i (
                                                + i 1
                                              )
                                            )
                                             (
                                              loop60
                                            )
                                          )
                                           '(
                                            
                                          )
                                        )
                                      )
                                    )
                                  )
                                   (
                                    loop60
                                  )
                                )
                              )
                            )
                             (
                              ret59 ans
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
              is_prime 97
            )
          )
        )
         (
          to-str-space (
            is_prime 97
          )
        )
         (
          to-str (
            to-str-space (
              is_prime 97
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
              sieve_er 20
            )
          )
        )
         (
          to-str-space (
            sieve_er 20
          )
        )
         (
          to-str (
            to-str-space (
              sieve_er 20
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
              get_prime_numbers 20
            )
          )
        )
         (
          to-str-space (
            get_prime_numbers 20
          )
        )
         (
          to-str (
            to-str-space (
              get_prime_numbers 20
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
              prime_factorization 287
            )
          )
        )
         (
          to-str-space (
            prime_factorization 287
          )
        )
         (
          to-str (
            to-str-space (
              prime_factorization 287
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
              greatest_prime_factor 287
            )
          )
        )
         (
          to-str-space (
            greatest_prime_factor 287
          )
        )
         (
          to-str (
            to-str-space (
              greatest_prime_factor 287
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
              smallest_prime_factor 287
            )
          )
        )
         (
          to-str-space (
            smallest_prime_factor 287
          )
        )
         (
          to-str (
            to-str-space (
              smallest_prime_factor 287
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
              kg_v 8 10
            )
          )
        )
         (
          to-str-space (
            kg_v 8 10
          )
        )
         (
          to-str (
            to-str-space (
              kg_v 8 10
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
              goldbach 28
            )
          )
        )
         (
          to-str-space (
            goldbach 28
          )
        )
         (
          to-str (
            to-str-space (
              goldbach 28
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
              get_prime 8
            )
          )
        )
         (
          to-str-space (
            get_prime 8
          )
        )
         (
          to-str (
            to-str-space (
              get_prime 8
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
              get_primes_between 3 20
            )
          )
        )
         (
          to-str-space (
            get_primes_between 3 20
          )
        )
         (
          to-str (
            to-str-space (
              get_primes_between 3 20
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
              get_divisors 28
            )
          )
        )
         (
          to-str-space (
            get_divisors 28
          )
        )
         (
          to-str (
            to-str-space (
              get_divisors 28
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
              is_perfect_number 28
            )
          )
        )
         (
          to-str-space (
            is_perfect_number 28
          )
        )
         (
          to-str (
            to-str-space (
              is_perfect_number 28
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
              simplify_fraction 10 20
            )
          )
        )
         (
          to-str-space (
            simplify_fraction 10 20
          )
        )
         (
          to-str (
            to-str-space (
              simplify_fraction 10 20
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
              factorial 5
            )
          )
        )
         (
          to-str-space (
            factorial 5
          )
        )
         (
          to-str (
            to-str-space (
              factorial 5
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
              fib 10
            )
          )
        )
         (
          to-str-space (
            fib 10
          )
        )
         (
          to-str (
            to-str-space (
              fib 10
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
          end63 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur64 (
              quotient (
                * (
                  - end63 start62
                )
                 1000000
              )
               jps65
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur64
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
