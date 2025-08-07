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
      start75 (
        current-jiffy
      )
    )
     (
      jps78 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      let (
        (
          seed 1
        )
      )
       (
        begin (
          define (
            rand
          )
           (
            call/cc (
              lambda (
                ret1
              )
               (
                begin (
                  set! seed (
                    _mod (
                      + (
                        * seed 1103515245
                      )
                       12345
                    )
                     2147483648
                  )
                )
                 (
                  ret1 seed
                )
              )
            )
          )
        )
         (
          define (
            random
          )
           (
            call/cc (
              lambda (
                ret2
              )
               (
                ret2 (
                  _div (
                    * 1.0 (
                      rand
                    )
                  )
                   2147483648.0
                )
              )
            )
          )
        )
         (
          define (
            expApprox x
          )
           (
            call/cc (
              lambda (
                ret3
              )
               (
                let (
                  (
                    y x
                  )
                )
                 (
                  begin (
                    let (
                      (
                        is_neg #f
                      )
                    )
                     (
                      begin (
                        if (
                          < x 0.0
                        )
                         (
                          begin (
                            set! is_neg #t
                          )
                           (
                            set! y (
                              - x
                            )
                          )
                        )
                         '(
                          
                        )
                      )
                       (
                        let (
                          (
                            term 1.0
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                sum 1.0
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    n 1
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
                                                  < n 30
                                                )
                                                 (
                                                  begin (
                                                    set! term (
                                                      _div (
                                                        * term y
                                                      )
                                                       (
                                                        + 0.0 n
                                                      )
                                                    )
                                                  )
                                                   (
                                                    set! sum (
                                                      + sum term
                                                    )
                                                  )
                                                   (
                                                    set! n (
                                                      + n 1
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
                                    if is_neg (
                                      begin (
                                        ret3 (
                                          _div 1.0 sum
                                        )
                                      )
                                    )
                                     '(
                                      
                                    )
                                  )
                                   (
                                    ret3 sum
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
            sigmoid z
          )
           (
            call/cc (
              lambda (
                ret6
              )
               (
                ret6 (
                  _div 1.0 (
                    _add 1.0 (
                      expApprox (
                        - z
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
            sigmoid_vec v
          )
           (
            call/cc (
              lambda (
                ret7
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
                                      < i (
                                        _len v
                                      )
                                    )
                                     (
                                      begin (
                                        set! res (
                                          append res (
                                            _list (
                                              sigmoid (
                                                list-ref-safe v i
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
                        ret7 res
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
            sigmoid_derivative out
          )
           (
            call/cc (
              lambda (
                ret10
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
                                        _len out
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            val (
                                              list-ref-safe out i
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            set! res (
                                              append res (
                                                _list (
                                                  * val (
                                                    - 1.0 val
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
                        ret10 res
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
            random_vector n
          )
           (
            call/cc (
              lambda (
                ret13
              )
               (
                let (
                  (
                    v (
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
                                      < i n
                                    )
                                     (
                                      begin (
                                        set! v (
                                          append v (
                                            _list (
                                              - (
                                                random
                                              )
                                               0.5
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
                        ret13 v
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
            random_matrix r c
          )
           (
            call/cc (
              lambda (
                ret16
              )
               (
                let (
                  (
                    m (
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
                                      < i r
                                    )
                                     (
                                      begin (
                                        set! m (
                                          append m (
                                            _list (
                                              random_vector c
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
                        ret16 m
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
            matvec mat vec
          )
           (
            call/cc (
              lambda (
                ret19
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
                                        _len mat
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            s 0.0
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
                                                    break23
                                                  )
                                                   (
                                                    letrec (
                                                      (
                                                        loop22 (
                                                          lambda (
                                                            
                                                          )
                                                           (
                                                            if (
                                                              < j (
                                                                _len vec
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                set! s (
                                                                  _add s (
                                                                    * (
                                                                      cond (
                                                                        (
                                                                          string? (
                                                                            list-ref-safe mat i
                                                                          )
                                                                        )
                                                                         (
                                                                          _substring (
                                                                            list-ref-safe mat i
                                                                          )
                                                                           j (
                                                                            + j 1
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        (
                                                                          hash-table? (
                                                                            list-ref-safe mat i
                                                                          )
                                                                        )
                                                                         (
                                                                          hash-table-ref (
                                                                            list-ref-safe mat i
                                                                          )
                                                                           j
                                                                        )
                                                                      )
                                                                       (
                                                                        else (
                                                                          list-ref-safe (
                                                                            list-ref-safe mat i
                                                                          )
                                                                           j
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      list-ref-safe vec j
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
                                                                loop22
                                                              )
                                                            )
                                                             '(
                                                              
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      loop22
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                set! res (
                                                  append res (
                                                    _list s
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
                        ret19 res
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
            matTvec mat vec
          )
           (
            call/cc (
              lambda (
                ret24
              )
               (
                let (
                  (
                    cols (
                      _len (
                        list-ref-safe mat 0
                      )
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
                            j 0
                          )
                        )
                         (
                          begin (
                            call/cc (
                              lambda (
                                break26
                              )
                               (
                                letrec (
                                  (
                                    loop25 (
                                      lambda (
                                        
                                      )
                                       (
                                        if (
                                          < j cols
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                s 0.0
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
                                                                    _len mat
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! s (
                                                                      _add s (
                                                                        * (
                                                                          cond (
                                                                            (
                                                                              string? (
                                                                                list-ref-safe mat i
                                                                              )
                                                                            )
                                                                             (
                                                                              _substring (
                                                                                list-ref-safe mat i
                                                                              )
                                                                               j (
                                                                                + j 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            (
                                                                              hash-table? (
                                                                                list-ref-safe mat i
                                                                              )
                                                                            )
                                                                             (
                                                                              hash-table-ref (
                                                                                list-ref-safe mat i
                                                                              )
                                                                               j
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref-safe (
                                                                                list-ref-safe mat i
                                                                              )
                                                                               j
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          list-ref-safe vec i
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
                                                    set! res (
                                                      append res (
                                                        _list s
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
                                            loop25
                                          )
                                        )
                                         '(
                                          
                                        )
                                      )
                                    )
                                  )
                                )
                                 (
                                  loop25
                                )
                              )
                            )
                          )
                           (
                            ret24 res
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
            vec_sub a b
          )
           (
            call/cc (
              lambda (
                ret29
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
                                      < i (
                                        _len a
                                      )
                                    )
                                     (
                                      begin (
                                        set! res (
                                          append res (
                                            _list (
                                              - (
                                                list-ref-safe a i
                                              )
                                               (
                                                list-ref-safe b i
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
                                        loop30
                                      )
                                    )
                                     '(
                                      
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
                        ret29 res
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
            vec_mul a b
          )
           (
            call/cc (
              lambda (
                ret32
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
                                        _len a
                                      )
                                    )
                                     (
                                      begin (
                                        set! res (
                                          append res (
                                            _list (
                                              * (
                                                list-ref-safe a i
                                              )
                                               (
                                                list-ref-safe b i
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
                        ret32 res
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
            vec_scalar_mul v s
          )
           (
            call/cc (
              lambda (
                ret35
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
                            break37
                          )
                           (
                            letrec (
                              (
                                loop36 (
                                  lambda (
                                    
                                  )
                                   (
                                    if (
                                      < i (
                                        _len v
                                      )
                                    )
                                     (
                                      begin (
                                        set! res (
                                          append res (
                                            _list (
                                              * (
                                                list-ref-safe v i
                                              )
                                               s
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
                                        loop36
                                      )
                                    )
                                     '(
                                      
                                    )
                                  )
                                )
                              )
                            )
                             (
                              loop36
                            )
                          )
                        )
                      )
                       (
                        ret35 res
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
            outer a b
          )
           (
            call/cc (
              lambda (
                ret38
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
                            break40
                          )
                           (
                            letrec (
                              (
                                loop39 (
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
                                                    break42
                                                  )
                                                   (
                                                    letrec (
                                                      (
                                                        loop41 (
                                                          lambda (
                                                            
                                                          )
                                                           (
                                                            if (
                                                              < j (
                                                                _len b
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                set! row (
                                                                  append row (
                                                                    _list (
                                                                      * (
                                                                        list-ref-safe a i
                                                                      )
                                                                       (
                                                                        list-ref-safe b j
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
                                                                loop41
                                                              )
                                                            )
                                                             '(
                                                              
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      loop41
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
                                        loop39
                                      )
                                    )
                                     '(
                                      
                                    )
                                  )
                                )
                              )
                            )
                             (
                              loop39
                            )
                          )
                        )
                      )
                       (
                        ret38 res
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
            mat_scalar_mul mat s
          )
           (
            call/cc (
              lambda (
                ret43
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
                            break45
                          )
                           (
                            letrec (
                              (
                                loop44 (
                                  lambda (
                                    
                                  )
                                   (
                                    if (
                                      < i (
                                        _len mat
                                      )
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
                                                    break47
                                                  )
                                                   (
                                                    letrec (
                                                      (
                                                        loop46 (
                                                          lambda (
                                                            
                                                          )
                                                           (
                                                            if (
                                                              < j (
                                                                _len (
                                                                  list-ref-safe mat i
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                set! row (
                                                                  append row (
                                                                    _list (
                                                                      * (
                                                                        cond (
                                                                          (
                                                                            string? (
                                                                              list-ref-safe mat i
                                                                            )
                                                                          )
                                                                           (
                                                                            _substring (
                                                                              list-ref-safe mat i
                                                                            )
                                                                             j (
                                                                              + j 1
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          (
                                                                            hash-table? (
                                                                              list-ref-safe mat i
                                                                            )
                                                                          )
                                                                           (
                                                                            hash-table-ref (
                                                                              list-ref-safe mat i
                                                                            )
                                                                             j
                                                                          )
                                                                        )
                                                                         (
                                                                          else (
                                                                            list-ref-safe (
                                                                              list-ref-safe mat i
                                                                            )
                                                                             j
                                                                          )
                                                                        )
                                                                      )
                                                                       s
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
                                                                loop46
                                                              )
                                                            )
                                                             '(
                                                              
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      loop46
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
                                        loop44
                                      )
                                    )
                                     '(
                                      
                                    )
                                  )
                                )
                              )
                            )
                             (
                              loop44
                            )
                          )
                        )
                      )
                       (
                        ret43 res
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
            mat_sub a b
          )
           (
            call/cc (
              lambda (
                ret48
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
                            break50
                          )
                           (
                            letrec (
                              (
                                loop49 (
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
                                                    break52
                                                  )
                                                   (
                                                    letrec (
                                                      (
                                                        loop51 (
                                                          lambda (
                                                            
                                                          )
                                                           (
                                                            if (
                                                              < j (
                                                                _len (
                                                                  list-ref-safe a i
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                set! row (
                                                                  append row (
                                                                    _list (
                                                                      - (
                                                                        cond (
                                                                          (
                                                                            string? (
                                                                              list-ref-safe a i
                                                                            )
                                                                          )
                                                                           (
                                                                            _substring (
                                                                              list-ref-safe a i
                                                                            )
                                                                             j (
                                                                              + j 1
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          (
                                                                            hash-table? (
                                                                              list-ref-safe a i
                                                                            )
                                                                          )
                                                                           (
                                                                            hash-table-ref (
                                                                              list-ref-safe a i
                                                                            )
                                                                             j
                                                                          )
                                                                        )
                                                                         (
                                                                          else (
                                                                            list-ref-safe (
                                                                              list-ref-safe a i
                                                                            )
                                                                             j
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        cond (
                                                                          (
                                                                            string? (
                                                                              list-ref-safe b i
                                                                            )
                                                                          )
                                                                           (
                                                                            _substring (
                                                                              list-ref-safe b i
                                                                            )
                                                                             j (
                                                                              + j 1
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          (
                                                                            hash-table? (
                                                                              list-ref-safe b i
                                                                            )
                                                                          )
                                                                           (
                                                                            hash-table-ref (
                                                                              list-ref-safe b i
                                                                            )
                                                                             j
                                                                          )
                                                                        )
                                                                         (
                                                                          else (
                                                                            list-ref-safe (
                                                                              list-ref-safe b i
                                                                            )
                                                                             j
                                                                          )
                                                                        )
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
                                                                loop51
                                                              )
                                                            )
                                                             '(
                                                              
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      loop51
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
                                        loop49
                                      )
                                    )
                                     '(
                                      
                                    )
                                  )
                                )
                              )
                            )
                             (
                              loop49
                            )
                          )
                        )
                      )
                       (
                        ret48 res
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
            init_layer units back_units lr
          )
           (
            call/cc (
              lambda (
                ret53
              )
               (
                ret53 (
                  alist->hash-table (
                    _list (
                      cons "units" units
                    )
                     (
                      cons "weight" (
                        random_matrix units back_units
                      )
                    )
                     (
                      cons "bias" (
                        random_vector units
                      )
                    )
                     (
                      cons "output" (
                        _list
                      )
                    )
                     (
                      cons "xdata" (
                        _list
                      )
                    )
                     (
                      cons "learn_rate" lr
                    )
                  )
                )
              )
            )
          )
        )
         (
          define (
            forward layers x
          )
           (
            call/cc (
              lambda (
                ret54
              )
               (
                let (
                  (
                    data x
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
                            break56
                          )
                           (
                            letrec (
                              (
                                loop55 (
                                  lambda (
                                    
                                  )
                                   (
                                    if (
                                      < i (
                                        _len layers
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            layer (
                                              list-ref-safe layers i
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            hash-table-set! layer "xdata" data
                                          )
                                           (
                                            if (
                                              equal? i 0
                                            )
                                             (
                                              begin (
                                                hash-table-set! layer "output" data
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    z (
                                                      vec_sub (
                                                        matvec (
                                                          hash-table-ref layer "weight"
                                                        )
                                                         data
                                                      )
                                                       (
                                                        hash-table-ref layer "bias"
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    hash-table-set! layer "output" (
                                                      sigmoid_vec z
                                                    )
                                                  )
                                                   (
                                                    set! data (
                                                      hash-table-ref layer "output"
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                           (
                                            list-set! layers i layer
                                          )
                                           (
                                            set! i (
                                              + i 1
                                            )
                                          )
                                        )
                                      )
                                       (
                                        loop55
                                      )
                                    )
                                     '(
                                      
                                    )
                                  )
                                )
                              )
                            )
                             (
                              loop55
                            )
                          )
                        )
                      )
                       (
                        ret54 layers
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
            backward layers grad
          )
           (
            call/cc (
              lambda (
                ret57
              )
               (
                let (
                  (
                    g grad
                  )
                )
                 (
                  begin (
                    let (
                      (
                        i (
                          - (
                            _len layers
                          )
                           1
                        )
                      )
                    )
                     (
                      begin (
                        call/cc (
                          lambda (
                            break59
                          )
                           (
                            letrec (
                              (
                                loop58 (
                                  lambda (
                                    
                                  )
                                   (
                                    if (
                                      > i 0
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            layer (
                                              list-ref-safe layers i
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                deriv (
                                                  sigmoid_derivative (
                                                    hash-table-ref layer "output"
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    delta (
                                                      vec_mul g deriv
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        grad_w (
                                                          outer delta (
                                                            hash-table-ref layer "xdata"
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        hash-table-set! layer "weight" (
                                                          mat_sub (
                                                            hash-table-ref layer "weight"
                                                          )
                                                           (
                                                            mat_scalar_mul grad_w (
                                                              hash-table-ref layer "learn_rate"
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        hash-table-set! layer "bias" (
                                                          vec_sub (
                                                            hash-table-ref layer "bias"
                                                          )
                                                           (
                                                            vec_scalar_mul delta (
                                                              hash-table-ref layer "learn_rate"
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        set! g (
                                                          matTvec (
                                                            hash-table-ref layer "weight"
                                                          )
                                                           delta
                                                        )
                                                      )
                                                       (
                                                        list-set! layers i layer
                                                      )
                                                       (
                                                        set! i (
                                                          - i 1
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
                                        loop58
                                      )
                                    )
                                     '(
                                      
                                    )
                                  )
                                )
                              )
                            )
                             (
                              loop58
                            )
                          )
                        )
                      )
                       (
                        ret57 layers
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
            calc_loss y yhat
          )
           (
            call/cc (
              lambda (
                ret60
              )
               (
                let (
                  (
                    s 0.0
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
                            break62
                          )
                           (
                            letrec (
                              (
                                loop61 (
                                  lambda (
                                    
                                  )
                                   (
                                    if (
                                      < i (
                                        _len y
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            d (
                                              - (
                                                list-ref-safe y i
                                              )
                                               (
                                                list-ref-safe yhat i
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            set! s (
                                              _add s (
                                                * d d
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
                                        loop61
                                      )
                                    )
                                     '(
                                      
                                    )
                                  )
                                )
                              )
                            )
                             (
                              loop61
                            )
                          )
                        )
                      )
                       (
                        ret60 s
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
            calc_gradient y yhat
          )
           (
            call/cc (
              lambda (
                ret63
              )
               (
                let (
                  (
                    g (
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
                            break65
                          )
                           (
                            letrec (
                              (
                                loop64 (
                                  lambda (
                                    
                                  )
                                   (
                                    if (
                                      < i (
                                        _len y
                                      )
                                    )
                                     (
                                      begin (
                                        set! g (
                                          append g (
                                            _list (
                                              * 2.0 (
                                                - (
                                                  list-ref-safe yhat i
                                                )
                                                 (
                                                  list-ref-safe y i
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
                                        loop64
                                      )
                                    )
                                     '(
                                      
                                    )
                                  )
                                )
                              )
                            )
                             (
                              loop64
                            )
                          )
                        )
                      )
                       (
                        ret63 g
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
            train layers xdata ydata rounds acc
          )
           (
            call/cc (
              lambda (
                ret66
              )
               (
                let (
                  (
                    r 0
                  )
                )
                 (
                  begin (
                    call/cc (
                      lambda (
                        break68
                      )
                       (
                        letrec (
                          (
                            loop67 (
                              lambda (
                                
                              )
                               (
                                if (
                                  < r rounds
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
                                            break70
                                          )
                                           (
                                            letrec (
                                              (
                                                loop69 (
                                                  lambda (
                                                    
                                                  )
                                                   (
                                                    if (
                                                      < i (
                                                        _len xdata
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        set! layers (
                                                          forward layers (
                                                            list-ref-safe xdata i
                                                          )
                                                        )
                                                      )
                                                       (
                                                        let (
                                                          (
                                                            out (
                                                              hash-table-ref (
                                                                list-ref-safe layers (
                                                                  - (
                                                                    _len layers
                                                                  )
                                                                   1
                                                                )
                                                              )
                                                               "output"
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                grad (
                                                                  calc_gradient (
                                                                    list-ref-safe ydata i
                                                                  )
                                                                   out
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                set! layers (
                                                                  backward layers grad
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
                                                        loop69
                                                      )
                                                    )
                                                     '(
                                                      
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              loop69
                                            )
                                          )
                                        )
                                      )
                                       (
                                        set! r (
                                          + r 1
                                        )
                                      )
                                    )
                                  )
                                   (
                                    loop67
                                  )
                                )
                                 '(
                                  
                                )
                              )
                            )
                          )
                        )
                         (
                          loop67
                        )
                      )
                    )
                  )
                   (
                    ret66 0.0
                  )
                )
              )
            )
          )
        )
         (
          define (
            create_data
          )
           (
            call/cc (
              lambda (
                ret71
              )
               (
                let (
                  (
                    x (
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
                            break73
                          )
                           (
                            letrec (
                              (
                                loop72 (
                                  lambda (
                                    
                                  )
                                   (
                                    if (
                                      < i 10
                                    )
                                     (
                                      begin (
                                        set! x (
                                          append x (
                                            _list (
                                              random_vector 10
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
                                        loop72
                                      )
                                    )
                                     '(
                                      
                                    )
                                  )
                                )
                              )
                            )
                             (
                              loop72
                            )
                          )
                        )
                      )
                       (
                        let (
                          (
                            y (
                              _list (
                                _list 0.8 0.4
                              )
                               (
                                _list 0.4 0.3
                              )
                               (
                                _list 0.34 0.45
                              )
                               (
                                _list 0.67 0.32
                              )
                               (
                                _list 0.88 0.67
                              )
                               (
                                _list 0.78 0.77
                              )
                               (
                                _list 0.55 0.66
                              )
                               (
                                _list 0.55 0.43
                              )
                               (
                                _list 0.54 0.1
                              )
                               (
                                _list 0.1 0.5
                              )
                            )
                          )
                        )
                         (
                          begin (
                            ret71 (
                              alist->hash-table (
                                _list (
                                  cons "x" x
                                )
                                 (
                                  cons "y" y
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
                ret74
              )
               (
                let (
                  (
                    data (
                      create_data
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        x (
                          hash-table-ref data "x"
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            y (
                              hash-table-ref data "y"
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                layers (
                                  _list
                                )
                              )
                            )
                             (
                              begin (
                                set! layers (
                                  append layers (
                                    _list (
                                      init_layer 10 0 0.3
                                    )
                                  )
                                )
                              )
                               (
                                set! layers (
                                  append layers (
                                    _list (
                                      init_layer 20 10 0.3
                                    )
                                  )
                                )
                              )
                               (
                                set! layers (
                                  append layers (
                                    _list (
                                      init_layer 30 20 0.3
                                    )
                                  )
                                )
                              )
                               (
                                set! layers (
                                  append layers (
                                    _list (
                                      init_layer 2 30 0.3
                                    )
                                  )
                                )
                              )
                               (
                                let (
                                  (
                                    final_mse (
                                      train layers x y 100 0.01
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    _display (
                                      if (
                                        string? final_mse
                                      )
                                       final_mse (
                                        to-str final_mse
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
          end76 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur77 (
              quotient (
                * (
                  - end76 start75
                )
                 1000000
              )
               jps78
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur77
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
