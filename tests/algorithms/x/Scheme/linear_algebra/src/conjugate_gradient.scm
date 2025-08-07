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
      start28 (
        current-jiffy
      )
    )
     (
      jps31 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        zeros n
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
                                  < i n
                                )
                                 (
                                  begin (
                                    set! res (
                                      append res (
                                        _list 0.0
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
        dot a b
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            let (
              (
                sum 0.0
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
                                  < i (
                                    _len a
                                  )
                                )
                                 (
                                  begin (
                                    set! sum (
                                      _add sum (
                                        * (
                                          list-ref-safe a i
                                        )
                                         (
                                          list-ref-safe b i
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
                    ret4 sum
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
        mat_vec_mul m v
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
                                    _len m
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
                                                break11
                                              )
                                               (
                                                letrec (
                                                  (
                                                    loop10 (
                                                      lambda (
                                                        
                                                      )
                                                       (
                                                        if (
                                                          < j (
                                                            _len (
                                                              list-ref-safe m i
                                                            )
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
                                                                        list-ref-safe m i
                                                                      )
                                                                    )
                                                                     (
                                                                      _substring (
                                                                        list-ref-safe m i
                                                                      )
                                                                       j (
                                                                        + j 1
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    (
                                                                      hash-table? (
                                                                        list-ref-safe m i
                                                                      )
                                                                    )
                                                                     (
                                                                      hash-table-ref (
                                                                        list-ref-safe m i
                                                                      )
                                                                       j
                                                                    )
                                                                  )
                                                                   (
                                                                    else (
                                                                      list-ref-safe (
                                                                        list-ref-safe m i
                                                                      )
                                                                       j
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  list-ref-safe v j
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
                                                            loop10
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
                                                  loop10
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
        vec_add a b
      )
       (
        call/cc (
          lambda (
            ret12
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
                                  < i (
                                    _len a
                                  )
                                )
                                 (
                                  begin (
                                    set! res (
                                      append res (
                                        _list (
                                          + (
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
                                    loop13
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
                          loop13
                        )
                      )
                    )
                  )
                   (
                    ret12 res
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
            ret15
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
                        break17
                      )
                       (
                        letrec (
                          (
                            loop16 (
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
                                    loop16
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
                          loop16
                        )
                      )
                    )
                  )
                   (
                    ret15 res
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
        scalar_mul s v
      )
       (
        call/cc (
          lambda (
            ret18
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
                                  < i (
                                    _len v
                                  )
                                )
                                 (
                                  begin (
                                    set! res (
                                      append res (
                                        _list (
                                          * s (
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
     (
      define (
        sqrtApprox x
      )
       (
        call/cc (
          lambda (
            ret21
          )
           (
            begin (
              if (
                <= x 0.0
              )
               (
                begin (
                  ret21 0.0
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
                                    < i 20
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
                                      loop22
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
                            loop22
                          )
                        )
                      )
                    )
                     (
                      ret21 guess
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
        norm v
      )
       (
        call/cc (
          lambda (
            ret24
          )
           (
            ret24 (
              sqrtApprox (
                dot v v
              )
            )
          )
        )
      )
    )
     (
      define (
        conjugate_gradient A b max_iterations tol
      )
       (
        call/cc (
          lambda (
            ret25
          )
           (
            let (
              (
                n (
                  _len b
                )
              )
            )
             (
              begin (
                let (
                  (
                    x (
                      zeros n
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        r (
                          vec_sub b (
                            mat_vec_mul A x
                          )
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            p r
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                rs_old (
                                  dot r r
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
                                                  < i max_iterations
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        Ap (
                                                          mat_vec_mul A p
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            alpha (
                                                              _div rs_old (
                                                                dot p Ap
                                                              )
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            set! x (
                                                              vec_add x (
                                                                scalar_mul alpha p
                                                              )
                                                            )
                                                          )
                                                           (
                                                            set! r (
                                                              vec_sub r (
                                                                scalar_mul alpha Ap
                                                              )
                                                            )
                                                          )
                                                           (
                                                            let (
                                                              (
                                                                rs_new (
                                                                  dot r r
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                if (
                                                                  _lt (
                                                                    sqrtApprox rs_new
                                                                  )
                                                                   tol
                                                                )
                                                                 (
                                                                  begin (
                                                                    break27 (
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
                                                                    beta (
                                                                      _div rs_new rs_old
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! p (
                                                                      vec_add r (
                                                                        scalar_mul beta p
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    set! rs_old rs_new
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
                                    ret25 x
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
      let (
        (
          A (
            _list (
              _list 8.73256573 (
                - 5.02034289
              )
               (
                - 2.68709226
              )
            )
             (
              _list (
                - 5.02034289
              )
               3.78188322 0.91980451
            )
             (
              _list (
                - 2.68709226
              )
               0.91980451 1.94746467
            )
          )
        )
      )
       (
        begin (
          let (
            (
              b (
                _list (
                  - 5.80872761
                )
                 3.23807431 1.95381422
              )
            )
          )
           (
            begin (
              let (
                (
                  x (
                    conjugate_gradient A b 1000 1e-08
                  )
                )
              )
               (
                begin (
                  _display (
                    if (
                      string? (
                        to-str-space (
                          cond (
                            (
                              string? x
                            )
                             (
                              _substring x 0 (
                                + 0 1
                              )
                            )
                          )
                           (
                            (
                              hash-table? x
                            )
                             (
                              hash-table-ref x 0
                            )
                          )
                           (
                            else (
                              list-ref-safe x 0
                            )
                          )
                        )
                      )
                    )
                     (
                      to-str-space (
                        cond (
                          (
                            string? x
                          )
                           (
                            _substring x 0 (
                              + 0 1
                            )
                          )
                        )
                         (
                          (
                            hash-table? x
                          )
                           (
                            hash-table-ref x 0
                          )
                        )
                         (
                          else (
                            list-ref-safe x 0
                          )
                        )
                      )
                    )
                     (
                      to-str (
                        to-str-space (
                          cond (
                            (
                              string? x
                            )
                             (
                              _substring x 0 (
                                + 0 1
                              )
                            )
                          )
                           (
                            (
                              hash-table? x
                            )
                             (
                              hash-table-ref x 0
                            )
                          )
                           (
                            else (
                              list-ref-safe x 0
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
                          cond (
                            (
                              string? x
                            )
                             (
                              _substring x 1 (
                                + 1 1
                              )
                            )
                          )
                           (
                            (
                              hash-table? x
                            )
                             (
                              hash-table-ref x 1
                            )
                          )
                           (
                            else (
                              list-ref-safe x 1
                            )
                          )
                        )
                      )
                    )
                     (
                      to-str-space (
                        cond (
                          (
                            string? x
                          )
                           (
                            _substring x 1 (
                              + 1 1
                            )
                          )
                        )
                         (
                          (
                            hash-table? x
                          )
                           (
                            hash-table-ref x 1
                          )
                        )
                         (
                          else (
                            list-ref-safe x 1
                          )
                        )
                      )
                    )
                     (
                      to-str (
                        to-str-space (
                          cond (
                            (
                              string? x
                            )
                             (
                              _substring x 1 (
                                + 1 1
                              )
                            )
                          )
                           (
                            (
                              hash-table? x
                            )
                             (
                              hash-table-ref x 1
                            )
                          )
                           (
                            else (
                              list-ref-safe x 1
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
                          cond (
                            (
                              string? x
                            )
                             (
                              _substring x 2 (
                                + 2 1
                              )
                            )
                          )
                           (
                            (
                              hash-table? x
                            )
                             (
                              hash-table-ref x 2
                            )
                          )
                           (
                            else (
                              list-ref-safe x 2
                            )
                          )
                        )
                      )
                    )
                     (
                      to-str-space (
                        cond (
                          (
                            string? x
                          )
                           (
                            _substring x 2 (
                              + 2 1
                            )
                          )
                        )
                         (
                          (
                            hash-table? x
                          )
                           (
                            hash-table-ref x 2
                          )
                        )
                         (
                          else (
                            list-ref-safe x 2
                          )
                        )
                      )
                    )
                     (
                      to-str (
                        to-str-space (
                          cond (
                            (
                              string? x
                            )
                             (
                              _substring x 2 (
                                + 2 1
                              )
                            )
                          )
                           (
                            (
                              hash-table? x
                            )
                             (
                              hash-table-ref x 2
                            )
                          )
                           (
                            else (
                              list-ref-safe x 2
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
              )
            )
          )
        )
      )
    )
     (
      let (
        (
          end29 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur30 (
              quotient (
                * (
                  - end29 start28
                )
                 1000000
              )
               jps31
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur30
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
