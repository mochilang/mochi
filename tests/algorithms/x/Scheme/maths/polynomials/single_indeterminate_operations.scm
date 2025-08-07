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
      start42 (
        current-jiffy
      )
    )
     (
      jps45 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        copy_list xs
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
                                    set! res (
                                      append res (
                                        _list (
                                          list-ref-safe xs i
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
        polynomial_new degree coeffs
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            begin (
              if (
                not (
                  equal? (
                    _len coeffs
                  )
                   (
                    + degree 1
                  )
                )
              )
               (
                begin (
                  panic "The number of coefficients should be equal to the degree + 1."
                )
              )
               '(
                
              )
            )
             (
              ret4 (
                alist->hash-table (
                  _list (
                    cons "degree" degree
                  )
                   (
                    cons "coefficients" (
                      copy_list coeffs
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
        add p q
      )
       (
        call/cc (
          lambda (
            ret5
          )
           (
            if (
              > (
                hash-table-ref p "degree"
              )
               (
                hash-table-ref q "degree"
              )
            )
             (
              begin (
                let (
                  (
                    coeffs (
                      copy_list (
                        hash-table-ref p "coefficients"
                      )
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
                                      <= i (
                                        hash-table-ref q "degree"
                                      )
                                    )
                                     (
                                      begin (
                                        list-set! coeffs i (
                                          _add (
                                            cond (
                                              (
                                                string? coeffs
                                              )
                                               (
                                                _substring coeffs i (
                                                  + i 1
                                                )
                                              )
                                            )
                                             (
                                              (
                                                hash-table? coeffs
                                              )
                                               (
                                                hash-table-ref coeffs i
                                              )
                                            )
                                             (
                                              else (
                                                list-ref-safe coeffs i
                                              )
                                            )
                                          )
                                           (
                                            list-ref-safe (
                                              hash-table-ref q "coefficients"
                                            )
                                             i
                                          )
                                        )
                                      )
                                       (
                                        set! i (
                                          + i 1
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
                        ret5 (
                          alist->hash-table (
                            _list (
                              cons "degree" (
                                hash-table-ref p "degree"
                              )
                            )
                             (
                              cons "coefficients" coeffs
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
              begin (
                let (
                  (
                    coeffs (
                      copy_list (
                        hash-table-ref q "coefficients"
                      )
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
                                      <= i (
                                        hash-table-ref p "degree"
                                      )
                                    )
                                     (
                                      begin (
                                        list-set! coeffs i (
                                          _add (
                                            cond (
                                              (
                                                string? coeffs
                                              )
                                               (
                                                _substring coeffs i (
                                                  + i 1
                                                )
                                              )
                                            )
                                             (
                                              (
                                                hash-table? coeffs
                                              )
                                               (
                                                hash-table-ref coeffs i
                                              )
                                            )
                                             (
                                              else (
                                                list-ref-safe coeffs i
                                              )
                                            )
                                          )
                                           (
                                            list-ref-safe (
                                              hash-table-ref p "coefficients"
                                            )
                                             i
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
                        ret5 (
                          alist->hash-table (
                            _list (
                              cons "degree" (
                                hash-table-ref q "degree"
                              )
                            )
                             (
                              cons "coefficients" coeffs
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
        neg p
      )
       (
        call/cc (
          lambda (
            ret10
          )
           (
            let (
              (
                coeffs (
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
                                  <= i (
                                    hash-table-ref p "degree"
                                  )
                                )
                                 (
                                  begin (
                                    set! coeffs (
                                      append coeffs (
                                        _list (
                                          - (
                                            list-ref-safe (
                                              hash-table-ref p "coefficients"
                                            )
                                             i
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
                    ret10 (
                      alist->hash-table (
                        _list (
                          cons "degree" (
                            hash-table-ref p "degree"
                          )
                        )
                         (
                          cons "coefficients" coeffs
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
        sub p q
      )
       (
        call/cc (
          lambda (
            ret13
          )
           (
            ret13 (
              add p (
                neg q
              )
            )
          )
        )
      )
    )
     (
      define (
        mul p q
      )
       (
        call/cc (
          lambda (
            ret14
          )
           (
            let (
              (
                size (
                  + (
                    + (
                      hash-table-ref p "degree"
                    )
                     (
                      hash-table-ref q "degree"
                    )
                  )
                   1
                )
              )
            )
             (
              begin (
                let (
                  (
                    coeffs (
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
                                      < i size
                                    )
                                     (
                                      begin (
                                        set! coeffs (
                                          append coeffs (
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
                        set! i 0
                      )
                       (
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
                                      <= i (
                                        hash-table-ref p "degree"
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
                                                          <= j (
                                                            hash-table-ref q "degree"
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            list-set! coeffs (
                                                              + i j
                                                            )
                                                             (
                                                              _add (
                                                                list-ref-safe coeffs (
                                                                  + i j
                                                                )
                                                              )
                                                               (
                                                                * (
                                                                  list-ref-safe (
                                                                    hash-table-ref p "coefficients"
                                                                  )
                                                                   i
                                                                )
                                                                 (
                                                                  list-ref-safe (
                                                                    hash-table-ref q "coefficients"
                                                                  )
                                                                   j
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
                                                         '(
                                                          
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
                        ret14 (
                          alist->hash-table (
                            _list (
                              cons "degree" (
                                + (
                                  hash-table-ref p "degree"
                                )
                                 (
                                  hash-table-ref q "degree"
                                )
                              )
                            )
                             (
                              cons "coefficients" coeffs
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
        power base exp
      )
       (
        call/cc (
          lambda (
            ret21
          )
           (
            let (
              (
                result 1.0
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
                                  < i exp
                                )
                                 (
                                  begin (
                                    set! result (
                                      * result base
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
                    ret21 result
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
        evaluate p x
      )
       (
        call/cc (
          lambda (
            ret24
          )
           (
            let (
              (
                result 0.0
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
                                  <= i (
                                    hash-table-ref p "degree"
                                  )
                                )
                                 (
                                  begin (
                                    set! result (
                                      _add result (
                                        * (
                                          list-ref-safe (
                                            hash-table-ref p "coefficients"
                                          )
                                           i
                                        )
                                         (
                                          power x i
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
                    ret24 result
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
        poly_to_string p
      )
       (
        call/cc (
          lambda (
            ret27
          )
           (
            let (
              (
                s ""
              )
            )
             (
              begin (
                let (
                  (
                    i (
                      hash-table-ref p "degree"
                    )
                  )
                )
                 (
                  begin (
                    call/cc (
                      lambda (
                        break29
                      )
                       (
                        letrec (
                          (
                            loop28 (
                              lambda (
                                
                              )
                               (
                                if (
                                  >= i 0
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        coeff (
                                          list-ref-safe (
                                            hash-table-ref p "coefficients"
                                          )
                                           i
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        if (
                                          not (
                                            equal? coeff 0.0
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              > (
                                                _len s
                                              )
                                               0
                                            )
                                             (
                                              begin (
                                                if (
                                                  > coeff 0.0
                                                )
                                                 (
                                                  begin (
                                                    set! s (
                                                      string-append s " + "
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    set! s (
                                                      string-append s " - "
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                if (
                                                  < coeff 0.0
                                                )
                                                 (
                                                  begin (
                                                    set! s (
                                                      string-append s "-"
                                                    )
                                                  )
                                                )
                                                 '(
                                                  
                                                )
                                              )
                                            )
                                          )
                                           (
                                            let (
                                              (
                                                abs_coeff (
                                                  if (
                                                    < coeff 0.0
                                                  )
                                                   (
                                                    - coeff
                                                  )
                                                   coeff
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                if (
                                                  equal? i 0
                                                )
                                                 (
                                                  begin (
                                                    set! s (
                                                      string-append s (
                                                        to-str-space abs_coeff
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  if (
                                                    equal? i 1
                                                  )
                                                   (
                                                    begin (
                                                      set! s (
                                                        string-append (
                                                          string-append s (
                                                            to-str-space abs_coeff
                                                          )
                                                        )
                                                         "x"
                                                      )
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      set! s (
                                                        string-append (
                                                          string-append (
                                                            string-append s (
                                                              to-str-space abs_coeff
                                                            )
                                                          )
                                                           "x^"
                                                        )
                                                         (
                                                          to-str-space i
                                                        )
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
                                          - i 1
                                        )
                                      )
                                    )
                                  )
                                   (
                                    loop28
                                  )
                                )
                                 '(
                                  
                                )
                              )
                            )
                          )
                        )
                         (
                          loop28
                        )
                      )
                    )
                  )
                   (
                    if (
                      string=? s ""
                    )
                     (
                      begin (
                        set! s "0"
                      )
                    )
                     '(
                      
                    )
                  )
                   (
                    ret27 s
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
        derivative p
      )
       (
        call/cc (
          lambda (
            ret30
          )
           (
            begin (
              if (
                equal? (
                  hash-table-ref p "degree"
                )
                 0
              )
               (
                begin (
                  ret30 (
                    alist->hash-table (
                      _list (
                        cons "degree" 0
                      )
                       (
                        cons "coefficients" (
                          _list 0.0
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
              let (
                (
                  coeffs (
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
                          break32
                        )
                         (
                          letrec (
                            (
                              loop31 (
                                lambda (
                                  
                                )
                                 (
                                  if (
                                    < i (
                                      hash-table-ref p "degree"
                                    )
                                  )
                                   (
                                    begin (
                                      set! coeffs (
                                        append coeffs (
                                          _list (
                                            * (
                                              list-ref-safe (
                                                hash-table-ref p "coefficients"
                                              )
                                               (
                                                + i 1
                                              )
                                            )
                                             (
                                              exact->inexact (
                                                + i 1
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
                                      loop31
                                    )
                                  )
                                   '(
                                    
                                  )
                                )
                              )
                            )
                          )
                           (
                            loop31
                          )
                        )
                      )
                    )
                     (
                      ret30 (
                        alist->hash-table (
                          _list (
                            cons "degree" (
                              - (
                                hash-table-ref p "degree"
                              )
                               1
                            )
                          )
                           (
                            cons "coefficients" coeffs
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
        integral p constant
      )
       (
        call/cc (
          lambda (
            ret33
          )
           (
            let (
              (
                coeffs (
                  _list constant
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
                        break35
                      )
                       (
                        letrec (
                          (
                            loop34 (
                              lambda (
                                
                              )
                               (
                                if (
                                  <= i (
                                    hash-table-ref p "degree"
                                  )
                                )
                                 (
                                  begin (
                                    set! coeffs (
                                      append coeffs (
                                        _list (
                                          _div (
                                            list-ref-safe (
                                              hash-table-ref p "coefficients"
                                            )
                                             i
                                          )
                                           (
                                            exact->inexact (
                                              + i 1
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
                                    loop34
                                  )
                                )
                                 '(
                                  
                                )
                              )
                            )
                          )
                        )
                         (
                          loop34
                        )
                      )
                    )
                  )
                   (
                    ret33 (
                      alist->hash-table (
                        _list (
                          cons "degree" (
                            + (
                              hash-table-ref p "degree"
                            )
                             1
                          )
                        )
                         (
                          cons "coefficients" coeffs
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
        equals p q
      )
       (
        call/cc (
          lambda (
            ret36
          )
           (
            begin (
              if (
                not (
                  equal? (
                    hash-table-ref p "degree"
                  )
                   (
                    hash-table-ref q "degree"
                  )
                )
              )
               (
                begin (
                  ret36 #f
                )
              )
               '(
                
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
                      break38
                    )
                     (
                      letrec (
                        (
                          loop37 (
                            lambda (
                              
                            )
                             (
                              if (
                                <= i (
                                  hash-table-ref p "degree"
                                )
                              )
                               (
                                begin (
                                  if (
                                    not (
                                      equal? (
                                        list-ref-safe (
                                          hash-table-ref p "coefficients"
                                        )
                                         i
                                      )
                                       (
                                        list-ref-safe (
                                          hash-table-ref q "coefficients"
                                        )
                                         i
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      ret36 #f
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
                                  loop37
                                )
                              )
                               '(
                                
                              )
                            )
                          )
                        )
                      )
                       (
                        loop37
                      )
                    )
                  )
                )
                 (
                  ret36 #t
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        not_equals p q
      )
       (
        call/cc (
          lambda (
            ret39
          )
           (
            ret39 (
              not (
                equals p q
              )
            )
          )
        )
      )
    )
     (
      define (
        test_polynomial
      )
       (
        call/cc (
          lambda (
            ret40
          )
           (
            let (
              (
                p (
                  polynomial_new 2 (
                    _list 1.0 2.0 3.0
                  )
                )
              )
            )
             (
              begin (
                let (
                  (
                    q (
                      polynomial_new 2 (
                        _list 1.0 2.0 3.0
                      )
                    )
                  )
                )
                 (
                  begin (
                    if (
                      not (
                        string=? (
                          poly_to_string (
                            add p q
                          )
                        )
                         "6x^2 + 4x + 2"
                      )
                    )
                     (
                      begin (
                        panic "add failed"
                      )
                    )
                     '(
                      
                    )
                  )
                   (
                    if (
                      not (
                        string=? (
                          poly_to_string (
                            sub p q
                          )
                        )
                         "0"
                      )
                    )
                     (
                      begin (
                        panic "sub failed"
                      )
                    )
                     '(
                      
                    )
                  )
                   (
                    if (
                      not (
                        equal? (
                          evaluate p 2.0
                        )
                         17.0
                      )
                    )
                     (
                      begin (
                        panic "evaluate failed"
                      )
                    )
                     '(
                      
                    )
                  )
                   (
                    if (
                      not (
                        string=? (
                          poly_to_string (
                            derivative p
                          )
                        )
                         "6x + 2"
                      )
                    )
                     (
                      begin (
                        panic "derivative failed"
                      )
                    )
                     '(
                      
                    )
                  )
                   (
                    let (
                      (
                        integ (
                          poly_to_string (
                            integral p 0.0
                          )
                        )
                      )
                    )
                     (
                      begin (
                        if (
                          not (
                            string=? integ "1x^3 + 1x^2 + 1x"
                          )
                        )
                         (
                          begin (
                            panic "integral failed"
                          )
                        )
                         '(
                          
                        )
                      )
                       (
                        if (
                          not (
                            equals p q
                          )
                        )
                         (
                          begin (
                            panic "equals failed"
                          )
                        )
                         '(
                          
                        )
                      )
                       (
                        if (
                          not_equals p q
                        )
                         (
                          begin (
                            panic "not_equals failed"
                          )
                        )
                         '(
                          
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
            ret41
          )
           (
            begin (
              test_polynomial
            )
             (
              let (
                (
                  p (
                    polynomial_new 2 (
                      _list 1.0 2.0 3.0
                    )
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      d (
                        derivative p
                      )
                    )
                  )
                   (
                    begin (
                      _display (
                        if (
                          string? (
                            poly_to_string d
                          )
                        )
                         (
                          poly_to_string d
                        )
                         (
                          to-str (
                            poly_to_string d
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
      )
    )
     (
      main
    )
     (
      let (
        (
          end43 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur44 (
              quotient (
                * (
                  - end43 start42
                )
                 1000000
              )
               jps45
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur44
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
