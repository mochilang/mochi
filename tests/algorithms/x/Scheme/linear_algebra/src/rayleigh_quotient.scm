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
        complex_conj z
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            ret1 (
              alist->hash-table (
                _list (
                  cons "re" (
                    hash-table-ref z "re"
                  )
                )
                 (
                  cons "im" (
                    - (
                      hash-table-ref z "im"
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
        complex_eq a b
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            ret2 (
              and (
                equal? (
                  hash-table-ref a "re"
                )
                 (
                  hash-table-ref b "re"
                )
              )
               (
                equal? (
                  hash-table-ref a "im"
                )
                 (
                  hash-table-ref b "im"
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        complex_add a b
      )
       (
        call/cc (
          lambda (
            ret3
          )
           (
            ret3 (
              alist->hash-table (
                _list (
                  cons "re" (
                    + (
                      hash-table-ref a "re"
                    )
                     (
                      hash-table-ref b "re"
                    )
                  )
                )
                 (
                  cons "im" (
                    + (
                      hash-table-ref a "im"
                    )
                     (
                      hash-table-ref b "im"
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
        complex_mul a b
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            let (
              (
                real (
                  - (
                    * (
                      hash-table-ref a "re"
                    )
                     (
                      hash-table-ref b "re"
                    )
                  )
                   (
                    * (
                      hash-table-ref a "im"
                    )
                     (
                      hash-table-ref b "im"
                    )
                  )
                )
              )
            )
             (
              begin (
                let (
                  (
                    imag (
                      _add (
                        * (
                          hash-table-ref a "re"
                        )
                         (
                          hash-table-ref b "im"
                        )
                      )
                       (
                        * (
                          hash-table-ref a "im"
                        )
                         (
                          hash-table-ref b "re"
                        )
                      )
                    )
                  )
                )
                 (
                  begin (
                    ret4 (
                      alist->hash-table (
                        _list (
                          cons "re" real
                        )
                         (
                          cons "im" imag
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
        conj_vector v
      )
       (
        call/cc (
          lambda (
            ret5
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
                                  < i (
                                    _len v
                                  )
                                )
                                 (
                                  begin (
                                    set! res (
                                      append res (
                                        _list (
                                          complex_conj (
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
                    ret5 res
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
        vec_mat_mul v m
      )
       (
        call/cc (
          lambda (
            ret8
          )
           (
            let (
              (
                result (
                  _list
                )
              )
            )
             (
              begin (
                let (
                  (
                    col 0
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
                                  < col (
                                    _len (
                                      list-ref-safe m 0
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        sum (
                                          alist->hash-table (
                                            _list (
                                              cons "re" 0.0
                                            )
                                             (
                                              cons "im" 0.0
                                            )
                                          )
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            row 0
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
                                                          < row (
                                                            _len v
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            set! sum (
                                                              complex_add sum (
                                                                complex_mul (
                                                                  list-ref-safe v row
                                                                )
                                                                 (
                                                                  cond (
                                                                    (
                                                                      string? (
                                                                        list-ref-safe m row
                                                                      )
                                                                    )
                                                                     (
                                                                      _substring (
                                                                        list-ref-safe m row
                                                                      )
                                                                       col (
                                                                        + col 1
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    (
                                                                      hash-table? (
                                                                        list-ref-safe m row
                                                                      )
                                                                    )
                                                                     (
                                                                      hash-table-ref (
                                                                        list-ref-safe m row
                                                                      )
                                                                       col
                                                                    )
                                                                  )
                                                                   (
                                                                    else (
                                                                      list-ref-safe (
                                                                        list-ref-safe m row
                                                                      )
                                                                       col
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                           (
                                                            set! row (
                                                              + row 1
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
                                            set! result (
                                              append result (
                                                _list sum
                                              )
                                            )
                                          )
                                           (
                                            set! col (
                                              + col 1
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
                    ret8 result
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
            ret13
          )
           (
            let (
              (
                sum (
                  alist->hash-table (
                    _list (
                      cons "re" 0.0
                    )
                     (
                      cons "im" 0.0
                    )
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
                                  < i (
                                    _len a
                                  )
                                )
                                 (
                                  begin (
                                    set! sum (
                                      complex_add sum (
                                        complex_mul (
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
                    ret13 sum
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
        is_hermitian m
      )
       (
        call/cc (
          lambda (
            ret16
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
                              < i (
                                _len m
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
                                                  < j (
                                                    _len m
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    if (
                                                      not (
                                                        complex_eq (
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
                                                          complex_conj (
                                                            cond (
                                                              (
                                                                string? (
                                                                  list-ref-safe m j
                                                                )
                                                              )
                                                               (
                                                                _substring (
                                                                  list-ref-safe m j
                                                                )
                                                                 i (
                                                                  + i 1
                                                                )
                                                              )
                                                            )
                                                             (
                                                              (
                                                                hash-table? (
                                                                  list-ref-safe m j
                                                                )
                                                              )
                                                               (
                                                                hash-table-ref (
                                                                  list-ref-safe m j
                                                                )
                                                                 i
                                                              )
                                                            )
                                                             (
                                                              else (
                                                                list-ref-safe (
                                                                  list-ref-safe m j
                                                                )
                                                                 i
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        ret16 #f
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
                ret16 #t
              )
            )
          )
        )
      )
    )
     (
      define (
        rayleigh_quotient a v
      )
       (
        call/cc (
          lambda (
            ret21
          )
           (
            let (
              (
                v_star (
                  conj_vector v
                )
              )
            )
             (
              begin (
                let (
                  (
                    v_star_dot (
                      vec_mat_mul v_star a
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        num (
                          dot v_star_dot v
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            den (
                              dot v_star v
                            )
                          )
                        )
                         (
                          begin (
                            ret21 (
                              _div (
                                hash-table-ref num "re"
                              )
                               (
                                hash-table-ref den "re"
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
          a (
            _list (
              _list (
                alist->hash-table (
                  _list (
                    cons "re" 2.0
                  )
                   (
                    cons "im" 0.0
                  )
                )
              )
               (
                alist->hash-table (
                  _list (
                    cons "re" 2.0
                  )
                   (
                    cons "im" 1.0
                  )
                )
              )
               (
                alist->hash-table (
                  _list (
                    cons "re" 4.0
                  )
                   (
                    cons "im" 0.0
                  )
                )
              )
            )
             (
              _list (
                alist->hash-table (
                  _list (
                    cons "re" 2.0
                  )
                   (
                    cons "im" (
                      - 1.0
                    )
                  )
                )
              )
               (
                alist->hash-table (
                  _list (
                    cons "re" 3.0
                  )
                   (
                    cons "im" 0.0
                  )
                )
              )
               (
                alist->hash-table (
                  _list (
                    cons "re" 0.0
                  )
                   (
                    cons "im" 1.0
                  )
                )
              )
            )
             (
              _list (
                alist->hash-table (
                  _list (
                    cons "re" 4.0
                  )
                   (
                    cons "im" 0.0
                  )
                )
              )
               (
                alist->hash-table (
                  _list (
                    cons "re" 0.0
                  )
                   (
                    cons "im" (
                      - 1.0
                    )
                  )
                )
              )
               (
                alist->hash-table (
                  _list (
                    cons "re" 1.0
                  )
                   (
                    cons "im" 0.0
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
              v (
                _list (
                  alist->hash-table (
                    _list (
                      cons "re" 1.0
                    )
                     (
                      cons "im" 0.0
                    )
                  )
                )
                 (
                  alist->hash-table (
                    _list (
                      cons "re" 2.0
                    )
                     (
                      cons "im" 0.0
                    )
                  )
                )
                 (
                  alist->hash-table (
                    _list (
                      cons "re" 3.0
                    )
                     (
                      cons "im" 0.0
                    )
                  )
                )
              )
            )
          )
           (
            begin (
              if (
                is_hermitian a
              )
               (
                begin (
                  let (
                    (
                      r1 (
                        rayleigh_quotient a v
                      )
                    )
                  )
                   (
                    begin (
                      _display (
                        if (
                          string? r1
                        )
                         r1 (
                          to-str r1
                        )
                      )
                    )
                     (
                      newline
                    )
                     (
                      _display (
                        if (
                          string? "\n"
                        )
                         "\n" (
                          to-str "\n"
                        )
                      )
                    )
                     (
                      newline
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
                  b (
                    _list (
                      _list (
                        alist->hash-table (
                          _list (
                            cons "re" 1.0
                          )
                           (
                            cons "im" 0.0
                          )
                        )
                      )
                       (
                        alist->hash-table (
                          _list (
                            cons "re" 2.0
                          )
                           (
                            cons "im" 0.0
                          )
                        )
                      )
                       (
                        alist->hash-table (
                          _list (
                            cons "re" 4.0
                          )
                           (
                            cons "im" 0.0
                          )
                        )
                      )
                    )
                     (
                      _list (
                        alist->hash-table (
                          _list (
                            cons "re" 2.0
                          )
                           (
                            cons "im" 0.0
                          )
                        )
                      )
                       (
                        alist->hash-table (
                          _list (
                            cons "re" 3.0
                          )
                           (
                            cons "im" 0.0
                          )
                        )
                      )
                       (
                        alist->hash-table (
                          _list (
                            cons "re" (
                              - 1.0
                            )
                          )
                           (
                            cons "im" 0.0
                          )
                        )
                      )
                    )
                     (
                      _list (
                        alist->hash-table (
                          _list (
                            cons "re" 4.0
                          )
                           (
                            cons "im" 0.0
                          )
                        )
                      )
                       (
                        alist->hash-table (
                          _list (
                            cons "re" (
                              - 1.0
                            )
                          )
                           (
                            cons "im" 0.0
                          )
                        )
                      )
                       (
                        alist->hash-table (
                          _list (
                            cons "re" 1.0
                          )
                           (
                            cons "im" 0.0
                          )
                        )
                      )
                    )
                  )
                )
              )
               (
                begin (
                  if (
                    is_hermitian b
                  )
                   (
                    begin (
                      let (
                        (
                          r2 (
                            rayleigh_quotient b v
                          )
                        )
                      )
                       (
                        begin (
                          _display (
                            if (
                              string? r2
                            )
                             r2 (
                              to-str r2
                            )
                          )
                        )
                         (
                          newline
                        )
                      )
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
