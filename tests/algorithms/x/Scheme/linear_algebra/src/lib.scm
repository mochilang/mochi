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
      start95 (
        current-jiffy
      )
    )
     (
      jps98 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      let (
        (
          PI 3.141592653589793
        )
      )
       (
        begin (
          let (
            (
              seed 123456789
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
                random_int a b
              )
               (
                call/cc (
                  lambda (
                    ret2
                  )
                   (
                    let (
                      (
                        r (
                          _mod (
                            rand
                          )
                           (
                            + (
                              - b a
                            )
                             1
                          )
                        )
                      )
                    )
                     (
                      begin (
                        ret2 (
                          _add a r
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
                    ret3
                  )
                   (
                    begin (
                      if (
                        <= x 0.0
                      )
                       (
                        begin (
                          ret3 0.0
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
                              ret3 guess
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
                arcsin_taylor x
              )
               (
                call/cc (
                  lambda (
                    ret6
                  )
                   (
                    let (
                      (
                        term x
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            sum x
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
                                              < n 10
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    num (
                                                      * (
                                                        * (
                                                          * (
                                                            * (
                                                              - (
                                                                * 2.0 (
                                                                  + 0.0 n
                                                                )
                                                              )
                                                               1.0
                                                            )
                                                             (
                                                              - (
                                                                * 2.0 (
                                                                  + 0.0 n
                                                                )
                                                              )
                                                               1.0
                                                            )
                                                          )
                                                           x
                                                        )
                                                         x
                                                      )
                                                       term
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        den (
                                                          * (
                                                            * 2.0 (
                                                              + 0.0 n
                                                            )
                                                          )
                                                           (
                                                            _add (
                                                              * 2.0 (
                                                                + 0.0 n
                                                              )
                                                            )
                                                             1.0
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        set! term (
                                                          _div num den
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
                                                    )
                                                  )
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
                                ret6 sum
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
                acos_taylor x
              )
               (
                call/cc (
                  lambda (
                    ret9
                  )
                   (
                    ret9 (
                      - (
                        _div PI 2.0
                      )
                       (
                        arcsin_taylor x
                      )
                    )
                  )
                )
              )
            )
             (
              define (
                vector_len v
              )
               (
                call/cc (
                  lambda (
                    ret10
                  )
                   (
                    ret10 (
                      _len (
                        hash-table-ref v "components"
                      )
                    )
                  )
                )
              )
            )
             (
              define (
                vector_to_string v
              )
               (
                call/cc (
                  lambda (
                    ret11
                  )
                   (
                    let (
                      (
                        s "("
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
                                            _len (
                                              hash-table-ref v "components"
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            set! s (
                                              string-append s (
                                                to-str-space (
                                                  list-ref-safe (
                                                    hash-table-ref v "components"
                                                  )
                                                   i
                                                )
                                              )
                                            )
                                          )
                                           (
                                            if (
                                              < i (
                                                - (
                                                  _len (
                                                    hash-table-ref v "components"
                                                  )
                                                )
                                                 1
                                              )
                                            )
                                             (
                                              begin (
                                                set! s (
                                                  string-append s ","
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
                            set! s (
                              string-append s ")"
                            )
                          )
                           (
                            ret11 s
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
                vector_add a b
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
                          vector_len a
                        )
                      )
                    )
                     (
                      begin (
                        if (
                          not (
                            equal? size (
                              vector_len b
                            )
                          )
                        )
                         (
                          begin (
                            ret14 (
                              alist->hash-table (
                                _list (
                                  cons "components" (
                                    _list
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
                                              _lt i size
                                            )
                                             (
                                              begin (
                                                set! res (
                                                  append res (
                                                    _list (
                                                      + (
                                                        list-ref-safe (
                                                          hash-table-ref a "components"
                                                        )
                                                         i
                                                      )
                                                       (
                                                        list-ref-safe (
                                                          hash-table-ref b "components"
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
                                ret14 (
                                  alist->hash-table (
                                    _list (
                                      cons "components" res
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
                vector_sub a b
              )
               (
                call/cc (
                  lambda (
                    ret17
                  )
                   (
                    let (
                      (
                        size (
                          vector_len a
                        )
                      )
                    )
                     (
                      begin (
                        if (
                          not (
                            equal? size (
                              vector_len b
                            )
                          )
                        )
                         (
                          begin (
                            ret17 (
                              alist->hash-table (
                                _list (
                                  cons "components" (
                                    _list
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
                                              _lt i size
                                            )
                                             (
                                              begin (
                                                set! res (
                                                  append res (
                                                    _list (
                                                      - (
                                                        list-ref-safe (
                                                          hash-table-ref a "components"
                                                        )
                                                         i
                                                      )
                                                       (
                                                        list-ref-safe (
                                                          hash-table-ref b "components"
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
                                                loop18
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
                                      loop18
                                    )
                                  )
                                )
                              )
                               (
                                ret17 (
                                  alist->hash-table (
                                    _list (
                                      cons "components" res
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
                vector_eq a b
              )
               (
                call/cc (
                  lambda (
                    ret20
                  )
                   (
                    begin (
                      if (
                        not (
                          equal? (
                            vector_len a
                          )
                           (
                            vector_len b
                          )
                        )
                      )
                       (
                        begin (
                          ret20 #f
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
                                        _lt i (
                                          vector_len a
                                        )
                                      )
                                       (
                                        begin (
                                          if (
                                            not (
                                              equal? (
                                                list-ref-safe (
                                                  hash-table-ref a "components"
                                                )
                                                 i
                                              )
                                               (
                                                list-ref-safe (
                                                  hash-table-ref b "components"
                                                )
                                                 i
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              ret20 #f
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
                                          loop21
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
                                loop21
                              )
                            )
                          )
                        )
                         (
                          ret20 #t
                        )
                      )
                    )
                  )
                )
              )
            )
             (
              define (
                vector_mul_scalar v s
              )
               (
                call/cc (
                  lambda (
                    ret23
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
                                          _lt i (
                                            vector_len v
                                          )
                                        )
                                         (
                                          begin (
                                            set! res (
                                              append res (
                                                _list (
                                                  * (
                                                    list-ref-safe (
                                                      hash-table-ref v "components"
                                                    )
                                                     i
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
                                            loop24
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
                                  loop24
                                )
                              )
                            )
                          )
                           (
                            ret23 (
                              alist->hash-table (
                                _list (
                                  cons "components" res
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
                vector_dot a b
              )
               (
                call/cc (
                  lambda (
                    ret26
                  )
                   (
                    let (
                      (
                        size (
                          vector_len a
                        )
                      )
                    )
                     (
                      begin (
                        if (
                          not (
                            equal? size (
                              vector_len b
                            )
                          )
                        )
                         (
                          begin (
                            ret26 0.0
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
                                              _lt i size
                                            )
                                             (
                                              begin (
                                                set! sum (
                                                  _add sum (
                                                    * (
                                                      list-ref-safe (
                                                        hash-table-ref a "components"
                                                      )
                                                       i
                                                    )
                                                     (
                                                      list-ref-safe (
                                                        hash-table-ref b "components"
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
                                                loop27
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
                                      loop27
                                    )
                                  )
                                )
                              )
                               (
                                ret26 sum
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
                vector_copy v
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
                                          _lt i (
                                            vector_len v
                                          )
                                        )
                                         (
                                          begin (
                                            set! res (
                                              append res (
                                                _list (
                                                  list-ref-safe (
                                                    hash-table-ref v "components"
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
                            ret29 (
                              alist->hash-table (
                                _list (
                                  cons "components" res
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
                vector_component v idx
              )
               (
                call/cc (
                  lambda (
                    ret32
                  )
                   (
                    ret32 (
                      list-ref-safe (
                        hash-table-ref v "components"
                      )
                       idx
                    )
                  )
                )
              )
            )
             (
              define (
                vector_change_component v pos value
              )
               (
                call/cc (
                  lambda (
                    ret33
                  )
                   (
                    let (
                      (
                        comps (
                          hash-table-ref v "components"
                        )
                      )
                    )
                     (
                      begin (
                        list-set! comps pos value
                      )
                       (
                        ret33 (
                          alist->hash-table (
                            _list (
                              cons "components" comps
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
                vector_euclidean_length v
              )
               (
                call/cc (
                  lambda (
                    ret34
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
                                          < i (
                                            _len (
                                              hash-table-ref v "components"
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            set! sum (
                                              _add sum (
                                                * (
                                                  list-ref-safe (
                                                    hash-table-ref v "components"
                                                  )
                                                   i
                                                )
                                                 (
                                                  list-ref-safe (
                                                    hash-table-ref v "components"
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
                                            loop35
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
                                  loop35
                                )
                              )
                            )
                          )
                           (
                            let (
                              (
                                result (
                                  sqrtApprox sum
                                )
                              )
                            )
                             (
                              begin (
                                ret34 result
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
                vector_angle a b deg
              )
               (
                call/cc (
                  lambda (
                    ret37
                  )
                   (
                    let (
                      (
                        num (
                          vector_dot a b
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            den (
                              * (
                                vector_euclidean_length a
                              )
                               (
                                vector_euclidean_length b
                              )
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                ang (
                                  acos_taylor (
                                    _div num den
                                  )
                                )
                              )
                            )
                             (
                              begin (
                                if deg (
                                  begin (
                                    set! ang (
                                      _div (
                                        * ang 180.0
                                      )
                                       PI
                                    )
                                  )
                                )
                                 (
                                  quote (
                                    
                                  )
                                )
                              )
                               (
                                ret37 ang
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
                zero_vector d
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
                                          < i d
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
                                            loop39
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
                                  loop39
                                )
                              )
                            )
                          )
                           (
                            ret38 (
                              alist->hash-table (
                                _list (
                                  cons "components" res
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
                unit_basis_vector d pos
              )
               (
                call/cc (
                  lambda (
                    ret41
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
                                break43
                              )
                               (
                                letrec (
                                  (
                                    loop42 (
                                      lambda (
                                        
                                      )
                                       (
                                        if (
                                          < i d
                                        )
                                         (
                                          begin (
                                            if (
                                              equal? i pos
                                            )
                                             (
                                              begin (
                                                set! res (
                                                  append res (
                                                    _list 1.0
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                set! res (
                                                  append res (
                                                    _list 0.0
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
                                            loop42
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
                                  loop42
                                )
                              )
                            )
                          )
                           (
                            ret41 (
                              alist->hash-table (
                                _list (
                                  cons "components" res
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
                axpy s x y
              )
               (
                call/cc (
                  lambda (
                    ret44
                  )
                   (
                    ret44 (
                      vector_add (
                        vector_mul_scalar x s
                      )
                       y
                    )
                  )
                )
              )
            )
             (
              define (
                random_vector n a b
              )
               (
                call/cc (
                  lambda (
                    ret45
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
                                          < i n
                                        )
                                         (
                                          begin (
                                            set! res (
                                              append res (
                                                _list (
                                                  + 0.0 (
                                                    random_int a b
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
                                            loop46
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
                                  loop46
                                )
                              )
                            )
                          )
                           (
                            ret45 (
                              alist->hash-table (
                                _list (
                                  cons "components" res
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
                matrix_to_string m
              )
               (
                call/cc (
                  lambda (
                    ret48
                  )
                   (
                    let (
                      (
                        ans ""
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
                                            hash-table-ref m "height"
                                          )
                                        )
                                         (
                                          begin (
                                            set! ans (
                                              string-append ans "|"
                                            )
                                          )
                                           (
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
                                                                hash-table-ref m "width"
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                set! ans (
                                                                  string-append ans (
                                                                    to-str-space (
                                                                      cond (
                                                                        (
                                                                          string? (
                                                                            list-ref-safe (
                                                                              hash-table-ref m "data"
                                                                            )
                                                                             i
                                                                          )
                                                                        )
                                                                         (
                                                                          _substring (
                                                                            list-ref-safe (
                                                                              hash-table-ref m "data"
                                                                            )
                                                                             i
                                                                          )
                                                                           j (
                                                                            + j 1
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        (
                                                                          hash-table? (
                                                                            list-ref-safe (
                                                                              hash-table-ref m "data"
                                                                            )
                                                                             i
                                                                          )
                                                                        )
                                                                         (
                                                                          hash-table-ref (
                                                                            list-ref-safe (
                                                                              hash-table-ref m "data"
                                                                            )
                                                                             i
                                                                          )
                                                                           j
                                                                        )
                                                                      )
                                                                       (
                                                                        else (
                                                                          list-ref-safe (
                                                                            list-ref-safe (
                                                                              hash-table-ref m "data"
                                                                            )
                                                                             i
                                                                          )
                                                                           j
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                if (
                                                                  < j (
                                                                    - (
                                                                      hash-table-ref m "width"
                                                                    )
                                                                     1
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! ans (
                                                                      string-append ans ","
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  quote (
                                                                    
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
                                                             (
                                                              quote (
                                                                
                                                              )
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
                                                set! ans (
                                                  string-append ans "|\n"
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
                                            loop49
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
                                  loop49
                                )
                              )
                            )
                          )
                           (
                            ret48 ans
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
                matrix_add a b
              )
               (
                call/cc (
                  lambda (
                    ret53
                  )
                   (
                    begin (
                      if (
                        or (
                          not (
                            equal? (
                              hash-table-ref a "width"
                            )
                             (
                              hash-table-ref b "width"
                            )
                          )
                        )
                         (
                          not (
                            equal? (
                              hash-table-ref a "height"
                            )
                             (
                              hash-table-ref b "height"
                            )
                          )
                        )
                      )
                       (
                        begin (
                          ret53 (
                            alist->hash-table (
                              _list (
                                cons "data" (
                                  _list
                                )
                              )
                               (
                                cons "width" 0
                              )
                               (
                                cons "height" 0
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
                          mat (
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
                                  break55
                                )
                                 (
                                  letrec (
                                    (
                                      loop54 (
                                        lambda (
                                          
                                        )
                                         (
                                          if (
                                            < i (
                                              hash-table-ref a "height"
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
                                                          break57
                                                        )
                                                         (
                                                          letrec (
                                                            (
                                                              loop56 (
                                                                lambda (
                                                                  
                                                                )
                                                                 (
                                                                  if (
                                                                    < j (
                                                                      hash-table-ref a "width"
                                                                    )
                                                                  )
                                                                   (
                                                                    begin (
                                                                      set! row (
                                                                        append row (
                                                                          _list (
                                                                            + (
                                                                              cond (
                                                                                (
                                                                                  string? (
                                                                                    list-ref-safe (
                                                                                      hash-table-ref a "data"
                                                                                    )
                                                                                     i
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  _substring (
                                                                                    list-ref-safe (
                                                                                      hash-table-ref a "data"
                                                                                    )
                                                                                     i
                                                                                  )
                                                                                   j (
                                                                                    + j 1
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                (
                                                                                  hash-table? (
                                                                                    list-ref-safe (
                                                                                      hash-table-ref a "data"
                                                                                    )
                                                                                     i
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  hash-table-ref (
                                                                                    list-ref-safe (
                                                                                      hash-table-ref a "data"
                                                                                    )
                                                                                     i
                                                                                  )
                                                                                   j
                                                                                )
                                                                              )
                                                                               (
                                                                                else (
                                                                                  list-ref-safe (
                                                                                    list-ref-safe (
                                                                                      hash-table-ref a "data"
                                                                                    )
                                                                                     i
                                                                                  )
                                                                                   j
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              cond (
                                                                                (
                                                                                  string? (
                                                                                    list-ref-safe (
                                                                                      hash-table-ref b "data"
                                                                                    )
                                                                                     i
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  _substring (
                                                                                    list-ref-safe (
                                                                                      hash-table-ref b "data"
                                                                                    )
                                                                                     i
                                                                                  )
                                                                                   j (
                                                                                    + j 1
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                (
                                                                                  hash-table? (
                                                                                    list-ref-safe (
                                                                                      hash-table-ref b "data"
                                                                                    )
                                                                                     i
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  hash-table-ref (
                                                                                    list-ref-safe (
                                                                                      hash-table-ref b "data"
                                                                                    )
                                                                                     i
                                                                                  )
                                                                                   j
                                                                                )
                                                                              )
                                                                               (
                                                                                else (
                                                                                  list-ref-safe (
                                                                                    list-ref-safe (
                                                                                      hash-table-ref b "data"
                                                                                    )
                                                                                     i
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
                                                                      loop56
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
                                                            loop56
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      set! mat (
                                                        append mat (
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
                                              loop54
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
                                    loop54
                                  )
                                )
                              )
                            )
                             (
                              ret53 (
                                alist->hash-table (
                                  _list (
                                    cons "data" mat
                                  )
                                   (
                                    cons "width" (
                                      hash-table-ref a "width"
                                    )
                                  )
                                   (
                                    cons "height" (
                                      hash-table-ref a "height"
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
                matrix_sub a b
              )
               (
                call/cc (
                  lambda (
                    ret58
                  )
                   (
                    begin (
                      if (
                        or (
                          not (
                            equal? (
                              hash-table-ref a "width"
                            )
                             (
                              hash-table-ref b "width"
                            )
                          )
                        )
                         (
                          not (
                            equal? (
                              hash-table-ref a "height"
                            )
                             (
                              hash-table-ref b "height"
                            )
                          )
                        )
                      )
                       (
                        begin (
                          ret58 (
                            alist->hash-table (
                              _list (
                                cons "data" (
                                  _list
                                )
                              )
                               (
                                cons "width" 0
                              )
                               (
                                cons "height" 0
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
                          mat (
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
                                  break60
                                )
                                 (
                                  letrec (
                                    (
                                      loop59 (
                                        lambda (
                                          
                                        )
                                         (
                                          if (
                                            < i (
                                              hash-table-ref a "height"
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
                                                                    < j (
                                                                      hash-table-ref a "width"
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
                                                                                    list-ref-safe (
                                                                                      hash-table-ref a "data"
                                                                                    )
                                                                                     i
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  _substring (
                                                                                    list-ref-safe (
                                                                                      hash-table-ref a "data"
                                                                                    )
                                                                                     i
                                                                                  )
                                                                                   j (
                                                                                    + j 1
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                (
                                                                                  hash-table? (
                                                                                    list-ref-safe (
                                                                                      hash-table-ref a "data"
                                                                                    )
                                                                                     i
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  hash-table-ref (
                                                                                    list-ref-safe (
                                                                                      hash-table-ref a "data"
                                                                                    )
                                                                                     i
                                                                                  )
                                                                                   j
                                                                                )
                                                                              )
                                                                               (
                                                                                else (
                                                                                  list-ref-safe (
                                                                                    list-ref-safe (
                                                                                      hash-table-ref a "data"
                                                                                    )
                                                                                     i
                                                                                  )
                                                                                   j
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              cond (
                                                                                (
                                                                                  string? (
                                                                                    list-ref-safe (
                                                                                      hash-table-ref b "data"
                                                                                    )
                                                                                     i
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  _substring (
                                                                                    list-ref-safe (
                                                                                      hash-table-ref b "data"
                                                                                    )
                                                                                     i
                                                                                  )
                                                                                   j (
                                                                                    + j 1
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                (
                                                                                  hash-table? (
                                                                                    list-ref-safe (
                                                                                      hash-table-ref b "data"
                                                                                    )
                                                                                     i
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  hash-table-ref (
                                                                                    list-ref-safe (
                                                                                      hash-table-ref b "data"
                                                                                    )
                                                                                     i
                                                                                  )
                                                                                   j
                                                                                )
                                                                              )
                                                                               (
                                                                                else (
                                                                                  list-ref-safe (
                                                                                    list-ref-safe (
                                                                                      hash-table-ref b "data"
                                                                                    )
                                                                                     i
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
                                                                      loop61
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
                                                            loop61
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      set! mat (
                                                        append mat (
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
                                              loop59
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
                                    loop59
                                  )
                                )
                              )
                            )
                             (
                              ret58 (
                                alist->hash-table (
                                  _list (
                                    cons "data" mat
                                  )
                                   (
                                    cons "width" (
                                      hash-table-ref a "width"
                                    )
                                  )
                                   (
                                    cons "height" (
                                      hash-table-ref a "height"
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
                matrix_mul_vector m v
              )
               (
                call/cc (
                  lambda (
                    ret63
                  )
                   (
                    begin (
                      if (
                        not (
                          equal? (
                            _len (
                              hash-table-ref v "components"
                            )
                          )
                           (
                            hash-table-ref m "width"
                          )
                        )
                      )
                       (
                        begin (
                          ret63 (
                            alist->hash-table (
                              _list (
                                cons "components" (
                                  _list
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
                      let (
                        (
                          res (
                            zero_vector (
                              hash-table-ref m "height"
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
                                              hash-table-ref m "height"
                                            )
                                          )
                                           (
                                            begin (
                                              let (
                                                (
                                                  sum 0.0
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
                                                          break67
                                                        )
                                                         (
                                                          letrec (
                                                            (
                                                              loop66 (
                                                                lambda (
                                                                  
                                                                )
                                                                 (
                                                                  if (
                                                                    < j (
                                                                      hash-table-ref m "width"
                                                                    )
                                                                  )
                                                                   (
                                                                    begin (
                                                                      set! sum (
                                                                        _add sum (
                                                                          * (
                                                                            cond (
                                                                              (
                                                                                string? (
                                                                                  list-ref-safe (
                                                                                    hash-table-ref m "data"
                                                                                  )
                                                                                   i
                                                                                )
                                                                              )
                                                                               (
                                                                                _substring (
                                                                                  list-ref-safe (
                                                                                    hash-table-ref m "data"
                                                                                  )
                                                                                   i
                                                                                )
                                                                                 j (
                                                                                  + j 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? (
                                                                                  list-ref-safe (
                                                                                    hash-table-ref m "data"
                                                                                  )
                                                                                   i
                                                                                )
                                                                              )
                                                                               (
                                                                                hash-table-ref (
                                                                                  list-ref-safe (
                                                                                    hash-table-ref m "data"
                                                                                  )
                                                                                   i
                                                                                )
                                                                                 j
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref-safe (
                                                                                  list-ref-safe (
                                                                                    hash-table-ref m "data"
                                                                                  )
                                                                                   i
                                                                                )
                                                                                 j
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            list-ref-safe (
                                                                              hash-table-ref v "components"
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
                                                                      loop66
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
                                                            loop66
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      set! res (
                                                        vector_change_component res i sum
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
                                              loop64
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
                                    loop64
                                  )
                                )
                              )
                            )
                             (
                              ret63 res
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
                matrix_mul_scalar m s
              )
               (
                call/cc (
                  lambda (
                    ret68
                  )
                   (
                    let (
                      (
                        mat (
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
                                            hash-table-ref m "height"
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
                                                        break72
                                                      )
                                                       (
                                                        letrec (
                                                          (
                                                            loop71 (
                                                              lambda (
                                                                
                                                              )
                                                               (
                                                                if (
                                                                  < j (
                                                                    hash-table-ref m "width"
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
                                                                                  list-ref-safe (
                                                                                    hash-table-ref m "data"
                                                                                  )
                                                                                   i
                                                                                )
                                                                              )
                                                                               (
                                                                                _substring (
                                                                                  list-ref-safe (
                                                                                    hash-table-ref m "data"
                                                                                  )
                                                                                   i
                                                                                )
                                                                                 j (
                                                                                  + j 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? (
                                                                                  list-ref-safe (
                                                                                    hash-table-ref m "data"
                                                                                  )
                                                                                   i
                                                                                )
                                                                              )
                                                                               (
                                                                                hash-table-ref (
                                                                                  list-ref-safe (
                                                                                    hash-table-ref m "data"
                                                                                  )
                                                                                   i
                                                                                )
                                                                                 j
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref-safe (
                                                                                  list-ref-safe (
                                                                                    hash-table-ref m "data"
                                                                                  )
                                                                                   i
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
                                                                    loop71
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
                                                          loop71
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    set! mat (
                                                      append mat (
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
                                            loop69
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
                                  loop69
                                )
                              )
                            )
                          )
                           (
                            ret68 (
                              alist->hash-table (
                                _list (
                                  cons "data" mat
                                )
                                 (
                                  cons "width" (
                                    hash-table-ref m "width"
                                  )
                                )
                                 (
                                  cons "height" (
                                    hash-table-ref m "height"
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
                matrix_component m x y
              )
               (
                call/cc (
                  lambda (
                    ret73
                  )
                   (
                    ret73 (
                      cond (
                        (
                          string? (
                            list-ref-safe (
                              hash-table-ref m "data"
                            )
                             x
                          )
                        )
                         (
                          _substring (
                            list-ref-safe (
                              hash-table-ref m "data"
                            )
                             x
                          )
                           y (
                            + y 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? (
                            list-ref-safe (
                              hash-table-ref m "data"
                            )
                             x
                          )
                        )
                         (
                          hash-table-ref (
                            list-ref-safe (
                              hash-table-ref m "data"
                            )
                             x
                          )
                           y
                        )
                      )
                       (
                        else (
                          list-ref-safe (
                            list-ref-safe (
                              hash-table-ref m "data"
                            )
                             x
                          )
                           y
                        )
                      )
                    )
                  )
                )
              )
            )
             (
              define (
                matrix_change_component m x y value
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
                          hash-table-ref m "data"
                        )
                      )
                    )
                     (
                      begin (
                        list-set! (
                          list-ref-safe data x
                        )
                         y value
                      )
                       (
                        ret74 (
                          alist->hash-table (
                            _list (
                              cons "data" data
                            )
                             (
                              cons "width" (
                                hash-table-ref m "width"
                              )
                            )
                             (
                              cons "height" (
                                hash-table-ref m "height"
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
                matrix_minor m x y
              )
               (
                call/cc (
                  lambda (
                    ret75
                  )
                   (
                    begin (
                      if (
                        not (
                          equal? (
                            hash-table-ref m "height"
                          )
                           (
                            hash-table-ref m "width"
                          )
                        )
                      )
                       (
                        begin (
                          ret75 0.0
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
                          minor (
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
                                  break77
                                )
                                 (
                                  letrec (
                                    (
                                      loop76 (
                                        lambda (
                                          
                                        )
                                         (
                                          if (
                                            < i (
                                              hash-table-ref m "height"
                                            )
                                          )
                                           (
                                            begin (
                                              if (
                                                not (
                                                  equal? i x
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
                                                              break79
                                                            )
                                                             (
                                                              letrec (
                                                                (
                                                                  loop78 (
                                                                    lambda (
                                                                      
                                                                    )
                                                                     (
                                                                      if (
                                                                        < j (
                                                                          hash-table-ref m "width"
                                                                        )
                                                                      )
                                                                       (
                                                                        begin (
                                                                          if (
                                                                            not (
                                                                              equal? j y
                                                                            )
                                                                          )
                                                                           (
                                                                            begin (
                                                                              set! row (
                                                                                append row (
                                                                                  _list (
                                                                                    cond (
                                                                                      (
                                                                                        string? (
                                                                                          list-ref-safe (
                                                                                            hash-table-ref m "data"
                                                                                          )
                                                                                           i
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        _substring (
                                                                                          list-ref-safe (
                                                                                            hash-table-ref m "data"
                                                                                          )
                                                                                           i
                                                                                        )
                                                                                         j (
                                                                                          + j 1
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      (
                                                                                        hash-table? (
                                                                                          list-ref-safe (
                                                                                            hash-table-ref m "data"
                                                                                          )
                                                                                           i
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        hash-table-ref (
                                                                                          list-ref-safe (
                                                                                            hash-table-ref m "data"
                                                                                          )
                                                                                           i
                                                                                        )
                                                                                         j
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      else (
                                                                                        list-ref-safe (
                                                                                          list-ref-safe (
                                                                                            hash-table-ref m "data"
                                                                                          )
                                                                                           i
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
                                                                            quote (
                                                                              
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          set! j (
                                                                            + j 1
                                                                          )
                                                                        )
                                                                         (
                                                                          loop78
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
                                                                loop78
                                                              )
                                                            )
                                                          )
                                                        )
                                                         (
                                                          set! minor (
                                                            append minor (
                                                              _list row
                                                            )
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
                                              set! i (
                                                + i 1
                                              )
                                            )
                                             (
                                              loop76
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
                                    loop76
                                  )
                                )
                              )
                            )
                             (
                              let (
                                (
                                  sub (
                                    alist->hash-table (
                                      _list (
                                        cons "data" minor
                                      )
                                       (
                                        cons "width" (
                                          - (
                                            hash-table-ref m "width"
                                          )
                                           1
                                        )
                                      )
                                       (
                                        cons "height" (
                                          - (
                                            hash-table-ref m "height"
                                          )
                                           1
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                               (
                                begin (
                                  ret75 (
                                    matrix_determinant sub
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
                matrix_cofactor m x y
              )
               (
                call/cc (
                  lambda (
                    ret80
                  )
                   (
                    let (
                      (
                        sign (
                          if (
                            equal? (
                              _mod (
                                + x y
                              )
                               2
                            )
                             0
                          )
                           1.0 (
                            - 1.0
                          )
                        )
                      )
                    )
                     (
                      begin (
                        ret80 (
                          * sign (
                            matrix_minor m x y
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
                matrix_determinant m
              )
               (
                call/cc (
                  lambda (
                    ret81
                  )
                   (
                    begin (
                      if (
                        not (
                          equal? (
                            hash-table-ref m "height"
                          )
                           (
                            hash-table-ref m "width"
                          )
                        )
                      )
                       (
                        begin (
                          ret81 0.0
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      if (
                        equal? (
                          hash-table-ref m "height"
                        )
                         0
                      )
                       (
                        begin (
                          ret81 0.0
                        )
                      )
                       (
                        quote (
                          
                        )
                      )
                    )
                     (
                      if (
                        equal? (
                          hash-table-ref m "height"
                        )
                         1
                      )
                       (
                        begin (
                          ret81 (
                            cond (
                              (
                                string? (
                                  list-ref-safe (
                                    hash-table-ref m "data"
                                  )
                                   0
                                )
                              )
                               (
                                _substring (
                                  list-ref-safe (
                                    hash-table-ref m "data"
                                  )
                                   0
                                )
                                 0 (
                                  + 0 1
                                )
                              )
                            )
                             (
                              (
                                hash-table? (
                                  list-ref-safe (
                                    hash-table-ref m "data"
                                  )
                                   0
                                )
                              )
                               (
                                hash-table-ref (
                                  list-ref-safe (
                                    hash-table-ref m "data"
                                  )
                                   0
                                )
                                 0
                              )
                            )
                             (
                              else (
                                list-ref-safe (
                                  list-ref-safe (
                                    hash-table-ref m "data"
                                  )
                                   0
                                )
                                 0
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
                        equal? (
                          hash-table-ref m "height"
                        )
                         2
                      )
                       (
                        begin (
                          ret81 (
                            - (
                              * (
                                cond (
                                  (
                                    string? (
                                      list-ref-safe (
                                        hash-table-ref m "data"
                                      )
                                       0
                                    )
                                  )
                                   (
                                    _substring (
                                      list-ref-safe (
                                        hash-table-ref m "data"
                                      )
                                       0
                                    )
                                     0 (
                                      + 0 1
                                    )
                                  )
                                )
                                 (
                                  (
                                    hash-table? (
                                      list-ref-safe (
                                        hash-table-ref m "data"
                                      )
                                       0
                                    )
                                  )
                                   (
                                    hash-table-ref (
                                      list-ref-safe (
                                        hash-table-ref m "data"
                                      )
                                       0
                                    )
                                     0
                                  )
                                )
                                 (
                                  else (
                                    list-ref-safe (
                                      list-ref-safe (
                                        hash-table-ref m "data"
                                      )
                                       0
                                    )
                                     0
                                  )
                                )
                              )
                               (
                                cond (
                                  (
                                    string? (
                                      list-ref-safe (
                                        hash-table-ref m "data"
                                      )
                                       1
                                    )
                                  )
                                   (
                                    _substring (
                                      list-ref-safe (
                                        hash-table-ref m "data"
                                      )
                                       1
                                    )
                                     1 (
                                      + 1 1
                                    )
                                  )
                                )
                                 (
                                  (
                                    hash-table? (
                                      list-ref-safe (
                                        hash-table-ref m "data"
                                      )
                                       1
                                    )
                                  )
                                   (
                                    hash-table-ref (
                                      list-ref-safe (
                                        hash-table-ref m "data"
                                      )
                                       1
                                    )
                                     1
                                  )
                                )
                                 (
                                  else (
                                    list-ref-safe (
                                      list-ref-safe (
                                        hash-table-ref m "data"
                                      )
                                       1
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
                                      list-ref-safe (
                                        hash-table-ref m "data"
                                      )
                                       0
                                    )
                                  )
                                   (
                                    _substring (
                                      list-ref-safe (
                                        hash-table-ref m "data"
                                      )
                                       0
                                    )
                                     1 (
                                      + 1 1
                                    )
                                  )
                                )
                                 (
                                  (
                                    hash-table? (
                                      list-ref-safe (
                                        hash-table-ref m "data"
                                      )
                                       0
                                    )
                                  )
                                   (
                                    hash-table-ref (
                                      list-ref-safe (
                                        hash-table-ref m "data"
                                      )
                                       0
                                    )
                                     1
                                  )
                                )
                                 (
                                  else (
                                    list-ref-safe (
                                      list-ref-safe (
                                        hash-table-ref m "data"
                                      )
                                       0
                                    )
                                     1
                                  )
                                )
                              )
                               (
                                cond (
                                  (
                                    string? (
                                      list-ref-safe (
                                        hash-table-ref m "data"
                                      )
                                       1
                                    )
                                  )
                                   (
                                    _substring (
                                      list-ref-safe (
                                        hash-table-ref m "data"
                                      )
                                       1
                                    )
                                     0 (
                                      + 0 1
                                    )
                                  )
                                )
                                 (
                                  (
                                    hash-table? (
                                      list-ref-safe (
                                        hash-table-ref m "data"
                                      )
                                       1
                                    )
                                  )
                                   (
                                    hash-table-ref (
                                      list-ref-safe (
                                        hash-table-ref m "data"
                                      )
                                       1
                                    )
                                     0
                                  )
                                )
                                 (
                                  else (
                                    list-ref-safe (
                                      list-ref-safe (
                                        hash-table-ref m "data"
                                      )
                                       1
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
                        quote (
                          
                        )
                      )
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
                              y 0
                            )
                          )
                           (
                            begin (
                              call/cc (
                                lambda (
                                  break83
                                )
                                 (
                                  letrec (
                                    (
                                      loop82 (
                                        lambda (
                                          
                                        )
                                         (
                                          if (
                                            < y (
                                              hash-table-ref m "width"
                                            )
                                          )
                                           (
                                            begin (
                                              set! sum (
                                                _add sum (
                                                  * (
                                                    cond (
                                                      (
                                                        string? (
                                                          list-ref-safe (
                                                            hash-table-ref m "data"
                                                          )
                                                           0
                                                        )
                                                      )
                                                       (
                                                        _substring (
                                                          list-ref-safe (
                                                            hash-table-ref m "data"
                                                          )
                                                           0
                                                        )
                                                         y (
                                                          + y 1
                                                        )
                                                      )
                                                    )
                                                     (
                                                      (
                                                        hash-table? (
                                                          list-ref-safe (
                                                            hash-table-ref m "data"
                                                          )
                                                           0
                                                        )
                                                      )
                                                       (
                                                        hash-table-ref (
                                                          list-ref-safe (
                                                            hash-table-ref m "data"
                                                          )
                                                           0
                                                        )
                                                         y
                                                      )
                                                    )
                                                     (
                                                      else (
                                                        list-ref-safe (
                                                          list-ref-safe (
                                                            hash-table-ref m "data"
                                                          )
                                                           0
                                                        )
                                                         y
                                                      )
                                                    )
                                                  )
                                                   (
                                                    matrix_cofactor m 0 y
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              set! y (
                                                + y 1
                                              )
                                            )
                                             (
                                              loop82
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
                                    loop82
                                  )
                                )
                              )
                            )
                             (
                              ret81 sum
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
                square_zero_matrix n
              )
               (
                call/cc (
                  lambda (
                    ret84
                  )
                   (
                    let (
                      (
                        mat (
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
                                break86
                              )
                               (
                                letrec (
                                  (
                                    loop85 (
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
                                                        break88
                                                      )
                                                       (
                                                        letrec (
                                                          (
                                                            loop87 (
                                                              lambda (
                                                                
                                                              )
                                                               (
                                                                if (
                                                                  < j n
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! row (
                                                                      append row (
                                                                        _list 0.0
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    set! j (
                                                                      + j 1
                                                                    )
                                                                  )
                                                                   (
                                                                    loop87
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
                                                          loop87
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    set! mat (
                                                      append mat (
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
                                            loop85
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
                                  loop85
                                )
                              )
                            )
                          )
                           (
                            ret84 (
                              alist->hash-table (
                                _list (
                                  cons "data" mat
                                )
                                 (
                                  cons "width" n
                                )
                                 (
                                  cons "height" n
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
                random_matrix w h a b
              )
               (
                call/cc (
                  lambda (
                    ret89
                  )
                   (
                    let (
                      (
                        mat (
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
                                break91
                              )
                               (
                                letrec (
                                  (
                                    loop90 (
                                      lambda (
                                        
                                      )
                                       (
                                        if (
                                          < i h
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
                                                        break93
                                                      )
                                                       (
                                                        letrec (
                                                          (
                                                            loop92 (
                                                              lambda (
                                                                
                                                              )
                                                               (
                                                                if (
                                                                  < j w
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! row (
                                                                      append row (
                                                                        _list (
                                                                          + 0.0 (
                                                                            random_int a b
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
                                                                    loop92
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
                                                          loop92
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    set! mat (
                                                      append mat (
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
                                            loop90
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
                                  loop90
                                )
                              )
                            )
                          )
                           (
                            ret89 (
                              alist->hash-table (
                                _list (
                                  cons "data" mat
                                )
                                 (
                                  cons "width" w
                                )
                                 (
                                  cons "height" h
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
                    ret94
                  )
                   (
                    let (
                      (
                        v1 (
                          alist->hash-table (
                            _list (
                              cons "components" (
                                _list 1.0 2.0 3.0
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
                            v2 (
                              alist->hash-table (
                                _list (
                                  cons "components" (
                                    _list 4.0 5.0 6.0
                                  )
                                )
                              )
                            )
                          )
                        )
                         (
                          begin (
                            _display (
                              if (
                                string? (
                                  vector_to_string (
                                    vector_add v1 v2
                                  )
                                )
                              )
                               (
                                vector_to_string (
                                  vector_add v1 v2
                                )
                              )
                               (
                                to-str (
                                  vector_to_string (
                                    vector_add v1 v2
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
                                    vector_dot v1 v2
                                  )
                                )
                              )
                               (
                                to-str-space (
                                  vector_dot v1 v2
                                )
                              )
                               (
                                to-str (
                                  to-str-space (
                                    vector_dot v1 v2
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
                                    vector_euclidean_length v1
                                  )
                                )
                              )
                               (
                                to-str-space (
                                  vector_euclidean_length v1
                                )
                              )
                               (
                                to-str (
                                  to-str-space (
                                    vector_euclidean_length v1
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
                                m (
                                  alist->hash-table (
                                    _list (
                                      cons "data" (
                                        _list (
                                          _list 1.0 2.0
                                        )
                                         (
                                          _list 3.0 4.0
                                        )
                                      )
                                    )
                                     (
                                      cons "width" 2
                                    )
                                     (
                                      cons "height" 2
                                    )
                                  )
                                )
                              )
                            )
                             (
                              begin (
                                _display (
                                  if (
                                    string? (
                                      to-str-space (
                                        matrix_determinant m
                                      )
                                    )
                                  )
                                   (
                                    to-str-space (
                                      matrix_determinant m
                                    )
                                  )
                                   (
                                    to-str (
                                      to-str-space (
                                        matrix_determinant m
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
                )
              )
            )
             (
              main
            )
          )
        )
      )
    )
     (
      let (
        (
          end96 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur97 (
              quotient (
                * (
                  - end96 start95
                )
                 1000000
              )
               jps98
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur97
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
