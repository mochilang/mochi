;; Generated on 2025-08-07 08:56 +0700
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
      start86 (
        current-jiffy
      )
    )
     (
      jps89 (
        jiffies-per-second
      )
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
            absf x
          )
           (
            call/cc (
              lambda (
                ret6
              )
               (
                ret6 (
                  if (
                    < x 0.0
                  )
                   (
                    - x
                  )
                   x
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
                ret7
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
                                        _len a
                                      )
                                    )
                                     (
                                      begin (
                                        set! s (
                                          _add s (
                                            * (
                                              list-ref a i
                                            )
                                             (
                                              list-ref b i
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
                        ret7 s
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
            vector_scale v s
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
                                        _len v
                                      )
                                    )
                                     (
                                      begin (
                                        set! res (
                                          append res (
                                            _list (
                                              * (
                                                list-ref v i
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
            vector_sub a b
          )
           (
            call/cc (
              lambda (
                ret13
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
                                        set! res (
                                          append res (
                                            _list (
                                              - (
                                                list-ref a i
                                              )
                                               (
                                                list-ref b i
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
                                        loop14
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
                              loop14
                            )
                          )
                        )
                      )
                       (
                        ret13 res
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
                ret16
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
                                        _len a
                                      )
                                    )
                                     (
                                      begin (
                                        set! res (
                                          append res (
                                            _list (
                                              + (
                                                list-ref a i
                                              )
                                               (
                                                list-ref b i
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
                                        loop17
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
                              loop17
                            )
                          )
                        )
                      )
                       (
                        ret16 res
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
            zeros_matrix r c
          )
           (
            call/cc (
              lambda (
                ret19
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
                                      < i r
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
                                                              < j c
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
                                                set! m (
                                                  append m (
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
                                        loop20
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
                              loop20
                            )
                          )
                        )
                      )
                       (
                        ret19 m
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
            column m idx
          )
           (
            call/cc (
              lambda (
                ret24
              )
               (
                let (
                  (
                    col (
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
                                      < i (
                                        _len m
                                      )
                                    )
                                     (
                                      begin (
                                        set! col (
                                          append col (
                                            _list (
                                              cond (
                                                (
                                                  string? (
                                                    list-ref m i
                                                  )
                                                )
                                                 (
                                                  _substring (
                                                    list-ref m i
                                                  )
                                                   idx (
                                                    + idx 1
                                                  )
                                                )
                                              )
                                               (
                                                (
                                                  hash-table? (
                                                    list-ref m i
                                                  )
                                                )
                                                 (
                                                  hash-table-ref (
                                                    list-ref m i
                                                  )
                                                   idx
                                                )
                                              )
                                               (
                                                else (
                                                  list-ref (
                                                    list-ref m i
                                                  )
                                                   idx
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
                                        loop25
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
                              loop25
                            )
                          )
                        )
                      )
                       (
                        ret24 col
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
            validate_adjacency_list graph
          )
           (
            call/cc (
              lambda (
                ret27
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
                                  < i (
                                    _len graph
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
                                                      < j (
                                                        _len (
                                                          list-ref graph i
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            v (
                                                              cond (
                                                                (
                                                                  string? (
                                                                    list-ref graph i
                                                                  )
                                                                )
                                                                 (
                                                                  _substring (
                                                                    list-ref graph i
                                                                  )
                                                                   j (
                                                                    + j 1
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                (
                                                                  hash-table? (
                                                                    list-ref graph i
                                                                  )
                                                                )
                                                                 (
                                                                  hash-table-ref (
                                                                    list-ref graph i
                                                                  )
                                                                   j
                                                                )
                                                              )
                                                               (
                                                                else (
                                                                  list-ref (
                                                                    list-ref graph i
                                                                  )
                                                                   j
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            if (
                                                              or (
                                                                < v 0
                                                              )
                                                               (
                                                                >= v (
                                                                  _len graph
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                panic "Invalid neighbor"
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
                                        set! i (
                                          + i 1
                                        )
                                      )
                                    )
                                  )
                                   (
                                    loop28
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
                          loop28
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
            multiply_matrix_vector graph vector
          )
           (
            call/cc (
              lambda (
                ret32
              )
               (
                let (
                  (
                    n (
                      _len graph
                    )
                  )
                )
                 (
                  begin (
                    if (
                      not (
                        equal? (
                          _len vector
                        )
                         n
                      )
                    )
                     (
                      begin (
                        panic "Vector length must match number of nodes"
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
                                          < i n
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
                                                                    _len (
                                                                      list-ref graph i
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        nb (
                                                                          cond (
                                                                            (
                                                                              string? (
                                                                                list-ref graph i
                                                                              )
                                                                            )
                                                                             (
                                                                              _substring (
                                                                                list-ref graph i
                                                                              )
                                                                               j (
                                                                                + j 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            (
                                                                              hash-table? (
                                                                                list-ref graph i
                                                                              )
                                                                            )
                                                                             (
                                                                              hash-table-ref (
                                                                                list-ref graph i
                                                                              )
                                                                               j
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref (
                                                                                list-ref graph i
                                                                              )
                                                                               j
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        set! sum (
                                                                          + sum (
                                                                            list-ref vector nb
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
                                                    set! result (
                                                      append result (
                                                        _list sum
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
                                            loop33
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
                                  loop33
                                )
                              )
                            )
                          )
                           (
                            ret32 result
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
            lanczos_iteration graph k
          )
           (
            call/cc (
              lambda (
                ret37
              )
               (
                let (
                  (
                    n (
                      _len graph
                    )
                  )
                )
                 (
                  begin (
                    if (
                      or (
                        < k 1
                      )
                       (
                        > k n
                      )
                    )
                     (
                      begin (
                        panic "invalid number of eigenvectors"
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
                        q (
                          zeros_matrix n k
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            t (
                              zeros_matrix k k
                            )
                          )
                        )
                         (
                          begin (
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
                                                  < i n
                                                )
                                                 (
                                                  begin (
                                                    set! v (
                                                      append v (
                                                        _list (
                                                          random
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
                                                    loop38
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
                                          loop38
                                        )
                                      )
                                    )
                                  )
                                   (
                                    let (
                                      (
                                        ss 0.0
                                      )
                                    )
                                     (
                                      begin (
                                        set! i 0
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
                                                      < i n
                                                    )
                                                     (
                                                      begin (
                                                        set! ss (
                                                          _add ss (
                                                            * (
                                                              list-ref v i
                                                            )
                                                             (
                                                              list-ref v i
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
                                                        loop40
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
                                              loop40
                                            )
                                          )
                                        )
                                      )
                                       (
                                        let (
                                          (
                                            vnorm (
                                              sqrtApprox ss
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            set! i 0
                                          )
                                           (
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
                                                          < i n
                                                        )
                                                         (
                                                          begin (
                                                            list-set! (
                                                              list-ref q i
                                                            )
                                                             0 (
                                                              _div (
                                                                list-ref v i
                                                              )
                                                               vnorm
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
                                            let (
                                              (
                                                beta 0.0
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
                                                                  < j k
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        w (
                                                                          multiply_matrix_vector graph (
                                                                            column q j
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        if (
                                                                          > j 0
                                                                        )
                                                                         (
                                                                          begin (
                                                                            set! w (
                                                                              vector_sub w (
                                                                                vector_scale (
                                                                                  column q (
                                                                                    - j 1
                                                                                  )
                                                                                )
                                                                                 beta
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
                                                                            alpha (
                                                                              dot (
                                                                                column q j
                                                                              )
                                                                               w
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            set! w (
                                                                              vector_sub w (
                                                                                vector_scale (
                                                                                  column q j
                                                                                )
                                                                                 alpha
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            let (
                                                                              (
                                                                                ss2 0.0
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                let (
                                                                                  (
                                                                                    p 0
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
                                                                                                  < p n
                                                                                                )
                                                                                                 (
                                                                                                  begin (
                                                                                                    set! ss2 (
                                                                                                      _add ss2 (
                                                                                                        * (
                                                                                                          cond (
                                                                                                            (
                                                                                                              string? w
                                                                                                            )
                                                                                                             (
                                                                                                              _substring w p (
                                                                                                                + p 1
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                           (
                                                                                                            (
                                                                                                              hash-table? w
                                                                                                            )
                                                                                                             (
                                                                                                              hash-table-ref w p
                                                                                                            )
                                                                                                          )
                                                                                                           (
                                                                                                            else (
                                                                                                              list-ref w p
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          cond (
                                                                                                            (
                                                                                                              string? w
                                                                                                            )
                                                                                                             (
                                                                                                              _substring w p (
                                                                                                                + p 1
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                           (
                                                                                                            (
                                                                                                              hash-table? w
                                                                                                            )
                                                                                                             (
                                                                                                              hash-table-ref w p
                                                                                                            )
                                                                                                          )
                                                                                                           (
                                                                                                            else (
                                                                                                              list-ref w p
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    set! p (
                                                                                                      + p 1
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
                                                                                    set! beta (
                                                                                      sqrtApprox ss2
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    list-set! (
                                                                                      list-ref t j
                                                                                    )
                                                                                     j alpha
                                                                                  )
                                                                                   (
                                                                                    if (
                                                                                      < j (
                                                                                        - k 1
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        list-set! (
                                                                                          list-ref t j
                                                                                        )
                                                                                         (
                                                                                          + j 1
                                                                                        )
                                                                                         beta
                                                                                      )
                                                                                       (
                                                                                        list-set! (
                                                                                          list-ref t (
                                                                                            + j 1
                                                                                          )
                                                                                        )
                                                                                         j beta
                                                                                      )
                                                                                       (
                                                                                        if (
                                                                                          > beta 1e-10
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            let (
                                                                                              (
                                                                                                wnorm (
                                                                                                  vector_scale w (
                                                                                                    _div 1.0 beta
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                let (
                                                                                                  (
                                                                                                    r 0
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  begin (
                                                                                                    call/cc (
                                                                                                      lambda (
                                                                                                        break49
                                                                                                      )
                                                                                                       (
                                                                                                        letrec (
                                                                                                          (
                                                                                                            loop48 (
                                                                                                              lambda (
                                                                                                                
                                                                                                              )
                                                                                                               (
                                                                                                                if (
                                                                                                                  < r n
                                                                                                                )
                                                                                                                 (
                                                                                                                  begin (
                                                                                                                    list-set! (
                                                                                                                      list-ref q r
                                                                                                                    )
                                                                                                                     (
                                                                                                                      + j 1
                                                                                                                    )
                                                                                                                     (
                                                                                                                      cond (
                                                                                                                        (
                                                                                                                          string? wnorm
                                                                                                                        )
                                                                                                                         (
                                                                                                                          _substring wnorm r (
                                                                                                                            + r 1
                                                                                                                          )
                                                                                                                        )
                                                                                                                      )
                                                                                                                       (
                                                                                                                        (
                                                                                                                          hash-table? wnorm
                                                                                                                        )
                                                                                                                         (
                                                                                                                          hash-table-ref wnorm r
                                                                                                                        )
                                                                                                                      )
                                                                                                                       (
                                                                                                                        else (
                                                                                                                          list-ref wnorm r
                                                                                                                        )
                                                                                                                      )
                                                                                                                    )
                                                                                                                  )
                                                                                                                   (
                                                                                                                    set! r (
                                                                                                                      + r 1
                                                                                                                    )
                                                                                                                  )
                                                                                                                   (
                                                                                                                    loop48
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
                                                                                                          loop48
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
                                                                                    set! j (
                                                                                      + j 1
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
                                                                    loop44
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
                                                          loop44
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    ret37 (
                                                      alist->hash-table (
                                                        _list (
                                                          cons "t" t
                                                        )
                                                         (
                                                          cons "q" q
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
                  )
                )
              )
            )
          )
        )
         (
          define (
            jacobi_eigen a_in max_iter
          )
           (
            call/cc (
              lambda (
                ret50
              )
               (
                let (
                  (
                    n (
                      _len a_in
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        a a_in
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            v (
                              zeros_matrix n n
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
                                              < i n
                                            )
                                             (
                                              begin (
                                                list-set! (
                                                  list-ref v i
                                                )
                                                 i 1.0
                                              )
                                               (
                                                set! i (
                                                  + i 1
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
                                let (
                                  (
                                    iter 0
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
                                                  < iter max_iter
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        p 0
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            q 1
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                max (
                                                                  absf (
                                                                    cond (
                                                                      (
                                                                        string? (
                                                                          list-ref a p
                                                                        )
                                                                      )
                                                                       (
                                                                        _substring (
                                                                          list-ref a p
                                                                        )
                                                                         q (
                                                                          + q 1
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      (
                                                                        hash-table? (
                                                                          list-ref a p
                                                                        )
                                                                      )
                                                                       (
                                                                        hash-table-ref (
                                                                          list-ref a p
                                                                        )
                                                                         q
                                                                      )
                                                                    )
                                                                     (
                                                                      else (
                                                                        list-ref (
                                                                          list-ref a p
                                                                        )
                                                                         q
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                set! i 0
                                                              )
                                                               (
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
                                                                              < i n
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
                                                                                                  < j n
                                                                                                )
                                                                                                 (
                                                                                                  begin (
                                                                                                    let (
                                                                                                      (
                                                                                                        val (
                                                                                                          absf (
                                                                                                            cond (
                                                                                                              (
                                                                                                                string? (
                                                                                                                  list-ref a i
                                                                                                                )
                                                                                                              )
                                                                                                               (
                                                                                                                _substring (
                                                                                                                  list-ref a i
                                                                                                                )
                                                                                                                 j (
                                                                                                                  + j 1
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              (
                                                                                                                hash-table? (
                                                                                                                  list-ref a i
                                                                                                                )
                                                                                                              )
                                                                                                               (
                                                                                                                hash-table-ref (
                                                                                                                  list-ref a i
                                                                                                                )
                                                                                                                 j
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              else (
                                                                                                                list-ref (
                                                                                                                  list-ref a i
                                                                                                                )
                                                                                                                 j
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      begin (
                                                                                                        if (
                                                                                                          _gt val max
                                                                                                        )
                                                                                                         (
                                                                                                          begin (
                                                                                                            set! max val
                                                                                                          )
                                                                                                           (
                                                                                                            set! p i
                                                                                                          )
                                                                                                           (
                                                                                                            set! q j
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
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    loop57
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
                                                                                          loop57
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
                                                                                loop55
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
                                                                      loop55
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                if (
                                                                  _lt max 1e-08
                                                                )
                                                                 (
                                                                  begin (
                                                                    break54 (
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
                                                                    app (
                                                                      cond (
                                                                        (
                                                                          string? (
                                                                            list-ref a p
                                                                          )
                                                                        )
                                                                         (
                                                                          _substring (
                                                                            list-ref a p
                                                                          )
                                                                           p (
                                                                            + p 1
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        (
                                                                          hash-table? (
                                                                            list-ref a p
                                                                          )
                                                                        )
                                                                         (
                                                                          hash-table-ref (
                                                                            list-ref a p
                                                                          )
                                                                           p
                                                                        )
                                                                      )
                                                                       (
                                                                        else (
                                                                          list-ref (
                                                                            list-ref a p
                                                                          )
                                                                           p
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        aqq (
                                                                          cond (
                                                                            (
                                                                              string? (
                                                                                list-ref a q
                                                                              )
                                                                            )
                                                                             (
                                                                              _substring (
                                                                                list-ref a q
                                                                              )
                                                                               q (
                                                                                + q 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            (
                                                                              hash-table? (
                                                                                list-ref a q
                                                                              )
                                                                            )
                                                                             (
                                                                              hash-table-ref (
                                                                                list-ref a q
                                                                              )
                                                                               q
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref (
                                                                                list-ref a q
                                                                              )
                                                                               q
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            apq (
                                                                              cond (
                                                                                (
                                                                                  string? (
                                                                                    list-ref a p
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  _substring (
                                                                                    list-ref a p
                                                                                  )
                                                                                   q (
                                                                                    + q 1
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                (
                                                                                  hash-table? (
                                                                                    list-ref a p
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  hash-table-ref (
                                                                                    list-ref a p
                                                                                  )
                                                                                   q
                                                                                )
                                                                              )
                                                                               (
                                                                                else (
                                                                                  list-ref (
                                                                                    list-ref a p
                                                                                  )
                                                                                   q
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                theta (
                                                                                  _div (
                                                                                    - aqq app
                                                                                  )
                                                                                   (
                                                                                    * 2.0 apq
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                let (
                                                                                  (
                                                                                    t (
                                                                                      _div 1.0 (
                                                                                        _add (
                                                                                          absf theta
                                                                                        )
                                                                                         (
                                                                                          sqrtApprox (
                                                                                            _add (
                                                                                              * theta theta
                                                                                            )
                                                                                             1.0
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    if (
                                                                                      < theta 0.0
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        set! t (
                                                                                          - t
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
                                                                                        c (
                                                                                          _div 1.0 (
                                                                                            sqrtApprox (
                                                                                              _add 1.0 (
                                                                                                * t t
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
                                                                                            s (
                                                                                              * t c
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            let (
                                                                                              (
                                                                                                tau (
                                                                                                  _div s (
                                                                                                    _add 1.0 c
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                list-set! (
                                                                                                  list-ref a p
                                                                                                )
                                                                                                 p (
                                                                                                  - app (
                                                                                                    * t apq
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                list-set! (
                                                                                                  list-ref a q
                                                                                                )
                                                                                                 q (
                                                                                                  _add aqq (
                                                                                                    * t apq
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                list-set! (
                                                                                                  list-ref a p
                                                                                                )
                                                                                                 q 0.0
                                                                                              )
                                                                                               (
                                                                                                list-set! (
                                                                                                  list-ref a q
                                                                                                )
                                                                                                 p 0.0
                                                                                              )
                                                                                               (
                                                                                                let (
                                                                                                  (
                                                                                                    k 0
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
                                                                                                                  < k n
                                                                                                                )
                                                                                                                 (
                                                                                                                  begin (
                                                                                                                    if (
                                                                                                                      and (
                                                                                                                        not (
                                                                                                                          equal? k p
                                                                                                                        )
                                                                                                                      )
                                                                                                                       (
                                                                                                                        not (
                                                                                                                          equal? k q
                                                                                                                        )
                                                                                                                      )
                                                                                                                    )
                                                                                                                     (
                                                                                                                      begin (
                                                                                                                        let (
                                                                                                                          (
                                                                                                                            akp (
                                                                                                                              cond (
                                                                                                                                (
                                                                                                                                  string? (
                                                                                                                                    list-ref a k
                                                                                                                                  )
                                                                                                                                )
                                                                                                                                 (
                                                                                                                                  _substring (
                                                                                                                                    list-ref a k
                                                                                                                                  )
                                                                                                                                   p (
                                                                                                                                    + p 1
                                                                                                                                  )
                                                                                                                                )
                                                                                                                              )
                                                                                                                               (
                                                                                                                                (
                                                                                                                                  hash-table? (
                                                                                                                                    list-ref a k
                                                                                                                                  )
                                                                                                                                )
                                                                                                                                 (
                                                                                                                                  hash-table-ref (
                                                                                                                                    list-ref a k
                                                                                                                                  )
                                                                                                                                   p
                                                                                                                                )
                                                                                                                              )
                                                                                                                               (
                                                                                                                                else (
                                                                                                                                  list-ref (
                                                                                                                                    list-ref a k
                                                                                                                                  )
                                                                                                                                   p
                                                                                                                                )
                                                                                                                              )
                                                                                                                            )
                                                                                                                          )
                                                                                                                        )
                                                                                                                         (
                                                                                                                          begin (
                                                                                                                            let (
                                                                                                                              (
                                                                                                                                akq (
                                                                                                                                  cond (
                                                                                                                                    (
                                                                                                                                      string? (
                                                                                                                                        list-ref a k
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      _substring (
                                                                                                                                        list-ref a k
                                                                                                                                      )
                                                                                                                                       q (
                                                                                                                                        + q 1
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    (
                                                                                                                                      hash-table? (
                                                                                                                                        list-ref a k
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      hash-table-ref (
                                                                                                                                        list-ref a k
                                                                                                                                      )
                                                                                                                                       q
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    else (
                                                                                                                                      list-ref (
                                                                                                                                        list-ref a k
                                                                                                                                      )
                                                                                                                                       q
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                )
                                                                                                                              )
                                                                                                                            )
                                                                                                                             (
                                                                                                                              begin (
                                                                                                                                list-set! (
                                                                                                                                  list-ref a k
                                                                                                                                )
                                                                                                                                 p (
                                                                                                                                  - akp (
                                                                                                                                    * s (
                                                                                                                                      _add akq (
                                                                                                                                        * tau akp
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                )
                                                                                                                              )
                                                                                                                               (
                                                                                                                                list-set! (
                                                                                                                                  list-ref a p
                                                                                                                                )
                                                                                                                                 k (
                                                                                                                                  cond (
                                                                                                                                    (
                                                                                                                                      string? (
                                                                                                                                        list-ref a k
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      _substring (
                                                                                                                                        list-ref a k
                                                                                                                                      )
                                                                                                                                       p (
                                                                                                                                        + p 1
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    (
                                                                                                                                      hash-table? (
                                                                                                                                        list-ref a k
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      hash-table-ref (
                                                                                                                                        list-ref a k
                                                                                                                                      )
                                                                                                                                       p
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    else (
                                                                                                                                      list-ref (
                                                                                                                                        list-ref a k
                                                                                                                                      )
                                                                                                                                       p
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                )
                                                                                                                              )
                                                                                                                               (
                                                                                                                                list-set! (
                                                                                                                                  list-ref a k
                                                                                                                                )
                                                                                                                                 q (
                                                                                                                                  _add akq (
                                                                                                                                    * s (
                                                                                                                                      - akp (
                                                                                                                                        * tau akq
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                )
                                                                                                                              )
                                                                                                                               (
                                                                                                                                list-set! (
                                                                                                                                  list-ref a q
                                                                                                                                )
                                                                                                                                 k (
                                                                                                                                  cond (
                                                                                                                                    (
                                                                                                                                      string? (
                                                                                                                                        list-ref a k
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      _substring (
                                                                                                                                        list-ref a k
                                                                                                                                      )
                                                                                                                                       q (
                                                                                                                                        + q 1
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    (
                                                                                                                                      hash-table? (
                                                                                                                                        list-ref a k
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      hash-table-ref (
                                                                                                                                        list-ref a k
                                                                                                                                      )
                                                                                                                                       q
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    else (
                                                                                                                                      list-ref (
                                                                                                                                        list-ref a k
                                                                                                                                      )
                                                                                                                                       q
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
                                                                                                                      quote (
                                                                                                                        
                                                                                                                      )
                                                                                                                    )
                                                                                                                  )
                                                                                                                   (
                                                                                                                    set! k (
                                                                                                                      + k 1
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
                                                                                                    set! k 0
                                                                                                  )
                                                                                                   (
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
                                                                                                                  < k n
                                                                                                                )
                                                                                                                 (
                                                                                                                  begin (
                                                                                                                    let (
                                                                                                                      (
                                                                                                                        vkp (
                                                                                                                          cond (
                                                                                                                            (
                                                                                                                              string? (
                                                                                                                                cond (
                                                                                                                                  (
                                                                                                                                    string? v
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    _substring v k (
                                                                                                                                      + k 1
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                )
                                                                                                                                 (
                                                                                                                                  (
                                                                                                                                    hash-table? v
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    hash-table-ref v k
                                                                                                                                  )
                                                                                                                                )
                                                                                                                                 (
                                                                                                                                  else (
                                                                                                                                    list-ref v k
                                                                                                                                  )
                                                                                                                                )
                                                                                                                              )
                                                                                                                            )
                                                                                                                             (
                                                                                                                              _substring (
                                                                                                                                cond (
                                                                                                                                  (
                                                                                                                                    string? v
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    _substring v k (
                                                                                                                                      + k 1
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                )
                                                                                                                                 (
                                                                                                                                  (
                                                                                                                                    hash-table? v
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    hash-table-ref v k
                                                                                                                                  )
                                                                                                                                )
                                                                                                                                 (
                                                                                                                                  else (
                                                                                                                                    list-ref v k
                                                                                                                                  )
                                                                                                                                )
                                                                                                                              )
                                                                                                                               p (
                                                                                                                                + p 1
                                                                                                                              )
                                                                                                                            )
                                                                                                                          )
                                                                                                                           (
                                                                                                                            (
                                                                                                                              hash-table? (
                                                                                                                                cond (
                                                                                                                                  (
                                                                                                                                    string? v
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    _substring v k (
                                                                                                                                      + k 1
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                )
                                                                                                                                 (
                                                                                                                                  (
                                                                                                                                    hash-table? v
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    hash-table-ref v k
                                                                                                                                  )
                                                                                                                                )
                                                                                                                                 (
                                                                                                                                  else (
                                                                                                                                    list-ref v k
                                                                                                                                  )
                                                                                                                                )
                                                                                                                              )
                                                                                                                            )
                                                                                                                             (
                                                                                                                              hash-table-ref (
                                                                                                                                cond (
                                                                                                                                  (
                                                                                                                                    string? v
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    _substring v k (
                                                                                                                                      + k 1
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                )
                                                                                                                                 (
                                                                                                                                  (
                                                                                                                                    hash-table? v
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    hash-table-ref v k
                                                                                                                                  )
                                                                                                                                )
                                                                                                                                 (
                                                                                                                                  else (
                                                                                                                                    list-ref v k
                                                                                                                                  )
                                                                                                                                )
                                                                                                                              )
                                                                                                                               p
                                                                                                                            )
                                                                                                                          )
                                                                                                                           (
                                                                                                                            else (
                                                                                                                              list-ref (
                                                                                                                                cond (
                                                                                                                                  (
                                                                                                                                    string? v
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    _substring v k (
                                                                                                                                      + k 1
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                )
                                                                                                                                 (
                                                                                                                                  (
                                                                                                                                    hash-table? v
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    hash-table-ref v k
                                                                                                                                  )
                                                                                                                                )
                                                                                                                                 (
                                                                                                                                  else (
                                                                                                                                    list-ref v k
                                                                                                                                  )
                                                                                                                                )
                                                                                                                              )
                                                                                                                               p
                                                                                                                            )
                                                                                                                          )
                                                                                                                        )
                                                                                                                      )
                                                                                                                    )
                                                                                                                     (
                                                                                                                      begin (
                                                                                                                        let (
                                                                                                                          (
                                                                                                                            vkq (
                                                                                                                              cond (
                                                                                                                                (
                                                                                                                                  string? (
                                                                                                                                    cond (
                                                                                                                                      (
                                                                                                                                        string? v
                                                                                                                                      )
                                                                                                                                       (
                                                                                                                                        _substring v k (
                                                                                                                                          + k 1
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      (
                                                                                                                                        hash-table? v
                                                                                                                                      )
                                                                                                                                       (
                                                                                                                                        hash-table-ref v k
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      else (
                                                                                                                                        list-ref v k
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                )
                                                                                                                                 (
                                                                                                                                  _substring (
                                                                                                                                    cond (
                                                                                                                                      (
                                                                                                                                        string? v
                                                                                                                                      )
                                                                                                                                       (
                                                                                                                                        _substring v k (
                                                                                                                                          + k 1
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      (
                                                                                                                                        hash-table? v
                                                                                                                                      )
                                                                                                                                       (
                                                                                                                                        hash-table-ref v k
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      else (
                                                                                                                                        list-ref v k
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   q (
                                                                                                                                    + q 1
                                                                                                                                  )
                                                                                                                                )
                                                                                                                              )
                                                                                                                               (
                                                                                                                                (
                                                                                                                                  hash-table? (
                                                                                                                                    cond (
                                                                                                                                      (
                                                                                                                                        string? v
                                                                                                                                      )
                                                                                                                                       (
                                                                                                                                        _substring v k (
                                                                                                                                          + k 1
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      (
                                                                                                                                        hash-table? v
                                                                                                                                      )
                                                                                                                                       (
                                                                                                                                        hash-table-ref v k
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      else (
                                                                                                                                        list-ref v k
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                )
                                                                                                                                 (
                                                                                                                                  hash-table-ref (
                                                                                                                                    cond (
                                                                                                                                      (
                                                                                                                                        string? v
                                                                                                                                      )
                                                                                                                                       (
                                                                                                                                        _substring v k (
                                                                                                                                          + k 1
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      (
                                                                                                                                        hash-table? v
                                                                                                                                      )
                                                                                                                                       (
                                                                                                                                        hash-table-ref v k
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      else (
                                                                                                                                        list-ref v k
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   q
                                                                                                                                )
                                                                                                                              )
                                                                                                                               (
                                                                                                                                else (
                                                                                                                                  list-ref (
                                                                                                                                    cond (
                                                                                                                                      (
                                                                                                                                        string? v
                                                                                                                                      )
                                                                                                                                       (
                                                                                                                                        _substring v k (
                                                                                                                                          + k 1
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      (
                                                                                                                                        hash-table? v
                                                                                                                                      )
                                                                                                                                       (
                                                                                                                                        hash-table-ref v k
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      else (
                                                                                                                                        list-ref v k
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   q
                                                                                                                                )
                                                                                                                              )
                                                                                                                            )
                                                                                                                          )
                                                                                                                        )
                                                                                                                         (
                                                                                                                          begin (
                                                                                                                            list-set! (
                                                                                                                              list-ref v k
                                                                                                                            )
                                                                                                                             p (
                                                                                                                              - vkp (
                                                                                                                                * s (
                                                                                                                                  _add vkq (
                                                                                                                                    * tau vkp
                                                                                                                                  )
                                                                                                                                )
                                                                                                                              )
                                                                                                                            )
                                                                                                                          )
                                                                                                                           (
                                                                                                                            list-set! (
                                                                                                                              list-ref v k
                                                                                                                            )
                                                                                                                             q (
                                                                                                                              _add vkq (
                                                                                                                                * s (
                                                                                                                                  - vkp (
                                                                                                                                    * tau vkq
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
                                                                                                                        )
                                                                                                                      )
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
                                                                                                    set! iter (
                                                                                                      + iter 1
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
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    loop53
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
                                          loop53
                                        )
                                      )
                                    )
                                  )
                                   (
                                    let (
                                      (
                                        eigenvalues (
                                          _list
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        set! i 0
                                      )
                                       (
                                        call/cc (
                                          lambda (
                                            break64
                                          )
                                           (
                                            letrec (
                                              (
                                                loop63 (
                                                  lambda (
                                                    
                                                  )
                                                   (
                                                    if (
                                                      < i n
                                                    )
                                                     (
                                                      begin (
                                                        set! eigenvalues (
                                                          append eigenvalues (
                                                            _list (
                                                              cond (
                                                                (
                                                                  string? (
                                                                    list-ref a i
                                                                  )
                                                                )
                                                                 (
                                                                  _substring (
                                                                    list-ref a i
                                                                  )
                                                                   i (
                                                                    + i 1
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                (
                                                                  hash-table? (
                                                                    list-ref a i
                                                                  )
                                                                )
                                                                 (
                                                                  hash-table-ref (
                                                                    list-ref a i
                                                                  )
                                                                   i
                                                                )
                                                              )
                                                               (
                                                                else (
                                                                  list-ref (
                                                                    list-ref a i
                                                                  )
                                                                   i
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
                                                        loop63
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
                                              loop63
                                            )
                                          )
                                        )
                                      )
                                       (
                                        ret50 (
                                          alist->hash-table (
                                            _list (
                                              cons "values" eigenvalues
                                            )
                                             (
                                              cons "vectors" v
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
            matmul a b
          )
           (
            call/cc (
              lambda (
                ret65
              )
               (
                let (
                  (
                    rows (
                      _len a
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        cols (
                          _len (
                            list-ref b 0
                          )
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            inner (
                              _len b
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                m (
                                  zeros_matrix rows cols
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
                                                  < i rows
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
                                                            break69
                                                          )
                                                           (
                                                            letrec (
                                                              (
                                                                loop68 (
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
                                                                                k 0
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                call/cc (
                                                                                  lambda (
                                                                                    break71
                                                                                  )
                                                                                   (
                                                                                    letrec (
                                                                                      (
                                                                                        loop70 (
                                                                                          lambda (
                                                                                            
                                                                                          )
                                                                                           (
                                                                                            if (
                                                                                              < k inner
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                set! s (
                                                                                                  _add s (
                                                                                                    * (
                                                                                                      cond (
                                                                                                        (
                                                                                                          string? (
                                                                                                            list-ref a i
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          _substring (
                                                                                                            list-ref a i
                                                                                                          )
                                                                                                           k (
                                                                                                            + k 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? (
                                                                                                            list-ref a i
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref (
                                                                                                            list-ref a i
                                                                                                          )
                                                                                                           k
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref (
                                                                                                            list-ref a i
                                                                                                          )
                                                                                                           k
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      cond (
                                                                                                        (
                                                                                                          string? (
                                                                                                            list-ref b k
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          _substring (
                                                                                                            list-ref b k
                                                                                                          )
                                                                                                           j (
                                                                                                            + j 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? (
                                                                                                            list-ref b k
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref (
                                                                                                            list-ref b k
                                                                                                          )
                                                                                                           j
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref (
                                                                                                            list-ref b k
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
                                                                                                loop70
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
                                                                                      loop70
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                list-set! (
                                                                                  list-ref m i
                                                                                )
                                                                                 j s
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
                                                                        loop68
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
                                                              loop68
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
                                    ret65 m
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
            sort_eigenpairs vals vecs
          )
           (
            call/cc (
              lambda (
                ret72
              )
               (
                let (
                  (
                    n (
                      _len vals
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        values vals
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            vectors vecs
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
                                    break74
                                  )
                                   (
                                    letrec (
                                      (
                                        loop73 (
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
                                                    j 0
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    call/cc (
                                                      lambda (
                                                        break76
                                                      )
                                                       (
                                                        letrec (
                                                          (
                                                            loop75 (
                                                              lambda (
                                                                
                                                              )
                                                               (
                                                                if (
                                                                  < j (
                                                                    - n 1
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    if (
                                                                      < (
                                                                        list-ref values j
                                                                      )
                                                                       (
                                                                        list-ref values (
                                                                          + j 1
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            tmp (
                                                                              list-ref values j
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            list-set! values j (
                                                                              list-ref values (
                                                                                + j 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            list-set! values (
                                                                              + j 1
                                                                            )
                                                                             tmp
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
                                                                                    break78
                                                                                  )
                                                                                   (
                                                                                    letrec (
                                                                                      (
                                                                                        loop77 (
                                                                                          lambda (
                                                                                            
                                                                                          )
                                                                                           (
                                                                                            if (
                                                                                              < r (
                                                                                                _len vectors
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                let (
                                                                                                  (
                                                                                                    tv (
                                                                                                      cond (
                                                                                                        (
                                                                                                          string? (
                                                                                                            list-ref vectors r
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          _substring (
                                                                                                            list-ref vectors r
                                                                                                          )
                                                                                                           j (
                                                                                                            + j 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? (
                                                                                                            list-ref vectors r
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref (
                                                                                                            list-ref vectors r
                                                                                                          )
                                                                                                           j
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref (
                                                                                                            list-ref vectors r
                                                                                                          )
                                                                                                           j
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  begin (
                                                                                                    list-set! (
                                                                                                      list-ref vectors r
                                                                                                    )
                                                                                                     j (
                                                                                                      cond (
                                                                                                        (
                                                                                                          string? (
                                                                                                            list-ref vectors r
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          _substring (
                                                                                                            list-ref vectors r
                                                                                                          )
                                                                                                           (
                                                                                                            + j 1
                                                                                                          )
                                                                                                           (
                                                                                                            + (
                                                                                                              + j 1
                                                                                                            )
                                                                                                             1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? (
                                                                                                            list-ref vectors r
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref (
                                                                                                            list-ref vectors r
                                                                                                          )
                                                                                                           (
                                                                                                            + j 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref (
                                                                                                            list-ref vectors r
                                                                                                          )
                                                                                                           (
                                                                                                            + j 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    list-set! (
                                                                                                      list-ref vectors r
                                                                                                    )
                                                                                                     (
                                                                                                      + j 1
                                                                                                    )
                                                                                                     tv
                                                                                                  )
                                                                                                   (
                                                                                                    set! r (
                                                                                                      + r 1
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                loop77
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
                                                                                      loop77
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
                                                                    loop75
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
                                                          loop75
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
                                                loop73
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
                                      loop73
                                    )
                                  )
                                )
                              )
                               (
                                ret72 (
                                  alist->hash-table (
                                    _list (
                                      cons "values" values
                                    )
                                     (
                                      cons "vectors" vectors
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
            find_lanczos_eigenvectors graph k
          )
           (
            call/cc (
              lambda (
                ret79
              )
               (
                begin (
                  validate_adjacency_list graph
                )
                 (
                  let (
                    (
                      res (
                        lanczos_iteration graph k
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          eig (
                            jacobi_eigen (
                              hash-table-ref res "t"
                            )
                             50
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              sorted (
                                sort_eigenpairs (
                                  hash-table-ref eig "values"
                                )
                                 (
                                  hash-table-ref eig "vectors"
                                )
                              )
                            )
                          )
                           (
                            begin (
                              let (
                                (
                                  final_vectors (
                                    matmul (
                                      hash-table-ref res "q"
                                    )
                                     (
                                      hash-table-ref sorted "vectors"
                                    )
                                  )
                                )
                              )
                               (
                                begin (
                                  ret79 (
                                    alist->hash-table (
                                      _list (
                                        cons "values" (
                                          hash-table-ref sorted "values"
                                        )
                                      )
                                       (
                                        cons "vectors" final_vectors
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
            list_to_string arr
          )
           (
            call/cc (
              lambda (
                ret80
              )
               (
                let (
                  (
                    s "["
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
                            break82
                          )
                           (
                            letrec (
                              (
                                loop81 (
                                  lambda (
                                    
                                  )
                                   (
                                    if (
                                      < i (
                                        _len arr
                                      )
                                    )
                                     (
                                      begin (
                                        set! s (
                                          string-append s (
                                            to-str-space (
                                              list-ref arr i
                                            )
                                          )
                                        )
                                      )
                                       (
                                        if (
                                          < i (
                                            - (
                                              _len arr
                                            )
                                             1
                                          )
                                        )
                                         (
                                          begin (
                                            set! s (
                                              string-append s ", "
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
                                        loop81
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
                              loop81
                            )
                          )
                        )
                      )
                       (
                        ret80 (
                          string-append s "]"
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
                ret83
              )
               (
                let (
                  (
                    s "["
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
                            break85
                          )
                           (
                            letrec (
                              (
                                loop84 (
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
                                        set! s (
                                          string-append s (
                                            list_to_string (
                                              list-ref m i
                                            )
                                          )
                                        )
                                      )
                                       (
                                        if (
                                          < i (
                                            - (
                                              _len m
                                            )
                                             1
                                          )
                                        )
                                         (
                                          begin (
                                            set! s (
                                              string-append s "; "
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
                                        loop84
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
                              loop84
                            )
                          )
                        )
                      )
                       (
                        ret83 (
                          string-append s "]"
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
              graph (
                _list (
                  _list 1 2
                )
                 (
                  _list 0 2
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
                  result (
                    find_lanczos_eigenvectors graph 2
                  )
                )
              )
               (
                begin (
                  _display (
                    if (
                      string? (
                        list_to_string (
                          hash-table-ref result "values"
                        )
                      )
                    )
                     (
                      list_to_string (
                        hash-table-ref result "values"
                      )
                    )
                     (
                      to-str (
                        list_to_string (
                          hash-table-ref result "values"
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
                        matrix_to_string (
                          hash-table-ref result "vectors"
                        )
                      )
                    )
                     (
                      matrix_to_string (
                        hash-table-ref result "vectors"
                      )
                    )
                     (
                      to-str (
                        matrix_to_string (
                          hash-table-ref result "vectors"
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
          end87 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur88 (
              quotient (
                * (
                  - end87 start86
                )
                 1000000
              )
               jps89
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur88
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
