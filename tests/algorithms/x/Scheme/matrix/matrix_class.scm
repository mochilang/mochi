;; Generated on 2025-08-07 11:54 +0700
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
(define (_div a b) (/ a b))
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
      start77 (
        current-jiffy
      )
    )
     (
      jps80 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        make_matrix values
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                r (
                  _len values
                )
              )
            )
             (
              begin (
                if (
                  equal? r 0
                )
                 (
                  begin (
                    ret1 (
                      alist->hash-table (
                        _list (
                          cons "data" (
                            _list
                          )
                        )
                         (
                          cons "rows" 0
                        )
                         (
                          cons "cols" 0
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
                    c (
                      _len (
                        list-ref values 0
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
                                      < i r
                                    )
                                     (
                                      begin (
                                        if (
                                          not (
                                            equal? (
                                              _len (
                                                list-ref values i
                                              )
                                            )
                                             c
                                          )
                                        )
                                         (
                                          begin (
                                            ret1 (
                                              alist->hash-table (
                                                _list (
                                                  cons "data" (
                                                    _list
                                                  )
                                                )
                                                 (
                                                  cons "rows" 0
                                                )
                                                 (
                                                  cons "cols" 0
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
                        ret1 (
                          alist->hash-table (
                            _list (
                              cons "data" values
                            )
                             (
                              cons "rows" r
                            )
                             (
                              cons "cols" c
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
        matrix_columns m
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            let (
              (
                cols (
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
                                  < j (
                                    hash-table-ref m "cols"
                                  )
                                )
                                 (
                                  begin (
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
                                                          < i (
                                                            hash-table-ref m "rows"
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
                                                                        list-ref (
                                                                          hash-table-ref m "data"
                                                                        )
                                                                         i
                                                                      )
                                                                    )
                                                                     (
                                                                      _substring (
                                                                        list-ref (
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
                                                                        list-ref (
                                                                          hash-table-ref m "data"
                                                                        )
                                                                         i
                                                                      )
                                                                    )
                                                                     (
                                                                      hash-table-ref (
                                                                        list-ref (
                                                                          hash-table-ref m "data"
                                                                        )
                                                                         i
                                                                      )
                                                                       j
                                                                    )
                                                                  )
                                                                   (
                                                                    else (
                                                                      list-ref (
                                                                        list-ref (
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
                                                            set! i (
                                                              + i 1
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
                                            set! cols (
                                              append cols (
                                                _list col
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
                    ret4 cols
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
        matrix_identity m
      )
       (
        call/cc (
          lambda (
            ret9
          )
           (
            let (
              (
                vals (
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
                                  < i (
                                    hash-table-ref m "rows"
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
                                                          < j (
                                                            hash-table-ref m "cols"
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                v (
                                                                  if (
                                                                    equal? i j
                                                                  )
                                                                   1.0 0.0
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                set! row (
                                                                  append row (
                                                                    _list v
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
                                            set! vals (
                                              append vals (
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
                    ret9 (
                      alist->hash-table (
                        _list (
                          cons "data" vals
                        )
                         (
                          cons "rows" (
                            hash-table-ref m "rows"
                          )
                        )
                         (
                          cons "cols" (
                            hash-table-ref m "cols"
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
        matrix_minor m r c
      )
       (
        call/cc (
          lambda (
            ret14
          )
           (
            let (
              (
                vals (
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
                                  < i (
                                    hash-table-ref m "rows"
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      not (
                                        equal? i r
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
                                                              < j (
                                                                hash-table-ref m "cols"
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                if (
                                                                  not (
                                                                    equal? j c
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
                                                                                list-ref (
                                                                                  hash-table-ref m "data"
                                                                                )
                                                                                 i
                                                                              )
                                                                            )
                                                                             (
                                                                              _substring (
                                                                                list-ref (
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
                                                                                list-ref (
                                                                                  hash-table-ref m "data"
                                                                                )
                                                                                 i
                                                                              )
                                                                            )
                                                                             (
                                                                              hash-table-ref (
                                                                                list-ref (
                                                                                  hash-table-ref m "data"
                                                                                )
                                                                                 i
                                                                              )
                                                                               j
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref (
                                                                                list-ref (
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
                                                set! vals (
                                                  append vals (
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
                    let (
                      (
                        sub (
                          alist->hash-table (
                            _list (
                              cons "data" vals
                            )
                             (
                              cons "rows" (
                                - (
                                  hash-table-ref m "rows"
                                )
                                 1
                              )
                            )
                             (
                              cons "cols" (
                                - (
                                  hash-table-ref m "cols"
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
                        ret14 (
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
     (
      define (
        matrix_cofactor m r c
      )
       (
        call/cc (
          lambda (
            ret19
          )
           (
            let (
              (
                minor (
                  matrix_minor m r c
                )
              )
            )
             (
              begin (
                if (
                  equal? (
                    _mod (
                      + r c
                    )
                     2
                  )
                   0
                )
                 (
                  begin (
                    ret19 minor
                  )
                )
                 (
                  quote (
                    
                  )
                )
              )
               (
                ret19 (
                  * (
                    - 1.0
                  )
                   minor
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        matrix_minors m
      )
       (
        call/cc (
          lambda (
            ret20
          )
           (
            let (
              (
                vals (
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
                                  < i (
                                    hash-table-ref m "rows"
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
                                                          < j (
                                                            hash-table-ref m "cols"
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            set! row (
                                                              append row (
                                                                _list (
                                                                  matrix_minor m i j
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
                                            set! vals (
                                              append vals (
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
                    ret20 (
                      alist->hash-table (
                        _list (
                          cons "data" vals
                        )
                         (
                          cons "rows" (
                            hash-table-ref m "rows"
                          )
                        )
                         (
                          cons "cols" (
                            hash-table-ref m "cols"
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
        matrix_cofactors m
      )
       (
        call/cc (
          lambda (
            ret25
          )
           (
            let (
              (
                vals (
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
                                  < i (
                                    hash-table-ref m "rows"
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
                                                          < j (
                                                            hash-table-ref m "cols"
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            set! row (
                                                              append row (
                                                                _list (
                                                                  matrix_cofactor m i j
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
                                           (
                                            set! vals (
                                              append vals (
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
                    ret25 (
                      alist->hash-table (
                        _list (
                          cons "data" vals
                        )
                         (
                          cons "rows" (
                            hash-table-ref m "rows"
                          )
                        )
                         (
                          cons "cols" (
                            hash-table-ref m "cols"
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
        matrix_determinant m
      )
       (
        call/cc (
          lambda (
            ret30
          )
           (
            begin (
              if (
                not (
                  equal? (
                    hash-table-ref m "rows"
                  )
                   (
                    hash-table-ref m "cols"
                  )
                )
              )
               (
                begin (
                  ret30 0.0
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
                  hash-table-ref m "rows"
                )
                 0
              )
               (
                begin (
                  ret30 0.0
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
                  hash-table-ref m "rows"
                )
                 1
              )
               (
                begin (
                  ret30 (
                    cond (
                      (
                        string? (
                          list-ref (
                            hash-table-ref m "data"
                          )
                           0
                        )
                      )
                       (
                        _substring (
                          list-ref (
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
                          list-ref (
                            hash-table-ref m "data"
                          )
                           0
                        )
                      )
                       (
                        hash-table-ref (
                          list-ref (
                            hash-table-ref m "data"
                          )
                           0
                        )
                         0
                      )
                    )
                     (
                      else (
                        list-ref (
                          list-ref (
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
                  hash-table-ref m "rows"
                )
                 2
              )
               (
                begin (
                  ret30 (
                    - (
                      * (
                        cond (
                          (
                            string? (
                              list-ref (
                                hash-table-ref m "data"
                              )
                               0
                            )
                          )
                           (
                            _substring (
                              list-ref (
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
                              list-ref (
                                hash-table-ref m "data"
                              )
                               0
                            )
                          )
                           (
                            hash-table-ref (
                              list-ref (
                                hash-table-ref m "data"
                              )
                               0
                            )
                             0
                          )
                        )
                         (
                          else (
                            list-ref (
                              list-ref (
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
                              list-ref (
                                hash-table-ref m "data"
                              )
                               1
                            )
                          )
                           (
                            _substring (
                              list-ref (
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
                              list-ref (
                                hash-table-ref m "data"
                              )
                               1
                            )
                          )
                           (
                            hash-table-ref (
                              list-ref (
                                hash-table-ref m "data"
                              )
                               1
                            )
                             1
                          )
                        )
                         (
                          else (
                            list-ref (
                              list-ref (
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
                              list-ref (
                                hash-table-ref m "data"
                              )
                               0
                            )
                          )
                           (
                            _substring (
                              list-ref (
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
                              list-ref (
                                hash-table-ref m "data"
                              )
                               0
                            )
                          )
                           (
                            hash-table-ref (
                              list-ref (
                                hash-table-ref m "data"
                              )
                               0
                            )
                             1
                          )
                        )
                         (
                          else (
                            list-ref (
                              list-ref (
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
                              list-ref (
                                hash-table-ref m "data"
                              )
                               1
                            )
                          )
                           (
                            _substring (
                              list-ref (
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
                              list-ref (
                                hash-table-ref m "data"
                              )
                               1
                            )
                          )
                           (
                            hash-table-ref (
                              list-ref (
                                hash-table-ref m "data"
                              )
                               1
                            )
                             0
                          )
                        )
                         (
                          else (
                            list-ref (
                              list-ref (
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
                      j 0
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
                                    < j (
                                      hash-table-ref m "cols"
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
                                                  list-ref (
                                                    hash-table-ref m "data"
                                                  )
                                                   0
                                                )
                                              )
                                               (
                                                _substring (
                                                  list-ref (
                                                    hash-table-ref m "data"
                                                  )
                                                   0
                                                )
                                                 j (
                                                  + j 1
                                                )
                                              )
                                            )
                                             (
                                              (
                                                hash-table? (
                                                  list-ref (
                                                    hash-table-ref m "data"
                                                  )
                                                   0
                                                )
                                              )
                                               (
                                                hash-table-ref (
                                                  list-ref (
                                                    hash-table-ref m "data"
                                                  )
                                                   0
                                                )
                                                 j
                                              )
                                            )
                                             (
                                              else (
                                                list-ref (
                                                  list-ref (
                                                    hash-table-ref m "data"
                                                  )
                                                   0
                                                )
                                                 j
                                              )
                                            )
                                          )
                                           (
                                            matrix_cofactor m 0 j
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
                                      loop31
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
                            loop31
                          )
                        )
                      )
                    )
                     (
                      ret30 sum
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
        matrix_is_invertible m
      )
       (
        call/cc (
          lambda (
            ret33
          )
           (
            ret33 (
              not (
                equal? (
                  matrix_determinant m
                )
                 0.0
              )
            )
          )
        )
      )
    )
     (
      define (
        matrix_adjugate m
      )
       (
        call/cc (
          lambda (
            ret34
          )
           (
            let (
              (
                cof (
                  matrix_cofactors m
                )
              )
            )
             (
              begin (
                let (
                  (
                    vals (
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
                                        hash-table-ref m "rows"
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
                                                              < j (
                                                                hash-table-ref m "cols"
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
                                                                            cond (
                                                                              (
                                                                                string? (
                                                                                  hash-table-ref cof "data"
                                                                                )
                                                                              )
                                                                               (
                                                                                _substring (
                                                                                  hash-table-ref cof "data"
                                                                                )
                                                                                 j (
                                                                                  + j 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? (
                                                                                  hash-table-ref cof "data"
                                                                                )
                                                                              )
                                                                               (
                                                                                hash-table-ref (
                                                                                  hash-table-ref cof "data"
                                                                                )
                                                                                 j
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref (
                                                                                  hash-table-ref cof "data"
                                                                                )
                                                                                 j
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          _substring (
                                                                            cond (
                                                                              (
                                                                                string? (
                                                                                  hash-table-ref cof "data"
                                                                                )
                                                                              )
                                                                               (
                                                                                _substring (
                                                                                  hash-table-ref cof "data"
                                                                                )
                                                                                 j (
                                                                                  + j 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? (
                                                                                  hash-table-ref cof "data"
                                                                                )
                                                                              )
                                                                               (
                                                                                hash-table-ref (
                                                                                  hash-table-ref cof "data"
                                                                                )
                                                                                 j
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref (
                                                                                  hash-table-ref cof "data"
                                                                                )
                                                                                 j
                                                                              )
                                                                            )
                                                                          )
                                                                           i (
                                                                            + i 1
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        (
                                                                          hash-table? (
                                                                            cond (
                                                                              (
                                                                                string? (
                                                                                  hash-table-ref cof "data"
                                                                                )
                                                                              )
                                                                               (
                                                                                _substring (
                                                                                  hash-table-ref cof "data"
                                                                                )
                                                                                 j (
                                                                                  + j 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? (
                                                                                  hash-table-ref cof "data"
                                                                                )
                                                                              )
                                                                               (
                                                                                hash-table-ref (
                                                                                  hash-table-ref cof "data"
                                                                                )
                                                                                 j
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref (
                                                                                  hash-table-ref cof "data"
                                                                                )
                                                                                 j
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          hash-table-ref (
                                                                            cond (
                                                                              (
                                                                                string? (
                                                                                  hash-table-ref cof "data"
                                                                                )
                                                                              )
                                                                               (
                                                                                _substring (
                                                                                  hash-table-ref cof "data"
                                                                                )
                                                                                 j (
                                                                                  + j 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? (
                                                                                  hash-table-ref cof "data"
                                                                                )
                                                                              )
                                                                               (
                                                                                hash-table-ref (
                                                                                  hash-table-ref cof "data"
                                                                                )
                                                                                 j
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref (
                                                                                  hash-table-ref cof "data"
                                                                                )
                                                                                 j
                                                                              )
                                                                            )
                                                                          )
                                                                           i
                                                                        )
                                                                      )
                                                                       (
                                                                        else (
                                                                          list-ref (
                                                                            cond (
                                                                              (
                                                                                string? (
                                                                                  hash-table-ref cof "data"
                                                                                )
                                                                              )
                                                                               (
                                                                                _substring (
                                                                                  hash-table-ref cof "data"
                                                                                )
                                                                                 j (
                                                                                  + j 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? (
                                                                                  hash-table-ref cof "data"
                                                                                )
                                                                              )
                                                                               (
                                                                                hash-table-ref (
                                                                                  hash-table-ref cof "data"
                                                                                )
                                                                                 j
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref (
                                                                                  hash-table-ref cof "data"
                                                                                )
                                                                                 j
                                                                              )
                                                                            )
                                                                          )
                                                                           i
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
                                                                loop37
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
                                                      loop37
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                set! vals (
                                                  append vals (
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
                        ret34 (
                          alist->hash-table (
                            _list (
                              cons "data" vals
                            )
                             (
                              cons "rows" (
                                hash-table-ref m "rows"
                              )
                            )
                             (
                              cons "cols" (
                                hash-table-ref m "cols"
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
        matrix_inverse m
      )
       (
        call/cc (
          lambda (
            ret39
          )
           (
            let (
              (
                det (
                  matrix_determinant m
                )
              )
            )
             (
              begin (
                if (
                  equal? det 0.0
                )
                 (
                  begin (
                    ret39 (
                      alist->hash-table (
                        _list (
                          cons "data" (
                            _list
                          )
                        )
                         (
                          cons "rows" 0
                        )
                         (
                          cons "cols" 0
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
                    adj (
                      matrix_adjugate m
                    )
                  )
                )
                 (
                  begin (
                    ret39 (
                      matrix_mul_scalar adj (
                        _div 1.0 det
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
        matrix_add_row m row
      )
       (
        call/cc (
          lambda (
            ret40
          )
           (
            let (
              (
                newData (
                  hash-table-ref m "data"
                )
              )
            )
             (
              begin (
                set! newData (
                  append newData (
                    _list row
                  )
                )
              )
               (
                ret40 (
                  alist->hash-table (
                    _list (
                      cons "data" newData
                    )
                     (
                      cons "rows" (
                        + (
                          hash-table-ref m "rows"
                        )
                         1
                      )
                    )
                     (
                      cons "cols" (
                        hash-table-ref m "cols"
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
        matrix_add_column m col
      )
       (
        call/cc (
          lambda (
            ret41
          )
           (
            let (
              (
                newData (
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
                                  < i (
                                    hash-table-ref m "rows"
                                  )
                                )
                                 (
                                  begin (
                                    set! newData (
                                      append newData (
                                        _list (
                                          append (
                                            list-ref (
                                              hash-table-ref m "data"
                                            )
                                             i
                                          )
                                           (
                                            _list (
                                              list-ref col i
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
                          cons "data" newData
                        )
                         (
                          cons "rows" (
                            hash-table-ref m "rows"
                          )
                        )
                         (
                          cons "cols" (
                            + (
                              hash-table-ref m "cols"
                            )
                             1
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
        matrix_mul_scalar m s
      )
       (
        call/cc (
          lambda (
            ret44
          )
           (
            let (
              (
                vals (
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
                                  < i (
                                    hash-table-ref m "rows"
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
                                                          < j (
                                                            hash-table-ref m "cols"
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
                                                                          list-ref (
                                                                            hash-table-ref m "data"
                                                                          )
                                                                           i
                                                                        )
                                                                      )
                                                                       (
                                                                        _substring (
                                                                          list-ref (
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
                                                                          list-ref (
                                                                            hash-table-ref m "data"
                                                                          )
                                                                           i
                                                                        )
                                                                      )
                                                                       (
                                                                        hash-table-ref (
                                                                          list-ref (
                                                                            hash-table-ref m "data"
                                                                          )
                                                                           i
                                                                        )
                                                                         j
                                                                      )
                                                                    )
                                                                     (
                                                                      else (
                                                                        list-ref (
                                                                          list-ref (
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
                                                            loop47
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
                                                  loop47
                                                )
                                              )
                                            )
                                          )
                                           (
                                            set! vals (
                                              append vals (
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
                                    loop45
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
                          loop45
                        )
                      )
                    )
                  )
                   (
                    ret44 (
                      alist->hash-table (
                        _list (
                          cons "data" vals
                        )
                         (
                          cons "rows" (
                            hash-table-ref m "rows"
                          )
                        )
                         (
                          cons "cols" (
                            hash-table-ref m "cols"
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
        matrix_neg m
      )
       (
        call/cc (
          lambda (
            ret49
          )
           (
            ret49 (
              matrix_mul_scalar m (
                - 1.0
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
            ret50
          )
           (
            begin (
              if (
                or (
                  not (
                    equal? (
                      hash-table-ref a "rows"
                    )
                     (
                      hash-table-ref b "rows"
                    )
                  )
                )
                 (
                  not (
                    equal? (
                      hash-table-ref a "cols"
                    )
                     (
                      hash-table-ref b "cols"
                    )
                  )
                )
              )
               (
                begin (
                  ret50 (
                    alist->hash-table (
                      _list (
                        cons "data" (
                          _list
                        )
                      )
                       (
                        cons "rows" 0
                      )
                       (
                        cons "cols" 0
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
                  vals (
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
                                    < i (
                                      hash-table-ref a "rows"
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
                                                            < j (
                                                              hash-table-ref a "cols"
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
                                                                            list-ref (
                                                                              hash-table-ref a "data"
                                                                            )
                                                                             i
                                                                          )
                                                                        )
                                                                         (
                                                                          _substring (
                                                                            list-ref (
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
                                                                            list-ref (
                                                                              hash-table-ref a "data"
                                                                            )
                                                                             i
                                                                          )
                                                                        )
                                                                         (
                                                                          hash-table-ref (
                                                                            list-ref (
                                                                              hash-table-ref a "data"
                                                                            )
                                                                             i
                                                                          )
                                                                           j
                                                                        )
                                                                      )
                                                                       (
                                                                        else (
                                                                          list-ref (
                                                                            list-ref (
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
                                                                            list-ref (
                                                                              hash-table-ref b "data"
                                                                            )
                                                                             i
                                                                          )
                                                                        )
                                                                         (
                                                                          _substring (
                                                                            list-ref (
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
                                                                            list-ref (
                                                                              hash-table-ref b "data"
                                                                            )
                                                                             i
                                                                          )
                                                                        )
                                                                         (
                                                                          hash-table-ref (
                                                                            list-ref (
                                                                              hash-table-ref b "data"
                                                                            )
                                                                             i
                                                                          )
                                                                           j
                                                                        )
                                                                      )
                                                                       (
                                                                        else (
                                                                          list-ref (
                                                                            list-ref (
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
                                              set! vals (
                                                append vals (
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
                      ret50 (
                        alist->hash-table (
                          _list (
                            cons "data" vals
                          )
                           (
                            cons "rows" (
                              hash-table-ref a "rows"
                            )
                          )
                           (
                            cons "cols" (
                              hash-table-ref a "cols"
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
            ret55
          )
           (
            begin (
              if (
                or (
                  not (
                    equal? (
                      hash-table-ref a "rows"
                    )
                     (
                      hash-table-ref b "rows"
                    )
                  )
                )
                 (
                  not (
                    equal? (
                      hash-table-ref a "cols"
                    )
                     (
                      hash-table-ref b "cols"
                    )
                  )
                )
              )
               (
                begin (
                  ret55 (
                    alist->hash-table (
                      _list (
                        cons "data" (
                          _list
                        )
                      )
                       (
                        cons "rows" 0
                      )
                       (
                        cons "cols" 0
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
                  vals (
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
                                    < i (
                                      hash-table-ref a "rows"
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
                                                            < j (
                                                              hash-table-ref a "cols"
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
                                                                            list-ref (
                                                                              hash-table-ref a "data"
                                                                            )
                                                                             i
                                                                          )
                                                                        )
                                                                         (
                                                                          _substring (
                                                                            list-ref (
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
                                                                            list-ref (
                                                                              hash-table-ref a "data"
                                                                            )
                                                                             i
                                                                          )
                                                                        )
                                                                         (
                                                                          hash-table-ref (
                                                                            list-ref (
                                                                              hash-table-ref a "data"
                                                                            )
                                                                             i
                                                                          )
                                                                           j
                                                                        )
                                                                      )
                                                                       (
                                                                        else (
                                                                          list-ref (
                                                                            list-ref (
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
                                                                            list-ref (
                                                                              hash-table-ref b "data"
                                                                            )
                                                                             i
                                                                          )
                                                                        )
                                                                         (
                                                                          _substring (
                                                                            list-ref (
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
                                                                            list-ref (
                                                                              hash-table-ref b "data"
                                                                            )
                                                                             i
                                                                          )
                                                                        )
                                                                         (
                                                                          hash-table-ref (
                                                                            list-ref (
                                                                              hash-table-ref b "data"
                                                                            )
                                                                             i
                                                                          )
                                                                           j
                                                                        )
                                                                      )
                                                                       (
                                                                        else (
                                                                          list-ref (
                                                                            list-ref (
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
                                                              loop58
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
                                                    loop58
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              set! vals (
                                                append vals (
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
                      ret55 (
                        alist->hash-table (
                          _list (
                            cons "data" vals
                          )
                           (
                            cons "rows" (
                              hash-table-ref a "rows"
                            )
                          )
                           (
                            cons "cols" (
                              hash-table-ref a "cols"
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
        matrix_dot row col
      )
       (
        call/cc (
          lambda (
            ret60
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
                                    _len row
                                  )
                                )
                                 (
                                  begin (
                                    set! sum (
                                      _add sum (
                                        * (
                                          list-ref row i
                                        )
                                         (
                                          list-ref col i
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
                    ret60 sum
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
            ret63
          )
           (
            begin (
              if (
                not (
                  equal? (
                    hash-table-ref a "cols"
                  )
                   (
                    hash-table-ref b "rows"
                  )
                )
              )
               (
                begin (
                  ret63 (
                    alist->hash-table (
                      _list (
                        cons "data" (
                          _list
                        )
                      )
                       (
                        cons "rows" 0
                      )
                       (
                        cons "cols" 0
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
                  bcols (
                    matrix_columns b
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      vals (
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
                                          hash-table-ref a "rows"
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
                                                                  hash-table-ref b "cols"
                                                                )
                                                              )
                                                               (
                                                                begin (
                                                                  set! row (
                                                                    append row (
                                                                      _list (
                                                                        matrix_dot (
                                                                          list-ref (
                                                                            hash-table-ref a "data"
                                                                          )
                                                                           i
                                                                        )
                                                                         (
                                                                          cond (
                                                                            (
                                                                              string? bcols
                                                                            )
                                                                             (
                                                                              _substring bcols j (
                                                                                + j 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            (
                                                                              hash-table? bcols
                                                                            )
                                                                             (
                                                                              hash-table-ref bcols j
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref bcols j
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
                                                  set! vals (
                                                    append vals (
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
                          ret63 (
                            alist->hash-table (
                              _list (
                                cons "data" vals
                              )
                               (
                                cons "rows" (
                                  hash-table-ref a "rows"
                                )
                              )
                               (
                                cons "cols" (
                                  hash-table-ref b "cols"
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
        matrix_pow m p
      )
       (
        call/cc (
          lambda (
            ret68
          )
           (
            begin (
              if (
                equal? p 0
              )
               (
                begin (
                  ret68 (
                    matrix_identity m
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
                < p 0
              )
               (
                begin (
                  if (
                    matrix_is_invertible m
                  )
                   (
                    begin (
                      ret68 (
                        matrix_pow (
                          matrix_inverse m
                        )
                         (
                          - p
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
                  ret68 (
                    alist->hash-table (
                      _list (
                        cons "data" (
                          _list
                        )
                      )
                       (
                        cons "rows" 0
                      )
                       (
                        cons "cols" 0
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
                  result m
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
                                    < i p
                                  )
                                   (
                                    begin (
                                      set! result (
                                        matrix_mul result m
                                      )
                                    )
                                     (
                                      set! i (
                                        + i 1
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
                      ret68 result
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
            ret71
          )
           (
            begin (
              if (
                equal? (
                  hash-table-ref m "rows"
                )
                 0
              )
               (
                begin (
                  ret71 "[]"
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
                                    < i (
                                      hash-table-ref m "rows"
                                    )
                                  )
                                   (
                                    begin (
                                      set! s (
                                        string-append s "["
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
                                              break75
                                            )
                                             (
                                              letrec (
                                                (
                                                  loop74 (
                                                    lambda (
                                                      
                                                    )
                                                     (
                                                      if (
                                                        < j (
                                                          hash-table-ref m "cols"
                                                        )
                                                      )
                                                       (
                                                        begin (
                                                          set! s (
                                                            string-append s (
                                                              to-str-space (
                                                                cond (
                                                                  (
                                                                    string? (
                                                                      list-ref (
                                                                        hash-table-ref m "data"
                                                                      )
                                                                       i
                                                                    )
                                                                  )
                                                                   (
                                                                    _substring (
                                                                      list-ref (
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
                                                                      list-ref (
                                                                        hash-table-ref m "data"
                                                                      )
                                                                       i
                                                                    )
                                                                  )
                                                                   (
                                                                    hash-table-ref (
                                                                      list-ref (
                                                                        hash-table-ref m "data"
                                                                      )
                                                                       i
                                                                    )
                                                                     j
                                                                  )
                                                                )
                                                                 (
                                                                  else (
                                                                    list-ref (
                                                                      list-ref (
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
                                                                hash-table-ref m "cols"
                                                              )
                                                               1
                                                            )
                                                          )
                                                           (
                                                            begin (
                                                              set! s (
                                                                string-append s " "
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
                                                          loop74
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
                                                loop74
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
                                          if (
                                            < i (
                                              - (
                                                hash-table-ref m "rows"
                                              )
                                               1
                                            )
                                          )
                                           (
                                            begin (
                                              set! s (
                                                string-append s "\n "
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
                                      )
                                    )
                                     (
                                      loop72
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
                            loop72
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
                      ret71 s
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
            ret76
          )
           (
            let (
              (
                m (
                  make_matrix (
                    _list (
                      _list 1.0 2.0 3.0
                    )
                     (
                      _list 4.0 5.0 6.0
                    )
                     (
                      _list 7.0 8.0 9.0
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
                      matrix_to_string m
                    )
                  )
                   (
                    matrix_to_string m
                  )
                   (
                    to-str (
                      matrix_to_string m
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
                        matrix_columns m
                      )
                    )
                  )
                   (
                    to-str-space (
                      matrix_columns m
                    )
                  )
                   (
                    to-str (
                      to-str-space (
                        matrix_columns m
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
                      string-append (
                        string-append (
                          to-str-space (
                            hash-table-ref m "rows"
                          )
                        )
                         ","
                      )
                       (
                        to-str-space (
                          hash-table-ref m "cols"
                        )
                      )
                    )
                  )
                   (
                    string-append (
                      string-append (
                        to-str-space (
                          hash-table-ref m "rows"
                        )
                      )
                       ","
                    )
                     (
                      to-str-space (
                        hash-table-ref m "cols"
                      )
                    )
                  )
                   (
                    to-str (
                      string-append (
                        string-append (
                          to-str-space (
                            hash-table-ref m "rows"
                          )
                        )
                         ","
                      )
                       (
                        to-str-space (
                          hash-table-ref m "cols"
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
                        matrix_is_invertible m
                      )
                    )
                  )
                   (
                    to-str-space (
                      matrix_is_invertible m
                    )
                  )
                   (
                    to-str (
                      to-str-space (
                        matrix_is_invertible m
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
                        matrix_identity m
                      )
                    )
                  )
                   (
                    matrix_to_string (
                      matrix_identity m
                    )
                  )
                   (
                    to-str (
                      matrix_to_string (
                        matrix_identity m
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
               (
                _display (
                  if (
                    string? (
                      matrix_to_string (
                        matrix_minors m
                      )
                    )
                  )
                   (
                    matrix_to_string (
                      matrix_minors m
                    )
                  )
                   (
                    to-str (
                      matrix_to_string (
                        matrix_minors m
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
                        matrix_cofactors m
                      )
                    )
                  )
                   (
                    matrix_to_string (
                      matrix_cofactors m
                    )
                  )
                   (
                    to-str (
                      matrix_to_string (
                        matrix_cofactors m
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
                        matrix_adjugate m
                      )
                    )
                  )
                   (
                    matrix_to_string (
                      matrix_adjugate m
                    )
                  )
                   (
                    to-str (
                      matrix_to_string (
                        matrix_adjugate m
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
                    m2 (
                      matrix_mul_scalar m 3.0
                    )
                  )
                )
                 (
                  begin (
                    _display (
                      if (
                        string? (
                          matrix_to_string m2
                        )
                      )
                       (
                        matrix_to_string m2
                      )
                       (
                        to-str (
                          matrix_to_string m2
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
                            matrix_add m m2
                          )
                        )
                      )
                       (
                        matrix_to_string (
                          matrix_add m m2
                        )
                      )
                       (
                        to-str (
                          matrix_to_string (
                            matrix_add m m2
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
                            matrix_sub m m2
                          )
                        )
                      )
                       (
                        matrix_to_string (
                          matrix_sub m m2
                        )
                      )
                       (
                        to-str (
                          matrix_to_string (
                            matrix_sub m m2
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
                            matrix_pow m 3
                          )
                        )
                      )
                       (
                        matrix_to_string (
                          matrix_pow m 3
                        )
                      )
                       (
                        to-str (
                          matrix_to_string (
                            matrix_pow m 3
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
                        m3 (
                          matrix_add_row m (
                            _list 10.0 11.0 12.0
                          )
                        )
                      )
                    )
                     (
                      begin (
                        _display (
                          if (
                            string? (
                              matrix_to_string m3
                            )
                          )
                           (
                            matrix_to_string m3
                          )
                           (
                            to-str (
                              matrix_to_string m3
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
                            m4 (
                              matrix_add_column m2 (
                                _list 8.0 16.0 32.0
                              )
                            )
                          )
                        )
                         (
                          begin (
                            _display (
                              if (
                                string? (
                                  matrix_to_string (
                                    matrix_mul m3 m4
                                  )
                                )
                              )
                               (
                                matrix_to_string (
                                  matrix_mul m3 m4
                                )
                              )
                               (
                                to-str (
                                  matrix_to_string (
                                    matrix_mul m3 m4
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
      )
    )
     (
      main
    )
     (
      let (
        (
          end78 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur79 (
              quotient (
                * (
                  - end78 start77
                )
                 1000000
              )
               jps80
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur79
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
