;; Generated on 2025-08-07 08:20 +0700
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
        make_matrix rows cols value
      )
       (
        call/cc (
          lambda (
            ret1
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
                                  < i rows
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
                                                          < j cols
                                                        )
                                                         (
                                                          begin (
                                                            set! row (
                                                              append row (
                                                                _list value
                                                              )
                                                            )
                                                          )
                                                           (
                                                            set! j (
                                                              + j 1
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
                                            set! result (
                                              append result (
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
                    ret1 result
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
        my_laplacian src ksize
      )
       (
        call/cc (
          lambda (
            ret6
          )
           (
            let (
              (
                kernel (
                  _list
                )
              )
            )
             (
              begin (
                if (
                  equal? ksize 1
                )
                 (
                  begin (
                    set! kernel (
                      _list (
                        _list 0 (
                          - 1
                        )
                         0
                      )
                       (
                        _list (
                          - 1
                        )
                         4 (
                          - 1
                        )
                      )
                       (
                        _list 0 (
                          - 1
                        )
                         0
                      )
                    )
                  )
                )
                 (
                  if (
                    equal? ksize 3
                  )
                   (
                    begin (
                      set! kernel (
                        _list (
                          _list 0 1 0
                        )
                         (
                          _list 1 (
                            - 4
                          )
                           1
                        )
                         (
                          _list 0 1 0
                        )
                      )
                    )
                  )
                   (
                    if (
                      equal? ksize 5
                    )
                     (
                      begin (
                        set! kernel (
                          _list (
                            _list 0 0 (
                              - 1
                            )
                             0 0
                          )
                           (
                            _list 0 (
                              - 1
                            )
                             (
                              - 2
                            )
                             (
                              - 1
                            )
                             0
                          )
                           (
                            _list (
                              - 1
                            )
                             (
                              - 2
                            )
                             16 (
                              - 2
                            )
                             (
                              - 1
                            )
                          )
                           (
                            _list 0 (
                              - 1
                            )
                             (
                              - 2
                            )
                             (
                              - 1
                            )
                             0
                          )
                           (
                            _list 0 0 (
                              - 1
                            )
                             0 0
                          )
                        )
                      )
                    )
                     (
                      if (
                        equal? ksize 7
                      )
                       (
                        begin (
                          set! kernel (
                            _list (
                              _list 0 0 0 (
                                - 1
                              )
                               0 0 0
                            )
                             (
                              _list 0 0 (
                                - 2
                              )
                               (
                                - 3
                              )
                               (
                                - 2
                              )
                               0 0
                            )
                             (
                              _list 0 (
                                - 2
                              )
                               (
                                - 7
                              )
                               (
                                - 10
                              )
                               (
                                - 7
                              )
                               (
                                - 2
                              )
                               0
                            )
                             (
                              _list (
                                - 1
                              )
                               (
                                - 3
                              )
                               (
                                - 10
                              )
                               68 (
                                - 10
                              )
                               (
                                - 3
                              )
                               (
                                - 1
                              )
                            )
                             (
                              _list 0 (
                                - 2
                              )
                               (
                                - 7
                              )
                               (
                                - 10
                              )
                               (
                                - 7
                              )
                               (
                                - 2
                              )
                               0
                            )
                             (
                              _list 0 0 (
                                - 2
                              )
                               (
                                - 3
                              )
                               (
                                - 2
                              )
                               0 0
                            )
                             (
                              _list 0 0 0 (
                                - 1
                              )
                               0 0 0
                            )
                          )
                        )
                      )
                       (
                        begin (
                          panic "ksize must be in (1, 3, 5, 7)"
                        )
                      )
                    )
                  )
                )
              )
               (
                let (
                  (
                    rows (
                      _len src
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        cols (
                          _len (
                            list-ref src 0
                          )
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            k (
                              _len kernel
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                pad (
                                  _div k 2
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    output (
                                      make_matrix rows cols 0
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
                                                                          < j cols
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
                                                                                    ki 0
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
                                                                                                  < ki k
                                                                                                )
                                                                                                 (
                                                                                                  begin (
                                                                                                    let (
                                                                                                      (
                                                                                                        kj 0
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
                                                                                                                      < kj k
                                                                                                                    )
                                                                                                                     (
                                                                                                                      begin (
                                                                                                                        let (
                                                                                                                          (
                                                                                                                            ii (
                                                                                                                              - (
                                                                                                                                + i ki
                                                                                                                              )
                                                                                                                               pad
                                                                                                                            )
                                                                                                                          )
                                                                                                                        )
                                                                                                                         (
                                                                                                                          begin (
                                                                                                                            let (
                                                                                                                              (
                                                                                                                                jj (
                                                                                                                                  - (
                                                                                                                                    + j kj
                                                                                                                                  )
                                                                                                                                   pad
                                                                                                                                )
                                                                                                                              )
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
                                                                                                                                    if (
                                                                                                                                      and (
                                                                                                                                        and (
                                                                                                                                          and (
                                                                                                                                            >= ii 0
                                                                                                                                          )
                                                                                                                                           (
                                                                                                                                            < ii rows
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                         (
                                                                                                                                          >= jj 0
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                       (
                                                                                                                                        < jj cols
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      begin (
                                                                                                                                        set! val (
                                                                                                                                          cond (
                                                                                                                                            (
                                                                                                                                              string? (
                                                                                                                                                list-ref src ii
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                             (
                                                                                                                                              _substring (
                                                                                                                                                list-ref src ii
                                                                                                                                              )
                                                                                                                                               jj (
                                                                                                                                                + jj 1
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                           (
                                                                                                                                            (
                                                                                                                                              hash-table? (
                                                                                                                                                list-ref src ii
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                             (
                                                                                                                                              hash-table-ref (
                                                                                                                                                list-ref src ii
                                                                                                                                              )
                                                                                                                                               jj
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                           (
                                                                                                                                            else (
                                                                                                                                              list-ref (
                                                                                                                                                list-ref src ii
                                                                                                                                              )
                                                                                                                                               jj
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
                                                                                                                                    set! sum (
                                                                                                                                      + sum (
                                                                                                                                        * val (
                                                                                                                                          cond (
                                                                                                                                            (
                                                                                                                                              string? (
                                                                                                                                                list-ref kernel ki
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                             (
                                                                                                                                              _substring (
                                                                                                                                                list-ref kernel ki
                                                                                                                                              )
                                                                                                                                               kj (
                                                                                                                                                + kj 1
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                           (
                                                                                                                                            (
                                                                                                                                              hash-table? (
                                                                                                                                                list-ref kernel ki
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                             (
                                                                                                                                              hash-table-ref (
                                                                                                                                                list-ref kernel ki
                                                                                                                                              )
                                                                                                                                               kj
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                           (
                                                                                                                                            else (
                                                                                                                                              list-ref (
                                                                                                                                                list-ref kernel ki
                                                                                                                                              )
                                                                                                                                               kj
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    set! kj (
                                                                                                                                      + kj 1
                                                                                                                                    )
                                                                                                                                  )
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
                                                                                                        set! ki (
                                                                                                          + ki 1
                                                                                                        )
                                                                                                      )
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
                                                                                    list-set! (
                                                                                      list-ref output i
                                                                                    )
                                                                                     j sum
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
                                                            set! i (
                                                              + i 1
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
                                        ret6 output
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
      let (
        (
          image (
            _list (
              _list 0 0 0 0 0
            )
             (
              _list 0 10 10 10 0
            )
             (
              _list 0 10 10 10 0
            )
             (
              _list 0 10 10 10 0
            )
             (
              _list 0 0 0 0 0
            )
          )
        )
      )
       (
        begin (
          let (
            (
              result (
                my_laplacian image 3
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
                                < r (
                                  _len result
                                )
                              )
                               (
                                begin (
                                  let (
                                    (
                                      row_str "["
                                    )
                                  )
                                   (
                                    begin (
                                      let (
                                        (
                                          c 0
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
                                                        < c (
                                                          _len (
                                                            list-ref result r
                                                          )
                                                        )
                                                      )
                                                       (
                                                        begin (
                                                          set! row_str (
                                                            string-append row_str (
                                                              to-str-space (
                                                                cond (
                                                                  (
                                                                    string? (
                                                                      list-ref result r
                                                                    )
                                                                  )
                                                                   (
                                                                    _substring (
                                                                      list-ref result r
                                                                    )
                                                                     c (
                                                                      + c 1
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  (
                                                                    hash-table? (
                                                                      list-ref result r
                                                                    )
                                                                  )
                                                                   (
                                                                    hash-table-ref (
                                                                      list-ref result r
                                                                    )
                                                                     c
                                                                  )
                                                                )
                                                                 (
                                                                  else (
                                                                    list-ref (
                                                                      list-ref result r
                                                                    )
                                                                     c
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                         (
                                                          if (
                                                            < (
                                                              + c 1
                                                            )
                                                             (
                                                              _len (
                                                                list-ref result r
                                                              )
                                                            )
                                                          )
                                                           (
                                                            begin (
                                                              set! row_str (
                                                                string-append row_str ", "
                                                              )
                                                            )
                                                          )
                                                           (
                                                            quote (
                                                              
                                                            )
                                                          )
                                                        )
                                                         (
                                                          set! c (
                                                            + c 1
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
                                          set! row_str (
                                            string-append row_str "]"
                                          )
                                        )
                                         (
                                          _display (
                                            if (
                                              string? row_str
                                            )
                                             row_str (
                                              to-str row_str
                                            )
                                          )
                                        )
                                         (
                                          newline
                                        )
                                         (
                                          set! r (
                                            + r 1
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
              )
            )
          )
        )
      )
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
