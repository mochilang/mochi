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
      start41 (
        current-jiffy
      )
    )
     (
      jps44 (
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
                arr (
                  _list
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
                                  < r rows
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
                                            c 0
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
                                                          < c cols
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
                                                            set! c (
                                                              + c 1
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
                                            set! arr (
                                              append arr (
                                                _list row
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
                    ret1 (
                      alist->hash-table (
                        _list (
                          cons "data" arr
                        )
                         (
                          cons "rows" rows
                        )
                         (
                          cons "cols" cols
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
        matrix_from_lists vals
      )
       (
        call/cc (
          lambda (
            ret6
          )
           (
            let (
              (
                r (
                  _len vals
                )
              )
            )
             (
              begin (
                let (
                  (
                    c (
                      if (
                        equal? r 0
                      )
                       0 (
                        _len (
                          list-ref-safe vals 0
                        )
                      )
                    )
                  )
                )
                 (
                  begin (
                    ret6 (
                      alist->hash-table (
                        _list (
                          cons "data" vals
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
     (
      define (
        matrix_to_string m
      )
       (
        call/cc (
          lambda (
            ret7
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
                                                              hash-table-ref m "cols"
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
                                                         '(
                                                          
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
                                                     '(
                                                      
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
                                              string-append s "\n"
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
        matrix_add a b
      )
       (
        call/cc (
          lambda (
            ret12
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
                  ret12 (
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
               '(
                
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
                      ret12 (
                        alist->hash-table (
                          _list (
                            cons "data" res
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
            ret17
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
                  ret17 (
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
               '(
                
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
                      ret17 (
                        alist->hash-table (
                          _list (
                            cons "data" res
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
        matrix_mul_scalar m k
      )
       (
        call/cc (
          lambda (
            ret22
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
                                                                   k
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
                                    loop23
                                  )
                                )
                                 '(
                                  
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
                    ret22 (
                      alist->hash-table (
                        _list (
                          cons "data" res
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
        matrix_mul a b
      )
       (
        call/cc (
          lambda (
            ret27
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
                  ret27 (
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
               '(
                
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
                                                              hash-table-ref b "cols"
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
                                                                      k 0
                                                                    )
                                                                  )
                                                                   (
                                                                    begin (
                                                                      call/cc (
                                                                        lambda (
                                                                          break33
                                                                        )
                                                                         (
                                                                          letrec (
                                                                            (
                                                                              loop32 (
                                                                                lambda (
                                                                                  
                                                                                )
                                                                                 (
                                                                                  if (
                                                                                    < k (
                                                                                      hash-table-ref a "cols"
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
                                                                                                 k (
                                                                                                  + k 1
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
                                                                                                 k
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
                                                                                                 k
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
                                                                                                   k
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                _substring (
                                                                                                  list-ref-safe (
                                                                                                    hash-table-ref b "data"
                                                                                                  )
                                                                                                   k
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
                                                                                                   k
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                hash-table-ref (
                                                                                                  list-ref-safe (
                                                                                                    hash-table-ref b "data"
                                                                                                  )
                                                                                                   k
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
                                                                                                   k
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
                                                                                      loop32
                                                                                    )
                                                                                  )
                                                                                   '(
                                                                                    
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            loop32
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      set! row (
                                                                        append row (
                                                                          _list sum
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
                      ret27 (
                        alist->hash-table (
                          _list (
                            cons "data" res
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
     (
      define (
        matrix_transpose m
      )
       (
        call/cc (
          lambda (
            ret34
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
                    c 0
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
                                  < c (
                                    hash-table-ref m "cols"
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
                                            r 0
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
                                                          < r (
                                                            hash-table-ref m "rows"
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
                                                                         r
                                                                      )
                                                                    )
                                                                     (
                                                                      _substring (
                                                                        list-ref-safe (
                                                                          hash-table-ref m "data"
                                                                        )
                                                                         r
                                                                      )
                                                                       c (
                                                                        + c 1
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    (
                                                                      hash-table? (
                                                                        list-ref-safe (
                                                                          hash-table-ref m "data"
                                                                        )
                                                                         r
                                                                      )
                                                                    )
                                                                     (
                                                                      hash-table-ref (
                                                                        list-ref-safe (
                                                                          hash-table-ref m "data"
                                                                        )
                                                                         r
                                                                      )
                                                                       c
                                                                    )
                                                                  )
                                                                   (
                                                                    else (
                                                                      list-ref-safe (
                                                                        list-ref-safe (
                                                                          hash-table-ref m "data"
                                                                        )
                                                                         r
                                                                      )
                                                                       c
                                                                    )
                                                                  )
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
                                            set! res (
                                              append res (
                                                _list row
                                              )
                                            )
                                          )
                                           (
                                            set! c (
                                              + c 1
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
                    ret34 (
                      alist->hash-table (
                        _list (
                          cons "data" res
                        )
                         (
                          cons "rows" (
                            hash-table-ref m "cols"
                          )
                        )
                         (
                          cons "cols" (
                            hash-table-ref m "rows"
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
        sherman_morrison ainv u v
      )
       (
        call/cc (
          lambda (
            ret39
          )
           (
            let (
              (
                vt (
                  matrix_transpose v
                )
              )
            )
             (
              begin (
                let (
                  (
                    vu (
                      matrix_mul (
                        matrix_mul vt ainv
                      )
                       u
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        factor (
                          _add (
                            cond (
                              (
                                string? (
                                  cond (
                                    (
                                      string? (
                                        hash-table-ref vu "data"
                                      )
                                    )
                                     (
                                      _substring (
                                        hash-table-ref vu "data"
                                      )
                                       0 (
                                        + 0 1
                                      )
                                    )
                                  )
                                   (
                                    (
                                      hash-table? (
                                        hash-table-ref vu "data"
                                      )
                                    )
                                     (
                                      hash-table-ref (
                                        hash-table-ref vu "data"
                                      )
                                       0
                                    )
                                  )
                                   (
                                    else (
                                      list-ref-safe (
                                        hash-table-ref vu "data"
                                      )
                                       0
                                    )
                                  )
                                )
                              )
                               (
                                _substring (
                                  cond (
                                    (
                                      string? (
                                        hash-table-ref vu "data"
                                      )
                                    )
                                     (
                                      _substring (
                                        hash-table-ref vu "data"
                                      )
                                       0 (
                                        + 0 1
                                      )
                                    )
                                  )
                                   (
                                    (
                                      hash-table? (
                                        hash-table-ref vu "data"
                                      )
                                    )
                                     (
                                      hash-table-ref (
                                        hash-table-ref vu "data"
                                      )
                                       0
                                    )
                                  )
                                   (
                                    else (
                                      list-ref-safe (
                                        hash-table-ref vu "data"
                                      )
                                       0
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
                                      string? (
                                        hash-table-ref vu "data"
                                      )
                                    )
                                     (
                                      _substring (
                                        hash-table-ref vu "data"
                                      )
                                       0 (
                                        + 0 1
                                      )
                                    )
                                  )
                                   (
                                    (
                                      hash-table? (
                                        hash-table-ref vu "data"
                                      )
                                    )
                                     (
                                      hash-table-ref (
                                        hash-table-ref vu "data"
                                      )
                                       0
                                    )
                                  )
                                   (
                                    else (
                                      list-ref-safe (
                                        hash-table-ref vu "data"
                                      )
                                       0
                                    )
                                  )
                                )
                              )
                               (
                                hash-table-ref (
                                  cond (
                                    (
                                      string? (
                                        hash-table-ref vu "data"
                                      )
                                    )
                                     (
                                      _substring (
                                        hash-table-ref vu "data"
                                      )
                                       0 (
                                        + 0 1
                                      )
                                    )
                                  )
                                   (
                                    (
                                      hash-table? (
                                        hash-table-ref vu "data"
                                      )
                                    )
                                     (
                                      hash-table-ref (
                                        hash-table-ref vu "data"
                                      )
                                       0
                                    )
                                  )
                                   (
                                    else (
                                      list-ref-safe (
                                        hash-table-ref vu "data"
                                      )
                                       0
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
                                      string? (
                                        hash-table-ref vu "data"
                                      )
                                    )
                                     (
                                      _substring (
                                        hash-table-ref vu "data"
                                      )
                                       0 (
                                        + 0 1
                                      )
                                    )
                                  )
                                   (
                                    (
                                      hash-table? (
                                        hash-table-ref vu "data"
                                      )
                                    )
                                     (
                                      hash-table-ref (
                                        hash-table-ref vu "data"
                                      )
                                       0
                                    )
                                  )
                                   (
                                    else (
                                      list-ref-safe (
                                        hash-table-ref vu "data"
                                      )
                                       0
                                    )
                                  )
                                )
                                 0
                              )
                            )
                          )
                           1.0
                        )
                      )
                    )
                     (
                      begin (
                        if (
                          equal? factor 0.0
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
                         '(
                          
                        )
                      )
                       (
                        let (
                          (
                            term1 (
                              matrix_mul ainv u
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                term2 (
                                  matrix_mul vt ainv
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    numerator (
                                      matrix_mul term1 term2
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        scaled (
                                          matrix_mul_scalar numerator (
                                            _div 1.0 factor
                                          )
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        ret39 (
                                          matrix_sub ainv scaled
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
        main
      )
       (
        call/cc (
          lambda (
            ret40
          )
           (
            let (
              (
                ainv (
                  matrix_from_lists (
                    _list (
                      _list 1.0 0.0 0.0
                    )
                     (
                      _list 0.0 1.0 0.0
                    )
                     (
                      _list 0.0 0.0 1.0
                    )
                  )
                )
              )
            )
             (
              begin (
                let (
                  (
                    u (
                      matrix_from_lists (
                        _list (
                          _list 1.0
                        )
                         (
                          _list 2.0
                        )
                         (
                          _list (
                            - 3.0
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
                          matrix_from_lists (
                            _list (
                              _list 4.0
                            )
                             (
                              _list (
                                - 2.0
                              )
                            )
                             (
                              _list 5.0
                            )
                          )
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            result (
                              sherman_morrison ainv u v
                            )
                          )
                        )
                         (
                          begin (
                            _display (
                              if (
                                string? (
                                  matrix_to_string result
                                )
                              )
                               (
                                matrix_to_string result
                              )
                               (
                                to-str (
                                  matrix_to_string result
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
          end42 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur43 (
              quotient (
                * (
                  - end42 start41
                )
                 1000000
              )
               jps44
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur43
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
