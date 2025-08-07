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
      start17 (
        current-jiffy
      )
    )
     (
      jps20 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        min3 a b c
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                m a
              )
            )
             (
              begin (
                if (
                  < b m
                )
                 (
                  begin (
                    set! m b
                  )
                )
                 (
                  quote (
                    
                  )
                )
              )
               (
                if (
                  < c m
                )
                 (
                  begin (
                    set! m c
                  )
                )
                 (
                  quote (
                    
                  )
                )
              )
               (
                ret1 m
              )
            )
          )
        )
      )
    )
     (
      define (
        helper_top_down word1 word2 dp i j
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            begin (
              if (
                < i 0
              )
               (
                begin (
                  ret2 (
                    + j 1
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
                < j 0
              )
               (
                begin (
                  ret2 (
                    + i 1
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
                not (
                  equal? (
                    cond (
                      (
                        string? (
                          list-ref dp i
                        )
                      )
                       (
                        _substring (
                          list-ref dp i
                        )
                         j (
                          + j 1
                        )
                      )
                    )
                     (
                      (
                        hash-table? (
                          list-ref dp i
                        )
                      )
                       (
                        hash-table-ref (
                          list-ref dp i
                        )
                         j
                      )
                    )
                     (
                      else (
                        list-ref (
                          list-ref dp i
                        )
                         j
                      )
                    )
                  )
                   (
                    - 0 1
                  )
                )
              )
               (
                begin (
                  ret2 (
                    cond (
                      (
                        string? (
                          list-ref dp i
                        )
                      )
                       (
                        _substring (
                          list-ref dp i
                        )
                         j (
                          + j 1
                        )
                      )
                    )
                     (
                      (
                        hash-table? (
                          list-ref dp i
                        )
                      )
                       (
                        hash-table-ref (
                          list-ref dp i
                        )
                         j
                      )
                    )
                     (
                      else (
                        list-ref (
                          list-ref dp i
                        )
                         j
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
                string=? (
                  _substring word1 i (
                    + i 1
                  )
                )
                 (
                  _substring word2 j (
                    + j 1
                  )
                )
              )
               (
                begin (
                  list-set! (
                    list-ref dp i
                  )
                   j (
                    helper_top_down word1 word2 dp (
                      - i 1
                    )
                     (
                      - j 1
                    )
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      insert (
                        helper_top_down word1 word2 dp i (
                          - j 1
                        )
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          delete (
                            helper_top_down word1 word2 dp (
                              - i 1
                            )
                             j
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              replace (
                                helper_top_down word1 word2 dp (
                                  - i 1
                                )
                                 (
                                  - j 1
                                )
                              )
                            )
                          )
                           (
                            begin (
                              list-set! (
                                list-ref dp i
                              )
                               j (
                                _add 1 (
                                  min3 insert delete replace
                                )
                              )
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
              ret2 (
                cond (
                  (
                    string? (
                      list-ref dp i
                    )
                  )
                   (
                    _substring (
                      list-ref dp i
                    )
                     j (
                      + j 1
                    )
                  )
                )
                 (
                  (
                    hash-table? (
                      list-ref dp i
                    )
                  )
                   (
                    hash-table-ref (
                      list-ref dp i
                    )
                     j
                  )
                )
                 (
                  else (
                    list-ref (
                      list-ref dp i
                    )
                     j
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
        min_dist_top_down word1 word2
      )
       (
        call/cc (
          lambda (
            ret3
          )
           (
            let (
              (
                m (
                  _len word1
                )
              )
            )
             (
              begin (
                let (
                  (
                    n (
                      _len word2
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        dp (
                          _list
                        )
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
                                    _
                                  )
                                   (
                                    if (
                                      < _ m
                                    )
                                     (
                                      begin (
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
                                              call/cc (
                                                lambda (
                                                  break7
                                                )
                                                 (
                                                  letrec (
                                                    (
                                                      loop6 (
                                                        lambda (
                                                          _2
                                                        )
                                                         (
                                                          if (
                                                            < _2 n
                                                          )
                                                           (
                                                            begin (
                                                              begin (
                                                                set! row (
                                                                  append row (
                                                                    _list (
                                                                      - 0 1
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             (
                                                              loop6 (
                                                                + _2 1
                                                              )
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
                                                    loop6 0
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              set! dp (
                                                append dp (
                                                  _list row
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                       (
                                        loop4 (
                                          + _ 1
                                        )
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
                              loop4 0
                            )
                          )
                        )
                      )
                       (
                        ret3 (
                          helper_top_down word1 word2 dp (
                            - m 1
                          )
                           (
                            - n 1
                          )
                        )
                      )
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
        min_dist_bottom_up word1 word2
      )
       (
        call/cc (
          lambda (
            ret8
          )
           (
            let (
              (
                m (
                  _len word1
                )
              )
            )
             (
              begin (
                let (
                  (
                    n (
                      _len word2
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        dp (
                          _list
                        )
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
                                    _
                                  )
                                   (
                                    if (
                                      < _ (
                                        + m 1
                                      )
                                    )
                                     (
                                      begin (
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
                                              call/cc (
                                                lambda (
                                                  break12
                                                )
                                                 (
                                                  letrec (
                                                    (
                                                      loop11 (
                                                        lambda (
                                                          _2
                                                        )
                                                         (
                                                          if (
                                                            < _2 (
                                                              + n 1
                                                            )
                                                          )
                                                           (
                                                            begin (
                                                              begin (
                                                                set! row (
                                                                  append row (
                                                                    _list 0
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             (
                                                              loop11 (
                                                                + _2 1
                                                              )
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
                                                    loop11 0
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              set! dp (
                                                append dp (
                                                  _list row
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                       (
                                        loop9 (
                                          + _ 1
                                        )
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
                              loop9 0
                            )
                          )
                        )
                      )
                       (
                        call/cc (
                          lambda (
                            break14
                          )
                           (
                            letrec (
                              (
                                loop13 (
                                  lambda (
                                    i
                                  )
                                   (
                                    if (
                                      < i (
                                        + m 1
                                      )
                                    )
                                     (
                                      begin (
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
                                                      j
                                                    )
                                                     (
                                                      if (
                                                        < j (
                                                          + n 1
                                                        )
                                                      )
                                                       (
                                                        begin (
                                                          begin (
                                                            if (
                                                              equal? i 0
                                                            )
                                                             (
                                                              begin (
                                                                list-set! (
                                                                  list-ref dp i
                                                                )
                                                                 j j
                                                              )
                                                            )
                                                             (
                                                              if (
                                                                equal? j 0
                                                              )
                                                               (
                                                                begin (
                                                                  list-set! (
                                                                    list-ref dp i
                                                                  )
                                                                   j i
                                                                )
                                                              )
                                                               (
                                                                if (
                                                                  string=? (
                                                                    _substring word1 (
                                                                      - i 1
                                                                    )
                                                                     i
                                                                  )
                                                                   (
                                                                    _substring word2 (
                                                                      - j 1
                                                                    )
                                                                     j
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    list-set! (
                                                                      list-ref dp i
                                                                    )
                                                                     j (
                                                                      cond (
                                                                        (
                                                                          string? (
                                                                            list-ref dp (
                                                                              - i 1
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          _substring (
                                                                            list-ref dp (
                                                                              - i 1
                                                                            )
                                                                          )
                                                                           (
                                                                            - j 1
                                                                          )
                                                                           (
                                                                            + (
                                                                              - j 1
                                                                            )
                                                                             1
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        (
                                                                          hash-table? (
                                                                            list-ref dp (
                                                                              - i 1
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          hash-table-ref (
                                                                            list-ref dp (
                                                                              - i 1
                                                                            )
                                                                          )
                                                                           (
                                                                            - j 1
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        else (
                                                                          list-ref (
                                                                            list-ref dp (
                                                                              - i 1
                                                                            )
                                                                          )
                                                                           (
                                                                            - j 1
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
                                                                        insert (
                                                                          cond (
                                                                            (
                                                                              string? (
                                                                                list-ref dp i
                                                                              )
                                                                            )
                                                                             (
                                                                              _substring (
                                                                                list-ref dp i
                                                                              )
                                                                               (
                                                                                - j 1
                                                                              )
                                                                               (
                                                                                + (
                                                                                  - j 1
                                                                                )
                                                                                 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            (
                                                                              hash-table? (
                                                                                list-ref dp i
                                                                              )
                                                                            )
                                                                             (
                                                                              hash-table-ref (
                                                                                list-ref dp i
                                                                              )
                                                                               (
                                                                                - j 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref (
                                                                                list-ref dp i
                                                                              )
                                                                               (
                                                                                - j 1
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
                                                                            delete (
                                                                              cond (
                                                                                (
                                                                                  string? (
                                                                                    list-ref dp (
                                                                                      - i 1
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  _substring (
                                                                                    list-ref dp (
                                                                                      - i 1
                                                                                    )
                                                                                  )
                                                                                   j (
                                                                                    + j 1
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                (
                                                                                  hash-table? (
                                                                                    list-ref dp (
                                                                                      - i 1
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  hash-table-ref (
                                                                                    list-ref dp (
                                                                                      - i 1
                                                                                    )
                                                                                  )
                                                                                   j
                                                                                )
                                                                              )
                                                                               (
                                                                                else (
                                                                                  list-ref (
                                                                                    list-ref dp (
                                                                                      - i 1
                                                                                    )
                                                                                  )
                                                                                   j
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                replace (
                                                                                  cond (
                                                                                    (
                                                                                      string? (
                                                                                        list-ref dp (
                                                                                          - i 1
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref dp (
                                                                                          - i 1
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        - j 1
                                                                                      )
                                                                                       (
                                                                                        + (
                                                                                          - j 1
                                                                                        )
                                                                                         1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref dp (
                                                                                          - i 1
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref dp (
                                                                                          - i 1
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        - j 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref dp (
                                                                                          - i 1
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        - j 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                list-set! (
                                                                                  list-ref dp i
                                                                                )
                                                                                 j (
                                                                                  _add 1 (
                                                                                    min3 insert delete replace
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                          )
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
                                                          loop15 (
                                                            + j 1
                                                          )
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
                                                loop15 0
                                              )
                                            )
                                          )
                                        )
                                      )
                                       (
                                        loop13 (
                                          + i 1
                                        )
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
                              loop13 0
                            )
                          )
                        )
                      )
                       (
                        ret8 (
                          cond (
                            (
                              string? (
                                list-ref dp m
                              )
                            )
                             (
                              _substring (
                                list-ref dp m
                              )
                               n (
                                + n 1
                              )
                            )
                          )
                           (
                            (
                              hash-table? (
                                list-ref dp m
                              )
                            )
                             (
                              hash-table-ref (
                                list-ref dp m
                              )
                               n
                            )
                          )
                           (
                            else (
                              list-ref (
                                list-ref dp m
                              )
                               n
                            )
                          )
                        )
                      )
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
              min_dist_top_down "intention" "execution"
            )
          )
        )
         (
          to-str-space (
            min_dist_top_down "intention" "execution"
          )
        )
         (
          to-str (
            to-str-space (
              min_dist_top_down "intention" "execution"
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
              min_dist_top_down "intention" ""
            )
          )
        )
         (
          to-str-space (
            min_dist_top_down "intention" ""
          )
        )
         (
          to-str (
            to-str-space (
              min_dist_top_down "intention" ""
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
              min_dist_top_down "" ""
            )
          )
        )
         (
          to-str-space (
            min_dist_top_down "" ""
          )
        )
         (
          to-str (
            to-str-space (
              min_dist_top_down "" ""
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
              min_dist_bottom_up "intention" "execution"
            )
          )
        )
         (
          to-str-space (
            min_dist_bottom_up "intention" "execution"
          )
        )
         (
          to-str (
            to-str-space (
              min_dist_bottom_up "intention" "execution"
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
              min_dist_bottom_up "intention" ""
            )
          )
        )
         (
          to-str-space (
            min_dist_bottom_up "intention" ""
          )
        )
         (
          to-str (
            to-str-space (
              min_dist_bottom_up "intention" ""
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
              min_dist_bottom_up "" ""
            )
          )
        )
         (
          to-str-space (
            min_dist_bottom_up "" ""
          )
        )
         (
          to-str (
            to-str-space (
              min_dist_bottom_up "" ""
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
          end18 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur19 (
              quotient (
                * (
                  - end18 start17
                )
                 1000000
              )
               jps20
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur19
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
