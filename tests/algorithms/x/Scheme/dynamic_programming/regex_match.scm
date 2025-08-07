;; Generated on 2025-08-07 08:40 +0700
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
      start14 (
        current-jiffy
      )
    )
     (
      jps17 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        recursive_match text pattern
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            begin (
              if (
                equal? (
                  _len pattern
                )
                 0
              )
               (
                begin (
                  ret1 (
                    equal? (
                      _len text
                    )
                     0
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
                  _len text
                )
                 0
              )
               (
                begin (
                  if (
                    and (
                      >= (
                        _len pattern
                      )
                       2
                    )
                     (
                      string=? (
                        _substring pattern (
                          - (
                            _len pattern
                          )
                           1
                        )
                         (
                          _len pattern
                        )
                      )
                       "*"
                    )
                  )
                   (
                    begin (
                      ret1 (
                        recursive_match text (
                          _substring pattern 0 (
                            - (
                              _len pattern
                            )
                             2
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
                  ret1 #f
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
                  last_text (
                    _substring text (
                      - (
                        _len text
                      )
                       1
                    )
                     (
                      _len text
                    )
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      last_pattern (
                        _substring pattern (
                          - (
                            _len pattern
                          )
                           1
                        )
                         (
                          _len pattern
                        )
                      )
                    )
                  )
                   (
                    begin (
                      if (
                        or (
                          string=? last_text last_pattern
                        )
                         (
                          string=? last_pattern "."
                        )
                      )
                       (
                        begin (
                          ret1 (
                            recursive_match (
                              _substring text 0 (
                                - (
                                  _len text
                                )
                                 1
                              )
                            )
                             (
                              _substring pattern 0 (
                                - (
                                  _len pattern
                                )
                                 1
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
                        string=? last_pattern "*"
                      )
                       (
                        begin (
                          if (
                            recursive_match (
                              _substring text 0 (
                                - (
                                  _len text
                                )
                                 1
                              )
                            )
                             pattern
                          )
                           (
                            begin (
                              ret1 #t
                            )
                          )
                           (
                            quote (
                              
                            )
                          )
                        )
                         (
                          ret1 (
                            recursive_match text (
                              _substring pattern 0 (
                                - (
                                  _len pattern
                                )
                                 2
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
                      ret1 #f
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
        dp_match text pattern
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            let (
              (
                m (
                  _len text
                )
              )
            )
             (
              begin (
                let (
                  (
                    n (
                      _len pattern
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
                        let (
                          (
                            i 0
                          )
                        )
                         (
                          begin (
                            call/cc (
                              lambda (
                                break4
                              )
                               (
                                letrec (
                                  (
                                    loop3 (
                                      lambda (
                                        
                                      )
                                       (
                                        if (
                                          <= i m
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
                                                                  <= j n
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! row (
                                                                      append row (
                                                                        _list #f
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    set! j (
                                                                      + j 1
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
                                                    set! dp (
                                                      append dp (
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
                                            loop3
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
                                  loop3
                                )
                              )
                            )
                          )
                           (
                            list-set! (
                              list-ref dp 0
                            )
                             0 #t
                          )
                           (
                            let (
                              (
                                j 1
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
                                              <= j n
                                            )
                                             (
                                              begin (
                                                if (
                                                  and (
                                                    string=? (
                                                      _substring pattern (
                                                        - j 1
                                                      )
                                                       j
                                                    )
                                                     "*"
                                                  )
                                                   (
                                                    >= j 2
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    if (
                                                      cond (
                                                        (
                                                          string? (
                                                            list-ref dp 0
                                                          )
                                                        )
                                                         (
                                                          _substring (
                                                            list-ref dp 0
                                                          )
                                                           (
                                                            - j 2
                                                          )
                                                           (
                                                            + (
                                                              - j 2
                                                            )
                                                             1
                                                          )
                                                        )
                                                      )
                                                       (
                                                        (
                                                          hash-table? (
                                                            list-ref dp 0
                                                          )
                                                        )
                                                         (
                                                          hash-table-ref (
                                                            list-ref dp 0
                                                          )
                                                           (
                                                            - j 2
                                                          )
                                                        )
                                                      )
                                                       (
                                                        else (
                                                          list-ref (
                                                            list-ref dp 0
                                                          )
                                                           (
                                                            - j 2
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        list-set! (
                                                          list-ref dp 0
                                                        )
                                                         j #t
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
                                set! i 1
                              )
                               (
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
                                              <= i m
                                            )
                                             (
                                              begin (
                                                set! j 1
                                              )
                                               (
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
                                                              <= j n
                                                            )
                                                             (
                                                              begin (
                                                                let (
                                                                  (
                                                                    p_char (
                                                                      _substring pattern (
                                                                        - j 1
                                                                      )
                                                                       j
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        t_char (
                                                                          _substring text (
                                                                            - i 1
                                                                          )
                                                                           i
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        if (
                                                                          or (
                                                                            string=? p_char "."
                                                                          )
                                                                           (
                                                                            string=? p_char t_char
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            if (
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
                                                                             (
                                                                              begin (
                                                                                list-set! (
                                                                                  list-ref dp i
                                                                                )
                                                                                 j #t
                                                                              )
                                                                            )
                                                                             (
                                                                              quote (
                                                                                
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          if (
                                                                            string=? p_char "*"
                                                                          )
                                                                           (
                                                                            begin (
                                                                              if (
                                                                                >= j 2
                                                                              )
                                                                               (
                                                                                begin (
                                                                                  if (
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
                                                                                          - j 2
                                                                                        )
                                                                                         (
                                                                                          + (
                                                                                            - j 2
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
                                                                                          - j 2
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      else (
                                                                                        list-ref (
                                                                                          list-ref dp i
                                                                                        )
                                                                                         (
                                                                                          - j 2
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    begin (
                                                                                      list-set! (
                                                                                        list-ref dp i
                                                                                      )
                                                                                       j #t
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
                                                                                      prev_p (
                                                                                        _substring pattern (
                                                                                          - j 2
                                                                                        )
                                                                                         (
                                                                                          - j 1
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    begin (
                                                                                      if (
                                                                                        or (
                                                                                          string=? prev_p "."
                                                                                        )
                                                                                         (
                                                                                          string=? prev_p t_char
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        begin (
                                                                                          if (
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
                                                                                           (
                                                                                            begin (
                                                                                              list-set! (
                                                                                                list-ref dp i
                                                                                              )
                                                                                               j #t
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
                                                                            begin (
                                                                              list-set! (
                                                                                list-ref dp i
                                                                              )
                                                                               j #f
                                                                            )
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
                                                set! i (
                                                  + i 1
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
                                ret2 (
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
          )
        )
      )
    )
     (
      define (
        print_bool b
      )
       (
        call/cc (
          lambda (
            ret13
          )
           (
            if b (
              begin (
                _display (
                  if (
                    string? (
                      if #t #t #f
                    )
                  )
                   (
                    if #t #t #f
                  )
                   (
                    to-str (
                      if #t #t #f
                    )
                  )
                )
              )
               (
                newline
              )
            )
             (
              begin (
                _display (
                  if (
                    string? (
                      if #f #t #f
                    )
                  )
                   (
                    if #f #t #f
                  )
                   (
                    to-str (
                      if #f #t #f
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
     (
      print_bool (
        recursive_match "abc" "a.c"
      )
    )
     (
      print_bool (
        recursive_match "abc" "af*.c"
      )
    )
     (
      print_bool (
        recursive_match "abc" "a.c*"
      )
    )
     (
      print_bool (
        recursive_match "abc" "a.c*d"
      )
    )
     (
      print_bool (
        recursive_match "aa" ".*"
      )
    )
     (
      print_bool (
        dp_match "abc" "a.c"
      )
    )
     (
      print_bool (
        dp_match "abc" "af*.c"
      )
    )
     (
      print_bool (
        dp_match "abc" "a.c*"
      )
    )
     (
      print_bool (
        dp_match "abc" "a.c*d"
      )
    )
     (
      print_bool (
        dp_match "aa" ".*"
      )
    )
     (
      let (
        (
          end15 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur16 (
              quotient (
                * (
                  - end15 start14
                )
                 1000000
              )
               jps17
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur16
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
