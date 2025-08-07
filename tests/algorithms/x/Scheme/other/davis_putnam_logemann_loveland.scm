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
      start27 (
        current-jiffy
      )
    )
     (
      jps30 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        new_clause lits
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                m (
                  alist->hash-table (
                    _list
                  )
                )
              )
            )
             (
              begin (
                let (
                  (
                    names (
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
                                        _len lits
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            lit (
                                              list-ref-safe lits i
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            hash-table-set! m lit (
                                              - 0 1
                                            )
                                          )
                                           (
                                            set! names (
                                              append names (
                                                _list lit
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
                              cons "literals" m
                            )
                             (
                              cons "names" names
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
        assign_clause c model
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            let (
              (
                lits (
                  hash-table-ref c "literals"
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
                                    _len (
                                      hash-table-ref c "names"
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        lit (
                                          list-ref-safe (
                                            hash-table-ref c "names"
                                          )
                                           i
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            symbol (
                                              _substring lit 0 2
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              cond (
                                                (
                                                  string? model
                                                )
                                                 (
                                                  if (
                                                    string-contains model symbol
                                                  )
                                                   #t #f
                                                )
                                              )
                                               (
                                                (
                                                  hash-table? model
                                                )
                                                 (
                                                  if (
                                                    hash-table-exists? model symbol
                                                  )
                                                   #t #f
                                                )
                                              )
                                               (
                                                else (
                                                  if (
                                                    member symbol model
                                                  )
                                                   #t #f
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    value (
                                                      hash-table-ref/default model symbol '(
                                                        
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    if (
                                                      and (
                                                        string=? (
                                                          _substring lit (
                                                            - (
                                                              _len lit
                                                            )
                                                             1
                                                          )
                                                           (
                                                            _len lit
                                                          )
                                                        )
                                                         "'"
                                                      )
                                                       (
                                                        not (
                                                          equal? value (
                                                            - 0 1
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        set! value (
                                                          - 1 value
                                                        )
                                                      )
                                                    )
                                                     '(
                                                      
                                                    )
                                                  )
                                                   (
                                                    hash-table-set! lits lit value
                                                  )
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
                                    )
                                  )
                                   (
                                    loop5
                                  )
                                )
                                 '(
                                  
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
                    hash-table-set! c "literals" lits
                  )
                   (
                    ret4 c
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
        evaluate_clause c model
      )
       (
        call/cc (
          lambda (
            ret7
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
                                _len (
                                  hash-table-ref c "names"
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    lit (
                                      list-ref-safe (
                                        hash-table-ref c "names"
                                      )
                                       i
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        sym (
                                          if (
                                            string=? (
                                              _substring lit (
                                                - (
                                                  _len lit
                                                )
                                                 1
                                              )
                                               (
                                                _len lit
                                              )
                                            )
                                             "'"
                                          )
                                           (
                                            _substring lit 0 2
                                          )
                                           (
                                            string-append lit "'"
                                          )
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        if (
                                          cond (
                                            (
                                              string? (
                                                hash-table-ref c "literals"
                                              )
                                            )
                                             (
                                              if (
                                                string-contains (
                                                  hash-table-ref c "literals"
                                                )
                                                 sym
                                              )
                                               #t #f
                                            )
                                          )
                                           (
                                            (
                                              hash-table? (
                                                hash-table-ref c "literals"
                                              )
                                            )
                                             (
                                              if (
                                                hash-table-exists? (
                                                  hash-table-ref c "literals"
                                                )
                                                 sym
                                              )
                                               #t #f
                                            )
                                          )
                                           (
                                            else (
                                              if (
                                                member sym (
                                                  hash-table-ref c "literals"
                                                )
                                              )
                                               #t #f
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            ret7 (
                                              alist->hash-table (
                                                _list (
                                                  cons "value" 1
                                                )
                                                 (
                                                  cons "clause" c
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
                set! c (
                  assign_clause c model
                )
              )
               (
                set! i 0
              )
               (
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
                                _len (
                                  hash-table-ref c "names"
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    lit (
                                      list-ref-safe (
                                        hash-table-ref c "names"
                                      )
                                       i
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        value (
                                          hash-table-ref/default (
                                            hash-table-ref c "literals"
                                          )
                                           lit '(
                                            
                                          )
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        if (
                                          equal? value 1
                                        )
                                         (
                                          begin (
                                            ret7 (
                                              alist->hash-table (
                                                _list (
                                                  cons "value" 1
                                                )
                                                 (
                                                  cons "clause" c
                                                )
                                              )
                                            )
                                          )
                                        )
                                         '(
                                          
                                        )
                                      )
                                       (
                                        if (
                                          equal? value (
                                            - 0 1
                                          )
                                        )
                                         (
                                          begin (
                                            ret7 (
                                              alist->hash-table (
                                                _list (
                                                  cons "value" (
                                                    - 0 1
                                                  )
                                                )
                                                 (
                                                  cons "clause" c
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
                let (
                  (
                    any_true 0
                  )
                )
                 (
                  begin (
                    set! i 0
                  )
                   (
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
                                      hash-table-ref c "names"
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        lit (
                                          list-ref-safe (
                                            hash-table-ref c "names"
                                          )
                                           i
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        if (
                                          equal? (
                                            hash-table-ref/default (
                                              hash-table-ref c "literals"
                                            )
                                             lit '(
                                              
                                            )
                                          )
                                           1
                                        )
                                         (
                                          begin (
                                            set! any_true 1
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
                                    loop12
                                  )
                                )
                                 '(
                                  
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
                    ret7 (
                      alist->hash-table (
                        _list (
                          cons "value" any_true
                        )
                         (
                          cons "clause" c
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
        new_formula cs
      )
       (
        call/cc (
          lambda (
            ret14
          )
           (
            ret14 (
              alist->hash-table (
                _list (
                  cons "clauses" cs
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        remove_symbol symbols s
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
                                    _len symbols
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      not (
                                        string=? (
                                          list-ref-safe symbols i
                                        )
                                         s
                                      )
                                    )
                                     (
                                      begin (
                                        set! res (
                                          append res (
                                            _list (
                                              list-ref-safe symbols i
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
                                      + i 1
                                    )
                                  )
                                   (
                                    loop16
                                  )
                                )
                                 '(
                                  
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
        dpll_algorithm clauses symbols model
      )
       (
        call/cc (
          lambda (
            ret18
          )
           (
            let (
              (
                all_true #t
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
                                    _len clauses
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        ev (
                                          evaluate_clause (
                                            list-ref-safe clauses i
                                          )
                                           model
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        list-set! clauses i (
                                          hash-table-ref ev "clause"
                                        )
                                      )
                                       (
                                        if (
                                          equal? (
                                            hash-table-ref ev "value"
                                          )
                                           0
                                        )
                                         (
                                          begin (
                                            ret18 (
                                              alist->hash-table (
                                                _list (
                                                  cons "sat" #f
                                                )
                                                 (
                                                  cons "model" (
                                                    alist->hash-table (
                                                      _list
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                         (
                                          if (
                                            equal? (
                                              hash-table-ref ev "value"
                                            )
                                             (
                                              - 0 1
                                            )
                                          )
                                           (
                                            begin (
                                              set! all_true #f
                                            )
                                          )
                                           '(
                                            
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
                    if all_true (
                      begin (
                        ret18 (
                          alist->hash-table (
                            _list (
                              cons "sat" #t
                            )
                             (
                              cons "model" model
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
                        p (
                          list-ref-safe symbols 0
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            rest (
                              remove_symbol symbols p
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                tmp1 model
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    tmp2 model
                                  )
                                )
                                 (
                                  begin (
                                    hash-table-set! tmp1 p 1
                                  )
                                   (
                                    hash-table-set! tmp2 p 0
                                  )
                                   (
                                    let (
                                      (
                                        res1 (
                                          dpll_algorithm clauses rest tmp1
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        if (
                                          hash-table-ref res1 "sat"
                                        )
                                         (
                                          begin (
                                            ret18 res1
                                          )
                                        )
                                         '(
                                          
                                        )
                                      )
                                       (
                                        ret18 (
                                          dpll_algorithm clauses rest tmp2
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
        str_clause c
      )
       (
        call/cc (
          lambda (
            ret21
          )
           (
            let (
              (
                line "{"
              )
            )
             (
              begin (
                let (
                  (
                    first #t
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
                                      < i (
                                        _len (
                                          hash-table-ref c "names"
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            lit (
                                              list-ref-safe (
                                                hash-table-ref c "names"
                                              )
                                               i
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            if first (
                                              begin (
                                                set! first #f
                                              )
                                            )
                                             (
                                              begin (
                                                set! line (
                                                  string-append line " , "
                                                )
                                              )
                                            )
                                          )
                                           (
                                            set! line (
                                              string-append line lit
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
                        set! line (
                          string-append line "}"
                        )
                      )
                       (
                        ret21 line
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
        str_formula f
      )
       (
        call/cc (
          lambda (
            ret24
          )
           (
            let (
              (
                line "{"
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
                                    _len (
                                      hash-table-ref f "clauses"
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    set! line (
                                      string-append line (
                                        str_clause (
                                          list-ref-safe (
                                            hash-table-ref f "clauses"
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
                                            hash-table-ref f "clauses"
                                          )
                                        )
                                         1
                                      )
                                    )
                                     (
                                      begin (
                                        set! line (
                                          string-append line " , "
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
                    set! line (
                      string-append line "}"
                    )
                  )
                   (
                    ret24 line
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
          clause1 (
            new_clause (
              _list "A4" "A3" "A5'" "A1" "A3'"
            )
          )
        )
      )
       (
        begin (
          let (
            (
              clause2 (
                new_clause (
                  _list "A4"
                )
              )
            )
          )
           (
            begin (
              let (
                (
                  formula (
                    new_formula (
                      _list clause1 clause2
                    )
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      formula_str (
                        str_formula formula
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          clauses (
                            _list clause1 clause2
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              symbols (
                                _list "A4" "A3" "A5" "A1"
                              )
                            )
                          )
                           (
                            begin (
                              let (
                                (
                                  model (
                                    alist->hash-table (
                                      _list
                                    )
                                  )
                                )
                              )
                               (
                                begin (
                                  let (
                                    (
                                      result (
                                        dpll_algorithm clauses symbols model
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      if (
                                        hash-table-ref result "sat"
                                      )
                                       (
                                        begin (
                                          _display (
                                            if (
                                              string? (
                                                string-append (
                                                  string-append "The formula " formula_str
                                                )
                                                 " is satisfiable."
                                              )
                                            )
                                             (
                                              string-append (
                                                string-append "The formula " formula_str
                                              )
                                               " is satisfiable."
                                            )
                                             (
                                              to-str (
                                                string-append (
                                                  string-append "The formula " formula_str
                                                )
                                                 " is satisfiable."
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
                                                string-append (
                                                  string-append "The formula " formula_str
                                                )
                                                 " is not satisfiable."
                                              )
                                            )
                                             (
                                              string-append (
                                                string-append "The formula " formula_str
                                              )
                                               " is not satisfiable."
                                            )
                                             (
                                              to-str (
                                                string-append (
                                                  string-append "The formula " formula_str
                                                )
                                                 " is not satisfiable."
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
          end28 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur29 (
              quotient (
                * (
                  - end28 start27
                )
                 1000000
              )
               jps30
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur29
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
