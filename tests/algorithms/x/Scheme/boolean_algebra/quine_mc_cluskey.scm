;; Generated on 2025-08-06 21:38 +0700
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
      start70 (
        current-jiffy
      )
    )
     (
      jps73 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        compare_string string1 string2
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                result ""
              )
            )
             (
              begin (
                let (
                  (
                    count 0
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
                                        _len string1
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            c1 (
                                              _substring string1 i (
                                                + i 1
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                c2 (
                                                  _substring string2 i (
                                                    + i 1
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                if (
                                                  not (
                                                    string=? c1 c2
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    set! count (
                                                      + count 1
                                                    )
                                                  )
                                                   (
                                                    set! result (
                                                      string-append result "_"
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    set! result (
                                                      string-append result c1
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
                        if (
                          > count 1
                        )
                         (
                          begin (
                            ret1 ""
                          )
                        )
                         (
                          quote (
                            
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
      )
    )
     (
      define (
        contains_string arr value
      )
       (
        call/cc (
          lambda (
            ret4
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
                                _len arr
                              )
                            )
                             (
                              begin (
                                if (
                                  string=? (
                                    list-ref arr i
                                  )
                                   value
                                )
                                 (
                                  begin (
                                    ret4 #t
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
                ret4 #f
              )
            )
          )
        )
      )
    )
     (
      define (
        unique_strings arr
      )
       (
        call/cc (
          lambda (
            ret7
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
                                    _len arr
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      not (
                                        contains_string res (
                                          list-ref arr i
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        set! res (
                                          append res (
                                            _list (
                                              list-ref arr i
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
                    ret7 res
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
        check binary
      )
       (
        call/cc (
          lambda (
            ret10
          )
           (
            let (
              (
                pi (
                  _list
                )
              )
            )
             (
              begin (
                let (
                  (
                    current binary
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
                                if #t (
                                  begin (
                                    let (
                                      (
                                        check1 (
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
                                                            _len current
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            set! check1 (
                                                              append check1 (
                                                                _list "$"
                                                              )
                                                            )
                                                          )
                                                           (
                                                            set! i (
                                                              + i 1
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
                                            let (
                                              (
                                                temp (
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
                                                                _len current
                                                              )
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
                                                                                    _len current
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    let (
                                                                                      (
                                                                                        k (
                                                                                          compare_string (
                                                                                            list-ref current i
                                                                                          )
                                                                                           (
                                                                                            list-ref current j
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        if (
                                                                                          string=? k ""
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            list-set! check1 i "*"
                                                                                          )
                                                                                           (
                                                                                            list-set! check1 j "*"
                                                                                          )
                                                                                           (
                                                                                            set! temp (
                                                                                              append temp (
                                                                                                _list "X"
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
                                                                    set! i (
                                                                      + i 1
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
                                               (
                                                set! i 0
                                              )
                                               (
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
                                                                _len current
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                if (
                                                                  string=? (
                                                                    list-ref check1 i
                                                                  )
                                                                   "$"
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! pi (
                                                                      append pi (
                                                                        _list (
                                                                          list-ref current i
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
                                                                loop19
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
                                                      loop19
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                if (
                                                  equal? (
                                                    _len temp
                                                  )
                                                   0
                                                )
                                                 (
                                                  begin (
                                                    ret10 pi
                                                  )
                                                )
                                                 (
                                                  quote (
                                                    
                                                  )
                                                )
                                              )
                                               (
                                                set! current (
                                                  unique_strings temp
                                                )
                                              )
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
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        decimal_to_binary no_of_variable minterms
      )
       (
        call/cc (
          lambda (
            ret21
          )
           (
            let (
              (
                temp (
                  _list
                )
              )
            )
             (
              begin (
                let (
                  (
                    idx 0
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
                                  < idx (
                                    _len minterms
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        minterm (
                                          list-ref minterms idx
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            string ""
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
                                                              < i no_of_variable
                                                            )
                                                             (
                                                              begin (
                                                                set! string (
                                                                  string-append (
                                                                    to-str-space (
                                                                      modulo minterm 2
                                                                    )
                                                                  )
                                                                   string
                                                                )
                                                              )
                                                               (
                                                                set! minterm (
                                                                  quotient minterm 2
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
                                                set! temp (
                                                  append temp (
                                                    _list string
                                                  )
                                                )
                                              )
                                               (
                                                set! idx (
                                                  + idx 1
                                                )
                                              )
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
                    ret21 temp
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
        is_for_table string1 string2 count
      )
       (
        call/cc (
          lambda (
            ret26
          )
           (
            let (
              (
                count_n 0
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
                                  < i (
                                    _len string1
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        c1 (
                                          _substring string1 i (
                                            + i 1
                                          )
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            c2 (
                                              _substring string2 i (
                                                + i 1
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              not (
                                                string=? c1 c2
                                              )
                                            )
                                             (
                                              begin (
                                                set! count_n (
                                                  + count_n 1
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
                    ret26 (
                      equal? count_n count
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
        count_ones row
      )
       (
        call/cc (
          lambda (
            ret29
          )
           (
            let (
              (
                c 0
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
                                    _len row
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      equal? (
                                        list-ref row j
                                      )
                                       1
                                    )
                                     (
                                      begin (
                                        set! c (
                                          + c 1
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
                    ret29 c
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
        selection chart prime_implicants
      )
       (
        call/cc (
          lambda (
            ret32
          )
           (
            let (
              (
                temp (
                  _list
                )
              )
            )
             (
              begin (
                let (
                  (
                    select (
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
                                      < i (
                                        _len chart
                                      )
                                    )
                                     (
                                      begin (
                                        set! select (
                                          append select (
                                            _list 0
                                          )
                                        )
                                      )
                                       (
                                        set! i (
                                          + i 1
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
                        let (
                          (
                            col 0
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
                                          < col (
                                            _len (
                                              list-ref chart 0
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                count 0
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
                                                                  < row (
                                                                    _len chart
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    if (
                                                                      equal? (
                                                                        cond (
                                                                          (
                                                                            string? (
                                                                              list-ref chart row
                                                                            )
                                                                          )
                                                                           (
                                                                            _substring (
                                                                              list-ref chart row
                                                                            )
                                                                             col (
                                                                              + col 1
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          (
                                                                            hash-table? (
                                                                              list-ref chart row
                                                                            )
                                                                          )
                                                                           (
                                                                            hash-table-ref (
                                                                              list-ref chart row
                                                                            )
                                                                             col
                                                                          )
                                                                        )
                                                                         (
                                                                          else (
                                                                            list-ref (
                                                                              list-ref chart row
                                                                            )
                                                                             col
                                                                          )
                                                                        )
                                                                      )
                                                                       1
                                                                    )
                                                                     (
                                                                      begin (
                                                                        set! count (
                                                                          + count 1
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      quote (
                                                                        
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    set! row (
                                                                      + row 1
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
                                                    if (
                                                      equal? count 1
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            rem 0
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            set! row 0
                                                          )
                                                           (
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
                                                                          < row (
                                                                            _len chart
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            if (
                                                                              equal? (
                                                                                cond (
                                                                                  (
                                                                                    string? (
                                                                                      list-ref chart row
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    _substring (
                                                                                      list-ref chart row
                                                                                    )
                                                                                     col (
                                                                                      + col 1
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  (
                                                                                    hash-table? (
                                                                                      list-ref chart row
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    hash-table-ref (
                                                                                      list-ref chart row
                                                                                    )
                                                                                     col
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  else (
                                                                                    list-ref (
                                                                                      list-ref chart row
                                                                                    )
                                                                                     col
                                                                                  )
                                                                                )
                                                                              )
                                                                               1
                                                                            )
                                                                             (
                                                                              begin (
                                                                                set! rem row
                                                                              )
                                                                            )
                                                                             (
                                                                              quote (
                                                                                
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            set! row (
                                                                              + row 1
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
                                                            list-set! select rem 1
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
                                                    set! col (
                                                      + col 1
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
                            set! i 0
                          )
                           (
                            call/cc (
                              lambda (
                                break42
                              )
                               (
                                letrec (
                                  (
                                    loop41 (
                                      lambda (
                                        
                                      )
                                       (
                                        if (
                                          < i (
                                            _len select
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              equal? (
                                                list-ref select i
                                              )
                                               1
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
                                                        break44
                                                      )
                                                       (
                                                        letrec (
                                                          (
                                                            loop43 (
                                                              lambda (
                                                                
                                                              )
                                                               (
                                                                if (
                                                                  < j (
                                                                    _len (
                                                                      list-ref chart 0
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    if (
                                                                      equal? (
                                                                        cond (
                                                                          (
                                                                            string? (
                                                                              list-ref chart i
                                                                            )
                                                                          )
                                                                           (
                                                                            _substring (
                                                                              list-ref chart i
                                                                            )
                                                                             j (
                                                                              + j 1
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          (
                                                                            hash-table? (
                                                                              list-ref chart i
                                                                            )
                                                                          )
                                                                           (
                                                                            hash-table-ref (
                                                                              list-ref chart i
                                                                            )
                                                                             j
                                                                          )
                                                                        )
                                                                         (
                                                                          else (
                                                                            list-ref (
                                                                              list-ref chart i
                                                                            )
                                                                             j
                                                                          )
                                                                        )
                                                                      )
                                                                       1
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
                                                                                          < r (
                                                                                            _len chart
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            list-set! (
                                                                                              list-ref chart r
                                                                                            )
                                                                                             j 0
                                                                                          )
                                                                                           (
                                                                                            set! r (
                                                                                              + r 1
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
                                                                    loop43
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
                                                          loop43
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    set! temp (
                                                      append temp (
                                                        _list (
                                                          list-ref prime_implicants i
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
                                            loop41
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
                                  loop41
                                )
                              )
                            )
                          )
                           (
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
                                        if #t (
                                          begin (
                                            let (
                                              (
                                                counts (
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
                                                                  < r (
                                                                    _len chart
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! counts (
                                                                      append counts (
                                                                        _list (
                                                                          count_ones (
                                                                            list-ref chart r
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
                                                    let (
                                                      (
                                                        max_n (
                                                          list-ref counts 0
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            rem 0
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                k 1
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
                                                                              < k (
                                                                                _len counts
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                if (
                                                                                  > (
                                                                                    list-ref counts k
                                                                                  )
                                                                                   max_n
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    set! max_n (
                                                                                      list-ref counts k
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    set! rem k
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
                                                                if (
                                                                  equal? max_n 0
                                                                )
                                                                 (
                                                                  begin (
                                                                    ret32 temp
                                                                  )
                                                                )
                                                                 (
                                                                  quote (
                                                                    
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                set! temp (
                                                                  append temp (
                                                                    _list (
                                                                      list-ref prime_implicants rem
                                                                    )
                                                                  )
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
                                                                                    _len (
                                                                                      list-ref chart 0
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    if (
                                                                                      equal? (
                                                                                        cond (
                                                                                          (
                                                                                            string? (
                                                                                              list-ref chart rem
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            _substring (
                                                                                              list-ref chart rem
                                                                                            )
                                                                                             j (
                                                                                              + j 1
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          (
                                                                                            hash-table? (
                                                                                              list-ref chart rem
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            hash-table-ref (
                                                                                              list-ref chart rem
                                                                                            )
                                                                                             j
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          else (
                                                                                            list-ref (
                                                                                              list-ref chart rem
                                                                                            )
                                                                                             j
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       1
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        let (
                                                                                          (
                                                                                            r2 0
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          begin (
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
                                                                                                          < r2 (
                                                                                                            _len chart
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          begin (
                                                                                                            list-set! (
                                                                                                              list-ref chart r2
                                                                                                            )
                                                                                                             j 0
                                                                                                          )
                                                                                                           (
                                                                                                            set! r2 (
                                                                                                              + r2 1
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
                                                                )
                                                              )
                                                            )
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
                        )
                      )
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
        count_char s ch
      )
       (
        call/cc (
          lambda (
            ret57
          )
           (
            let (
              (
                cnt 0
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
                                  < i (
                                    _len s
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      string=? (
                                        _substring s i (
                                          + i 1
                                        )
                                      )
                                       ch
                                    )
                                     (
                                      begin (
                                        set! cnt (
                                          + cnt 1
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
                    ret57 cnt
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
        prime_implicant_chart prime_implicants binary
      )
       (
        call/cc (
          lambda (
            ret60
          )
           (
            let (
              (
                chart (
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
                                    _len prime_implicants
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
                                                          < j (
                                                            _len binary
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            set! row (
                                                              append row (
                                                                _list 0
                                                              )
                                                            )
                                                          )
                                                           (
                                                            set! j (
                                                              + j 1
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
                                            set! chart (
                                              append chart (
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
                    set! i 0
                  )
                   (
                    call/cc (
                      lambda (
                        break66
                      )
                       (
                        letrec (
                          (
                            loop65 (
                              lambda (
                                
                              )
                               (
                                if (
                                  < i (
                                    _len prime_implicants
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        count (
                                          count_char (
                                            list-ref prime_implicants i
                                          )
                                           "_"
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
                                                break68
                                              )
                                               (
                                                letrec (
                                                  (
                                                    loop67 (
                                                      lambda (
                                                        
                                                      )
                                                       (
                                                        if (
                                                          < j (
                                                            _len binary
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            if (
                                                              is_for_table (
                                                                list-ref prime_implicants i
                                                              )
                                                               (
                                                                list-ref binary j
                                                              )
                                                               count
                                                            )
                                                             (
                                                              begin (
                                                                list-set! (
                                                                  list-ref chart i
                                                                )
                                                                 j 1
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
                                                            loop67
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
                                                  loop67
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
                                    )
                                  )
                                   (
                                    loop65
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
                          loop65
                        )
                      )
                    )
                  )
                   (
                    ret60 chart
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
            ret69
          )
           (
            let (
              (
                no_of_variable 3
              )
            )
             (
              begin (
                let (
                  (
                    minterms (
                      _list 1 5 7
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        binary (
                          decimal_to_binary no_of_variable minterms
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            prime_implicants (
                              check binary
                            )
                          )
                        )
                         (
                          begin (
                            _display (
                              if (
                                string? "Prime Implicants are:"
                              )
                               "Prime Implicants are:" (
                                to-str "Prime Implicants are:"
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
                                  to-str-space prime_implicants
                                )
                              )
                               (
                                to-str-space prime_implicants
                              )
                               (
                                to-str (
                                  to-str-space prime_implicants
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
                                chart (
                                  prime_implicant_chart prime_implicants binary
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    essential_prime_implicants (
                                      selection chart prime_implicants
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    _display (
                                      if (
                                        string? "Essential Prime Implicants are:"
                                      )
                                       "Essential Prime Implicants are:" (
                                        to-str "Essential Prime Implicants are:"
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
                                          to-str-space essential_prime_implicants
                                        )
                                      )
                                       (
                                        to-str-space essential_prime_implicants
                                      )
                                       (
                                        to-str (
                                          to-str-space essential_prime_implicants
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
     (
      main
    )
     (
      let (
        (
          end71 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur72 (
              quotient (
                * (
                  - end71 start70
                )
                 1000000
              )
               jps73
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur72
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
