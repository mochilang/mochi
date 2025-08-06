;; Generated on 2025-08-06 22:04 +0700
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
      start24 (
        current-jiffy
      )
    )
     (
      jps27 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        contains xs x
      )
       (
        call/cc (
          lambda (
            ret1
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
                                _len xs
                              )
                            )
                             (
                              begin (
                                if (
                                  string=? (
                                    list-ref xs i
                                  )
                                   x
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
                ret1 #f
              )
            )
          )
        )
      )
    )
     (
      define (
        index_of xs x
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
                                _len xs
                              )
                            )
                             (
                              begin (
                                if (
                                  string=? (
                                    list-ref xs i
                                  )
                                   x
                                )
                                 (
                                  begin (
                                    ret4 i
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
                ret4 (
                  - 1
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        prepare_input dirty
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            let (
              (
                letters "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
              )
            )
             (
              begin (
                let (
                  (
                    upper_dirty (
                      upper dirty
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        filtered ""
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
                                            _len upper_dirty
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                c (
                                                  _substring upper_dirty i (
                                                    + i 1
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                if (
                                                  cond (
                                                    (
                                                      string? letters
                                                    )
                                                     (
                                                      if (
                                                        string-contains letters c
                                                      )
                                                       #t #f
                                                    )
                                                  )
                                                   (
                                                    (
                                                      hash-table? letters
                                                    )
                                                     (
                                                      if (
                                                        hash-table-exists? letters c
                                                      )
                                                       #t #f
                                                    )
                                                  )
                                                   (
                                                    else (
                                                      if (
                                                        member c letters
                                                      )
                                                       #t #f
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    set! filtered (
                                                      string-append filtered c
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
                            if (
                              < (
                                _len filtered
                              )
                               2
                            )
                             (
                              begin (
                                ret7 filtered
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
                                clean ""
                              )
                            )
                             (
                              begin (
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
                                                - (
                                                  _len filtered
                                                )
                                                 1
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    c1 (
                                                      _substring filtered i (
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
                                                          _substring filtered (
                                                            + i 1
                                                          )
                                                           (
                                                            + i 2
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        set! clean (
                                                          string-append clean c1
                                                        )
                                                      )
                                                       (
                                                        if (
                                                          string=? c1 c2
                                                        )
                                                         (
                                                          begin (
                                                            set! clean (
                                                              string-append clean "X"
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
                                set! clean (
                                  string-append clean (
                                    _substring filtered (
                                      - (
                                        _len filtered
                                      )
                                       1
                                    )
                                     (
                                      _len filtered
                                    )
                                  )
                                )
                              )
                               (
                                if (
                                  equal? (
                                    modulo (
                                      _len clean
                                    )
                                     2
                                  )
                                   1
                                )
                                 (
                                  begin (
                                    set! clean (
                                      string-append clean "X"
                                    )
                                  )
                                )
                                 (
                                  quote (
                                    
                                  )
                                )
                              )
                               (
                                ret7 clean
                              )
                            )
                          )
                        )
                      )
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
        generate_table key
      )
       (
        call/cc (
          lambda (
            ret12
          )
           (
            let (
              (
                alphabet "ABCDEFGHIKLMNOPQRSTUVWXYZ"
              )
            )
             (
              begin (
                let (
                  (
                    table (
                      _list
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        upper_key (
                          upper key
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
                                            _len upper_key
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                c (
                                                  _substring upper_key i (
                                                    + i 1
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                if (
                                                  cond (
                                                    (
                                                      string? alphabet
                                                    )
                                                     (
                                                      if (
                                                        string-contains alphabet c
                                                      )
                                                       #t #f
                                                    )
                                                  )
                                                   (
                                                    (
                                                      hash-table? alphabet
                                                    )
                                                     (
                                                      if (
                                                        hash-table-exists? alphabet c
                                                      )
                                                       #t #f
                                                    )
                                                  )
                                                   (
                                                    else (
                                                      if (
                                                        member c alphabet
                                                      )
                                                       #t #f
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    if (
                                                      not (
                                                        contains table c
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        set! table (
                                                          append table (
                                                            _list c
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
                                                set! i (
                                                  + i 1
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
                                            _len alphabet
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                c (
                                                  _substring alphabet i (
                                                    + i 1
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                if (
                                                  not (
                                                    contains table c
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    set! table (
                                                      append table (
                                                        _list c
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
                            ret12 table
                          )
                        )
                      )
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
        encode plaintext key
      )
       (
        call/cc (
          lambda (
            ret17
          )
           (
            let (
              (
                table (
                  generate_table key
                )
              )
            )
             (
              begin (
                let (
                  (
                    text (
                      prepare_input plaintext
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        cipher ""
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
                                            _len text
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                c1 (
                                                  _substring text i (
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
                                                      _substring text (
                                                        + i 1
                                                      )
                                                       (
                                                        + i 2
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        idx1 (
                                                          index_of table c1
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            idx2 (
                                                              index_of table c2
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                row1 (
                                                                  / idx1 5
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                let (
                                                                  (
                                                                    col1 (
                                                                      fmod idx1 5
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        row2 (
                                                                          / idx2 5
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            col2 (
                                                                              fmod idx2 5
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            if (
                                                                              equal? row1 row2
                                                                            )
                                                                             (
                                                                              begin (
                                                                                set! cipher (
                                                                                  string-append cipher (
                                                                                    cond (
                                                                                      (
                                                                                        string? table
                                                                                      )
                                                                                       (
                                                                                        _substring table (
                                                                                          _add (
                                                                                            * row1 5
                                                                                          )
                                                                                           (
                                                                                            fmod (
                                                                                              _add col1 1
                                                                                            )
                                                                                             5
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          + (
                                                                                            _add (
                                                                                              * row1 5
                                                                                            )
                                                                                             (
                                                                                              fmod (
                                                                                                _add col1 1
                                                                                              )
                                                                                               5
                                                                                            )
                                                                                          )
                                                                                           1
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      (
                                                                                        hash-table? table
                                                                                      )
                                                                                       (
                                                                                        hash-table-ref table (
                                                                                          _add (
                                                                                            * row1 5
                                                                                          )
                                                                                           (
                                                                                            fmod (
                                                                                              _add col1 1
                                                                                            )
                                                                                             5
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      else (
                                                                                        list-ref table (
                                                                                          _add (
                                                                                            * row1 5
                                                                                          )
                                                                                           (
                                                                                            fmod (
                                                                                              _add col1 1
                                                                                            )
                                                                                             5
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                set! cipher (
                                                                                  string-append cipher (
                                                                                    cond (
                                                                                      (
                                                                                        string? table
                                                                                      )
                                                                                       (
                                                                                        _substring table (
                                                                                          _add (
                                                                                            * row2 5
                                                                                          )
                                                                                           (
                                                                                            fmod (
                                                                                              _add col2 1
                                                                                            )
                                                                                             5
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          + (
                                                                                            _add (
                                                                                              * row2 5
                                                                                            )
                                                                                             (
                                                                                              fmod (
                                                                                                _add col2 1
                                                                                              )
                                                                                               5
                                                                                            )
                                                                                          )
                                                                                           1
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      (
                                                                                        hash-table? table
                                                                                      )
                                                                                       (
                                                                                        hash-table-ref table (
                                                                                          _add (
                                                                                            * row2 5
                                                                                          )
                                                                                           (
                                                                                            fmod (
                                                                                              _add col2 1
                                                                                            )
                                                                                             5
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      else (
                                                                                        list-ref table (
                                                                                          _add (
                                                                                            * row2 5
                                                                                          )
                                                                                           (
                                                                                            fmod (
                                                                                              _add col2 1
                                                                                            )
                                                                                             5
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              if (
                                                                                equal? col1 col2
                                                                              )
                                                                               (
                                                                                begin (
                                                                                  set! cipher (
                                                                                    string-append cipher (
                                                                                      cond (
                                                                                        (
                                                                                          string? table
                                                                                        )
                                                                                         (
                                                                                          _substring table (
                                                                                            _add (
                                                                                              * (
                                                                                                fmod (
                                                                                                  _add row1 1
                                                                                                )
                                                                                                 5
                                                                                              )
                                                                                               5
                                                                                            )
                                                                                             col1
                                                                                          )
                                                                                           (
                                                                                            + (
                                                                                              _add (
                                                                                                * (
                                                                                                  fmod (
                                                                                                    _add row1 1
                                                                                                  )
                                                                                                   5
                                                                                                )
                                                                                                 5
                                                                                              )
                                                                                               col1
                                                                                            )
                                                                                             1
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        (
                                                                                          hash-table? table
                                                                                        )
                                                                                         (
                                                                                          hash-table-ref table (
                                                                                            _add (
                                                                                              * (
                                                                                                fmod (
                                                                                                  _add row1 1
                                                                                                )
                                                                                                 5
                                                                                              )
                                                                                               5
                                                                                            )
                                                                                             col1
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        else (
                                                                                          list-ref table (
                                                                                            _add (
                                                                                              * (
                                                                                                fmod (
                                                                                                  _add row1 1
                                                                                                )
                                                                                                 5
                                                                                              )
                                                                                               5
                                                                                            )
                                                                                             col1
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  set! cipher (
                                                                                    string-append cipher (
                                                                                      cond (
                                                                                        (
                                                                                          string? table
                                                                                        )
                                                                                         (
                                                                                          _substring table (
                                                                                            _add (
                                                                                              * (
                                                                                                fmod (
                                                                                                  _add row2 1
                                                                                                )
                                                                                                 5
                                                                                              )
                                                                                               5
                                                                                            )
                                                                                             col2
                                                                                          )
                                                                                           (
                                                                                            + (
                                                                                              _add (
                                                                                                * (
                                                                                                  fmod (
                                                                                                    _add row2 1
                                                                                                  )
                                                                                                   5
                                                                                                )
                                                                                                 5
                                                                                              )
                                                                                               col2
                                                                                            )
                                                                                             1
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        (
                                                                                          hash-table? table
                                                                                        )
                                                                                         (
                                                                                          hash-table-ref table (
                                                                                            _add (
                                                                                              * (
                                                                                                fmod (
                                                                                                  _add row2 1
                                                                                                )
                                                                                                 5
                                                                                              )
                                                                                               5
                                                                                            )
                                                                                             col2
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        else (
                                                                                          list-ref table (
                                                                                            _add (
                                                                                              * (
                                                                                                fmod (
                                                                                                  _add row2 1
                                                                                                )
                                                                                                 5
                                                                                              )
                                                                                               5
                                                                                            )
                                                                                             col2
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                begin (
                                                                                  set! cipher (
                                                                                    string-append cipher (
                                                                                      cond (
                                                                                        (
                                                                                          string? table
                                                                                        )
                                                                                         (
                                                                                          _substring table (
                                                                                            _add (
                                                                                              * row1 5
                                                                                            )
                                                                                             col2
                                                                                          )
                                                                                           (
                                                                                            + (
                                                                                              _add (
                                                                                                * row1 5
                                                                                              )
                                                                                               col2
                                                                                            )
                                                                                             1
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        (
                                                                                          hash-table? table
                                                                                        )
                                                                                         (
                                                                                          hash-table-ref table (
                                                                                            _add (
                                                                                              * row1 5
                                                                                            )
                                                                                             col2
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        else (
                                                                                          list-ref table (
                                                                                            _add (
                                                                                              * row1 5
                                                                                            )
                                                                                             col2
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  set! cipher (
                                                                                    string-append cipher (
                                                                                      cond (
                                                                                        (
                                                                                          string? table
                                                                                        )
                                                                                         (
                                                                                          _substring table (
                                                                                            _add (
                                                                                              * row2 5
                                                                                            )
                                                                                             col1
                                                                                          )
                                                                                           (
                                                                                            + (
                                                                                              _add (
                                                                                                * row2 5
                                                                                              )
                                                                                               col1
                                                                                            )
                                                                                             1
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        (
                                                                                          hash-table? table
                                                                                        )
                                                                                         (
                                                                                          hash-table-ref table (
                                                                                            _add (
                                                                                              * row2 5
                                                                                            )
                                                                                             col1
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        else (
                                                                                          list-ref table (
                                                                                            _add (
                                                                                              * row2 5
                                                                                            )
                                                                                             col1
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
                                                                            set! i (
                                                                              + i 2
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
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
                            ret17 cipher
                          )
                        )
                      )
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
        decode cipher key
      )
       (
        call/cc (
          lambda (
            ret20
          )
           (
            let (
              (
                table (
                  generate_table key
                )
              )
            )
             (
              begin (
                let (
                  (
                    plain ""
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
                                        _len cipher
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            c1 (
                                              _substring cipher i (
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
                                                  _substring cipher (
                                                    + i 1
                                                  )
                                                   (
                                                    + i 2
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    idx1 (
                                                      index_of table c1
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        idx2 (
                                                          index_of table c2
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            row1 (
                                                              / idx1 5
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                col1 (
                                                                  fmod idx1 5
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                let (
                                                                  (
                                                                    row2 (
                                                                      / idx2 5
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        col2 (
                                                                          fmod idx2 5
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        if (
                                                                          equal? row1 row2
                                                                        )
                                                                         (
                                                                          begin (
                                                                            set! plain (
                                                                              string-append plain (
                                                                                cond (
                                                                                  (
                                                                                    string? table
                                                                                  )
                                                                                   (
                                                                                    _substring table (
                                                                                      _add (
                                                                                        * row1 5
                                                                                      )
                                                                                       (
                                                                                        fmod (
                                                                                          _add col1 4
                                                                                        )
                                                                                         5
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      + (
                                                                                        _add (
                                                                                          * row1 5
                                                                                        )
                                                                                         (
                                                                                          fmod (
                                                                                            _add col1 4
                                                                                          )
                                                                                           5
                                                                                        )
                                                                                      )
                                                                                       1
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  (
                                                                                    hash-table? table
                                                                                  )
                                                                                   (
                                                                                    hash-table-ref table (
                                                                                      _add (
                                                                                        * row1 5
                                                                                      )
                                                                                       (
                                                                                        fmod (
                                                                                          _add col1 4
                                                                                        )
                                                                                         5
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  else (
                                                                                    list-ref table (
                                                                                      _add (
                                                                                        * row1 5
                                                                                      )
                                                                                       (
                                                                                        fmod (
                                                                                          _add col1 4
                                                                                        )
                                                                                         5
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            set! plain (
                                                                              string-append plain (
                                                                                cond (
                                                                                  (
                                                                                    string? table
                                                                                  )
                                                                                   (
                                                                                    _substring table (
                                                                                      _add (
                                                                                        * row2 5
                                                                                      )
                                                                                       (
                                                                                        fmod (
                                                                                          _add col2 4
                                                                                        )
                                                                                         5
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      + (
                                                                                        _add (
                                                                                          * row2 5
                                                                                        )
                                                                                         (
                                                                                          fmod (
                                                                                            _add col2 4
                                                                                          )
                                                                                           5
                                                                                        )
                                                                                      )
                                                                                       1
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  (
                                                                                    hash-table? table
                                                                                  )
                                                                                   (
                                                                                    hash-table-ref table (
                                                                                      _add (
                                                                                        * row2 5
                                                                                      )
                                                                                       (
                                                                                        fmod (
                                                                                          _add col2 4
                                                                                        )
                                                                                         5
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  else (
                                                                                    list-ref table (
                                                                                      _add (
                                                                                        * row2 5
                                                                                      )
                                                                                       (
                                                                                        fmod (
                                                                                          _add col2 4
                                                                                        )
                                                                                         5
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          if (
                                                                            equal? col1 col2
                                                                          )
                                                                           (
                                                                            begin (
                                                                              set! plain (
                                                                                string-append plain (
                                                                                  cond (
                                                                                    (
                                                                                      string? table
                                                                                    )
                                                                                     (
                                                                                      _substring table (
                                                                                        _add (
                                                                                          * (
                                                                                            fmod (
                                                                                              _add row1 4
                                                                                            )
                                                                                             5
                                                                                          )
                                                                                           5
                                                                                        )
                                                                                         col1
                                                                                      )
                                                                                       (
                                                                                        + (
                                                                                          _add (
                                                                                            * (
                                                                                              fmod (
                                                                                                _add row1 4
                                                                                              )
                                                                                               5
                                                                                            )
                                                                                             5
                                                                                          )
                                                                                           col1
                                                                                        )
                                                                                         1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? table
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref table (
                                                                                        _add (
                                                                                          * (
                                                                                            fmod (
                                                                                              _add row1 4
                                                                                            )
                                                                                             5
                                                                                          )
                                                                                           5
                                                                                        )
                                                                                         col1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref table (
                                                                                        _add (
                                                                                          * (
                                                                                            fmod (
                                                                                              _add row1 4
                                                                                            )
                                                                                             5
                                                                                          )
                                                                                           5
                                                                                        )
                                                                                         col1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              set! plain (
                                                                                string-append plain (
                                                                                  cond (
                                                                                    (
                                                                                      string? table
                                                                                    )
                                                                                     (
                                                                                      _substring table (
                                                                                        _add (
                                                                                          * (
                                                                                            fmod (
                                                                                              _add row2 4
                                                                                            )
                                                                                             5
                                                                                          )
                                                                                           5
                                                                                        )
                                                                                         col2
                                                                                      )
                                                                                       (
                                                                                        + (
                                                                                          _add (
                                                                                            * (
                                                                                              fmod (
                                                                                                _add row2 4
                                                                                              )
                                                                                               5
                                                                                            )
                                                                                             5
                                                                                          )
                                                                                           col2
                                                                                        )
                                                                                         1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? table
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref table (
                                                                                        _add (
                                                                                          * (
                                                                                            fmod (
                                                                                              _add row2 4
                                                                                            )
                                                                                             5
                                                                                          )
                                                                                           5
                                                                                        )
                                                                                         col2
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref table (
                                                                                        _add (
                                                                                          * (
                                                                                            fmod (
                                                                                              _add row2 4
                                                                                            )
                                                                                             5
                                                                                          )
                                                                                           5
                                                                                        )
                                                                                         col2
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            begin (
                                                                              set! plain (
                                                                                string-append plain (
                                                                                  cond (
                                                                                    (
                                                                                      string? table
                                                                                    )
                                                                                     (
                                                                                      _substring table (
                                                                                        _add (
                                                                                          * row1 5
                                                                                        )
                                                                                         col2
                                                                                      )
                                                                                       (
                                                                                        + (
                                                                                          _add (
                                                                                            * row1 5
                                                                                          )
                                                                                           col2
                                                                                        )
                                                                                         1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? table
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref table (
                                                                                        _add (
                                                                                          * row1 5
                                                                                        )
                                                                                         col2
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref table (
                                                                                        _add (
                                                                                          * row1 5
                                                                                        )
                                                                                         col2
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              set! plain (
                                                                                string-append plain (
                                                                                  cond (
                                                                                    (
                                                                                      string? table
                                                                                    )
                                                                                     (
                                                                                      _substring table (
                                                                                        _add (
                                                                                          * row2 5
                                                                                        )
                                                                                         col1
                                                                                      )
                                                                                       (
                                                                                        + (
                                                                                          _add (
                                                                                            * row2 5
                                                                                          )
                                                                                           col1
                                                                                        )
                                                                                         1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? table
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref table (
                                                                                        _add (
                                                                                          * row2 5
                                                                                        )
                                                                                         col1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref table (
                                                                                        _add (
                                                                                          * row2 5
                                                                                        )
                                                                                         col1
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
                                                                        set! i (
                                                                          + i 2
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
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
                        ret20 plain
                      )
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
            ret23
          )
           (
            begin (
              _display (
                if (
                  string? "Encoded:"
                )
                 "Encoded:" (
                  to-str "Encoded:"
                )
              )
            )
             (
              _display " "
            )
             (
              _display (
                if (
                  string? (
                    encode "BYE AND THANKS" "GREETING"
                  )
                )
                 (
                  encode "BYE AND THANKS" "GREETING"
                )
                 (
                  to-str (
                    encode "BYE AND THANKS" "GREETING"
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
                  string? "Decoded:"
                )
                 "Decoded:" (
                  to-str "Decoded:"
                )
              )
            )
             (
              _display " "
            )
             (
              _display (
                if (
                  string? (
                    decode "CXRBANRLBALQ" "GREETING"
                  )
                )
                 (
                  decode "CXRBANRLBALQ" "GREETING"
                )
                 (
                  to-str (
                    decode "CXRBANRLBALQ" "GREETING"
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
     (
      main
    )
     (
      let (
        (
          end25 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur26 (
              quotient (
                * (
                  - end25 start24
                )
                 1000000
              )
               jps27
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur26
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
