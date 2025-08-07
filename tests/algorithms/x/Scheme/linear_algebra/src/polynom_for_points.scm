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
      start36 (
        current-jiffy
      )
    )
     (
      jps39 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        contains_int xs x
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
                                  equal? (
                                    list-ref-safe xs i
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
        split s sep
      )
       (
        call/cc (
          lambda (
            ret4
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
                    current ""
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
                                        _len s
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            ch (
                                              _substring s i (
                                                + i 1
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              string=? ch sep
                                            )
                                             (
                                              begin (
                                                set! res (
                                                  append res (
                                                    _list current
                                                  )
                                                )
                                              )
                                               (
                                                set! current ""
                                              )
                                            )
                                             (
                                              begin (
                                                set! current (
                                                  string-append current ch
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
                        set! res (
                          append res (
                            _list current
                          )
                        )
                      )
                       (
                        ret4 res
                      )
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
        pow_int_float base exp
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            let (
              (
                result 1.0
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
                                  < i exp
                                )
                                 (
                                  begin (
                                    set! result (
                                      * result (
                                        + 0.0 base
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
                    ret7 result
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
        points_to_polynomial coordinates
      )
       (
        call/cc (
          lambda (
            ret10
          )
           (
            begin (
              if (
                equal? (
                  _len coordinates
                )
                 0
              )
               (
                begin (
                  panic "The program cannot work out a fitting polynomial."
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
                                  _len coordinates
                                )
                              )
                               (
                                begin (
                                  if (
                                    not (
                                      equal? (
                                        _len (
                                          list-ref-safe coordinates i
                                        )
                                      )
                                       2
                                    )
                                  )
                                   (
                                    begin (
                                      panic "The program cannot work out a fitting polynomial."
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
                  let (
                    (
                      j 0
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
                                    < j (
                                      _len coordinates
                                    )
                                  )
                                   (
                                    begin (
                                      let (
                                        (
                                          k (
                                            + j 1
                                          )
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
                                                        < k (
                                                          _len coordinates
                                                        )
                                                      )
                                                       (
                                                        begin (
                                                          if (
                                                            and (
                                                              equal? (
                                                                cond (
                                                                  (
                                                                    string? (
                                                                      list-ref-safe coordinates j
                                                                    )
                                                                  )
                                                                   (
                                                                    _substring (
                                                                      list-ref-safe coordinates j
                                                                    )
                                                                     0 (
                                                                      + 0 1
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  (
                                                                    hash-table? (
                                                                      list-ref-safe coordinates j
                                                                    )
                                                                  )
                                                                   (
                                                                    hash-table-ref (
                                                                      list-ref-safe coordinates j
                                                                    )
                                                                     0
                                                                  )
                                                                )
                                                                 (
                                                                  else (
                                                                    list-ref-safe (
                                                                      list-ref-safe coordinates j
                                                                    )
                                                                     0
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                cond (
                                                                  (
                                                                    string? (
                                                                      list-ref-safe coordinates k
                                                                    )
                                                                  )
                                                                   (
                                                                    _substring (
                                                                      list-ref-safe coordinates k
                                                                    )
                                                                     0 (
                                                                      + 0 1
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  (
                                                                    hash-table? (
                                                                      list-ref-safe coordinates k
                                                                    )
                                                                  )
                                                                   (
                                                                    hash-table-ref (
                                                                      list-ref-safe coordinates k
                                                                    )
                                                                     0
                                                                  )
                                                                )
                                                                 (
                                                                  else (
                                                                    list-ref-safe (
                                                                      list-ref-safe coordinates k
                                                                    )
                                                                     0
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             (
                                                              equal? (
                                                                cond (
                                                                  (
                                                                    string? (
                                                                      list-ref-safe coordinates j
                                                                    )
                                                                  )
                                                                   (
                                                                    _substring (
                                                                      list-ref-safe coordinates j
                                                                    )
                                                                     1 (
                                                                      + 1 1
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  (
                                                                    hash-table? (
                                                                      list-ref-safe coordinates j
                                                                    )
                                                                  )
                                                                   (
                                                                    hash-table-ref (
                                                                      list-ref-safe coordinates j
                                                                    )
                                                                     1
                                                                  )
                                                                )
                                                                 (
                                                                  else (
                                                                    list-ref-safe (
                                                                      list-ref-safe coordinates j
                                                                    )
                                                                     1
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                cond (
                                                                  (
                                                                    string? (
                                                                      list-ref-safe coordinates k
                                                                    )
                                                                  )
                                                                   (
                                                                    _substring (
                                                                      list-ref-safe coordinates k
                                                                    )
                                                                     1 (
                                                                      + 1 1
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  (
                                                                    hash-table? (
                                                                      list-ref-safe coordinates k
                                                                    )
                                                                  )
                                                                   (
                                                                    hash-table-ref (
                                                                      list-ref-safe coordinates k
                                                                    )
                                                                     1
                                                                  )
                                                                )
                                                                 (
                                                                  else (
                                                                    list-ref-safe (
                                                                      list-ref-safe coordinates k
                                                                    )
                                                                     1
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                           (
                                                            begin (
                                                              panic "The program cannot work out a fitting polynomial."
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
                                          set! j (
                                            + j 1
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
                      let (
                        (
                          set_x (
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
                                          _len coordinates
                                        )
                                      )
                                       (
                                        begin (
                                          let (
                                            (
                                              x_val (
                                                cond (
                                                  (
                                                    string? (
                                                      list-ref-safe coordinates i
                                                    )
                                                  )
                                                   (
                                                    _substring (
                                                      list-ref-safe coordinates i
                                                    )
                                                     0 (
                                                      + 0 1
                                                    )
                                                  )
                                                )
                                                 (
                                                  (
                                                    hash-table? (
                                                      list-ref-safe coordinates i
                                                    )
                                                  )
                                                   (
                                                    hash-table-ref (
                                                      list-ref-safe coordinates i
                                                    )
                                                     0
                                                  )
                                                )
                                                 (
                                                  else (
                                                    list-ref-safe (
                                                      list-ref-safe coordinates i
                                                    )
                                                     0
                                                  )
                                                )
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              if (
                                                not (
                                                  contains_int set_x x_val
                                                )
                                              )
                                               (
                                                begin (
                                                  set! set_x (
                                                    append set_x (
                                                      _list x_val
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
                          if (
                            equal? (
                              _len set_x
                            )
                             1
                          )
                           (
                            begin (
                              ret10 (
                                string-append "x=" (
                                  to-str-space (
                                    cond (
                                      (
                                        string? (
                                          list-ref-safe coordinates 0
                                        )
                                      )
                                       (
                                        _substring (
                                          list-ref-safe coordinates 0
                                        )
                                         0 (
                                          + 0 1
                                        )
                                      )
                                    )
                                     (
                                      (
                                        hash-table? (
                                          list-ref-safe coordinates 0
                                        )
                                      )
                                       (
                                        hash-table-ref (
                                          list-ref-safe coordinates 0
                                        )
                                         0
                                      )
                                    )
                                     (
                                      else (
                                        list-ref-safe (
                                          list-ref-safe coordinates 0
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
                          if (
                            not (
                              equal? (
                                _len set_x
                              )
                               (
                                _len coordinates
                              )
                            )
                          )
                           (
                            begin (
                              panic "The program cannot work out a fitting polynomial."
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
                              n (
                                _len coordinates
                              )
                            )
                          )
                           (
                            begin (
                              let (
                                (
                                  matrix (
                                    _list
                                  )
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
                                                    < row n
                                                  )
                                                   (
                                                    begin (
                                                      let (
                                                        (
                                                          line (
                                                            _list
                                                          )
                                                        )
                                                      )
                                                       (
                                                        begin (
                                                          let (
                                                            (
                                                              col 0
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
                                                                            < col n
                                                                          )
                                                                           (
                                                                            begin (
                                                                              let (
                                                                                (
                                                                                  power (
                                                                                    pow_int_float (
                                                                                      cond (
                                                                                        (
                                                                                          string? (
                                                                                            list-ref-safe coordinates row
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          _substring (
                                                                                            list-ref-safe coordinates row
                                                                                          )
                                                                                           0 (
                                                                                            + 0 1
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        (
                                                                                          hash-table? (
                                                                                            list-ref-safe coordinates row
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          hash-table-ref (
                                                                                            list-ref-safe coordinates row
                                                                                          )
                                                                                           0
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        else (
                                                                                          list-ref-safe (
                                                                                            list-ref-safe coordinates row
                                                                                          )
                                                                                           0
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      - n (
                                                                                        + col 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                begin (
                                                                                  set! line (
                                                                                    append line (
                                                                                      _list power
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
                                                              set! matrix (
                                                                append matrix (
                                                                  _list line
                                                                )
                                                              )
                                                            )
                                                             (
                                                              set! row (
                                                                + row 1
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
                                      let (
                                        (
                                          vector (
                                            _list
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          set! row 0
                                        )
                                         (
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
                                                        < row n
                                                      )
                                                       (
                                                        begin (
                                                          set! vector (
                                                            append vector (
                                                              _list (
                                                                + 0.0 (
                                                                  cond (
                                                                    (
                                                                      string? (
                                                                        list-ref-safe coordinates row
                                                                      )
                                                                    )
                                                                     (
                                                                      _substring (
                                                                        list-ref-safe coordinates row
                                                                      )
                                                                       1 (
                                                                        + 1 1
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    (
                                                                      hash-table? (
                                                                        list-ref-safe coordinates row
                                                                      )
                                                                    )
                                                                     (
                                                                      hash-table-ref (
                                                                        list-ref-safe coordinates row
                                                                      )
                                                                       1
                                                                    )
                                                                  )
                                                                   (
                                                                    else (
                                                                      list-ref-safe (
                                                                        list-ref-safe coordinates row
                                                                      )
                                                                       1
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                         (
                                                          set! row (
                                                            + row 1
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
                                          let (
                                            (
                                              count 0
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
                                                            < count n
                                                          )
                                                           (
                                                            begin (
                                                              let (
                                                                (
                                                                  number 0
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
                                                                                < number n
                                                                              )
                                                                               (
                                                                                begin (
                                                                                  if (
                                                                                    not (
                                                                                      equal? count number
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    begin (
                                                                                      let (
                                                                                        (
                                                                                          fraction (
                                                                                            _div (
                                                                                              cond (
                                                                                                (
                                                                                                  string? (
                                                                                                    list-ref-safe matrix number
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  _substring (
                                                                                                    list-ref-safe matrix number
                                                                                                  )
                                                                                                   count (
                                                                                                    + count 1
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                (
                                                                                                  hash-table? (
                                                                                                    list-ref-safe matrix number
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  hash-table-ref (
                                                                                                    list-ref-safe matrix number
                                                                                                  )
                                                                                                   count
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                else (
                                                                                                  list-ref-safe (
                                                                                                    list-ref-safe matrix number
                                                                                                  )
                                                                                                   count
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              cond (
                                                                                                (
                                                                                                  string? (
                                                                                                    list-ref-safe matrix count
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  _substring (
                                                                                                    list-ref-safe matrix count
                                                                                                  )
                                                                                                   count (
                                                                                                    + count 1
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                (
                                                                                                  hash-table? (
                                                                                                    list-ref-safe matrix count
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  hash-table-ref (
                                                                                                    list-ref-safe matrix count
                                                                                                  )
                                                                                                   count
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                else (
                                                                                                  list-ref-safe (
                                                                                                    list-ref-safe matrix count
                                                                                                  )
                                                                                                   count
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
                                                                                              cc 0
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            begin (
                                                                                              call/cc (
                                                                                                lambda (
                                                                                                  break30
                                                                                                )
                                                                                                 (
                                                                                                  letrec (
                                                                                                    (
                                                                                                      loop29 (
                                                                                                        lambda (
                                                                                                          
                                                                                                        )
                                                                                                         (
                                                                                                          if (
                                                                                                            < cc n
                                                                                                          )
                                                                                                           (
                                                                                                            begin (
                                                                                                              list-set! (
                                                                                                                list-ref-safe matrix number
                                                                                                              )
                                                                                                               cc (
                                                                                                                - (
                                                                                                                  cond (
                                                                                                                    (
                                                                                                                      string? (
                                                                                                                        list-ref-safe matrix number
                                                                                                                      )
                                                                                                                    )
                                                                                                                     (
                                                                                                                      _substring (
                                                                                                                        list-ref-safe matrix number
                                                                                                                      )
                                                                                                                       cc (
                                                                                                                        + cc 1
                                                                                                                      )
                                                                                                                    )
                                                                                                                  )
                                                                                                                   (
                                                                                                                    (
                                                                                                                      hash-table? (
                                                                                                                        list-ref-safe matrix number
                                                                                                                      )
                                                                                                                    )
                                                                                                                     (
                                                                                                                      hash-table-ref (
                                                                                                                        list-ref-safe matrix number
                                                                                                                      )
                                                                                                                       cc
                                                                                                                    )
                                                                                                                  )
                                                                                                                   (
                                                                                                                    else (
                                                                                                                      list-ref-safe (
                                                                                                                        list-ref-safe matrix number
                                                                                                                      )
                                                                                                                       cc
                                                                                                                    )
                                                                                                                  )
                                                                                                                )
                                                                                                                 (
                                                                                                                  * (
                                                                                                                    cond (
                                                                                                                      (
                                                                                                                        string? (
                                                                                                                          list-ref-safe matrix count
                                                                                                                        )
                                                                                                                      )
                                                                                                                       (
                                                                                                                        _substring (
                                                                                                                          list-ref-safe matrix count
                                                                                                                        )
                                                                                                                         cc (
                                                                                                                          + cc 1
                                                                                                                        )
                                                                                                                      )
                                                                                                                    )
                                                                                                                     (
                                                                                                                      (
                                                                                                                        hash-table? (
                                                                                                                          list-ref-safe matrix count
                                                                                                                        )
                                                                                                                      )
                                                                                                                       (
                                                                                                                        hash-table-ref (
                                                                                                                          list-ref-safe matrix count
                                                                                                                        )
                                                                                                                         cc
                                                                                                                      )
                                                                                                                    )
                                                                                                                     (
                                                                                                                      else (
                                                                                                                        list-ref-safe (
                                                                                                                          list-ref-safe matrix count
                                                                                                                        )
                                                                                                                         cc
                                                                                                                      )
                                                                                                                    )
                                                                                                                  )
                                                                                                                   fraction
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              set! cc (
                                                                                                                + cc 1
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              loop29
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
                                                                                                    loop29
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              list-set! vector number (
                                                                                                - (
                                                                                                  list-ref-safe vector number
                                                                                                )
                                                                                                 (
                                                                                                  * (
                                                                                                    list-ref-safe vector count
                                                                                                  )
                                                                                                   fraction
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
                                                                                  set! number (
                                                                                    + number 1
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
                                                                  set! count (
                                                                    + count 1
                                                                  )
                                                                )
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
                                              let (
                                                (
                                                  solution (
                                                    _list
                                                  )
                                                )
                                              )
                                               (
                                                begin (
                                                  set! count 0
                                                )
                                                 (
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
                                                                < count n
                                                              )
                                                               (
                                                                begin (
                                                                  let (
                                                                    (
                                                                      value (
                                                                        _div (
                                                                          list-ref-safe vector count
                                                                        )
                                                                         (
                                                                          cond (
                                                                            (
                                                                              string? (
                                                                                list-ref-safe matrix count
                                                                              )
                                                                            )
                                                                             (
                                                                              _substring (
                                                                                list-ref-safe matrix count
                                                                              )
                                                                               count (
                                                                                + count 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            (
                                                                              hash-table? (
                                                                                list-ref-safe matrix count
                                                                              )
                                                                            )
                                                                             (
                                                                              hash-table-ref (
                                                                                list-ref-safe matrix count
                                                                              )
                                                                               count
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref-safe (
                                                                                list-ref-safe matrix count
                                                                              )
                                                                               count
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    begin (
                                                                      set! solution (
                                                                        append solution (
                                                                          _list (
                                                                            to-str-space value
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      set! count (
                                                                        + count 1
                                                                      )
                                                                    )
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
                                                  let (
                                                    (
                                                      solved "f(x)="
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      set! count 0
                                                    )
                                                     (
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
                                                                    < count n
                                                                  )
                                                                   (
                                                                    begin (
                                                                      let (
                                                                        (
                                                                          parts (
                                                                            split (
                                                                              list-ref-safe solution count
                                                                            )
                                                                             "e"
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        begin (
                                                                          let (
                                                                            (
                                                                              coeff (
                                                                                list-ref-safe solution count
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            begin (
                                                                              if (
                                                                                > (
                                                                                  _len parts
                                                                                )
                                                                                 1
                                                                              )
                                                                               (
                                                                                begin (
                                                                                  set! coeff (
                                                                                    string-append (
                                                                                      string-append (
                                                                                        cond (
                                                                                          (
                                                                                            string? parts
                                                                                          )
                                                                                           (
                                                                                            _substring parts 0 (
                                                                                              + 0 1
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          (
                                                                                            hash-table? parts
                                                                                          )
                                                                                           (
                                                                                            hash-table-ref parts 0
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          else (
                                                                                            list-ref-safe parts 0
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       "*10^"
                                                                                    )
                                                                                     (
                                                                                      cond (
                                                                                        (
                                                                                          string? parts
                                                                                        )
                                                                                         (
                                                                                          _substring parts 1 (
                                                                                            + 1 1
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        (
                                                                                          hash-table? parts
                                                                                        )
                                                                                         (
                                                                                          hash-table-ref parts 1
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        else (
                                                                                          list-ref-safe parts 1
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
                                                                              set! solved (
                                                                                string-append (
                                                                                  string-append (
                                                                                    string-append (
                                                                                      string-append solved "x^"
                                                                                    )
                                                                                     (
                                                                                      to-str-space (
                                                                                        - n (
                                                                                          + count 1
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   "*"
                                                                                )
                                                                                 coeff
                                                                              )
                                                                            )
                                                                             (
                                                                              if (
                                                                                not (
                                                                                  equal? (
                                                                                    + count 1
                                                                                  )
                                                                                   n
                                                                                )
                                                                              )
                                                                               (
                                                                                begin (
                                                                                  set! solved (
                                                                                    string-append solved "+"
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                quote (
                                                                                  
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              set! count (
                                                                                + count 1
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
                                                      ret10 solved
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
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
            ret35
          )
           (
            begin (
              _display (
                if (
                  string? (
                    points_to_polynomial (
                      _list (
                        _list 1 0
                      )
                       (
                        _list 2 0
                      )
                       (
                        _list 3 0
                      )
                    )
                  )
                )
                 (
                  points_to_polynomial (
                    _list (
                      _list 1 0
                    )
                     (
                      _list 2 0
                    )
                     (
                      _list 3 0
                    )
                  )
                )
                 (
                  to-str (
                    points_to_polynomial (
                      _list (
                        _list 1 0
                      )
                       (
                        _list 2 0
                      )
                       (
                        _list 3 0
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
                    points_to_polynomial (
                      _list (
                        _list 1 1
                      )
                       (
                        _list 2 1
                      )
                       (
                        _list 3 1
                      )
                    )
                  )
                )
                 (
                  points_to_polynomial (
                    _list (
                      _list 1 1
                    )
                     (
                      _list 2 1
                    )
                     (
                      _list 3 1
                    )
                  )
                )
                 (
                  to-str (
                    points_to_polynomial (
                      _list (
                        _list 1 1
                      )
                       (
                        _list 2 1
                      )
                       (
                        _list 3 1
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
                    points_to_polynomial (
                      _list (
                        _list 1 1
                      )
                       (
                        _list 2 4
                      )
                       (
                        _list 3 9
                      )
                    )
                  )
                )
                 (
                  points_to_polynomial (
                    _list (
                      _list 1 1
                    )
                     (
                      _list 2 4
                    )
                     (
                      _list 3 9
                    )
                  )
                )
                 (
                  to-str (
                    points_to_polynomial (
                      _list (
                        _list 1 1
                      )
                       (
                        _list 2 4
                      )
                       (
                        _list 3 9
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
                    points_to_polynomial (
                      _list (
                        _list 1 3
                      )
                       (
                        _list 2 6
                      )
                       (
                        _list 3 11
                      )
                    )
                  )
                )
                 (
                  points_to_polynomial (
                    _list (
                      _list 1 3
                    )
                     (
                      _list 2 6
                    )
                     (
                      _list 3 11
                    )
                  )
                )
                 (
                  to-str (
                    points_to_polynomial (
                      _list (
                        _list 1 3
                      )
                       (
                        _list 2 6
                      )
                       (
                        _list 3 11
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
                    points_to_polynomial (
                      _list (
                        _list 1 (
                          - 3
                        )
                      )
                       (
                        _list 2 (
                          - 6
                        )
                      )
                       (
                        _list 3 (
                          - 11
                        )
                      )
                    )
                  )
                )
                 (
                  points_to_polynomial (
                    _list (
                      _list 1 (
                        - 3
                      )
                    )
                     (
                      _list 2 (
                        - 6
                      )
                    )
                     (
                      _list 3 (
                        - 11
                      )
                    )
                  )
                )
                 (
                  to-str (
                    points_to_polynomial (
                      _list (
                        _list 1 (
                          - 3
                        )
                      )
                       (
                        _list 2 (
                          - 6
                        )
                      )
                       (
                        _list 3 (
                          - 11
                        )
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
                    points_to_polynomial (
                      _list (
                        _list 1 1
                      )
                       (
                        _list 1 2
                      )
                       (
                        _list 1 3
                      )
                    )
                  )
                )
                 (
                  points_to_polynomial (
                    _list (
                      _list 1 1
                    )
                     (
                      _list 1 2
                    )
                     (
                      _list 1 3
                    )
                  )
                )
                 (
                  to-str (
                    points_to_polynomial (
                      _list (
                        _list 1 1
                      )
                       (
                        _list 1 2
                      )
                       (
                        _list 1 3
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
                    points_to_polynomial (
                      _list (
                        _list 1 5
                      )
                       (
                        _list 2 2
                      )
                       (
                        _list 3 9
                      )
                    )
                  )
                )
                 (
                  points_to_polynomial (
                    _list (
                      _list 1 5
                    )
                     (
                      _list 2 2
                    )
                     (
                      _list 3 9
                    )
                  )
                )
                 (
                  to-str (
                    points_to_polynomial (
                      _list (
                        _list 1 5
                      )
                       (
                        _list 2 2
                      )
                       (
                        _list 3 9
                      )
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
     (
      main
    )
     (
      let (
        (
          end37 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur38 (
              quotient (
                * (
                  - end37 start36
                )
                 1000000
              )
               jps39
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur38
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
