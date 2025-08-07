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
      start29 (
        current-jiffy
      )
    )
     (
      jps32 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        min_int a b
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            begin (
              if (
                < a b
              )
               (
                begin (
                  ret1 a
                )
              )
               '(
                
              )
            )
             (
              ret1 b
            )
          )
        )
      )
    )
     (
      define (
        int_sqrt n
      )
       (
        call/cc (
          lambda (
            ret2
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
                              <= (
                                * (
                                  + r 1
                                )
                                 (
                                  + r 1
                                )
                              )
                               n
                            )
                             (
                              begin (
                                set! r (
                                  + r 1
                                )
                              )
                               (
                                loop3
                              )
                            )
                             '(
                              
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
                ret2 r
              )
            )
          )
        )
      )
    )
     (
      define (
        sieve n
      )
       (
        call/cc (
          lambda (
            ret5
          )
           (
            begin (
              if (
                <= n 0
              )
               (
                begin (
                  panic "Number must instead be a positive integer"
                )
              )
               '(
                
              )
            )
             (
              let (
                (
                  in_prime (
                    _list
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      start 2
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          end (
                            int_sqrt n
                          )
                        )
                      )
                       (
                        begin (
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
                                  i 0
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
                                              
                                            )
                                             (
                                              if (
                                                _lt i (
                                                  _add end 1
                                                )
                                              )
                                               (
                                                begin (
                                                  set! temp (
                                                    append temp (
                                                      _list 1
                                                    )
                                                  )
                                                )
                                                 (
                                                  set! i (
                                                    + i 1
                                                  )
                                                )
                                                 (
                                                  loop6
                                                )
                                              )
                                               '(
                                                
                                              )
                                            )
                                          )
                                        )
                                      )
                                       (
                                        loop6
                                      )
                                    )
                                  )
                                )
                                 (
                                  let (
                                    (
                                      prime (
                                        _list
                                      )
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
                                                    _le start end
                                                  )
                                                   (
                                                    begin (
                                                      if (
                                                        equal? (
                                                          list-ref-safe temp start
                                                        )
                                                         1
                                                      )
                                                       (
                                                        begin (
                                                          set! in_prime (
                                                            append in_prime (
                                                              _list start
                                                            )
                                                          )
                                                        )
                                                         (
                                                          let (
                                                            (
                                                              j (
                                                                * start start
                                                              )
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
                                                                            _le j end
                                                                          )
                                                                           (
                                                                            begin (
                                                                              list-set! temp j 0
                                                                            )
                                                                             (
                                                                              set! j (
                                                                                + j start
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
                                                          )
                                                        )
                                                      )
                                                       '(
                                                        
                                                      )
                                                    )
                                                     (
                                                      set! start (
                                                        + start 1
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
                                                      _len in_prime
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      set! prime (
                                                        append prime (
                                                          _list (
                                                            list-ref-safe in_prime i
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
                                      let (
                                        (
                                          low (
                                            _add end 1
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          let (
                                            (
                                              high (
                                                min_int (
                                                  * 2 end
                                                )
                                                 n
                                              )
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
                                                            _le low n
                                                          )
                                                           (
                                                            begin (
                                                              let (
                                                                (
                                                                  tempSeg (
                                                                    _list
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                begin (
                                                                  let (
                                                                    (
                                                                      size (
                                                                        _add (
                                                                          - high low
                                                                        )
                                                                         1
                                                                      )
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
                                                                                        _lt k size
                                                                                      )
                                                                                       (
                                                                                        begin (
                                                                                          set! tempSeg (
                                                                                            append tempSeg (
                                                                                              _list 1
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          set! k (
                                                                                            + k 1
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
                                                                          let (
                                                                            (
                                                                              idx 0
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
                                                                                            < idx (
                                                                                              _len in_prime
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            begin (
                                                                                              let (
                                                                                                (
                                                                                                  each (
                                                                                                    list-ref-safe in_prime idx
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                begin (
                                                                                                  let (
                                                                                                    (
                                                                                                      t (
                                                                                                        * (
                                                                                                          _div low each
                                                                                                        )
                                                                                                         each
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    begin (
                                                                                                      if (
                                                                                                        _lt t low
                                                                                                      )
                                                                                                       (
                                                                                                        begin (
                                                                                                          set! t (
                                                                                                            _add t each
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       '(
                                                                                                        
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      let (
                                                                                                        (
                                                                                                          j2 t
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
                                                                                                                        _le j2 high
                                                                                                                      )
                                                                                                                       (
                                                                                                                        begin (
                                                                                                                          list-set! tempSeg (
                                                                                                                            - j2 low
                                                                                                                          )
                                                                                                                           0
                                                                                                                        )
                                                                                                                         (
                                                                                                                          set! j2 (
                                                                                                                            _add j2 each
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
                                                                              let (
                                                                                (
                                                                                  j3 0
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
                                                                                                < j3 (
                                                                                                  _len tempSeg
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                begin (
                                                                                                  if (
                                                                                                    equal? (
                                                                                                      list-ref-safe tempSeg j3
                                                                                                    )
                                                                                                     1
                                                                                                  )
                                                                                                   (
                                                                                                    begin (
                                                                                                      set! prime (
                                                                                                        append prime (
                                                                                                          _list (
                                                                                                            _add j3 low
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                   '(
                                                                                                    
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  set! j3 (
                                                                                                    + j3 1
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
                                                                                  set! low (
                                                                                    _add high 1
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  set! high (
                                                                                    min_int (
                                                                                      _add high end
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
                                                             (
                                                              loop14
                                                            )
                                                          )
                                                           '(
                                                            
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
                                              ret5 prime
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
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
        lists_equal a b
      )
       (
        call/cc (
          lambda (
            ret24
          )
           (
            begin (
              if (
                not (
                  equal? (
                    _len a
                  )
                   (
                    _len b
                  )
                )
              )
               (
                begin (
                  ret24 #f
                )
              )
               '(
                
              )
            )
             (
              let (
                (
                  m 0
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
                                < m (
                                  _len a
                                )
                              )
                               (
                                begin (
                                  if (
                                    not (
                                      equal? (
                                        list-ref-safe a m
                                      )
                                       (
                                        list-ref-safe b m
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      ret24 #f
                                    )
                                  )
                                   '(
                                    
                                  )
                                )
                                 (
                                  set! m (
                                    + m 1
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
                  ret24 #t
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        test_sieve
      )
       (
        call/cc (
          lambda (
            ret27
          )
           (
            let (
              (
                e1 (
                  sieve 8
                )
              )
            )
             (
              begin (
                if (
                  not (
                    lists_equal e1 (
                      _list 2 3 5 7
                    )
                  )
                )
                 (
                  begin (
                    panic "sieve(8) failed"
                  )
                )
                 '(
                  
                )
              )
               (
                let (
                  (
                    e2 (
                      sieve 27
                    )
                  )
                )
                 (
                  begin (
                    if (
                      not (
                        lists_equal e2 (
                          _list 2 3 5 7 11 13 17 19 23
                        )
                      )
                    )
                     (
                      begin (
                        panic "sieve(27) failed"
                      )
                    )
                     '(
                      
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
            ret28
          )
           (
            begin (
              test_sieve
            )
             (
              _display (
                if (
                  string? (
                    to-str-space (
                      sieve 30
                    )
                  )
                )
                 (
                  to-str-space (
                    sieve 30
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      sieve 30
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
          end30 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur31 (
              quotient (
                * (
                  - end30 start29
                )
                 1000000
              )
               jps32
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur31
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
