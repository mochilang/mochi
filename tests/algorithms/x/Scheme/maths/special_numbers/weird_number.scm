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
      start25 (
        current-jiffy
      )
    )
     (
      jps28 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        bubble_sort xs
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                arr xs
              )
            )
             (
              begin (
                let (
                  (
                    n (
                      _len arr
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
                                      < i n
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
                                                          < j (
                                                            - (
                                                              - n i
                                                            )
                                                             1
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            if (
                                                              > (
                                                                list-ref-safe arr j
                                                              )
                                                               (
                                                                list-ref-safe arr (
                                                                  + j 1
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                let (
                                                                  (
                                                                    tmp (
                                                                      list-ref-safe arr j
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    list-set! arr j (
                                                                      list-ref-safe arr (
                                                                        + j 1
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    list-set! arr (
                                                                      + j 1
                                                                    )
                                                                     tmp
                                                                  )
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
                        ret1 arr
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
        factors num
      )
       (
        call/cc (
          lambda (
            ret6
          )
           (
            let (
              (
                values (
                  _list 1
                )
              )
            )
             (
              begin (
                let (
                  (
                    i 2
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
                                  <= (
                                    * i i
                                  )
                                   num
                                )
                                 (
                                  begin (
                                    if (
                                      equal? (
                                        _mod num i
                                      )
                                       0
                                    )
                                     (
                                      begin (
                                        set! values (
                                          append values (
                                            _list i
                                          )
                                        )
                                      )
                                       (
                                        let (
                                          (
                                            d (
                                              _div num i
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              not (
                                                equal? d i
                                              )
                                            )
                                             (
                                              begin (
                                                set! values (
                                                  append values (
                                                    _list d
                                                  )
                                                )
                                              )
                                            )
                                             '(
                                              
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
                                    loop7
                                  )
                                )
                                 '(
                                  
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
                    ret6 (
                      bubble_sort values
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
        sum_list xs
      )
       (
        call/cc (
          lambda (
            ret9
          )
           (
            let (
              (
                total 0
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
                                    _len xs
                                  )
                                )
                                 (
                                  begin (
                                    set! total (
                                      + total (
                                        list-ref-safe xs i
                                      )
                                    )
                                  )
                                   (
                                    set! i (
                                      + i 1
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
                    ret9 total
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
        abundant n
      )
       (
        call/cc (
          lambda (
            ret12
          )
           (
            ret12 (
              _gt (
                sum_list (
                  factors n
                )
              )
               n
            )
          )
        )
      )
    )
     (
      define (
        semi_perfect number
      )
       (
        call/cc (
          lambda (
            ret13
          )
           (
            begin (
              if (
                <= number 0
              )
               (
                begin (
                  ret13 #t
                )
              )
               '(
                
              )
            )
             (
              let (
                (
                  values (
                    factors number
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      possible (
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
                                        <= j number
                                      )
                                       (
                                        begin (
                                          set! possible (
                                            append possible (
                                              _list (
                                                equal? j 0
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
                          let (
                            (
                              idx 0
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
                                            < idx (
                                              _len values
                                            )
                                          )
                                           (
                                            begin (
                                              let (
                                                (
                                                  v (
                                                    cond (
                                                      (
                                                        string? values
                                                      )
                                                       (
                                                        _substring values idx (
                                                          + idx 1
                                                        )
                                                      )
                                                    )
                                                     (
                                                      (
                                                        hash-table? values
                                                      )
                                                       (
                                                        hash-table-ref values idx
                                                      )
                                                    )
                                                     (
                                                      else (
                                                        list-ref-safe values idx
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                begin (
                                                  let (
                                                    (
                                                      s number
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
                                                                    _ge s v
                                                                  )
                                                                   (
                                                                    begin (
                                                                      if (
                                                                        list-ref-safe possible (
                                                                          - s v
                                                                        )
                                                                      )
                                                                       (
                                                                        begin (
                                                                          list-set! possible s #t
                                                                        )
                                                                      )
                                                                       '(
                                                                        
                                                                      )
                                                                    )
                                                                     (
                                                                      set! s (
                                                                        - s 1
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
                                                      set! idx (
                                                        + idx 1
                                                      )
                                                    )
                                                  )
                                                )
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
                              ret13 (
                                list-ref-safe possible number
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
        weird number
      )
       (
        call/cc (
          lambda (
            ret20
          )
           (
            ret20 (
              and (
                abundant number
              )
               (
                eq? (
                  semi_perfect number
                )
                 #f
              )
            )
          )
        )
      )
    )
     (
      define (
        run_tests
      )
       (
        call/cc (
          lambda (
            ret21
          )
           (
            begin (
              if (
                not (
                  equal? (
                    factors 12
                  )
                   (
                    _list 1 2 3 4 6
                  )
                )
              )
               (
                begin (
                  panic "factors 12 failed"
                )
              )
               '(
                
              )
            )
             (
              if (
                not (
                  equal? (
                    factors 1
                  )
                   (
                    _list 1
                  )
                )
              )
               (
                begin (
                  panic "factors 1 failed"
                )
              )
               '(
                
              )
            )
             (
              if (
                not (
                  equal? (
                    factors 100
                  )
                   (
                    _list 1 2 4 5 10 20 25 50
                  )
                )
              )
               (
                begin (
                  panic "factors 100 failed"
                )
              )
               '(
                
              )
            )
             (
              if (
                not (
                  eq? (
                    abundant 0
                  )
                   #t
                )
              )
               (
                begin (
                  panic "abundant 0 failed"
                )
              )
               '(
                
              )
            )
             (
              if (
                not (
                  eq? (
                    abundant 1
                  )
                   #f
                )
              )
               (
                begin (
                  panic "abundant 1 failed"
                )
              )
               '(
                
              )
            )
             (
              if (
                not (
                  eq? (
                    abundant 12
                  )
                   #t
                )
              )
               (
                begin (
                  panic "abundant 12 failed"
                )
              )
               '(
                
              )
            )
             (
              if (
                not (
                  eq? (
                    abundant 13
                  )
                   #f
                )
              )
               (
                begin (
                  panic "abundant 13 failed"
                )
              )
               '(
                
              )
            )
             (
              if (
                not (
                  eq? (
                    abundant 20
                  )
                   #t
                )
              )
               (
                begin (
                  panic "abundant 20 failed"
                )
              )
               '(
                
              )
            )
             (
              if (
                not (
                  eq? (
                    semi_perfect 0
                  )
                   #t
                )
              )
               (
                begin (
                  panic "semi_perfect 0 failed"
                )
              )
               '(
                
              )
            )
             (
              if (
                not (
                  eq? (
                    semi_perfect 1
                  )
                   #t
                )
              )
               (
                begin (
                  panic "semi_perfect 1 failed"
                )
              )
               '(
                
              )
            )
             (
              if (
                not (
                  eq? (
                    semi_perfect 12
                  )
                   #t
                )
              )
               (
                begin (
                  panic "semi_perfect 12 failed"
                )
              )
               '(
                
              )
            )
             (
              if (
                not (
                  eq? (
                    semi_perfect 13
                  )
                   #f
                )
              )
               (
                begin (
                  panic "semi_perfect 13 failed"
                )
              )
               '(
                
              )
            )
             (
              if (
                not (
                  eq? (
                    weird 0
                  )
                   #f
                )
              )
               (
                begin (
                  panic "weird 0 failed"
                )
              )
               '(
                
              )
            )
             (
              if (
                not (
                  eq? (
                    weird 70
                  )
                   #t
                )
              )
               (
                begin (
                  panic "weird 70 failed"
                )
              )
               '(
                
              )
            )
             (
              if (
                not (
                  eq? (
                    weird 77
                  )
                   #f
                )
              )
               (
                begin (
                  panic "weird 77 failed"
                )
              )
               '(
                
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
            ret22
          )
           (
            begin (
              run_tests
            )
             (
              let (
                (
                  nums (
                    _list 69 70 71
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
                                      _len nums
                                    )
                                  )
                                   (
                                    begin (
                                      let (
                                        (
                                          n (
                                            list-ref-safe nums i
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          if (
                                            weird n
                                          )
                                           (
                                            begin (
                                              _display (
                                                if (
                                                  string? (
                                                    string-append (
                                                      to-str-space n
                                                    )
                                                     " is weird."
                                                  )
                                                )
                                                 (
                                                  string-append (
                                                    to-str-space n
                                                  )
                                                   " is weird."
                                                )
                                                 (
                                                  to-str (
                                                    string-append (
                                                      to-str-space n
                                                    )
                                                     " is weird."
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
                                                      to-str-space n
                                                    )
                                                     " is not weird."
                                                  )
                                                )
                                                 (
                                                  string-append (
                                                    to-str-space n
                                                  )
                                                   " is not weird."
                                                )
                                                 (
                                                  to-str (
                                                    string-append (
                                                      to-str-space n
                                                    )
                                                     " is not weird."
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              newline
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
          end26 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur27 (
              quotient (
                * (
                  - end26 start25
                )
                 1000000
              )
               jps28
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur27
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
