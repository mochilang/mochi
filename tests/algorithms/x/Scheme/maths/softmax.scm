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
        exp_approx x
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                term 1.0
              )
            )
             (
              begin (
                let (
                  (
                    sum 1.0
                  )
                )
                 (
                  begin (
                    let (
                      (
                        i 1
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
                                      < i 20
                                    )
                                     (
                                      begin (
                                        set! term (
                                          _div (
                                            * term x
                                          )
                                           (
                                            + 0.0 i
                                          )
                                        )
                                      )
                                       (
                                        set! sum (
                                          + sum term
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
                        ret1 sum
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
        softmax vec
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            let (
              (
                exps (
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
                                    _len vec
                                  )
                                )
                                 (
                                  begin (
                                    set! exps (
                                      append exps (
                                        _list (
                                          exp_approx (
                                            list-ref-safe vec i
                                          )
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
                    let (
                      (
                        total 0.0
                      )
                    )
                     (
                      begin (
                        set! i 0
                      )
                       (
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
                                      < i (
                                        _len exps
                                      )
                                    )
                                     (
                                      begin (
                                        set! total (
                                          + total (
                                            list-ref-safe exps i
                                          )
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
                        let (
                          (
                            result (
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
                                          < i (
                                            _len exps
                                          )
                                        )
                                         (
                                          begin (
                                            set! result (
                                              append result (
                                                _list (
                                                  _div (
                                                    list-ref-safe exps i
                                                  )
                                                   total
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
                                         '(
                                          
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
                            ret4 result
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
        abs_val x
      )
       (
        call/cc (
          lambda (
            ret11
          )
           (
            begin (
              if (
                < x 0.0
              )
               (
                begin (
                  ret11 (
                    - x
                  )
                )
              )
               '(
                
              )
            )
             (
              ret11 x
            )
          )
        )
      )
    )
     (
      define (
        approx_equal a b
      )
       (
        call/cc (
          lambda (
            ret12
          )
           (
            ret12 (
              _lt (
                abs_val (
                  - a b
                )
              )
               0.0001
            )
          )
        )
      )
    )
     (
      define (
        test_softmax
      )
       (
        call/cc (
          lambda (
            ret13
          )
           (
            let (
              (
                s1 (
                  softmax (
                    _list 1.0 2.0 3.0 4.0
                  )
                )
              )
            )
             (
              begin (
                let (
                  (
                    sum1 0.0
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
                                      < i (
                                        _len s1
                                      )
                                    )
                                     (
                                      begin (
                                        set! sum1 (
                                          _add sum1 (
                                            cond (
                                              (
                                                string? s1
                                              )
                                               (
                                                _substring s1 i (
                                                  + i 1
                                                )
                                              )
                                            )
                                             (
                                              (
                                                hash-table? s1
                                              )
                                               (
                                                hash-table-ref s1 i
                                              )
                                            )
                                             (
                                              else (
                                                list-ref-safe s1 i
                                              )
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
                        if (
                          not (
                            approx_equal sum1 1.0
                          )
                        )
                         (
                          begin (
                            panic "sum test failed"
                          )
                        )
                         '(
                          
                        )
                      )
                       (
                        let (
                          (
                            s2 (
                              softmax (
                                _list 5.0 5.0
                              )
                            )
                          )
                        )
                         (
                          begin (
                            if (
                              not (
                                and (
                                  approx_equal (
                                    cond (
                                      (
                                        string? s2
                                      )
                                       (
                                        _substring s2 0 (
                                          + 0 1
                                        )
                                      )
                                    )
                                     (
                                      (
                                        hash-table? s2
                                      )
                                       (
                                        hash-table-ref s2 0
                                      )
                                    )
                                     (
                                      else (
                                        list-ref-safe s2 0
                                      )
                                    )
                                  )
                                   0.5
                                )
                                 (
                                  approx_equal (
                                    cond (
                                      (
                                        string? s2
                                      )
                                       (
                                        _substring s2 1 (
                                          + 1 1
                                        )
                                      )
                                    )
                                     (
                                      (
                                        hash-table? s2
                                      )
                                       (
                                        hash-table-ref s2 1
                                      )
                                    )
                                     (
                                      else (
                                        list-ref-safe s2 1
                                      )
                                    )
                                  )
                                   0.5
                                )
                              )
                            )
                             (
                              begin (
                                panic "equal elements test failed"
                              )
                            )
                             '(
                              
                            )
                          )
                           (
                            let (
                              (
                                s3 (
                                  softmax (
                                    _list 0.0
                                  )
                                )
                              )
                            )
                             (
                              begin (
                                if (
                                  not (
                                    approx_equal (
                                      cond (
                                        (
                                          string? s3
                                        )
                                         (
                                          _substring s3 0 (
                                            + 0 1
                                          )
                                        )
                                      )
                                       (
                                        (
                                          hash-table? s3
                                        )
                                         (
                                          hash-table-ref s3 0
                                        )
                                      )
                                       (
                                        else (
                                          list-ref-safe s3 0
                                        )
                                      )
                                    )
                                     1.0
                                  )
                                )
                                 (
                                  begin (
                                    panic "zero vector test failed"
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
            ret16
          )
           (
            begin (
              test_softmax
            )
             (
              _display (
                if (
                  string? (
                    to-str-space (
                      softmax (
                        _list 1.0 2.0 3.0 4.0
                      )
                    )
                  )
                )
                 (
                  to-str-space (
                    softmax (
                      _list 1.0 2.0 3.0 4.0
                    )
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      softmax (
                        _list 1.0 2.0 3.0 4.0
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
