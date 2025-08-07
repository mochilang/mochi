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
      start20 (
        current-jiffy
      )
    )
     (
      jps23 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        abs x
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            begin (
              if (
                < x 0.0
              )
               (
                begin (
                  ret1 (
                    - x
                  )
                )
              )
               '(
                
              )
            )
             (
              ret1 x
            )
          )
        )
      )
    )
     (
      define (
        sqrtApprox x
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            begin (
              if (
                equal? x 0.0
              )
               (
                begin (
                  ret2 0.0
                )
              )
               '(
                
              )
            )
             (
              let (
                (
                  guess (
                    _div x 2.0
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
                                    < i 20
                                  )
                                   (
                                    begin (
                                      set! guess (
                                        _div (
                                          _add guess (
                                            _div x guess
                                          )
                                        )
                                         2.0
                                      )
                                    )
                                     (
                                      set! i (
                                        + i 1
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
                      ret2 guess
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
        dot a b
      )
       (
        call/cc (
          lambda (
            ret5
          )
           (
            let (
              (
                sum 0.0
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
                                  < i (
                                    _len a
                                  )
                                )
                                 (
                                  begin (
                                    set! sum (
                                      _add sum (
                                        * (
                                          list-ref-safe a i
                                        )
                                         (
                                          list-ref-safe b i
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
                    ret5 sum
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
        mat_vec_mult mat vec
      )
       (
        call/cc (
          lambda (
            ret8
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
                                    _len mat
                                  )
                                )
                                 (
                                  begin (
                                    set! res (
                                      append res (
                                        _list (
                                          dot (
                                            list-ref-safe mat i
                                          )
                                           vec
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
                    ret8 res
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
        norm vec
      )
       (
        call/cc (
          lambda (
            ret11
          )
           (
            let (
              (
                sum 0.0
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
                                    _len vec
                                  )
                                )
                                 (
                                  begin (
                                    set! sum (
                                      _add sum (
                                        * (
                                          list-ref-safe vec i
                                        )
                                         (
                                          list-ref-safe vec i
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
                        root (
                          sqrtApprox sum
                        )
                      )
                    )
                     (
                      begin (
                        ret11 root
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
        normalize vec
      )
       (
        call/cc (
          lambda (
            ret14
          )
           (
            let (
              (
                n (
                  norm vec
                )
              )
            )
             (
              begin (
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
                                        _len vec
                                      )
                                    )
                                     (
                                      begin (
                                        set! res (
                                          append res (
                                            _list (
                                              _div (
                                                list-ref-safe vec i
                                              )
                                               n
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
                        ret14 res
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
        power_iteration matrix vector error_tol max_iterations
      )
       (
        call/cc (
          lambda (
            ret17
          )
           (
            let (
              (
                v (
                  normalize vector
                )
              )
            )
             (
              begin (
                let (
                  (
                    lambda_prev 0.0
                  )
                )
                 (
                  begin (
                    let (
                      (
                        lambda 0.0
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            err 1000000000000.0
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                iterations 0
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
                                              and (
                                                > err error_tol
                                              )
                                               (
                                                < iterations max_iterations
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    w (
                                                      mat_vec_mult matrix v
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    set! v (
                                                      normalize w
                                                    )
                                                  )
                                                   (
                                                    let (
                                                      (
                                                        mv (
                                                          mat_vec_mult matrix v
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        set! lambda (
                                                          dot v mv
                                                        )
                                                      )
                                                       (
                                                        let (
                                                          (
                                                            denom (
                                                              if (
                                                                not (
                                                                  equal? lambda 0.0
                                                                )
                                                              )
                                                               (
                                                                abs lambda
                                                              )
                                                               1.0
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            set! err (
                                                              _div (
                                                                abs (
                                                                  - lambda lambda_prev
                                                                )
                                                              )
                                                               denom
                                                            )
                                                          )
                                                           (
                                                            set! lambda_prev lambda
                                                          )
                                                           (
                                                            set! iterations (
                                                              + iterations 1
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
                                ret17 (
                                  alist->hash-table (
                                    _list (
                                      cons "eigenvalue" lambda
                                    )
                                     (
                                      cons "eigenvector" v
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
          input_matrix (
            _list (
              _list 41.0 4.0 20.0
            )
             (
              _list 4.0 26.0 30.0
            )
             (
              _list 20.0 30.0 50.0
            )
          )
        )
      )
       (
        begin (
          let (
            (
              vector (
                _list 41.0 4.0 20.0
              )
            )
          )
           (
            begin (
              let (
                (
                  result (
                    power_iteration input_matrix vector 1e-12 100
                  )
                )
              )
               (
                begin (
                  _display (
                    if (
                      string? (
                        to-str-space (
                          hash-table-ref result "eigenvalue"
                        )
                      )
                    )
                     (
                      to-str-space (
                        hash-table-ref result "eigenvalue"
                      )
                    )
                     (
                      to-str (
                        to-str-space (
                          hash-table-ref result "eigenvalue"
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
                          hash-table-ref result "eigenvector"
                        )
                      )
                    )
                     (
                      to-str-space (
                        hash-table-ref result "eigenvector"
                      )
                    )
                     (
                      to-str (
                        to-str-space (
                          hash-table-ref result "eigenvector"
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
      )
    )
     (
      let (
        (
          end21 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur22 (
              quotient (
                * (
                  - end21 start20
                )
                 1000000
              )
               jps23
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur22
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
