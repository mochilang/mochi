;; Generated on 2025-08-07 10:06 +0700
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
      start22 (
        current-jiffy
      )
    )
     (
      jps25 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        expApprox x
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                y x
              )
            )
             (
              begin (
                let (
                  (
                    is_neg #f
                  )
                )
                 (
                  begin (
                    if (
                      < x 0.0
                    )
                     (
                      begin (
                        set! is_neg #t
                      )
                       (
                        set! y (
                          - x
                        )
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
                                n 1
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
                                              < n 30
                                            )
                                             (
                                              begin (
                                                set! term (
                                                  _div (
                                                    * term y
                                                  )
                                                   (
                                                    + 0.0 n
                                                  )
                                                )
                                              )
                                               (
                                                set! sum (
                                                  + sum term
                                                )
                                              )
                                               (
                                                set! n (
                                                  + n 1
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
                                if is_neg (
                                  begin (
                                    ret1 (
                                      _div 1.0 sum
                                    )
                                  )
                                )
                                 (
                                  quote (
                                    
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
          )
        )
      )
    )
     (
      define (
        sigmoid z
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            ret4 (
              _div 1.0 (
                _add 1.0 (
                  expApprox (
                    - z
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
                s 0.0
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
                                    set! s (
                                      _add s (
                                        * (
                                          list-ref a i
                                        )
                                         (
                                          list-ref b i
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
                                 (
                                  quote (
                                    
                                  )
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
                    ret5 s
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
        zeros n
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
                                  < i n
                                )
                                 (
                                  begin (
                                    set! res (
                                      append res (
                                        _list 0.0
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
        logistic_reg alpha x y iterations
      )
       (
        call/cc (
          lambda (
            ret11
          )
           (
            let (
              (
                m (
                  _len x
                )
              )
            )
             (
              begin (
                let (
                  (
                    n (
                      _len (
                        list-ref x 0
                      )
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        theta (
                          zeros n
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            iter 0
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
                                          < iter iterations
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                grad (
                                                  zeros n
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
                                                                  < i m
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        z (
                                                                          dot (
                                                                            list-ref x i
                                                                          )
                                                                           theta
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            h (
                                                                              sigmoid z
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
                                                                                              < k n
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                list-set! grad k (
                                                                                                  _add (
                                                                                                    cond (
                                                                                                      (
                                                                                                        string? grad
                                                                                                      )
                                                                                                       (
                                                                                                        _substring grad k (
                                                                                                          + k 1
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      (
                                                                                                        hash-table? grad
                                                                                                      )
                                                                                                       (
                                                                                                        hash-table-ref grad k
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      else (
                                                                                                        list-ref grad k
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    * (
                                                                                                      - h (
                                                                                                        list-ref y i
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      cond (
                                                                                                        (
                                                                                                          string? (
                                                                                                            list-ref x i
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          _substring (
                                                                                                            list-ref x i
                                                                                                          )
                                                                                                           k (
                                                                                                            + k 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? (
                                                                                                            list-ref x i
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref (
                                                                                                            list-ref x i
                                                                                                          )
                                                                                                           k
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref (
                                                                                                            list-ref x i
                                                                                                          )
                                                                                                           k
                                                                                                        )
                                                                                                      )
                                                                                                    )
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
                                                                                             (
                                                                                              quote (
                                                                                                
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
                                                                    )
                                                                  )
                                                                   (
                                                                    loop14
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
                                                          loop14
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    let (
                                                      (
                                                        k2 0
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
                                                                      < k2 n
                                                                    )
                                                                     (
                                                                      begin (
                                                                        list-set! theta k2 (
                                                                          - (
                                                                            cond (
                                                                              (
                                                                                string? theta
                                                                              )
                                                                               (
                                                                                _substring theta k2 (
                                                                                  + k2 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? theta
                                                                              )
                                                                               (
                                                                                hash-table-ref theta k2
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref theta k2
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            _div (
                                                                              * alpha (
                                                                                cond (
                                                                                  (
                                                                                    string? grad
                                                                                  )
                                                                                   (
                                                                                    _substring grad k2 (
                                                                                      + k2 1
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  (
                                                                                    hash-table? grad
                                                                                  )
                                                                                   (
                                                                                    hash-table-ref grad k2
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  else (
                                                                                    list-ref grad k2
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              + 0.0 m
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        set! k2 (
                                                                          + k2 1
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
                                                        set! iter (
                                                          + iter 1
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                           (
                                            loop12
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
                                  loop12
                                )
                              )
                            )
                          )
                           (
                            ret11 theta
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
          x (
            _list (
              _list 0.5 1.5
            )
             (
              _list 1.0 1.0
            )
             (
              _list 1.5 0.5
            )
             (
              _list 3.0 3.5
            )
             (
              _list 3.5 3.0
            )
             (
              _list 4.0 4.0
            )
          )
        )
      )
       (
        begin (
          let (
            (
              y (
                _list 0.0 0.0 0.0 1.0 1.0 1.0
              )
            )
          )
           (
            begin (
              let (
                (
                  alpha 0.1
                )
              )
               (
                begin (
                  let (
                    (
                      iterations 1000
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          theta (
                            logistic_reg alpha x y iterations
                          )
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
                                      i
                                    )
                                     (
                                      if (
                                        < i (
                                          _len theta
                                        )
                                      )
                                       (
                                        begin (
                                          begin (
                                            _display (
                                              if (
                                                string? (
                                                  cond (
                                                    (
                                                      string? theta
                                                    )
                                                     (
                                                      _substring theta i (
                                                        + i 1
                                                      )
                                                    )
                                                  )
                                                   (
                                                    (
                                                      hash-table? theta
                                                    )
                                                     (
                                                      hash-table-ref theta i
                                                    )
                                                  )
                                                   (
                                                    else (
                                                      list-ref theta i
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                cond (
                                                  (
                                                    string? theta
                                                  )
                                                   (
                                                    _substring theta i (
                                                      + i 1
                                                    )
                                                  )
                                                )
                                                 (
                                                  (
                                                    hash-table? theta
                                                  )
                                                   (
                                                    hash-table-ref theta i
                                                  )
                                                )
                                                 (
                                                  else (
                                                    list-ref theta i
                                                  )
                                                )
                                              )
                                               (
                                                to-str (
                                                  cond (
                                                    (
                                                      string? theta
                                                    )
                                                     (
                                                      _substring theta i (
                                                        + i 1
                                                      )
                                                    )
                                                  )
                                                   (
                                                    (
                                                      hash-table? theta
                                                    )
                                                     (
                                                      hash-table-ref theta i
                                                    )
                                                  )
                                                   (
                                                    else (
                                                      list-ref theta i
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
                                         (
                                          loop20 (
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
                                loop20 0
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
          end23 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur24 (
              quotient (
                * (
                  - end23 start22
                )
                 1000000
              )
               jps25
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur24
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
