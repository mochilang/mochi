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
      start35 (
        current-jiffy
      )
    )
     (
      jps38 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      let (
        (
          LOWER "abcdefghijklmnopqrstuvwxyz"
        )
      )
       (
        begin (
          let (
            (
              UPPER "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
            )
          )
           (
            begin (
              let (
                (
                  PUNCT "!\"#$%&'()*+,-./:;<=>?@[\\]^_{|}~"
                )
              )
               (
                begin (
                  define (
                    to_lowercase s
                  )
                   (
                    call/cc (
                      lambda (
                        ret1
                      )
                       (
                        let (
                          (
                            res ""
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
                                                _len s
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    c (
                                                      _substring s i (
                                                        + i 1
                                                      )
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
                                                        let (
                                                          (
                                                            found #f
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
                                                                            _len UPPER
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            if (
                                                                              string=? c (
                                                                                _substring UPPER j (
                                                                                  + j 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                set! res (
                                                                                  string-append res (
                                                                                    _substring LOWER j (
                                                                                      + j 1
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                set! found #t
                                                                              )
                                                                               (
                                                                                break5 (
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
                                                                            loop4
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
                                                                  loop4
                                                                )
                                                              )
                                                            )
                                                          )
                                                           (
                                                            if (
                                                              not found
                                                            )
                                                             (
                                                              begin (
                                                                set! res (
                                                                  string-append res c
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
                                ret1 res
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
                    is_punct c
                  )
                   (
                    call/cc (
                      lambda (
                        ret6
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
                                            _len PUNCT
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              string=? c (
                                                _substring PUNCT i (
                                                  + i 1
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                ret6 #t
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
                            ret6 #f
                          )
                        )
                      )
                    )
                  )
                )
                 (
                  define (
                    clean_text text keep_newlines
                  )
                   (
                    call/cc (
                      lambda (
                        ret9
                      )
                       (
                        let (
                          (
                            lower (
                              to_lowercase text
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                res ""
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
                                                    _len lower
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        ch (
                                                          cond (
                                                            (
                                                              string? lower
                                                            )
                                                             (
                                                              _substring lower i (
                                                                + i 1
                                                              )
                                                            )
                                                          )
                                                           (
                                                            (
                                                              hash-table? lower
                                                            )
                                                             (
                                                              hash-table-ref lower i
                                                            )
                                                          )
                                                           (
                                                            else (
                                                              list-ref lower i
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        if (
                                                          is_punct ch
                                                        )
                                                         (
                                                          begin
                                                        )
                                                         (
                                                          if (
                                                            string=? ch "\n"
                                                          )
                                                           (
                                                            begin (
                                                              if keep_newlines (
                                                                begin (
                                                                  set! res (
                                                                    string-append res "\n"
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
                                                              set! res (
                                                                string-append res ch
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
                                    ret9 res
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
                    split s sep
                  )
                   (
                    call/cc (
                      lambda (
                        ret12
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
                                    set! res (
                                      append res (
                                        _list current
                                      )
                                    )
                                  )
                                   (
                                    ret12 res
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
                    contains s sub
                  )
                   (
                    call/cc (
                      lambda (
                        ret15
                      )
                       (
                        let (
                          (
                            n (
                              _len s
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                m (
                                  _len sub
                                )
                              )
                            )
                             (
                              begin (
                                if (
                                  equal? m 0
                                )
                                 (
                                  begin (
                                    ret15 #t
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
                                                  <= i (
                                                    - n m
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
                                                        let (
                                                          (
                                                            is_match #t
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
                                                                          < j m
                                                                        )
                                                                         (
                                                                          begin (
                                                                            if (
                                                                              not (
                                                                                string=? (
                                                                                  _substring s (
                                                                                    + i j
                                                                                  )
                                                                                   (
                                                                                    + (
                                                                                      + i j
                                                                                    )
                                                                                     1
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  _substring sub j (
                                                                                    + j 1
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                set! is_match #f
                                                                              )
                                                                               (
                                                                                break19 (
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
                                                            if is_match (
                                                              begin (
                                                                ret15 #t
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
                                    ret15 #f
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
                    floor x
                  )
                   (
                    call/cc (
                      lambda (
                        ret20
                      )
                       (
                        let (
                          (
                            i (
                              let (
                                (
                                  v21 x
                                )
                              )
                               (
                                cond (
                                  (
                                    string? v21
                                  )
                                   (
                                    inexact->exact (
                                      floor (
                                        string->number v21
                                      )
                                    )
                                  )
                                )
                                 (
                                  (
                                    boolean? v21
                                  )
                                   (
                                    if v21 1 0
                                  )
                                )
                                 (
                                  else (
                                    inexact->exact (
                                      floor v21
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                         (
                          begin (
                            if (
                              > (
                                + 0.0 i
                              )
                               x
                            )
                             (
                              begin (
                                set! i (
                                  - i 1
                                )
                              )
                            )
                             (
                              quote (
                                
                              )
                            )
                          )
                           (
                            ret20 (
                              + 0.0 i
                            )
                          )
                        )
                      )
                    )
                  )
                )
                 (
                  define (
                    round3 x
                  )
                   (
                    call/cc (
                      lambda (
                        ret22
                      )
                       (
                        ret22 (
                          _div (
                            floor (
                              _add (
                                * x 1000.0
                              )
                               0.5
                            )
                          )
                           1000.0
                        )
                      )
                    )
                  )
                )
                 (
                  define (
                    ln x
                  )
                   (
                    call/cc (
                      lambda (
                        ret23
                      )
                       (
                        let (
                          (
                            t (
                              _div (
                                - x 1.0
                              )
                               (
                                + x 1.0
                              )
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                term t
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    sum 0.0
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
                                                      <= k 99
                                                    )
                                                     (
                                                      begin (
                                                        set! sum (
                                                          _add sum (
                                                            _div term (
                                                              + 0.0 k
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        set! term (
                                                          * (
                                                            * term t
                                                          )
                                                           t
                                                        )
                                                      )
                                                       (
                                                        set! k (
                                                          + k 2
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
                                        ret23 (
                                          * 2.0 sum
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
                    log10 x
                  )
                   (
                    call/cc (
                      lambda (
                        ret26
                      )
                       (
                        ret26 (
                          _div (
                            ln x
                          )
                           (
                            ln 10.0
                          )
                        )
                      )
                    )
                  )
                )
                 (
                  define (
                    term_frequency term document
                  )
                   (
                    call/cc (
                      lambda (
                        ret27
                      )
                       (
                        let (
                          (
                            clean (
                              clean_text document #f
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                tokens (
                                  split clean " "
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    t (
                                      to_lowercase term
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
                                            i 0
                                          )
                                        )
                                         (
                                          begin (
                                            call/cc (
                                              lambda (
                                                break29
                                              )
                                               (
                                                letrec (
                                                  (
                                                    loop28 (
                                                      lambda (
                                                        
                                                      )
                                                       (
                                                        if (
                                                          < i (
                                                            _len tokens
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            if (
                                                              and (
                                                                not (
                                                                  string=? (
                                                                    cond (
                                                                      (
                                                                        string? tokens
                                                                      )
                                                                       (
                                                                        _substring tokens i (
                                                                          + i 1
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      (
                                                                        hash-table? tokens
                                                                      )
                                                                       (
                                                                        hash-table-ref tokens i
                                                                      )
                                                                    )
                                                                     (
                                                                      else (
                                                                        list-ref tokens i
                                                                      )
                                                                    )
                                                                  )
                                                                   ""
                                                                )
                                                              )
                                                               (
                                                                equal? (
                                                                  cond (
                                                                    (
                                                                      string? tokens
                                                                    )
                                                                     (
                                                                      _substring tokens i (
                                                                        + i 1
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    (
                                                                      hash-table? tokens
                                                                    )
                                                                     (
                                                                      hash-table-ref tokens i
                                                                    )
                                                                  )
                                                                   (
                                                                    else (
                                                                      list-ref tokens i
                                                                    )
                                                                  )
                                                                )
                                                                 t
                                                              )
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
                                                            set! i (
                                                              + i 1
                                                            )
                                                          )
                                                           (
                                                            loop28
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
                                                  loop28
                                                )
                                              )
                                            )
                                          )
                                           (
                                            ret27 count
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
                    document_frequency term corpus
                  )
                   (
                    call/cc (
                      lambda (
                        ret30
                      )
                       (
                        let (
                          (
                            clean (
                              clean_text corpus #t
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                docs (
                                  split clean "\n"
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    t (
                                      to_lowercase term
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        matches 0
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
                                                          < i (
                                                            _len docs
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            if (
                                                              contains (
                                                                cond (
                                                                  (
                                                                    string? docs
                                                                  )
                                                                   (
                                                                    _substring docs i (
                                                                      + i 1
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  (
                                                                    hash-table? docs
                                                                  )
                                                                   (
                                                                    hash-table-ref docs i
                                                                  )
                                                                )
                                                                 (
                                                                  else (
                                                                    list-ref docs i
                                                                  )
                                                                )
                                                              )
                                                               t
                                                            )
                                                             (
                                                              begin (
                                                                set! matches (
                                                                  + matches 1
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
                                            ret30 (
                                              _list matches (
                                                _len docs
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
                    inverse_document_frequency df n smoothing
                  )
                   (
                    call/cc (
                      lambda (
                        ret33
                      )
                       (
                        begin (
                          if smoothing (
                            begin (
                              if (
                                equal? n 0
                              )
                               (
                                begin (
                                  panic "log10(0) is undefined."
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
                                  ratio (
                                    _div (
                                      + 0.0 n
                                    )
                                     (
                                      + 1.0 (
                                        + 0.0 df
                                      )
                                    )
                                  )
                                )
                              )
                               (
                                begin (
                                  let (
                                    (
                                      l (
                                        log10 ratio
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      let (
                                        (
                                          result (
                                            round3 (
                                              + 1.0 l
                                            )
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          _display (
                                            if (
                                              string? result
                                            )
                                             result (
                                              to-str result
                                            )
                                          )
                                        )
                                         (
                                          newline
                                        )
                                         (
                                          ret33 result
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
                          if (
                            equal? df 0
                          )
                           (
                            begin (
                              panic "df must be > 0"
                            )
                          )
                           (
                            quote (
                              
                            )
                          )
                        )
                         (
                          if (
                            equal? n 0
                          )
                           (
                            begin (
                              panic "log10(0) is undefined."
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
                              ratio (
                                _div (
                                  + 0.0 n
                                )
                                 (
                                  + 0.0 df
                                )
                              )
                            )
                          )
                           (
                            begin (
                              let (
                                (
                                  l (
                                    log10 ratio
                                  )
                                )
                              )
                               (
                                begin (
                                  let (
                                    (
                                      result (
                                        round3 l
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      _display (
                                        if (
                                          string? result
                                        )
                                         result (
                                          to-str result
                                        )
                                      )
                                    )
                                     (
                                      newline
                                    )
                                     (
                                      ret33 result
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
                    tf_idf tf idf
                  )
                   (
                    call/cc (
                      lambda (
                        ret34
                      )
                       (
                        let (
                          (
                            prod (
                              * (
                                + 0.0 tf
                              )
                               idf
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                result (
                                  round3 prod
                                )
                              )
                            )
                             (
                              begin (
                                _display (
                                  if (
                                    string? result
                                  )
                                   result (
                                    to-str result
                                  )
                                )
                              )
                               (
                                newline
                              )
                               (
                                ret34 result
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
                 (
                  _display (
                    if (
                      string? (
                        term_frequency "to" "To be, or not to be"
                      )
                    )
                     (
                      term_frequency "to" "To be, or not to be"
                    )
                     (
                      to-str (
                        term_frequency "to" "To be, or not to be"
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
                      corpus "This is the first document in the corpus.\nThIs is the second document in the corpus.\nTHIS is the third document in the corpus."
                    )
                  )
                   (
                    begin (
                      _display (
                        if (
                          string? (
                            to-str-space (
                              document_frequency "first" corpus
                            )
                          )
                        )
                         (
                          to-str-space (
                            document_frequency "first" corpus
                          )
                        )
                         (
                          to-str (
                            to-str-space (
                              document_frequency "first" corpus
                            )
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
                          idf_val (
                            inverse_document_frequency 1 3 #f
                          )
                        )
                      )
                       (
                        begin (
                          tf_idf 2 idf_val
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
          end36 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur37 (
              quotient (
                * (
                  - end36 start35
                )
                 1000000
              )
               jps38
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur37
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
