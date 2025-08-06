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
      start11 (
        current-jiffy
      )
    )
     (
      jps14 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        gcd a b
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                x (
                  if (
                    < a 0
                  )
                   (
                    - a
                  )
                   a
                )
              )
            )
             (
              begin (
                let (
                  (
                    y (
                      if (
                        < b 0
                      )
                       (
                        - b
                      )
                       b
                    )
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
                                  not (
                                    equal? y 0
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        t (
                                          modulo x y
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        set! x y
                                      )
                                       (
                                        set! y t
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
                    ret1 x
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
        extended_gcd a b
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            begin (
              if (
                equal? b 0
              )
               (
                begin (
                  ret4 (
                    _list a 1 0
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
                  res (
                    extended_gcd b (
                      modulo a b
                    )
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      d (
                        cond (
                          (
                            string? res
                          )
                           (
                            _substring res 0 (
                              + 0 1
                            )
                          )
                        )
                         (
                          (
                            hash-table? res
                          )
                           (
                            hash-table-ref res 0
                          )
                        )
                         (
                          else (
                            list-ref res 0
                          )
                        )
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          p (
                            cond (
                              (
                                string? res
                              )
                               (
                                _substring res 1 (
                                  + 1 1
                                )
                              )
                            )
                             (
                              (
                                hash-table? res
                              )
                               (
                                hash-table-ref res 1
                              )
                            )
                             (
                              else (
                                list-ref res 1
                              )
                            )
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              q (
                                cond (
                                  (
                                    string? res
                                  )
                                   (
                                    _substring res 2 (
                                      + 2 1
                                    )
                                  )
                                )
                                 (
                                  (
                                    hash-table? res
                                  )
                                   (
                                    hash-table-ref res 2
                                  )
                                )
                                 (
                                  else (
                                    list-ref res 2
                                  )
                                )
                              )
                            )
                          )
                           (
                            begin (
                              let (
                                (
                                  x q
                                )
                              )
                               (
                                begin (
                                  let (
                                    (
                                      y (
                                        - p (
                                          * q (
                                            quotient a b
                                          )
                                        )
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      ret4 (
                                        _list d x y
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
        diophantine a b c
      )
       (
        call/cc (
          lambda (
            ret5
          )
           (
            let (
              (
                d (
                  gcd a b
                )
              )
            )
             (
              begin (
                if (
                  not (
                    equal? (
                      fmod c d
                    )
                     0
                  )
                )
                 (
                  begin (
                    panic "No solution"
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
                    eg (
                      extended_gcd a b
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        r (
                          / c d
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            x (
                              * (
                                cond (
                                  (
                                    string? eg
                                  )
                                   (
                                    _substring eg 1 (
                                      + 1 1
                                    )
                                  )
                                )
                                 (
                                  (
                                    hash-table? eg
                                  )
                                   (
                                    hash-table-ref eg 1
                                  )
                                )
                                 (
                                  else (
                                    list-ref eg 1
                                  )
                                )
                              )
                               r
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                y (
                                  * (
                                    cond (
                                      (
                                        string? eg
                                      )
                                       (
                                        _substring eg 2 (
                                          + 2 1
                                        )
                                      )
                                    )
                                     (
                                      (
                                        hash-table? eg
                                      )
                                       (
                                        hash-table-ref eg 2
                                      )
                                    )
                                     (
                                      else (
                                        list-ref eg 2
                                      )
                                    )
                                  )
                                   r
                                )
                              )
                            )
                             (
                              begin (
                                ret5 (
                                  _list x y
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
        diophantine_all_soln a b c n
      )
       (
        call/cc (
          lambda (
            ret6
          )
           (
            let (
              (
                base (
                  diophantine a b c
                )
              )
            )
             (
              begin (
                let (
                  (
                    x0 (
                      cond (
                        (
                          string? base
                        )
                         (
                          _substring base 0 (
                            + 0 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? base
                        )
                         (
                          hash-table-ref base 0
                        )
                      )
                       (
                        else (
                          list-ref base 0
                        )
                      )
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        y0 (
                          cond (
                            (
                              string? base
                            )
                             (
                              _substring base 1 (
                                + 1 1
                              )
                            )
                          )
                           (
                            (
                              hash-table? base
                            )
                             (
                              hash-table-ref base 1
                            )
                          )
                           (
                            else (
                              list-ref base 1
                            )
                          )
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            d (
                              gcd a b
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                p (
                                  / a d
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    q (
                                      / b d
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        sols (
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
                                                          < i n
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                x (
                                                                  _add x0 (
                                                                    * i q
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                let (
                                                                  (
                                                                    y (
                                                                      - y0 (
                                                                        * i p
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! sols (
                                                                      append sols (
                                                                        _list (
                                                                          _list x y
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
                                            ret6 sols
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
      let (
        (
          s1 (
            diophantine 10 6 14
          )
        )
      )
       (
        begin (
          _display (
            if (
              string? (
                to-str-space s1
              )
            )
             (
              to-str-space s1
            )
             (
              to-str (
                to-str-space s1
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
              sols (
                diophantine_all_soln 10 6 14 4
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
                                < j (
                                  _len sols
                                )
                              )
                               (
                                begin (
                                  _display (
                                    if (
                                      string? (
                                        to-str-space (
                                          cond (
                                            (
                                              string? sols
                                            )
                                             (
                                              _substring sols j (
                                                + j 1
                                              )
                                            )
                                          )
                                           (
                                            (
                                              hash-table? sols
                                            )
                                             (
                                              hash-table-ref sols j
                                            )
                                          )
                                           (
                                            else (
                                              list-ref sols j
                                            )
                                          )
                                        )
                                      )
                                    )
                                     (
                                      to-str-space (
                                        cond (
                                          (
                                            string? sols
                                          )
                                           (
                                            _substring sols j (
                                              + j 1
                                            )
                                          )
                                        )
                                         (
                                          (
                                            hash-table? sols
                                          )
                                           (
                                            hash-table-ref sols j
                                          )
                                        )
                                         (
                                          else (
                                            list-ref sols j
                                          )
                                        )
                                      )
                                    )
                                     (
                                      to-str (
                                        to-str-space (
                                          cond (
                                            (
                                              string? sols
                                            )
                                             (
                                              _substring sols j (
                                                + j 1
                                              )
                                            )
                                          )
                                           (
                                            (
                                              hash-table? sols
                                            )
                                             (
                                              hash-table-ref sols j
                                            )
                                          )
                                           (
                                            else (
                                              list-ref sols j
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
                                  set! j (
                                    + j 1
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
                  _display (
                    if (
                      string? (
                        to-str-space (
                          diophantine 391 299 (
                            - 69
                          )
                        )
                      )
                    )
                     (
                      to-str-space (
                        diophantine 391 299 (
                          - 69
                        )
                      )
                    )
                     (
                      to-str (
                        to-str-space (
                          diophantine 391 299 (
                            - 69
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
                        to-str-space (
                          extended_gcd 10 6
                        )
                      )
                    )
                     (
                      to-str-space (
                        extended_gcd 10 6
                      )
                    )
                     (
                      to-str (
                        to-str-space (
                          extended_gcd 10 6
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
                          extended_gcd 7 5
                        )
                      )
                    )
                     (
                      to-str-space (
                        extended_gcd 7 5
                      )
                    )
                     (
                      to-str (
                        to-str-space (
                          extended_gcd 7 5
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
          end12 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur13 (
              quotient (
                * (
                  - end12 start11
                )
                 1000000
              )
               jps14
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur13
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
