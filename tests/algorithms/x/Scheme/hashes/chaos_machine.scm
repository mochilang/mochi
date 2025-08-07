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
      start23 (
        current-jiffy
      )
    )
     (
      jps26 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      let (
        (
          K (
            _list 0.33 0.44 0.55 0.44 0.33
          )
        )
      )
       (
        begin (
          let (
            (
              t 3
            )
          )
           (
            begin (
              let (
                (
                  size 5
                )
              )
               (
                begin (
                  define (
                    round_dec x n
                  )
                   (
                    call/cc (
                      lambda (
                        ret1
                      )
                       (
                        let (
                          (
                            m10 1.0
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
                                                set! m10 (
                                                  * m10 10.0
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
                                let (
                                  (
                                    y (
                                      _add (
                                        * x m10
                                      )
                                       0.5
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    ret1 (
                                      _div (
                                        * 1.0 (
                                          let (
                                            (
                                              v4 y
                                            )
                                          )
                                           (
                                            cond (
                                              (
                                                string? v4
                                              )
                                               (
                                                exact (
                                                  floor (
                                                    string->number v4
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              (
                                                boolean? v4
                                              )
                                               (
                                                if v4 1 0
                                              )
                                            )
                                             (
                                              else (
                                                exact (
                                                  floor v4
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                       m10
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
                    reset
                  )
                   (
                    call/cc (
                      lambda (
                        ret5
                      )
                       (
                        ret5 (
                          alist->hash-table (
                            _list (
                              cons "buffer" K
                            )
                             (
                              cons "params" (
                                _list 0.0 0.0 0.0 0.0 0.0
                              )
                            )
                             (
                              cons "time" 0
                            )
                          )
                        )
                      )
                    )
                  )
                )
                 (
                  define (
                    push m seed
                  )
                   (
                    call/cc (
                      lambda (
                        ret6
                      )
                       (
                        let (
                          (
                            buf (
                              hash-table-ref m "buffer"
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                par (
                                  hash-table-ref m "params"
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
                                                  < i (
                                                    _len buf
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        value (
                                                          list-ref-safe buf i
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            e (
                                                              _div (
                                                                * 1.0 seed
                                                              )
                                                               value
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                next_value (
                                                                  + (
                                                                    list-ref-safe buf (
                                                                      _mod (
                                                                        + i 1
                                                                      )
                                                                       size
                                                                    )
                                                                  )
                                                                   e
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                set! next_value (
                                                                  - next_value (
                                                                    * 1.0 (
                                                                      let (
                                                                        (
                                                                          v9 next_value
                                                                        )
                                                                      )
                                                                       (
                                                                        cond (
                                                                          (
                                                                            string? v9
                                                                          )
                                                                           (
                                                                            exact (
                                                                              floor (
                                                                                string->number v9
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          (
                                                                            boolean? v9
                                                                          )
                                                                           (
                                                                            if v9 1 0
                                                                          )
                                                                        )
                                                                         (
                                                                          else (
                                                                            exact (
                                                                              floor v9
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
                                                                    r (
                                                                      + (
                                                                        list-ref-safe par i
                                                                      )
                                                                       e
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! r (
                                                                      - r (
                                                                        * 1.0 (
                                                                          let (
                                                                            (
                                                                              v10 r
                                                                            )
                                                                          )
                                                                           (
                                                                            cond (
                                                                              (
                                                                                string? v10
                                                                              )
                                                                               (
                                                                                exact (
                                                                                  floor (
                                                                                    string->number v10
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                boolean? v10
                                                                              )
                                                                               (
                                                                                if v10 1 0
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                exact (
                                                                                  floor v10
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    set! r (
                                                                      + r 3.0
                                                                    )
                                                                  )
                                                                   (
                                                                    list-set! buf i (
                                                                      round_dec (
                                                                        * (
                                                                          * r next_value
                                                                        )
                                                                         (
                                                                          - 1.0 next_value
                                                                        )
                                                                      )
                                                                       10
                                                                    )
                                                                  )
                                                                   (
                                                                    list-set! par i r
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
                                    ret6 (
                                      alist->hash-table (
                                        _list (
                                          cons "buffer" buf
                                        )
                                         (
                                          cons "params" par
                                        )
                                         (
                                          cons "time" (
                                            + (
                                              hash-table-ref m "time"
                                            )
                                             1
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
                    xor a b
                  )
                   (
                    call/cc (
                      lambda (
                        ret11
                      )
                       (
                        let (
                          (
                            aa a
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                bb b
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    res 0
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        bit 1
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
                                                      or (
                                                        > aa 0
                                                      )
                                                       (
                                                        > bb 0
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            abit (
                                                              _mod aa 2
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                bbit (
                                                                  _mod bb 2
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                if (
                                                                  not (
                                                                    equal? abit bbit
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! res (
                                                                      + res bit
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  quote (
                                                                    
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                set! aa (
                                                                  _div aa 2
                                                                )
                                                              )
                                                               (
                                                                set! bb (
                                                                  _div bb 2
                                                                )
                                                              )
                                                               (
                                                                set! bit (
                                                                  * bit 2
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
                                        ret11 res
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
                    xorshift x y
                  )
                   (
                    call/cc (
                      lambda (
                        ret14
                      )
                       (
                        let (
                          (
                            xv x
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                yv y
                              )
                            )
                             (
                              begin (
                                set! xv (
                                  xor xv (
                                    _div yv 8192
                                  )
                                )
                              )
                               (
                                set! yv (
                                  xor yv (
                                    * xv 131072
                                  )
                                )
                              )
                               (
                                set! xv (
                                  xor xv (
                                    _div yv 32
                                  )
                                )
                              )
                               (
                                ret14 xv
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
                    pull m
                  )
                   (
                    call/cc (
                      lambda (
                        ret15
                      )
                       (
                        let (
                          (
                            buf (
                              hash-table-ref m "buffer"
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                par (
                                  hash-table-ref m "params"
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    key (
                                      _mod (
                                        hash-table-ref m "time"
                                      )
                                       size
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
                                                      < i t
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            r (
                                                              list-ref-safe par key
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                value (
                                                                  list-ref-safe buf key
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                list-set! buf key (
                                                                  round_dec (
                                                                    * (
                                                                      * r value
                                                                    )
                                                                     (
                                                                      - 1.0 value
                                                                    )
                                                                  )
                                                                   10
                                                                )
                                                              )
                                                               (
                                                                let (
                                                                  (
                                                                    new_r (
                                                                      _add (
                                                                        * (
                                                                          * 1.0 (
                                                                            hash-table-ref m "time"
                                                                          )
                                                                        )
                                                                         0.01
                                                                      )
                                                                       (
                                                                        * r 1.01
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! new_r (
                                                                      - new_r (
                                                                        * 1.0 (
                                                                          let (
                                                                            (
                                                                              v18 new_r
                                                                            )
                                                                          )
                                                                           (
                                                                            cond (
                                                                              (
                                                                                string? v18
                                                                              )
                                                                               (
                                                                                exact (
                                                                                  floor (
                                                                                    string->number v18
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                boolean? v18
                                                                              )
                                                                               (
                                                                                if v18 1 0
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                exact (
                                                                                  floor v18
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    list-set! par key (
                                                                      + new_r 3.0
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
                                        let (
                                          (
                                            x (
                                              let (
                                                (
                                                  v19 (
                                                    * (
                                                      list-ref-safe buf (
                                                        _mod (
                                                          + key 2
                                                        )
                                                         size
                                                      )
                                                    )
                                                     10000000000.0
                                                  )
                                                )
                                              )
                                               (
                                                cond (
                                                  (
                                                    string? v19
                                                  )
                                                   (
                                                    exact (
                                                      floor (
                                                        string->number v19
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  (
                                                    boolean? v19
                                                  )
                                                   (
                                                    if v19 1 0
                                                  )
                                                )
                                                 (
                                                  else (
                                                    exact (
                                                      floor v19
                                                    )
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
                                                y (
                                                  let (
                                                    (
                                                      v20 (
                                                        * (
                                                          list-ref-safe buf (
                                                            _mod (
                                                              - (
                                                                + key size
                                                              )
                                                               2
                                                            )
                                                             size
                                                          )
                                                        )
                                                         10000000000.0
                                                      )
                                                    )
                                                  )
                                                   (
                                                    cond (
                                                      (
                                                        string? v20
                                                      )
                                                       (
                                                        exact (
                                                          floor (
                                                            string->number v20
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      (
                                                        boolean? v20
                                                      )
                                                       (
                                                        if v20 1 0
                                                      )
                                                    )
                                                     (
                                                      else (
                                                        exact (
                                                          floor v20
                                                        )
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
                                                    new_machine (
                                                      alist->hash-table (
                                                        _list (
                                                          cons "buffer" buf
                                                        )
                                                         (
                                                          cons "params" par
                                                        )
                                                         (
                                                          cons "time" (
                                                            + (
                                                              hash-table-ref m "time"
                                                            )
                                                             1
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
                                                        value (
                                                          _mod (
                                                            xorshift x y
                                                          )
                                                           4294967295
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        ret15 (
                                                          alist->hash-table (
                                                            _list (
                                                              cons "value" value
                                                            )
                                                             (
                                                              cons "machine" new_machine
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
                  let (
                    (
                      machine (
                        reset
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
                                        < i 100
                                      )
                                       (
                                        begin (
                                          set! machine (
                                            push machine i
                                          )
                                        )
                                         (
                                          set! i (
                                            + i 1
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
                          let (
                            (
                              res (
                                pull machine
                              )
                            )
                          )
                           (
                            begin (
                              _display (
                                if (
                                  string? (
                                    hash-table-ref res "value"
                                  )
                                )
                                 (
                                  hash-table-ref res "value"
                                )
                                 (
                                  to-str (
                                    hash-table-ref res "value"
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
                                    hash-table-ref (
                                      hash-table-ref res "machine"
                                    )
                                     "buffer"
                                  )
                                )
                                 (
                                  hash-table-ref (
                                    hash-table-ref res "machine"
                                  )
                                   "buffer"
                                )
                                 (
                                  to-str (
                                    hash-table-ref (
                                      hash-table-ref res "machine"
                                    )
                                     "buffer"
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
                                    hash-table-ref (
                                      hash-table-ref res "machine"
                                    )
                                     "params"
                                  )
                                )
                                 (
                                  hash-table-ref (
                                    hash-table-ref res "machine"
                                  )
                                   "params"
                                )
                                 (
                                  to-str (
                                    hash-table-ref (
                                      hash-table-ref res "machine"
                                    )
                                     "params"
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
     (
      let (
        (
          end24 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur25 (
              quotient (
                * (
                  - end24 start23
                )
                 1000000
              )
               jps26
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur25
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
