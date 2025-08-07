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
      start45 (
        current-jiffy
      )
    )
     (
      jps48 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        identity n
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                mat (
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
                                        row (
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
                                                          < j n
                                                        )
                                                         (
                                                          begin (
                                                            if (
                                                              equal? i j
                                                            )
                                                             (
                                                              begin (
                                                                set! row (
                                                                  append row (
                                                                    _list 1.0
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                set! row (
                                                                  append row (
                                                                    _list 0.0
                                                                  )
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
                                            set! mat (
                                              append mat (
                                                _list row
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
                    ret1 mat
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
        transpose mat
      )
       (
        call/cc (
          lambda (
            ret6
          )
           (
            let (
              (
                rows (
                  _len mat
                )
              )
            )
             (
              begin (
                let (
                  (
                    cols (
                      _len (
                        list-ref-safe mat 0
                      )
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
                            j 0
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
                                          < j cols
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                row (
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
                                                                  < i rows
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! row (
                                                                      append row (
                                                                        _list (
                                                                          cond (
                                                                            (
                                                                              string? (
                                                                                list-ref-safe mat i
                                                                              )
                                                                            )
                                                                             (
                                                                              _substring (
                                                                                list-ref-safe mat i
                                                                              )
                                                                               j (
                                                                                + j 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            (
                                                                              hash-table? (
                                                                                list-ref-safe mat i
                                                                              )
                                                                            )
                                                                             (
                                                                              hash-table-ref (
                                                                                list-ref-safe mat i
                                                                              )
                                                                               j
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref-safe (
                                                                                list-ref-safe mat i
                                                                              )
                                                                               j
                                                                            )
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
                                                    set! res (
                                                      append res (
                                                        _list row
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
                            ret6 res
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
        matmul a b
      )
       (
        call/cc (
          lambda (
            ret11
          )
           (
            let (
              (
                rows (
                  _len a
                )
              )
            )
             (
              begin (
                let (
                  (
                    cols (
                      _len (
                        list-ref-safe b 0
                      )
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        inner (
                          _len (
                            list-ref-safe a 0
                          )
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
                                              < i rows
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    row (
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
                                                                      < j cols
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
                                                                                              < k inner
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                set! sum (
                                                                                                  _add sum (
                                                                                                    * (
                                                                                                      cond (
                                                                                                        (
                                                                                                          string? (
                                                                                                            list-ref-safe a i
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          _substring (
                                                                                                            list-ref-safe a i
                                                                                                          )
                                                                                                           k (
                                                                                                            + k 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? (
                                                                                                            list-ref-safe a i
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref (
                                                                                                            list-ref-safe a i
                                                                                                          )
                                                                                                           k
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref-safe (
                                                                                                            list-ref-safe a i
                                                                                                          )
                                                                                                           k
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      cond (
                                                                                                        (
                                                                                                          string? (
                                                                                                            list-ref-safe b k
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          _substring (
                                                                                                            list-ref-safe b k
                                                                                                          )
                                                                                                           j (
                                                                                                            + j 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? (
                                                                                                            list-ref-safe b k
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref (
                                                                                                            list-ref-safe b k
                                                                                                          )
                                                                                                           j
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref-safe (
                                                                                                            list-ref-safe b k
                                                                                                          )
                                                                                                           j
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
                                                                                set! row (
                                                                                  append row (
                                                                                    _list sum
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
                                                        set! res (
                                                          append res (
                                                            _list row
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
      )
    )
     (
      define (
        mat_sub a b
      )
       (
        call/cc (
          lambda (
            ret18
          )
           (
            let (
              (
                rows (
                  _len a
                )
              )
            )
             (
              begin (
                let (
                  (
                    cols (
                      _len (
                        list-ref-safe a 0
                      )
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
                                          < i rows
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                row (
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
                                                                  < j cols
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! row (
                                                                      append row (
                                                                        _list (
                                                                          - (
                                                                            cond (
                                                                              (
                                                                                string? (
                                                                                  list-ref-safe a i
                                                                                )
                                                                              )
                                                                               (
                                                                                _substring (
                                                                                  list-ref-safe a i
                                                                                )
                                                                                 j (
                                                                                  + j 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? (
                                                                                  list-ref-safe a i
                                                                                )
                                                                              )
                                                                               (
                                                                                hash-table-ref (
                                                                                  list-ref-safe a i
                                                                                )
                                                                                 j
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref-safe (
                                                                                  list-ref-safe a i
                                                                                )
                                                                                 j
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            cond (
                                                                              (
                                                                                string? (
                                                                                  list-ref-safe b i
                                                                                )
                                                                              )
                                                                               (
                                                                                _substring (
                                                                                  list-ref-safe b i
                                                                                )
                                                                                 j (
                                                                                  + j 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? (
                                                                                  list-ref-safe b i
                                                                                )
                                                                              )
                                                                               (
                                                                                hash-table-ref (
                                                                                  list-ref-safe b i
                                                                                )
                                                                                 j
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref-safe (
                                                                                  list-ref-safe b i
                                                                                )
                                                                                 j
                                                                              )
                                                                            )
                                                                          )
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
                                                                    loop21
                                                                  )
                                                                )
                                                                 '(
                                                                  
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
                                                    set! res (
                                                      append res (
                                                        _list row
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
                                            loop19
                                          )
                                        )
                                         '(
                                          
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
                            ret18 res
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
        inverse mat
      )
       (
        call/cc (
          lambda (
            ret23
          )
           (
            let (
              (
                n (
                  _len mat
                )
              )
            )
             (
              begin (
                let (
                  (
                    id (
                      identity n
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        aug (
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
                                          < i n
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                row (
                                                  append (
                                                    list-ref-safe mat i
                                                  )
                                                   (
                                                    cond (
                                                      (
                                                        string? id
                                                      )
                                                       (
                                                        _substring id i (
                                                          + i 1
                                                        )
                                                      )
                                                    )
                                                     (
                                                      (
                                                        hash-table? id
                                                      )
                                                       (
                                                        hash-table-ref id i
                                                      )
                                                    )
                                                     (
                                                      else (
                                                        list-ref-safe id i
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                set! aug (
                                                  append aug (
                                                    _list row
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
                                            loop24
                                          )
                                        )
                                         '(
                                          
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
                            let (
                              (
                                col 0
                              )
                            )
                             (
                              begin (
                                call/cc (
                                  lambda (
                                    break27
                                  )
                                   (
                                    letrec (
                                      (
                                        loop26 (
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
                                                    pivot_row (
                                                      list-ref-safe aug col
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        pivot (
                                                          list-ref-safe pivot_row col
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        if (
                                                          equal? pivot 0.0
                                                        )
                                                         (
                                                          begin (
                                                            panic "matrix is singular"
                                                          )
                                                        )
                                                         '(
                                                          
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
                                                                          < j (
                                                                            * 2 n
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            list-set! pivot_row j (
                                                                              _div (
                                                                                list-ref-safe pivot_row j
                                                                              )
                                                                               pivot
                                                                            )
                                                                          )
                                                                           (
                                                                            set! j (
                                                                              + j 1
                                                                            )
                                                                          )
                                                                           (
                                                                            loop28
                                                                          )
                                                                        )
                                                                         '(
                                                                          
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
                                                            list-set! aug col pivot_row
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
                                                                    break31
                                                                  )
                                                                   (
                                                                    letrec (
                                                                      (
                                                                        loop30 (
                                                                          lambda (
                                                                            
                                                                          )
                                                                           (
                                                                            if (
                                                                              < r n
                                                                            )
                                                                             (
                                                                              begin (
                                                                                if (
                                                                                  not (
                                                                                    equal? r col
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    let (
                                                                                      (
                                                                                        row_r (
                                                                                          list-ref-safe aug r
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        let (
                                                                                          (
                                                                                            factor (
                                                                                              list-ref-safe row_r col
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            set! j 0
                                                                                          )
                                                                                           (
                                                                                            call/cc (
                                                                                              lambda (
                                                                                                break33
                                                                                              )
                                                                                               (
                                                                                                letrec (
                                                                                                  (
                                                                                                    loop32 (
                                                                                                      lambda (
                                                                                                        
                                                                                                      )
                                                                                                       (
                                                                                                        if (
                                                                                                          < j (
                                                                                                            * 2 n
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          begin (
                                                                                                            list-set! row_r j (
                                                                                                              - (
                                                                                                                list-ref-safe row_r j
                                                                                                              )
                                                                                                               (
                                                                                                                * factor (
                                                                                                                  list-ref-safe pivot_row j
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
                                                                                                            loop32
                                                                                                          )
                                                                                                        )
                                                                                                         '(
                                                                                                          
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  loop32
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            list-set! aug r row_r
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
                                                                                set! r (
                                                                                  + r 1
                                                                                )
                                                                              )
                                                                               (
                                                                                loop30
                                                                              )
                                                                            )
                                                                             '(
                                                                              
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      loop30
                                                                    )
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
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                loop26
                                              )
                                            )
                                             '(
                                              
                                            )
                                          )
                                        )
                                      )
                                    )
                                     (
                                      loop26
                                    )
                                  )
                                )
                              )
                               (
                                let (
                                  (
                                    inv (
                                      _list
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        r 0
                                      )
                                    )
                                     (
                                      begin (
                                        call/cc (
                                          lambda (
                                            break35
                                          )
                                           (
                                            letrec (
                                              (
                                                loop34 (
                                                  lambda (
                                                    
                                                  )
                                                   (
                                                    if (
                                                      < r n
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            row (
                                                              _list
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                c n
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                call/cc (
                                                                  lambda (
                                                                    break37
                                                                  )
                                                                   (
                                                                    letrec (
                                                                      (
                                                                        loop36 (
                                                                          lambda (
                                                                            
                                                                          )
                                                                           (
                                                                            if (
                                                                              < c (
                                                                                * 2 n
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                set! row (
                                                                                  append row (
                                                                                    _list (
                                                                                      cond (
                                                                                        (
                                                                                          string? (
                                                                                            list-ref-safe aug r
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          _substring (
                                                                                            list-ref-safe aug r
                                                                                          )
                                                                                           c (
                                                                                            + c 1
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        (
                                                                                          hash-table? (
                                                                                            list-ref-safe aug r
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          hash-table-ref (
                                                                                            list-ref-safe aug r
                                                                                          )
                                                                                           c
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        else (
                                                                                          list-ref-safe (
                                                                                            list-ref-safe aug r
                                                                                          )
                                                                                           c
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                set! c (
                                                                                  + c 1
                                                                                )
                                                                              )
                                                                               (
                                                                                loop36
                                                                              )
                                                                            )
                                                                             '(
                                                                              
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      loop36
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                set! inv (
                                                                  append inv (
                                                                    _list row
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                set! r (
                                                                  + r 1
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        loop34
                                                      )
                                                    )
                                                     '(
                                                      
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              loop34
                                            )
                                          )
                                        )
                                      )
                                       (
                                        ret23 inv
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
        schur_complement mat_a mat_b mat_c pseudo_inv
      )
       (
        call/cc (
          lambda (
            ret38
          )
           (
            let (
              (
                a_rows (
                  _len mat_a
                )
              )
            )
             (
              begin (
                let (
                  (
                    a_cols (
                      _len (
                        list-ref-safe mat_a 0
                      )
                    )
                  )
                )
                 (
                  begin (
                    if (
                      not (
                        equal? a_rows a_cols
                      )
                    )
                     (
                      begin (
                        panic "Matrix A must be square"
                      )
                    )
                     '(
                      
                    )
                  )
                   (
                    if (
                      not (
                        equal? a_rows (
                          _len mat_b
                        )
                      )
                    )
                     (
                      begin (
                        panic "Expected the same number of rows for A and B"
                      )
                    )
                     '(
                      
                    )
                  )
                   (
                    if (
                      not (
                        equal? (
                          _len (
                            list-ref-safe mat_b 0
                          )
                        )
                         (
                          _len (
                            list-ref-safe mat_c 0
                          )
                        )
                      )
                    )
                     (
                      begin (
                        panic "Expected the same number of columns for B and C"
                      )
                    )
                     '(
                      
                    )
                  )
                   (
                    let (
                      (
                        a_inv '(
                          
                        )
                      )
                    )
                     (
                      begin (
                        if (
                          hash-table-ref pseudo_inv "ok"
                        )
                         (
                          begin (
                            set! a_inv (
                              hash-table-ref pseudo_inv "value"
                            )
                          )
                        )
                         (
                          begin (
                            set! a_inv (
                              inverse mat_a
                            )
                          )
                        )
                      )
                       (
                        let (
                          (
                            bt (
                              transpose mat_b
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                a_inv_b (
                                  matmul a_inv mat_b
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    bt_a_inv_b (
                                      matmul bt a_inv_b
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    ret38 (
                                      mat_sub mat_c bt_a_inv_b
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
        print_matrix mat
      )
       (
        call/cc (
          lambda (
            ret39
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
                    break41
                  )
                   (
                    letrec (
                      (
                        loop40 (
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
                                let (
                                  (
                                    line ""
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
                                            row (
                                              list-ref-safe mat i
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            call/cc (
                                              lambda (
                                                break43
                                              )
                                               (
                                                letrec (
                                                  (
                                                    loop42 (
                                                      lambda (
                                                        
                                                      )
                                                       (
                                                        if (
                                                          < j (
                                                            _len row
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            set! line (
                                                              string-append line (
                                                                to-str-space (
                                                                  list-ref-safe row j
                                                                )
                                                              )
                                                            )
                                                          )
                                                           (
                                                            if (
                                                              < (
                                                                + j 1
                                                              )
                                                               (
                                                                _len row
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                set! line (
                                                                  string-append line " "
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
                                                            loop42
                                                          )
                                                        )
                                                         '(
                                                          
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  loop42
                                                )
                                              )
                                            )
                                          )
                                           (
                                            _display (
                                              if (
                                                string? line
                                              )
                                               line (
                                                to-str line
                                              )
                                            )
                                          )
                                           (
                                            newline
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
                                loop40
                              )
                            )
                             '(
                              
                            )
                          )
                        )
                      )
                    )
                     (
                      loop40
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
            ret44
          )
           (
            let (
              (
                a (
                  _list (
                    _list 1.0 2.0
                  )
                   (
                    _list 2.0 1.0
                  )
                )
              )
            )
             (
              begin (
                let (
                  (
                    b (
                      _list (
                        _list 0.0 3.0
                      )
                       (
                        _list 3.0 0.0
                      )
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        c (
                          _list (
                            _list 2.0 1.0
                          )
                           (
                            _list 6.0 3.0
                          )
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            none (
                              alist->hash-table (
                                _list (
                                  cons "value" (
                                    _list
                                  )
                                )
                                 (
                                  cons "ok" #f
                                )
                              )
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                s (
                                  schur_complement a b c none
                                )
                              )
                            )
                             (
                              begin (
                                print_matrix s
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
      main
    )
     (
      let (
        (
          end46 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur47 (
              quotient (
                * (
                  - end46 start45
                )
                 1000000
              )
               jps48
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur47
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
