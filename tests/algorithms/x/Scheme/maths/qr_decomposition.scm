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
      start49 (
        current-jiffy
      )
    )
     (
      jps52 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        sqrt_approx x
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            begin (
              if (
                <= x 0.0
              )
               (
                begin (
                  ret1 0.0
                )
              )
               '(
                
              )
            )
             (
              let (
                (
                  guess x
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
                      ret1 guess
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
        sign x
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            if (
              >= x 0.0
            )
             (
              begin (
                ret4 1.0
              )
            )
             (
              begin (
                ret4 (
                  - 1.0
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        vector_norm v
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
                                    _len v
                                  )
                                )
                                 (
                                  begin (
                                    set! sum (
                                      _add sum (
                                        * (
                                          list-ref-safe v i
                                        )
                                         (
                                          list-ref-safe v i
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
                    let (
                      (
                        n (
                          sqrt_approx sum
                        )
                      )
                    )
                     (
                      begin (
                        ret5 n
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
        identity_matrix n
      )
       (
        call/cc (
          lambda (
            ret8
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
                                                            loop11
                                                          )
                                                        )
                                                         '(
                                                          
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
                    ret8 mat
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
        copy_matrix a
      )
       (
        call/cc (
          lambda (
            ret13
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
                                    _len a
                                  )
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
                                                          < j (
                                                            _len (
                                                              list-ref-safe a i
                                                            )
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
                                                              )
                                                            )
                                                          )
                                                           (
                                                            set! j (
                                                              + j 1
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
                    ret13 mat
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
            ret18
          )
           (
            let (
              (
                m (
                  _len a
                )
              )
            )
             (
              begin (
                let (
                  (
                    n (
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
                        p (
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
                                              < i m
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
                                                                      < j p
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
                                                                                              < k n
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
      )
    )
     (
      define (
        qr_decomposition a
      )
       (
        call/cc (
          lambda (
            ret25
          )
           (
            let (
              (
                m (
                  _len a
                )
              )
            )
             (
              begin (
                let (
                  (
                    n (
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
                        t (
                          if (
                            < m n
                          )
                           m n
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            q (
                              identity_matrix m
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                r (
                                  copy_matrix a
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
                                                  < k (
                                                    - t 1
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        x (
                                                          _list
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            i k
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
                                                                          < i m
                                                                        )
                                                                         (
                                                                          begin (
                                                                            set! x (
                                                                              append x (
                                                                                _list (
                                                                                  cond (
                                                                                    (
                                                                                      string? (
                                                                                        cond (
                                                                                          (
                                                                                            string? r
                                                                                          )
                                                                                           (
                                                                                            _substring r i (
                                                                                              + i 1
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          (
                                                                                            hash-table? r
                                                                                          )
                                                                                           (
                                                                                            hash-table-ref r i
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          else (
                                                                                            list-ref-safe r i
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        cond (
                                                                                          (
                                                                                            string? r
                                                                                          )
                                                                                           (
                                                                                            _substring r i (
                                                                                              + i 1
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          (
                                                                                            hash-table? r
                                                                                          )
                                                                                           (
                                                                                            hash-table-ref r i
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          else (
                                                                                            list-ref-safe r i
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       k (
                                                                                        + k 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        cond (
                                                                                          (
                                                                                            string? r
                                                                                          )
                                                                                           (
                                                                                            _substring r i (
                                                                                              + i 1
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          (
                                                                                            hash-table? r
                                                                                          )
                                                                                           (
                                                                                            hash-table-ref r i
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          else (
                                                                                            list-ref-safe r i
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        cond (
                                                                                          (
                                                                                            string? r
                                                                                          )
                                                                                           (
                                                                                            _substring r i (
                                                                                              + i 1
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          (
                                                                                            hash-table? r
                                                                                          )
                                                                                           (
                                                                                            hash-table-ref r i
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          else (
                                                                                            list-ref-safe r i
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       k
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref-safe (
                                                                                        cond (
                                                                                          (
                                                                                            string? r
                                                                                          )
                                                                                           (
                                                                                            _substring r i (
                                                                                              + i 1
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          (
                                                                                            hash-table? r
                                                                                          )
                                                                                           (
                                                                                            hash-table-ref r i
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          else (
                                                                                            list-ref-safe r i
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       k
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
                                                            let (
                                                              (
                                                                e1 (
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
                                                                              < i (
                                                                                _len x
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                if (
                                                                                  equal? i 0
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    set! e1 (
                                                                                      append e1 (
                                                                                        _list 1.0
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    set! e1 (
                                                                                      append e1 (
                                                                                        _list 0.0
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
                                                                let (
                                                                  (
                                                                    alpha (
                                                                      vector_norm x
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        s (
                                                                          * (
                                                                            sign (
                                                                              list-ref-safe x 0
                                                                            )
                                                                          )
                                                                           alpha
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            v (
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
                                                                                          < i (
                                                                                            _len x
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            set! v (
                                                                                              append v (
                                                                                                _list (
                                                                                                  _add (
                                                                                                    list-ref-safe x i
                                                                                                  )
                                                                                                   (
                                                                                                    * s (
                                                                                                      list-ref-safe e1 i
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
                                                                            let (
                                                                              (
                                                                                vnorm (
                                                                                  vector_norm v
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
                                                                                              < i (
                                                                                                _len v
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                list-set! v i (
                                                                                                  _div (
                                                                                                    list-ref-safe v i
                                                                                                  )
                                                                                                   vnorm
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                set! i (
                                                                                                  + i 1
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
                                                                                let (
                                                                                  (
                                                                                    size (
                                                                                      _len v
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    let (
                                                                                      (
                                                                                        qk_small (
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
                                                                                                      < i size
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
                                                                                                                    break39
                                                                                                                  )
                                                                                                                   (
                                                                                                                    letrec (
                                                                                                                      (
                                                                                                                        loop38 (
                                                                                                                          lambda (
                                                                                                                            
                                                                                                                          )
                                                                                                                           (
                                                                                                                            if (
                                                                                                                              < j size
                                                                                                                            )
                                                                                                                             (
                                                                                                                              begin (
                                                                                                                                let (
                                                                                                                                  (
                                                                                                                                    delta (
                                                                                                                                      if (
                                                                                                                                        equal? i j
                                                                                                                                      )
                                                                                                                                       1.0 0.0
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                )
                                                                                                                                 (
                                                                                                                                  begin (
                                                                                                                                    set! row (
                                                                                                                                      append row (
                                                                                                                                        _list (
                                                                                                                                          - delta (
                                                                                                                                            * (
                                                                                                                                              * 2.0 (
                                                                                                                                                list-ref-safe v i
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                             (
                                                                                                                                              list-ref-safe v j
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
                                                                                                                                )
                                                                                                                              )
                                                                                                                               (
                                                                                                                                loop38
                                                                                                                              )
                                                                                                                            )
                                                                                                                             '(
                                                                                                                              
                                                                                                                            )
                                                                                                                          )
                                                                                                                        )
                                                                                                                      )
                                                                                                                    )
                                                                                                                     (
                                                                                                                      loop38
                                                                                                                    )
                                                                                                                  )
                                                                                                                )
                                                                                                              )
                                                                                                               (
                                                                                                                set! qk_small (
                                                                                                                  append qk_small (
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
                                                                                        let (
                                                                                          (
                                                                                            qk (
                                                                                              identity_matrix m
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
                                                                                                          < i size
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
                                                                                                                              < j size
                                                                                                                            )
                                                                                                                             (
                                                                                                                              begin (
                                                                                                                                list-set! (
                                                                                                                                  list-ref-safe qk (
                                                                                                                                    + k i
                                                                                                                                  )
                                                                                                                                )
                                                                                                                                 (
                                                                                                                                  + k j
                                                                                                                                )
                                                                                                                                 (
                                                                                                                                  cond (
                                                                                                                                    (
                                                                                                                                      string? (
                                                                                                                                        list-ref-safe qk_small i
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      _substring (
                                                                                                                                        list-ref-safe qk_small i
                                                                                                                                      )
                                                                                                                                       j (
                                                                                                                                        + j 1
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    (
                                                                                                                                      hash-table? (
                                                                                                                                        list-ref-safe qk_small i
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      hash-table-ref (
                                                                                                                                        list-ref-safe qk_small i
                                                                                                                                      )
                                                                                                                                       j
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    else (
                                                                                                                                      list-ref-safe (
                                                                                                                                        list-ref-safe qk_small i
                                                                                                                                      )
                                                                                                                                       j
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
                                                                                                                set! i (
                                                                                                                  + i 1
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
                                                                                           (
                                                                                            set! q (
                                                                                              matmul q qk
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            set! r (
                                                                                              matmul qk r
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            set! k (
                                                                                              + k 1
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
                                    ret25 (
                                      alist->hash-table (
                                        _list (
                                          cons "q" q
                                        )
                                         (
                                          cons "r" r
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
        print_matrix mat
      )
       (
        call/cc (
          lambda (
            ret44
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
                    break46
                  )
                   (
                    letrec (
                      (
                        loop45 (
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
                                        call/cc (
                                          lambda (
                                            break48
                                          )
                                           (
                                            letrec (
                                              (
                                                loop47 (
                                                  lambda (
                                                    
                                                  )
                                                   (
                                                    if (
                                                      < j (
                                                        _len (
                                                          list-ref-safe mat i
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        set! line (
                                                          string-append line (
                                                            to-str-space (
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
                                                        if (
                                                          < (
                                                            + j 1
                                                          )
                                                           (
                                                            _len (
                                                              list-ref-safe mat i
                                                            )
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
                                                        loop47
                                                      )
                                                    )
                                                     '(
                                                      
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              loop47
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
                               (
                                loop45
                              )
                            )
                             '(
                              
                            )
                          )
                        )
                      )
                    )
                     (
                      loop45
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
          A (
            _list (
              _list 12.0 (
                - 51.0
              )
               4.0
            )
             (
              _list 6.0 167.0 (
                - 68.0
              )
            )
             (
              _list (
                - 4.0
              )
               24.0 (
                - 41.0
              )
            )
          )
        )
      )
       (
        begin (
          let (
            (
              result (
                qr_decomposition A
              )
            )
          )
           (
            begin (
              print_matrix (
                hash-table-ref result "q"
              )
            )
             (
              print_matrix (
                hash-table-ref result "r"
              )
            )
          )
        )
      )
    )
     (
      let (
        (
          end50 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur51 (
              quotient (
                * (
                  - end50 start49
                )
                 1000000
              )
               jps52
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur51
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
