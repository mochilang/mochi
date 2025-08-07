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
      start47 (
        current-jiffy
      )
    )
     (
      jps50 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        make_dual real rank
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                ds (
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
                                  < i rank
                                )
                                 (
                                  begin (
                                    set! ds (
                                      append ds (
                                        _list 1.0
                                      )
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
                    ret1 (
                      alist->hash-table (
                        _list (
                          cons "real" real
                        )
                         (
                          cons "duals" ds
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
        dual_from_list real ds
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            ret4 (
              alist->hash-table (
                _list (
                  cons "real" real
                )
                 (
                  cons "duals" ds
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        dual_add a b
      )
       (
        call/cc (
          lambda (
            ret5
          )
           (
            let (
              (
                s_dual (
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
                                  < i (
                                    _len (
                                      hash-table-ref a "duals"
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    set! s_dual (
                                      append s_dual (
                                        _list (
                                          list-ref (
                                            hash-table-ref a "duals"
                                          )
                                           i
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
                    let (
                      (
                        o_dual (
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
                                          < j (
                                            _len (
                                              hash-table-ref b "duals"
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            set! o_dual (
                                              append o_dual (
                                                _list (
                                                  list-ref (
                                                    hash-table-ref b "duals"
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
                                            loop8
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
                                  loop8
                                )
                              )
                            )
                          )
                           (
                            if (
                              > (
                                _len s_dual
                              )
                               (
                                _len o_dual
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    diff (
                                      - (
                                        _len s_dual
                                      )
                                       (
                                        _len o_dual
                                      )
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
                                                      < k diff
                                                    )
                                                     (
                                                      begin (
                                                        set! o_dual (
                                                          append o_dual (
                                                            _list 1.0
                                                          )
                                                        )
                                                      )
                                                       (
                                                        set! k (
                                                          + k 1
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
                                    )
                                  )
                                )
                              )
                            )
                             (
                              if (
                                < (
                                  _len s_dual
                                )
                                 (
                                  _len o_dual
                                )
                              )
                               (
                                begin (
                                  let (
                                    (
                                      diff2 (
                                        - (
                                          _len o_dual
                                        )
                                         (
                                          _len s_dual
                                        )
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      let (
                                        (
                                          k2 0
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
                                                        < k2 diff2
                                                      )
                                                       (
                                                        begin (
                                                          set! s_dual (
                                                            append s_dual (
                                                              _list 1.0
                                                            )
                                                          )
                                                        )
                                                         (
                                                          set! k2 (
                                                            + k2 1
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
                          )
                           (
                            let (
                              (
                                new_duals (
                                  _list
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    idx 0
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
                                                  < idx (
                                                    _len s_dual
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    set! new_duals (
                                                      append new_duals (
                                                        _list (
                                                          + (
                                                            list-ref s_dual idx
                                                          )
                                                           (
                                                            list-ref o_dual idx
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    set! idx (
                                                      + idx 1
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
                                    ret5 (
                                      alist->hash-table (
                                        _list (
                                          cons "real" (
                                            + (
                                              hash-table-ref a "real"
                                            )
                                             (
                                              hash-table-ref b "real"
                                            )
                                          )
                                        )
                                         (
                                          cons "duals" new_duals
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
        dual_add_real a b
      )
       (
        call/cc (
          lambda (
            ret16
          )
           (
            let (
              (
                ds (
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
                        break18
                      )
                       (
                        letrec (
                          (
                            loop17 (
                              lambda (
                                
                              )
                               (
                                if (
                                  < i (
                                    _len (
                                      hash-table-ref a "duals"
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    set! ds (
                                      append ds (
                                        _list (
                                          list-ref (
                                            hash-table-ref a "duals"
                                          )
                                           i
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
                                    loop17
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
                          loop17
                        )
                      )
                    )
                  )
                   (
                    ret16 (
                      alist->hash-table (
                        _list (
                          cons "real" (
                            + (
                              hash-table-ref a "real"
                            )
                             b
                          )
                        )
                         (
                          cons "duals" ds
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
        dual_mul a b
      )
       (
        call/cc (
          lambda (
            ret19
          )
           (
            let (
              (
                new_len (
                  + (
                    + (
                      _len (
                        hash-table-ref a "duals"
                      )
                    )
                     (
                      _len (
                        hash-table-ref b "duals"
                      )
                    )
                  )
                   1
                )
              )
            )
             (
              begin (
                let (
                  (
                    new_duals (
                      _list
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        idx 0
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
                                      < idx new_len
                                    )
                                     (
                                      begin (
                                        set! new_duals (
                                          append new_duals (
                                            _list 0.0
                                          )
                                        )
                                      )
                                       (
                                        set! idx (
                                          + idx 1
                                        )
                                      )
                                       (
                                        loop20
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
                              loop20
                            )
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
                                          < i (
                                            _len (
                                              hash-table-ref a "duals"
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
                                                              < j (
                                                                _len (
                                                                  hash-table-ref b "duals"
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                let (
                                                                  (
                                                                    pos (
                                                                      + (
                                                                        + i j
                                                                      )
                                                                       1
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        val (
                                                                          _add (
                                                                            list-ref new_duals pos
                                                                          )
                                                                           (
                                                                            * (
                                                                              list-ref (
                                                                                hash-table-ref a "duals"
                                                                              )
                                                                               i
                                                                            )
                                                                             (
                                                                              list-ref (
                                                                                hash-table-ref b "duals"
                                                                              )
                                                                               j
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        list-set! new_duals pos val
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
                                                set! i (
                                                  + i 1
                                                )
                                              )
                                            )
                                          )
                                           (
                                            loop22
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
                                  loop22
                                )
                              )
                            )
                          )
                           (
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
                                                _len (
                                                  hash-table-ref a "duals"
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    val (
                                                      _add (
                                                        list-ref new_duals k
                                                      )
                                                       (
                                                        * (
                                                          list-ref (
                                                            hash-table-ref a "duals"
                                                          )
                                                           k
                                                        )
                                                         (
                                                          hash-table-ref b "real"
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    list-set! new_duals k val
                                                  )
                                                   (
                                                    set! k (
                                                      + k 1
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                loop26
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
                                      loop26
                                    )
                                  )
                                )
                              )
                               (
                                let (
                                  (
                                    l 0
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
                                                  < l (
                                                    _len (
                                                      hash-table-ref b "duals"
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        val (
                                                          _add (
                                                            list-ref new_duals l
                                                          )
                                                           (
                                                            * (
                                                              list-ref (
                                                                hash-table-ref b "duals"
                                                              )
                                                               l
                                                            )
                                                             (
                                                              hash-table-ref a "real"
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        list-set! new_duals l val
                                                      )
                                                       (
                                                        set! l (
                                                          + l 1
                                                        )
                                                      )
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
                                    ret19 (
                                      alist->hash-table (
                                        _list (
                                          cons "real" (
                                            * (
                                              hash-table-ref a "real"
                                            )
                                             (
                                              hash-table-ref b "real"
                                            )
                                          )
                                        )
                                         (
                                          cons "duals" new_duals
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
        dual_mul_real a b
      )
       (
        call/cc (
          lambda (
            ret30
          )
           (
            let (
              (
                ds (
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
                                    _len (
                                      hash-table-ref a "duals"
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    set! ds (
                                      append ds (
                                        _list (
                                          * (
                                            list-ref (
                                              hash-table-ref a "duals"
                                            )
                                             i
                                          )
                                           b
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
                      alist->hash-table (
                        _list (
                          cons "real" (
                            * (
                              hash-table-ref a "real"
                            )
                             b
                          )
                        )
                         (
                          cons "duals" ds
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
        dual_pow x n
      )
       (
        call/cc (
          lambda (
            ret33
          )
           (
            begin (
              if (
                < n 0
              )
               (
                begin (
                  panic "power must be a positive integer"
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
                  ret33 (
                    alist->hash-table (
                      _list (
                        cons "real" 1.0
                      )
                       (
                        cons "duals" (
                          _list
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
              let (
                (
                  res x
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
                                    < i n
                                  )
                                   (
                                    begin (
                                      set! res (
                                        dual_mul res x
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
                                   (
                                    quote (
                                      
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
                      )
                    )
                     (
                      ret33 res
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
        factorial n
      )
       (
        call/cc (
          lambda (
            ret36
          )
           (
            let (
              (
                res 1.0
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
                        break38
                      )
                       (
                        letrec (
                          (
                            loop37 (
                              lambda (
                                
                              )
                               (
                                if (
                                  <= i n
                                )
                                 (
                                  begin (
                                    set! res (
                                      * res (
                                        + 0.0 i
                                      )
                                    )
                                  )
                                   (
                                    set! i (
                                      + i 1
                                    )
                                  )
                                   (
                                    loop37
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
                          loop37
                        )
                      )
                    )
                  )
                   (
                    ret36 res
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
        differentiate func position order
      )
       (
        call/cc (
          lambda (
            ret39
          )
           (
            let (
              (
                d (
                  make_dual position 1
                )
              )
            )
             (
              begin (
                let (
                  (
                    result (
                      func d
                    )
                  )
                )
                 (
                  begin (
                    if (
                      equal? order 0
                    )
                     (
                      begin (
                        ret39 (
                          hash-table-ref result "real"
                        )
                      )
                    )
                     (
                      quote (
                        
                      )
                    )
                  )
                   (
                    ret39 (
                      * (
                        list-ref (
                          hash-table-ref result "duals"
                        )
                         (
                          - order 1
                        )
                      )
                       (
                        factorial order
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
        test_differentiate
      )
       (
        call/cc (
          lambda (
            ret40
          )
           (
            begin (
              define (
                f1 x
              )
               (
                call/cc (
                  lambda (
                    ret41
                  )
                   (
                    ret41 (
                      dual_pow x 2
                    )
                  )
                )
              )
            )
             (
              if (
                not (
                  equal? (
                    differentiate f1 2.0 2
                  )
                   2.0
                )
              )
               (
                begin (
                  panic "f1 failed"
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              define (
                f2 x
              )
               (
                call/cc (
                  lambda (
                    ret42
                  )
                   (
                    ret42 (
                      dual_mul (
                        dual_pow x 2
                      )
                       (
                        dual_pow x 4
                      )
                    )
                  )
                )
              )
            )
             (
              if (
                not (
                  equal? (
                    differentiate f2 9.0 2
                  )
                   196830.0
                )
              )
               (
                begin (
                  panic "f2 failed"
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              define (
                f3 y
              )
               (
                call/cc (
                  lambda (
                    ret43
                  )
                   (
                    ret43 (
                      dual_mul_real (
                        dual_pow (
                          dual_add_real y 3.0
                        )
                         6
                      )
                       0.5
                    )
                  )
                )
              )
            )
             (
              if (
                not (
                  equal? (
                    differentiate f3 3.5 4
                  )
                   7605.0
                )
              )
               (
                begin (
                  panic "f3 failed"
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              define (
                f4 y
              )
               (
                call/cc (
                  lambda (
                    ret44
                  )
                   (
                    ret44 (
                      dual_pow y 2
                    )
                  )
                )
              )
            )
             (
              if (
                not (
                  equal? (
                    differentiate f4 4.0 3
                  )
                   0.0
                )
              )
               (
                begin (
                  panic "f4 failed"
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
    )
     (
      define (
        main
      )
       (
        call/cc (
          lambda (
            ret45
          )
           (
            begin (
              test_differentiate
            )
             (
              define (
                f y
              )
               (
                call/cc (
                  lambda (
                    ret46
                  )
                   (
                    ret46 (
                      dual_mul (
                        dual_pow y 2
                      )
                       (
                        dual_pow y 4
                      )
                    )
                  )
                )
              )
            )
             (
              let (
                (
                  res (
                    differentiate f 9.0 2
                  )
                )
              )
               (
                begin (
                  _display (
                    if (
                      string? res
                    )
                     res (
                      to-str res
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
      main
    )
     (
      let (
        (
          end48 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur49 (
              quotient (
                * (
                  - end48 start47
                )
                 1000000
              )
               jps50
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur49
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
