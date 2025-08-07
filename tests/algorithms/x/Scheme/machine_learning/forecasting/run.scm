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
      start70 (
        current-jiffy
      )
    )
     (
      jps73 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        int_to_float x
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            ret1 (
              * x 1.0
            )
          )
        )
      )
    )
     (
      define (
        abs_float x
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            begin (
              if (
                < x 0.0
              )
               (
                begin (
                  ret2 (
                    - 0.0 x
                  )
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              ret2 x
            )
          )
        )
      )
    )
     (
      define (
        exp_approx x
      )
       (
        call/cc (
          lambda (
            ret3
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
                                      < i 10
                                    )
                                     (
                                      begin (
                                        set! term (
                                          _div (
                                            * term x
                                          )
                                           (
                                            int_to_float i
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
                        ret3 sum
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
        floor_int x
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
                              _le (
                                int_to_float (
                                  + i 1
                                )
                              )
                               x
                            )
                             (
                              begin (
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
                ret6 i
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
            ret9
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
                    ret9 s
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
        transpose m
      )
       (
        call/cc (
          lambda (
            ret12
          )
           (
            let (
              (
                rows (
                  _len m
                )
              )
            )
             (
              begin (
                let (
                  (
                    cols (
                      _len (
                        list-ref m 0
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
                                                                                list-ref m i
                                                                              )
                                                                            )
                                                                             (
                                                                              _substring (
                                                                                list-ref m i
                                                                              )
                                                                               j (
                                                                                + j 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            (
                                                                              hash-table? (
                                                                                list-ref m i
                                                                              )
                                                                            )
                                                                             (
                                                                              hash-table-ref (
                                                                                list-ref m i
                                                                              )
                                                                               j
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref (
                                                                                list-ref m i
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
                                                                    loop15
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
                                                          loop15
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
      )
    )
     (
      define (
        matmul a b
      )
       (
        call/cc (
          lambda (
            ret17
          )
           (
            let (
              (
                n (
                  _len a
                )
              )
            )
             (
              begin (
                let (
                  (
                    m (
                      _len (
                        list-ref b 0
                      )
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        p (
                          _len b
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
                                                                      < j m
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            s 0.0
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
                                                                                              < k p
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                set! s (
                                                                                                  _add s (
                                                                                                    * (
                                                                                                      cond (
                                                                                                        (
                                                                                                          string? (
                                                                                                            list-ref a i
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          _substring (
                                                                                                            list-ref a i
                                                                                                          )
                                                                                                           k (
                                                                                                            + k 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? (
                                                                                                            list-ref a i
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref (
                                                                                                            list-ref a i
                                                                                                          )
                                                                                                           k
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref (
                                                                                                            list-ref a i
                                                                                                          )
                                                                                                           k
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      cond (
                                                                                                        (
                                                                                                          string? (
                                                                                                            list-ref b k
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          _substring (
                                                                                                            list-ref b k
                                                                                                          )
                                                                                                           j (
                                                                                                            + j 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? (
                                                                                                            list-ref b k
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref (
                                                                                                            list-ref b k
                                                                                                          )
                                                                                                           j
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref (
                                                                                                            list-ref b k
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
                                                                                set! row (
                                                                                  append row (
                                                                                    _list s
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
                                ret17 res
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
        matvec a b
      )
       (
        call/cc (
          lambda (
            ret24
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
                        break26
                      )
                       (
                        letrec (
                          (
                            loop25 (
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
                                    set! res (
                                      append res (
                                        _list (
                                          dot (
                                            list-ref a i
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
                                    loop25
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
                          loop25
                        )
                      )
                    )
                  )
                   (
                    ret24 res
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
        identity n
      )
       (
        call/cc (
          lambda (
            ret27
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
                                                          < j n
                                                        )
                                                         (
                                                          begin (
                                                            set! row (
                                                              append row (
                                                                _list (
                                                                  if (
                                                                    equal? i j
                                                                  )
                                                                   1.0 0.0
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
                                                            loop30
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
                                                  loop30
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
                    ret27 res
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
        invert mat
      )
       (
        call/cc (
          lambda (
            ret32
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
                    a mat
                  )
                )
                 (
                  begin (
                    let (
                      (
                        inv (
                          identity n
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
                                break34
                              )
                               (
                                letrec (
                                  (
                                    loop33 (
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
                                                pivot (
                                                  cond (
                                                    (
                                                      string? (
                                                        list-ref a i
                                                      )
                                                    )
                                                     (
                                                      _substring (
                                                        list-ref a i
                                                      )
                                                       i (
                                                        + i 1
                                                      )
                                                    )
                                                  )
                                                   (
                                                    (
                                                      hash-table? (
                                                        list-ref a i
                                                      )
                                                    )
                                                     (
                                                      hash-table-ref (
                                                        list-ref a i
                                                      )
                                                       i
                                                    )
                                                  )
                                                   (
                                                    else (
                                                      list-ref (
                                                        list-ref a i
                                                      )
                                                       i
                                                    )
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
                                                    call/cc (
                                                      lambda (
                                                        break36
                                                      )
                                                       (
                                                        letrec (
                                                          (
                                                            loop35 (
                                                              lambda (
                                                                
                                                              )
                                                               (
                                                                if (
                                                                  < j n
                                                                )
                                                                 (
                                                                  begin (
                                                                    list-set! (
                                                                      list-ref a i
                                                                    )
                                                                     j (
                                                                      _div (
                                                                        cond (
                                                                          (
                                                                            string? (
                                                                              list-ref a i
                                                                            )
                                                                          )
                                                                           (
                                                                            _substring (
                                                                              list-ref a i
                                                                            )
                                                                             j (
                                                                              + j 1
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          (
                                                                            hash-table? (
                                                                              list-ref a i
                                                                            )
                                                                          )
                                                                           (
                                                                            hash-table-ref (
                                                                              list-ref a i
                                                                            )
                                                                             j
                                                                          )
                                                                        )
                                                                         (
                                                                          else (
                                                                            list-ref (
                                                                              list-ref a i
                                                                            )
                                                                             j
                                                                          )
                                                                        )
                                                                      )
                                                                       pivot
                                                                    )
                                                                  )
                                                                   (
                                                                    list-set! (
                                                                      list-ref inv i
                                                                    )
                                                                     j (
                                                                      _div (
                                                                        cond (
                                                                          (
                                                                            string? (
                                                                              cond (
                                                                                (
                                                                                  string? inv
                                                                                )
                                                                                 (
                                                                                  _substring inv i (
                                                                                    + i 1
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                (
                                                                                  hash-table? inv
                                                                                )
                                                                                 (
                                                                                  hash-table-ref inv i
                                                                                )
                                                                              )
                                                                               (
                                                                                else (
                                                                                  list-ref inv i
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            _substring (
                                                                              cond (
                                                                                (
                                                                                  string? inv
                                                                                )
                                                                                 (
                                                                                  _substring inv i (
                                                                                    + i 1
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                (
                                                                                  hash-table? inv
                                                                                )
                                                                                 (
                                                                                  hash-table-ref inv i
                                                                                )
                                                                              )
                                                                               (
                                                                                else (
                                                                                  list-ref inv i
                                                                                )
                                                                              )
                                                                            )
                                                                             j (
                                                                              + j 1
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          (
                                                                            hash-table? (
                                                                              cond (
                                                                                (
                                                                                  string? inv
                                                                                )
                                                                                 (
                                                                                  _substring inv i (
                                                                                    + i 1
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                (
                                                                                  hash-table? inv
                                                                                )
                                                                                 (
                                                                                  hash-table-ref inv i
                                                                                )
                                                                              )
                                                                               (
                                                                                else (
                                                                                  list-ref inv i
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            hash-table-ref (
                                                                              cond (
                                                                                (
                                                                                  string? inv
                                                                                )
                                                                                 (
                                                                                  _substring inv i (
                                                                                    + i 1
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                (
                                                                                  hash-table? inv
                                                                                )
                                                                                 (
                                                                                  hash-table-ref inv i
                                                                                )
                                                                              )
                                                                               (
                                                                                else (
                                                                                  list-ref inv i
                                                                                )
                                                                              )
                                                                            )
                                                                             j
                                                                          )
                                                                        )
                                                                         (
                                                                          else (
                                                                            list-ref (
                                                                              cond (
                                                                                (
                                                                                  string? inv
                                                                                )
                                                                                 (
                                                                                  _substring inv i (
                                                                                    + i 1
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                (
                                                                                  hash-table? inv
                                                                                )
                                                                                 (
                                                                                  hash-table-ref inv i
                                                                                )
                                                                              )
                                                                               (
                                                                                else (
                                                                                  list-ref inv i
                                                                                )
                                                                              )
                                                                            )
                                                                             j
                                                                          )
                                                                        )
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
                                                                    loop35
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
                                                          loop35
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
                                                                      < k n
                                                                    )
                                                                     (
                                                                      begin (
                                                                        if (
                                                                          not (
                                                                            equal? k i
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                factor (
                                                                                  cond (
                                                                                    (
                                                                                      string? (
                                                                                        list-ref a k
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref a k
                                                                                      )
                                                                                       i (
                                                                                        + i 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref a k
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref a k
                                                                                      )
                                                                                       i
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref a k
                                                                                      )
                                                                                       i
                                                                                    )
                                                                                  )
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
                                                                                    break40
                                                                                  )
                                                                                   (
                                                                                    letrec (
                                                                                      (
                                                                                        loop39 (
                                                                                          lambda (
                                                                                            
                                                                                          )
                                                                                           (
                                                                                            if (
                                                                                              < j n
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                list-set! (
                                                                                                  list-ref a k
                                                                                                )
                                                                                                 j (
                                                                                                  - (
                                                                                                    cond (
                                                                                                      (
                                                                                                        string? (
                                                                                                          list-ref a k
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        _substring (
                                                                                                          list-ref a k
                                                                                                        )
                                                                                                         j (
                                                                                                          + j 1
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      (
                                                                                                        hash-table? (
                                                                                                          list-ref a k
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        hash-table-ref (
                                                                                                          list-ref a k
                                                                                                        )
                                                                                                         j
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      else (
                                                                                                        list-ref (
                                                                                                          list-ref a k
                                                                                                        )
                                                                                                         j
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    * factor (
                                                                                                      cond (
                                                                                                        (
                                                                                                          string? (
                                                                                                            list-ref a i
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          _substring (
                                                                                                            list-ref a i
                                                                                                          )
                                                                                                           j (
                                                                                                            + j 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? (
                                                                                                            list-ref a i
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref (
                                                                                                            list-ref a i
                                                                                                          )
                                                                                                           j
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref (
                                                                                                            list-ref a i
                                                                                                          )
                                                                                                           j
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                list-set! (
                                                                                                  list-ref inv k
                                                                                                )
                                                                                                 j (
                                                                                                  - (
                                                                                                    cond (
                                                                                                      (
                                                                                                        string? (
                                                                                                          cond (
                                                                                                            (
                                                                                                              string? inv
                                                                                                            )
                                                                                                             (
                                                                                                              _substring inv k (
                                                                                                                + k 1
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                           (
                                                                                                            (
                                                                                                              hash-table? inv
                                                                                                            )
                                                                                                             (
                                                                                                              hash-table-ref inv k
                                                                                                            )
                                                                                                          )
                                                                                                           (
                                                                                                            else (
                                                                                                              list-ref inv k
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        _substring (
                                                                                                          cond (
                                                                                                            (
                                                                                                              string? inv
                                                                                                            )
                                                                                                             (
                                                                                                              _substring inv k (
                                                                                                                + k 1
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                           (
                                                                                                            (
                                                                                                              hash-table? inv
                                                                                                            )
                                                                                                             (
                                                                                                              hash-table-ref inv k
                                                                                                            )
                                                                                                          )
                                                                                                           (
                                                                                                            else (
                                                                                                              list-ref inv k
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                         j (
                                                                                                          + j 1
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      (
                                                                                                        hash-table? (
                                                                                                          cond (
                                                                                                            (
                                                                                                              string? inv
                                                                                                            )
                                                                                                             (
                                                                                                              _substring inv k (
                                                                                                                + k 1
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                           (
                                                                                                            (
                                                                                                              hash-table? inv
                                                                                                            )
                                                                                                             (
                                                                                                              hash-table-ref inv k
                                                                                                            )
                                                                                                          )
                                                                                                           (
                                                                                                            else (
                                                                                                              list-ref inv k
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        hash-table-ref (
                                                                                                          cond (
                                                                                                            (
                                                                                                              string? inv
                                                                                                            )
                                                                                                             (
                                                                                                              _substring inv k (
                                                                                                                + k 1
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                           (
                                                                                                            (
                                                                                                              hash-table? inv
                                                                                                            )
                                                                                                             (
                                                                                                              hash-table-ref inv k
                                                                                                            )
                                                                                                          )
                                                                                                           (
                                                                                                            else (
                                                                                                              list-ref inv k
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                         j
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      else (
                                                                                                        list-ref (
                                                                                                          cond (
                                                                                                            (
                                                                                                              string? inv
                                                                                                            )
                                                                                                             (
                                                                                                              _substring inv k (
                                                                                                                + k 1
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                           (
                                                                                                            (
                                                                                                              hash-table? inv
                                                                                                            )
                                                                                                             (
                                                                                                              hash-table-ref inv k
                                                                                                            )
                                                                                                          )
                                                                                                           (
                                                                                                            else (
                                                                                                              list-ref inv k
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                         j
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    * factor (
                                                                                                      cond (
                                                                                                        (
                                                                                                          string? (
                                                                                                            cond (
                                                                                                              (
                                                                                                                string? inv
                                                                                                              )
                                                                                                               (
                                                                                                                _substring inv i (
                                                                                                                  + i 1
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              (
                                                                                                                hash-table? inv
                                                                                                              )
                                                                                                               (
                                                                                                                hash-table-ref inv i
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              else (
                                                                                                                list-ref inv i
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          _substring (
                                                                                                            cond (
                                                                                                              (
                                                                                                                string? inv
                                                                                                              )
                                                                                                               (
                                                                                                                _substring inv i (
                                                                                                                  + i 1
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              (
                                                                                                                hash-table? inv
                                                                                                              )
                                                                                                               (
                                                                                                                hash-table-ref inv i
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              else (
                                                                                                                list-ref inv i
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                           j (
                                                                                                            + j 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? (
                                                                                                            cond (
                                                                                                              (
                                                                                                                string? inv
                                                                                                              )
                                                                                                               (
                                                                                                                _substring inv i (
                                                                                                                  + i 1
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              (
                                                                                                                hash-table? inv
                                                                                                              )
                                                                                                               (
                                                                                                                hash-table-ref inv i
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              else (
                                                                                                                list-ref inv i
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref (
                                                                                                            cond (
                                                                                                              (
                                                                                                                string? inv
                                                                                                              )
                                                                                                               (
                                                                                                                _substring inv i (
                                                                                                                  + i 1
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              (
                                                                                                                hash-table? inv
                                                                                                              )
                                                                                                               (
                                                                                                                hash-table-ref inv i
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              else (
                                                                                                                list-ref inv i
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                           j
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref (
                                                                                                            cond (
                                                                                                              (
                                                                                                                string? inv
                                                                                                              )
                                                                                                               (
                                                                                                                _substring inv i (
                                                                                                                  + i 1
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              (
                                                                                                                hash-table? inv
                                                                                                              )
                                                                                                               (
                                                                                                                hash-table-ref inv i
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              else (
                                                                                                                list-ref inv i
                                                                                                              )
                                                                                                            )
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
                                                                                                loop39
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
                                                                                      loop39
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
                                                                        set! k (
                                                                          + k 1
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
                                            loop33
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
                                  loop33
                                )
                              )
                            )
                          )
                           (
                            ret32 inv
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
        normal_equation X y
      )
       (
        call/cc (
          lambda (
            ret41
          )
           (
            let (
              (
                Xt (
                  transpose X
                )
              )
            )
             (
              begin (
                let (
                  (
                    XtX (
                      matmul Xt X
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        XtX_inv (
                          invert XtX
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            Xty (
                              matvec Xt y
                            )
                          )
                        )
                         (
                          begin (
                            ret41 (
                              matvec XtX_inv Xty
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
        linear_regression_prediction train_dt train_usr train_mtch test_dt test_mtch
      )
       (
        call/cc (
          lambda (
            ret42
          )
           (
            let (
              (
                X (
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
                        break44
                      )
                       (
                        letrec (
                          (
                            loop43 (
                              lambda (
                                
                              )
                               (
                                if (
                                  < i (
                                    _len train_dt
                                  )
                                )
                                 (
                                  begin (
                                    set! X (
                                      append X (
                                        _list (
                                          _list 1.0 (
                                            list-ref train_dt i
                                          )
                                           (
                                            list-ref train_mtch i
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
                                    loop43
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
                          loop43
                        )
                      )
                    )
                  )
                   (
                    let (
                      (
                        beta (
                          normal_equation X train_usr
                        )
                      )
                    )
                     (
                      begin (
                        ret42 (
                          abs_float (
                            _add (
                              _add (
                                cond (
                                  (
                                    string? beta
                                  )
                                   (
                                    _substring beta 0 (
                                      + 0 1
                                    )
                                  )
                                )
                                 (
                                  (
                                    hash-table? beta
                                  )
                                   (
                                    hash-table-ref beta 0
                                  )
                                )
                                 (
                                  else (
                                    list-ref beta 0
                                  )
                                )
                              )
                               (
                                * (
                                  list-ref test_dt 0
                                )
                                 (
                                  cond (
                                    (
                                      string? beta
                                    )
                                     (
                                      _substring beta 1 (
                                        + 1 1
                                      )
                                    )
                                  )
                                   (
                                    (
                                      hash-table? beta
                                    )
                                     (
                                      hash-table-ref beta 1
                                    )
                                  )
                                   (
                                    else (
                                      list-ref beta 1
                                    )
                                  )
                                )
                              )
                            )
                             (
                              * (
                                list-ref test_mtch 0
                              )
                               (
                                cond (
                                  (
                                    string? beta
                                  )
                                   (
                                    _substring beta 2 (
                                      + 2 1
                                    )
                                  )
                                )
                                 (
                                  (
                                    hash-table? beta
                                  )
                                   (
                                    hash-table-ref beta 2
                                  )
                                )
                                 (
                                  else (
                                    list-ref beta 2
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
        sarimax_predictor train_user train_match test_match
      )
       (
        call/cc (
          lambda (
            ret45
          )
           (
            let (
              (
                n (
                  _len train_user
                )
              )
            )
             (
              begin (
                let (
                  (
                    X (
                      _list
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        y (
                          _list
                        )
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
                                break47
                              )
                               (
                                letrec (
                                  (
                                    loop46 (
                                      lambda (
                                        
                                      )
                                       (
                                        if (
                                          < i n
                                        )
                                         (
                                          begin (
                                            set! X (
                                              append X (
                                                _list (
                                                  _list 1.0 (
                                                    list-ref train_user (
                                                      - i 1
                                                    )
                                                  )
                                                   (
                                                    list-ref train_match i
                                                  )
                                                )
                                              )
                                            )
                                          )
                                           (
                                            set! y (
                                              append y (
                                                _list (
                                                  list-ref train_user i
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
                                            loop46
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
                                  loop46
                                )
                              )
                            )
                          )
                           (
                            let (
                              (
                                beta (
                                  normal_equation X y
                                )
                              )
                            )
                             (
                              begin (
                                ret45 (
                                  _add (
                                    _add (
                                      cond (
                                        (
                                          string? beta
                                        )
                                         (
                                          _substring beta 0 (
                                            + 0 1
                                          )
                                        )
                                      )
                                       (
                                        (
                                          hash-table? beta
                                        )
                                         (
                                          hash-table-ref beta 0
                                        )
                                      )
                                       (
                                        else (
                                          list-ref beta 0
                                        )
                                      )
                                    )
                                     (
                                      * (
                                        cond (
                                          (
                                            string? beta
                                          )
                                           (
                                            _substring beta 1 (
                                              + 1 1
                                            )
                                          )
                                        )
                                         (
                                          (
                                            hash-table? beta
                                          )
                                           (
                                            hash-table-ref beta 1
                                          )
                                        )
                                         (
                                          else (
                                            list-ref beta 1
                                          )
                                        )
                                      )
                                       (
                                        list-ref train_user (
                                          - n 1
                                        )
                                      )
                                    )
                                  )
                                   (
                                    * (
                                      cond (
                                        (
                                          string? beta
                                        )
                                         (
                                          _substring beta 2 (
                                            + 2 1
                                          )
                                        )
                                      )
                                       (
                                        (
                                          hash-table? beta
                                        )
                                         (
                                          hash-table-ref beta 2
                                        )
                                      )
                                       (
                                        else (
                                          list-ref beta 2
                                        )
                                      )
                                    )
                                     (
                                      list-ref test_match 0
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
        rbf_kernel a b gamma
      )
       (
        call/cc (
          lambda (
            ret48
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
                        break50
                      )
                       (
                        letrec (
                          (
                            loop49 (
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
                                        diff (
                                          - (
                                            list-ref a i
                                          )
                                           (
                                            list-ref b i
                                          )
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        set! sum (
                                          _add sum (
                                            * diff diff
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
                                    loop49
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
                          loop49
                        )
                      )
                    )
                  )
                   (
                    ret48 (
                      exp_approx (
                        * (
                          - gamma
                        )
                         sum
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
        support_vector_regressor x_train x_test train_user
      )
       (
        call/cc (
          lambda (
            ret51
          )
           (
            let (
              (
                gamma 0.1
              )
            )
             (
              begin (
                let (
                  (
                    weights (
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
                            break53
                          )
                           (
                            letrec (
                              (
                                loop52 (
                                  lambda (
                                    
                                  )
                                   (
                                    if (
                                      < i (
                                        _len x_train
                                      )
                                    )
                                     (
                                      begin (
                                        set! weights (
                                          append weights (
                                            _list (
                                              rbf_kernel (
                                                list-ref x_train i
                                              )
                                               (
                                                list-ref x_test 0
                                              )
                                               gamma
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
                                        loop52
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
                              loop52
                            )
                          )
                        )
                      )
                       (
                        let (
                          (
                            num 0.0
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                den 0.0
                              )
                            )
                             (
                              begin (
                                set! i 0
                              )
                               (
                                call/cc (
                                  lambda (
                                    break55
                                  )
                                   (
                                    letrec (
                                      (
                                        loop54 (
                                          lambda (
                                            
                                          )
                                           (
                                            if (
                                              < i (
                                                _len train_user
                                              )
                                            )
                                             (
                                              begin (
                                                set! num (
                                                  _add num (
                                                    * (
                                                      list-ref weights i
                                                    )
                                                     (
                                                      list-ref train_user i
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                set! den (
                                                  + den (
                                                    list-ref weights i
                                                  )
                                                )
                                              )
                                               (
                                                set! i (
                                                  + i 1
                                                )
                                              )
                                               (
                                                loop54
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
                                      loop54
                                    )
                                  )
                                )
                              )
                               (
                                ret51 (
                                  _div num den
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
        set_at_float xs idx value
      )
       (
        call/cc (
          lambda (
            ret56
          )
           (
            let (
              (
                i 0
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
                    call/cc (
                      lambda (
                        break58
                      )
                       (
                        letrec (
                          (
                            loop57 (
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
                                    if (
                                      equal? i idx
                                    )
                                     (
                                      begin (
                                        set! res (
                                          append res (
                                            _list value
                                          )
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        set! res (
                                          append res (
                                            _list (
                                              list-ref xs i
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
                                    loop57
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
                          loop57
                        )
                      )
                    )
                  )
                   (
                    ret56 res
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
        sort_float xs
      )
       (
        call/cc (
          lambda (
            ret59
          )
           (
            let (
              (
                res xs
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
                        break61
                      )
                       (
                        letrec (
                          (
                            loop60 (
                              lambda (
                                
                              )
                               (
                                if (
                                  < i (
                                    _len res
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        key (
                                          list-ref res i
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            j (
                                              - i 1
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            call/cc (
                                              lambda (
                                                break63
                                              )
                                               (
                                                letrec (
                                                  (
                                                    loop62 (
                                                      lambda (
                                                        
                                                      )
                                                       (
                                                        if (
                                                          and (
                                                            >= j 0
                                                          )
                                                           (
                                                            > (
                                                              list-ref res j
                                                            )
                                                             key
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            set! res (
                                                              set_at_float res (
                                                                + j 1
                                                              )
                                                               (
                                                                list-ref res j
                                                              )
                                                            )
                                                          )
                                                           (
                                                            set! j (
                                                              - j 1
                                                            )
                                                          )
                                                           (
                                                            loop62
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
                                                  loop62
                                                )
                                              )
                                            )
                                          )
                                           (
                                            set! res (
                                              set_at_float res (
                                                + j 1
                                              )
                                               key
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
                                    loop60
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
                          loop60
                        )
                      )
                    )
                  )
                   (
                    ret59 res
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
        percentile data q
      )
       (
        call/cc (
          lambda (
            ret64
          )
           (
            let (
              (
                sorted (
                  sort_float data
                )
              )
            )
             (
              begin (
                let (
                  (
                    n (
                      _len sorted
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        pos (
                          * (
                            _div q 100.0
                          )
                           (
                            int_to_float (
                              - n 1
                            )
                          )
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            idx (
                              floor_int pos
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                frac (
                                  - pos (
                                    int_to_float idx
                                  )
                                )
                              )
                            )
                             (
                              begin (
                                if (
                                  _lt (
                                    _add idx 1
                                  )
                                   n
                                )
                                 (
                                  begin (
                                    ret64 (
                                      _add (
                                        * (
                                          cond (
                                            (
                                              string? sorted
                                            )
                                             (
                                              _substring sorted idx (
                                                + idx 1
                                              )
                                            )
                                          )
                                           (
                                            (
                                              hash-table? sorted
                                            )
                                             (
                                              hash-table-ref sorted idx
                                            )
                                          )
                                           (
                                            else (
                                              list-ref sorted idx
                                            )
                                          )
                                        )
                                         (
                                          - 1.0 frac
                                        )
                                      )
                                       (
                                        * (
                                          cond (
                                            (
                                              string? sorted
                                            )
                                             (
                                              _substring sorted (
                                                _add idx 1
                                              )
                                               (
                                                + (
                                                  _add idx 1
                                                )
                                                 1
                                              )
                                            )
                                          )
                                           (
                                            (
                                              hash-table? sorted
                                            )
                                             (
                                              hash-table-ref sorted (
                                                _add idx 1
                                              )
                                            )
                                          )
                                           (
                                            else (
                                              list-ref sorted (
                                                _add idx 1
                                              )
                                            )
                                          )
                                        )
                                         frac
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
                                ret64 (
                                  cond (
                                    (
                                      string? sorted
                                    )
                                     (
                                      _substring sorted idx (
                                        + idx 1
                                      )
                                    )
                                  )
                                   (
                                    (
                                      hash-table? sorted
                                    )
                                     (
                                      hash-table-ref sorted idx
                                    )
                                  )
                                   (
                                    else (
                                      list-ref sorted idx
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
        interquartile_range_checker train_user
      )
       (
        call/cc (
          lambda (
            ret65
          )
           (
            let (
              (
                q1 (
                  percentile train_user 25.0
                )
              )
            )
             (
              begin (
                let (
                  (
                    q3 (
                      percentile train_user 75.0
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        iqr (
                          - q3 q1
                        )
                      )
                    )
                     (
                      begin (
                        ret65 (
                          - q1 (
                            * iqr 0.1
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
        data_safety_checker list_vote actual_result
      )
       (
        call/cc (
          lambda (
            ret66
          )
           (
            let (
              (
                safe 0
              )
            )
             (
              begin (
                let (
                  (
                    not_safe 0
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
                            break68
                          )
                           (
                            letrec (
                              (
                                loop67 (
                                  lambda (
                                    
                                  )
                                   (
                                    if (
                                      < i (
                                        _len list_vote
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            v (
                                              list-ref list_vote i
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              > v actual_result
                                            )
                                             (
                                              begin (
                                                set! safe (
                                                  + not_safe 1
                                                )
                                              )
                                            )
                                             (
                                              if (
                                                _le (
                                                  abs_float (
                                                    - (
                                                      abs_float v
                                                    )
                                                     (
                                                      abs_float actual_result
                                                    )
                                                  )
                                                )
                                                 0.1
                                              )
                                               (
                                                begin (
                                                  set! safe (
                                                    + safe 1
                                                  )
                                                )
                                              )
                                               (
                                                begin (
                                                  set! not_safe (
                                                    + not_safe 1
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
                                        loop67
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
                              loop67
                            )
                          )
                        )
                      )
                       (
                        ret66 (
                          > safe not_safe
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
            ret69
          )
           (
            let (
              (
                vote (
                  _list (
                    linear_regression_prediction (
                      _list 2.0 3.0 4.0 5.0
                    )
                     (
                      _list 5.0 3.0 4.0 6.0
                    )
                     (
                      _list 3.0 1.0 2.0 4.0
                    )
                     (
                      _list 2.0
                    )
                     (
                      _list 2.0
                    )
                  )
                   (
                    sarimax_predictor (
                      _list 4.0 2.0 6.0 8.0
                    )
                     (
                      _list 3.0 1.0 2.0 4.0
                    )
                     (
                      _list 2.0
                    )
                  )
                   (
                    support_vector_regressor (
                      _list (
                        _list 5.0 2.0
                      )
                       (
                        _list 1.0 5.0
                      )
                       (
                        _list 6.0 2.0
                      )
                    )
                     (
                      _list (
                        _list 3.0 2.0
                      )
                    )
                     (
                      _list 2.0 1.0 4.0
                    )
                  )
                )
              )
            )
             (
              begin (
                _display (
                  if (
                    string? (
                      list-ref vote 0
                    )
                  )
                   (
                    list-ref vote 0
                  )
                   (
                    to-str (
                      list-ref vote 0
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
                      list-ref vote 1
                    )
                  )
                   (
                    list-ref vote 1
                  )
                   (
                    to-str (
                      list-ref vote 1
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
                      list-ref vote 2
                    )
                  )
                   (
                    list-ref vote 2
                  )
                   (
                    to-str (
                      list-ref vote 2
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
                      data_safety_checker vote 5.0
                    )
                  )
                   (
                    data_safety_checker vote 5.0
                  )
                   (
                    to-str (
                      data_safety_checker vote 5.0
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
     (
      main
    )
     (
      let (
        (
          end71 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur72 (
              quotient (
                * (
                  - end71 start70
                )
                 1000000
              )
               jps73
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur72
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
