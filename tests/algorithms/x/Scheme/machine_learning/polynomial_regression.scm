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
        design_matrix xs degree
      )
       (
        call/cc (
          lambda (
            ret1
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
                    matrix (
                      _list
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
                                  < i (
                                    _len xs
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
                                            let (
                                              (
                                                pow 1.0
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
                                                              <= j degree
                                                            )
                                                             (
                                                              begin (
                                                                set! row (
                                                                  append row (
                                                                    _list pow
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                set! pow (
                                                                  * pow (
                                                                    list-ref xs i
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
                                                set! matrix (
                                                  append matrix (
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
                    ret1 matrix
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
        transpose matrix
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
                  _len matrix
                )
              )
            )
             (
              begin (
                let (
                  (
                    cols (
                      _len (
                        list-ref matrix 0
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
                            result (
                              _list
                            )
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
                                                                                list-ref matrix i
                                                                              )
                                                                            )
                                                                             (
                                                                              _substring (
                                                                                list-ref matrix i
                                                                              )
                                                                               j (
                                                                                + j 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            (
                                                                              hash-table? (
                                                                                list-ref matrix i
                                                                              )
                                                                            )
                                                                             (
                                                                              hash-table-ref (
                                                                                list-ref matrix i
                                                                              )
                                                                               j
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref (
                                                                                list-ref matrix i
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
                                                    set! result (
                                                      append result (
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
                            ret6 result
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
        matmul A B
      )
       (
        call/cc (
          lambda (
            ret11
          )
           (
            let (
              (
                n (
                  _len A
                )
              )
            )
             (
              begin (
                let (
                  (
                    m (
                      _len (
                        list-ref A 0
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
                            list-ref B 0
                          )
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
                            let (
                              (
                                result (
                                  _list
                                )
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
                                                        k 0
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
                                                                      < k p
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
                                                                                              < j m
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                set! sum (
                                                                                                  _add sum (
                                                                                                    * (
                                                                                                      cond (
                                                                                                        (
                                                                                                          string? (
                                                                                                            list-ref A i
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          _substring (
                                                                                                            list-ref A i
                                                                                                          )
                                                                                                           j (
                                                                                                            + j 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? (
                                                                                                            list-ref A i
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref (
                                                                                                            list-ref A i
                                                                                                          )
                                                                                                           j
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref (
                                                                                                            list-ref A i
                                                                                                          )
                                                                                                           j
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      cond (
                                                                                                        (
                                                                                                          string? (
                                                                                                            list-ref B j
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          _substring (
                                                                                                            list-ref B j
                                                                                                          )
                                                                                                           k (
                                                                                                            + k 1
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? (
                                                                                                            list-ref B j
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          hash-table-ref (
                                                                                                            list-ref B j
                                                                                                          )
                                                                                                           k
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          list-ref (
                                                                                                            list-ref B j
                                                                                                          )
                                                                                                           k
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
                                                                                set! row (
                                                                                  append row (
                                                                                    _list sum
                                                                                  )
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
                                                        set! result (
                                                          append result (
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
                                ret11 result
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
        matvec_mul A v
      )
       (
        call/cc (
          lambda (
            ret18
          )
           (
            let (
              (
                n (
                  _len A
                )
              )
            )
             (
              begin (
                let (
                  (
                    m (
                      _len (
                        list-ref A 0
                      )
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
                        let (
                          (
                            result (
                              _list
                            )
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
                                          < i n
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
                                                                  < j m
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! sum (
                                                                      _add sum (
                                                                        * (
                                                                          cond (
                                                                            (
                                                                              string? (
                                                                                list-ref A i
                                                                              )
                                                                            )
                                                                             (
                                                                              _substring (
                                                                                list-ref A i
                                                                              )
                                                                               j (
                                                                                + j 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            (
                                                                              hash-table? (
                                                                                list-ref A i
                                                                              )
                                                                            )
                                                                             (
                                                                              hash-table-ref (
                                                                                list-ref A i
                                                                              )
                                                                               j
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref (
                                                                                list-ref A i
                                                                              )
                                                                               j
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          list-ref v j
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
                                                    set! result (
                                                      append result (
                                                        _list sum
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
                                         (
                                          quote (
                                            
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
                            )
                          )
                           (
                            ret18 result
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
        gaussian_elimination A b
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
                  _len A
                )
              )
            )
             (
              begin (
                let (
                  (
                    M (
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
                                        set! M (
                                          append M (
                                            _list (
                                              append (
                                                list-ref A i
                                              )
                                               (
                                                _list (
                                                  list-ref b i
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
                                          < k n
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                j (
                                                  + k 1
                                                )
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
                                                              < j n
                                                            )
                                                             (
                                                              begin (
                                                                let (
                                                                  (
                                                                    factor (
                                                                      _div (
                                                                        cond (
                                                                          (
                                                                            string? (
                                                                              list-ref M j
                                                                            )
                                                                          )
                                                                           (
                                                                            _substring (
                                                                              list-ref M j
                                                                            )
                                                                             k (
                                                                              + k 1
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          (
                                                                            hash-table? (
                                                                              list-ref M j
                                                                            )
                                                                          )
                                                                           (
                                                                            hash-table-ref (
                                                                              list-ref M j
                                                                            )
                                                                             k
                                                                          )
                                                                        )
                                                                         (
                                                                          else (
                                                                            list-ref (
                                                                              list-ref M j
                                                                            )
                                                                             k
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        cond (
                                                                          (
                                                                            string? (
                                                                              list-ref M k
                                                                            )
                                                                          )
                                                                           (
                                                                            _substring (
                                                                              list-ref M k
                                                                            )
                                                                             k (
                                                                              + k 1
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          (
                                                                            hash-table? (
                                                                              list-ref M k
                                                                            )
                                                                          )
                                                                           (
                                                                            hash-table-ref (
                                                                              list-ref M k
                                                                            )
                                                                             k
                                                                          )
                                                                        )
                                                                         (
                                                                          else (
                                                                            list-ref (
                                                                              list-ref M k
                                                                            )
                                                                             k
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
                                                                        rowj (
                                                                          list-ref M j
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            rowk (
                                                                              list-ref M k
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                l k
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
                                                                                              <= l n
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                list-set! rowj l (
                                                                                                  - (
                                                                                                    list-ref rowj l
                                                                                                  )
                                                                                                   (
                                                                                                    * factor (
                                                                                                      list-ref rowk l
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                set! l (
                                                                                                  + l 1
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
                                                                                list-set! M j rowj
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
                                x (
                                  _list
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    t 0
                                  )
                                )
                                 (
                                  begin (
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
                                                  < t n
                                                )
                                                 (
                                                  begin (
                                                    set! x (
                                                      append x (
                                                        _list 0.0
                                                      )
                                                    )
                                                  )
                                                   (
                                                    set! t (
                                                      + t 1
                                                    )
                                                  )
                                                   (
                                                    loop32
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
                                          loop32
                                        )
                                      )
                                    )
                                  )
                                   (
                                    let (
                                      (
                                        i2 (
                                          - n 1
                                        )
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
                                                      >= i2 0
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            sum (
                                                              cond (
                                                                (
                                                                  string? (
                                                                    list-ref M i2
                                                                  )
                                                                )
                                                                 (
                                                                  _substring (
                                                                    list-ref M i2
                                                                  )
                                                                   n (
                                                                    + n 1
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                (
                                                                  hash-table? (
                                                                    list-ref M i2
                                                                  )
                                                                )
                                                                 (
                                                                  hash-table-ref (
                                                                    list-ref M i2
                                                                  )
                                                                   n
                                                                )
                                                              )
                                                               (
                                                                else (
                                                                  list-ref (
                                                                    list-ref M i2
                                                                  )
                                                                   n
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                j2 (
                                                                  + i2 1
                                                                )
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
                                                                              < j2 n
                                                                            )
                                                                             (
                                                                              begin (
                                                                                set! sum (
                                                                                  - sum (
                                                                                    * (
                                                                                      cond (
                                                                                        (
                                                                                          string? (
                                                                                            list-ref M i2
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          _substring (
                                                                                            list-ref M i2
                                                                                          )
                                                                                           j2 (
                                                                                            + j2 1
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        (
                                                                                          hash-table? (
                                                                                            list-ref M i2
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          hash-table-ref (
                                                                                            list-ref M i2
                                                                                          )
                                                                                           j2
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        else (
                                                                                          list-ref (
                                                                                            list-ref M i2
                                                                                          )
                                                                                           j2
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      list-ref x j2
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                set! j2 (
                                                                                  + j2 1
                                                                                )
                                                                              )
                                                                               (
                                                                                loop36
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
                                                                      loop36
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                list-set! x i2 (
                                                                  _div sum (
                                                                    cond (
                                                                      (
                                                                        string? (
                                                                          list-ref M i2
                                                                        )
                                                                      )
                                                                       (
                                                                        _substring (
                                                                          list-ref M i2
                                                                        )
                                                                         i2 (
                                                                          + i2 1
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      (
                                                                        hash-table? (
                                                                          list-ref M i2
                                                                        )
                                                                      )
                                                                       (
                                                                        hash-table-ref (
                                                                          list-ref M i2
                                                                        )
                                                                         i2
                                                                      )
                                                                    )
                                                                     (
                                                                      else (
                                                                        list-ref (
                                                                          list-ref M i2
                                                                        )
                                                                         i2
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                set! i2 (
                                                                  - i2 1
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
                                        ret23 x
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
        predict xs coeffs
      )
       (
        call/cc (
          lambda (
            ret38
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
                    result (
                      _list
                    )
                  )
                )
                 (
                  begin (
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
                                  < i (
                                    _len xs
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        x (
                                          list-ref xs i
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
                                                pow 1.0
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
                                                    call/cc (
                                                      lambda (
                                                        break42
                                                      )
                                                       (
                                                        letrec (
                                                          (
                                                            loop41 (
                                                              lambda (
                                                                
                                                              )
                                                               (
                                                                if (
                                                                  < j (
                                                                    _len coeffs
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! sum (
                                                                      _add sum (
                                                                        * (
                                                                          list-ref coeffs j
                                                                        )
                                                                         pow
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    set! pow (
                                                                      * pow x
                                                                    )
                                                                  )
                                                                   (
                                                                    set! j (
                                                                      + j 1
                                                                    )
                                                                  )
                                                                   (
                                                                    loop41
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
                                                          loop41
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    set! result (
                                                      append result (
                                                        _list sum
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
                   (
                    ret38 result
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
          xs (
            _list 0.0 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 10.0
          )
        )
      )
       (
        begin (
          let (
            (
              ys (
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
                                  _len xs
                                )
                              )
                               (
                                begin (
                                  let (
                                    (
                                      x (
                                        list-ref xs i
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      set! ys (
                                        append ys (
                                          _list (
                                            - (
                                              _add (
                                                - (
                                                  * (
                                                    * x x
                                                  )
                                                   x
                                                )
                                                 (
                                                  * (
                                                    * 2.0 x
                                                  )
                                                   x
                                                )
                                              )
                                               (
                                                * 3.0 x
                                              )
                                            )
                                             5.0
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
                      X (
                        design_matrix xs 3
                      )
                    )
                  )
                   (
                    begin (
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
                                  Xty (
                                    matvec_mul Xt ys
                                  )
                                )
                              )
                               (
                                begin (
                                  let (
                                    (
                                      coeffs (
                                        gaussian_elimination XtX Xty
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      _display (
                                        if (
                                          string? (
                                            to-str-space coeffs
                                          )
                                        )
                                         (
                                          to-str-space coeffs
                                        )
                                         (
                                          to-str (
                                            to-str-space coeffs
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
                                              predict (
                                                _list (
                                                  - 1.0
                                                )
                                              )
                                               coeffs
                                            )
                                          )
                                        )
                                         (
                                          to-str-space (
                                            predict (
                                              _list (
                                                - 1.0
                                              )
                                            )
                                             coeffs
                                          )
                                        )
                                         (
                                          to-str (
                                            to-str-space (
                                              predict (
                                                _list (
                                                  - 1.0
                                                )
                                              )
                                               coeffs
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
                                              predict (
                                                _list (
                                                  - 2.0
                                                )
                                              )
                                               coeffs
                                            )
                                          )
                                        )
                                         (
                                          to-str-space (
                                            predict (
                                              _list (
                                                - 2.0
                                              )
                                            )
                                             coeffs
                                          )
                                        )
                                         (
                                          to-str (
                                            to-str-space (
                                              predict (
                                                _list (
                                                  - 2.0
                                                )
                                              )
                                               coeffs
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
                                              predict (
                                                _list 6.0
                                              )
                                               coeffs
                                            )
                                          )
                                        )
                                         (
                                          to-str-space (
                                            predict (
                                              _list 6.0
                                            )
                                             coeffs
                                          )
                                        )
                                         (
                                          to-str (
                                            to-str-space (
                                              predict (
                                                _list 6.0
                                              )
                                               coeffs
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
