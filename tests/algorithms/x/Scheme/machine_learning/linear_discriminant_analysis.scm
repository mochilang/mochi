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
      start52 (
        current-jiffy
      )
    )
     (
      jps55 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      let (
        (
          PI 3.141592653589793
        )
      )
       (
        begin (
          let (
            (
              TWO_PI 6.283185307179586
            )
          )
           (
            begin (
              let (
                (
                  seed 1
                )
              )
               (
                begin (
                  define (
                    rand
                  )
                   (
                    call/cc (
                      lambda (
                        ret1
                      )
                       (
                        begin (
                          set! seed (
                            _mod (
                              + (
                                * seed 1103515245
                              )
                               12345
                            )
                             2147483648
                          )
                        )
                         (
                          ret1 seed
                        )
                      )
                    )
                  )
                )
                 (
                  define (
                    random
                  )
                   (
                    call/cc (
                      lambda (
                        ret2
                      )
                       (
                        ret2 (
                          _div (
                            + 0.0 (
                              rand
                            )
                          )
                           2147483648.0
                        )
                      )
                    )
                  )
                )
                 (
                  define (
                    _mod x m
                  )
                   (
                    call/cc (
                      lambda (
                        ret3
                      )
                       (
                        ret3 (
                          - x (
                            * (
                              + 0.0 (
                                let (
                                  (
                                    v4 (
                                      _div x m
                                    )
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
                             m
                          )
                        )
                      )
                    )
                  )
                )
                 (
                  define (
                    cos x
                  )
                   (
                    call/cc (
                      lambda (
                        ret5
                      )
                       (
                        let (
                          (
                            y (
                              - (
                                _mod (
                                  + x PI
                                )
                                 TWO_PI
                              )
                               PI
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                y2 (
                                  * y y
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    y4 (
                                      * y2 y2
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        y6 (
                                          * y4 y2
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        ret5 (
                                          - (
                                            _add (
                                              - 1.0 (
                                                _div y2 2.0
                                              )
                                            )
                                             (
                                              _div y4 24.0
                                            )
                                          )
                                           (
                                            _div y6 720.0
                                          )
                                        )
                                      )
                                    )
                                  )
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
                    sqrtApprox x
                  )
                   (
                    call/cc (
                      lambda (
                        ret6
                      )
                       (
                        begin (
                          if (
                            <= x 0.0
                          )
                           (
                            begin (
                              ret6 0.0
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
                                                < i 10
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
                                  ret6 guess
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
                    ln x
                  )
                   (
                    call/cc (
                      lambda (
                        ret9
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
                                        n 1
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
                                                      <= n 19
                                                    )
                                                     (
                                                      begin (
                                                        set! sum (
                                                          _add sum (
                                                            _div term (
                                                              + 0.0 n
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
                                                        set! n (
                                                          + n 2
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
                                        ret9 (
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
                    gaussian_distribution mean std_dev instance_count
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
                                              < i instance_count
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    u1 (
                                                      random
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        u2 (
                                                          random
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            r (
                                                              sqrtApprox (
                                                                * (
                                                                  - 2.0
                                                                )
                                                                 (
                                                                  ln u1
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                theta (
                                                                  * TWO_PI u2
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                let (
                                                                  (
                                                                    z (
                                                                      * r (
                                                                        cos theta
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! res (
                                                                      append res (
                                                                        _list (
                                                                          _add mean (
                                                                            * z std_dev
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
                                                            )
                                                          )
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
                 (
                  define (
                    y_generator class_count instance_count
                  )
                   (
                    call/cc (
                      lambda (
                        ret15
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
                                              < k class_count
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
                                                                  < i (
                                                                    list-ref instance_count k
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! res (
                                                                      append res (
                                                                        _list k
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    set! i (
                                                                      + i 1
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
                                                    set! k (
                                                      + k 1
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
                                ret15 res
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
                    calculate_mean instance_count items
                  )
                   (
                    call/cc (
                      lambda (
                        ret20
                      )
                       (
                        let (
                          (
                            total 0.0
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
                                              < i instance_count
                                            )
                                             (
                                              begin (
                                                set! total (
                                                  + total (
                                                    list-ref items i
                                                  )
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
                                ret20 (
                                  _div total (
                                    + 0.0 instance_count
                                  )
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
                    calculate_probabilities instance_count total_count
                  )
                   (
                    call/cc (
                      lambda (
                        ret23
                      )
                       (
                        ret23 (
                          _div (
                            + 0.0 instance_count
                          )
                           (
                            + 0.0 total_count
                          )
                        )
                      )
                    )
                  )
                )
                 (
                  define (
                    calculate_variance items means total_count
                  )
                   (
                    call/cc (
                      lambda (
                        ret24
                      )
                       (
                        let (
                          (
                            squared_diff (
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
                                                _len items
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
                                                        break28
                                                      )
                                                       (
                                                        letrec (
                                                          (
                                                            loop27 (
                                                              lambda (
                                                                
                                                              )
                                                               (
                                                                if (
                                                                  < j (
                                                                    _len (
                                                                      list-ref items i
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        diff (
                                                                          - (
                                                                            cond (
                                                                              (
                                                                                string? (
                                                                                  list-ref items i
                                                                                )
                                                                              )
                                                                               (
                                                                                _substring (
                                                                                  list-ref items i
                                                                                )
                                                                                 j (
                                                                                  + j 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? (
                                                                                  list-ref items i
                                                                                )
                                                                              )
                                                                               (
                                                                                hash-table-ref (
                                                                                  list-ref items i
                                                                                )
                                                                                 j
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref (
                                                                                  list-ref items i
                                                                                )
                                                                                 j
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            list-ref means i
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        set! squared_diff (
                                                                          append squared_diff (
                                                                            _list (
                                                                              * diff diff
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
                                                                    loop27
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
                                                          loop27
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
                                let (
                                  (
                                    sum_sq 0.0
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
                                            break30
                                          )
                                           (
                                            letrec (
                                              (
                                                loop29 (
                                                  lambda (
                                                    
                                                  )
                                                   (
                                                    if (
                                                      < k (
                                                        _len squared_diff
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        set! sum_sq (
                                                          + sum_sq (
                                                            list-ref squared_diff k
                                                          )
                                                        )
                                                      )
                                                       (
                                                        set! k (
                                                          + k 1
                                                        )
                                                      )
                                                       (
                                                        loop29
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
                                              loop29
                                            )
                                          )
                                        )
                                      )
                                       (
                                        let (
                                          (
                                            n_classes (
                                              _len means
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            ret24 (
                                              * (
                                                _div 1.0 (
                                                  + 0.0 (
                                                    - total_count n_classes
                                                  )
                                                )
                                              )
                                               sum_sq
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
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
                    predict_y_values x_items means variance probabilities
                  )
                   (
                    call/cc (
                      lambda (
                        ret31
                      )
                       (
                        let (
                          (
                            results (
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
                                                _len x_items
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
                                                                  < j (
                                                                    _len (
                                                                      list-ref x_items i
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        temp (
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
                                                                                          < k (
                                                                                            _len x_items
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            let (
                                                                                              (
                                                                                                discr (
                                                                                                  _add (
                                                                                                    - (
                                                                                                      * (
                                                                                                        cond (
                                                                                                          (
                                                                                                            string? (
                                                                                                              list-ref x_items i
                                                                                                            )
                                                                                                          )
                                                                                                           (
                                                                                                            _substring (
                                                                                                              list-ref x_items i
                                                                                                            )
                                                                                                             j (
                                                                                                              + j 1
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          (
                                                                                                            hash-table? (
                                                                                                              list-ref x_items i
                                                                                                            )
                                                                                                          )
                                                                                                           (
                                                                                                            hash-table-ref (
                                                                                                              list-ref x_items i
                                                                                                            )
                                                                                                             j
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          else (
                                                                                                            list-ref (
                                                                                                              list-ref x_items i
                                                                                                            )
                                                                                                             j
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        _div (
                                                                                                          list-ref means k
                                                                                                        )
                                                                                                         variance
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      _div (
                                                                                                        * (
                                                                                                          list-ref means k
                                                                                                        )
                                                                                                         (
                                                                                                          list-ref means k
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        * 2.0 variance
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    ln (
                                                                                                      list-ref probabilities k
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                set! temp (
                                                                                                  append temp (
                                                                                                    _list discr
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
                                                                            let (
                                                                              (
                                                                                max_idx 0
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                let (
                                                                                  (
                                                                                    max_val (
                                                                                      list-ref temp 0
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    let (
                                                                                      (
                                                                                        t 1
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
                                                                                                      < t (
                                                                                                        _len temp
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      begin (
                                                                                                        if (
                                                                                                          > (
                                                                                                            list-ref temp t
                                                                                                          )
                                                                                                           max_val
                                                                                                        )
                                                                                                         (
                                                                                                          begin (
                                                                                                            set! max_val (
                                                                                                              list-ref temp t
                                                                                                            )
                                                                                                          )
                                                                                                           (
                                                                                                            set! max_idx t
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          quote (
                                                                                                            
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        set! t (
                                                                                                          + t 1
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        loop38
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
                                                                                              loop38
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        set! results (
                                                                                          append results (
                                                                                            _list max_idx
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
                                                    set! i (
                                                      + i 1
                                                    )
                                                  )
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
                                ret31 results
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
                    accuracy actual_y predicted_y
                  )
                   (
                    call/cc (
                      lambda (
                        ret40
                      )
                       (
                        let (
                          (
                            correct 0
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
                                              < i (
                                                _len actual_y
                                              )
                                            )
                                             (
                                              begin (
                                                if (
                                                  equal? (
                                                    list-ref actual_y i
                                                  )
                                                   (
                                                    list-ref predicted_y i
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    set! correct (
                                                      + correct 1
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
                                ret40 (
                                  * (
                                    _div (
                                      + 0.0 correct
                                    )
                                     (
                                      + 0.0 (
                                        _len actual_y
                                      )
                                    )
                                  )
                                   100.0
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
                        ret43
                      )
                       (
                        begin (
                          set! seed 1
                        )
                         (
                          let (
                            (
                              counts (
                                _list 20 20 20
                              )
                            )
                          )
                           (
                            begin (
                              let (
                                (
                                  means (
                                    _list 5.0 10.0 15.0
                                  )
                                )
                              )
                               (
                                begin (
                                  let (
                                    (
                                      std_dev 1.0
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
                                              i 0
                                            )
                                          )
                                           (
                                            begin (
                                              call/cc (
                                                lambda (
                                                  break45
                                                )
                                                 (
                                                  letrec (
                                                    (
                                                      loop44 (
                                                        lambda (
                                                          
                                                        )
                                                         (
                                                          if (
                                                            < i (
                                                              _len counts
                                                            )
                                                          )
                                                           (
                                                            begin (
                                                              set! x (
                                                                append x (
                                                                  _list (
                                                                    gaussian_distribution (
                                                                      list-ref means i
                                                                    )
                                                                     std_dev (
                                                                      list-ref counts i
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
                                                              loop44
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
                                                    loop44
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              let (
                                                (
                                                  y (
                                                    y_generator (
                                                      _len counts
                                                    )
                                                     counts
                                                  )
                                                )
                                              )
                                               (
                                                begin (
                                                  let (
                                                    (
                                                      actual_means (
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
                                                                    < i (
                                                                      _len counts
                                                                    )
                                                                  )
                                                                   (
                                                                    begin (
                                                                      set! actual_means (
                                                                        append actual_means (
                                                                          _list (
                                                                            calculate_mean (
                                                                              list-ref counts i
                                                                            )
                                                                             (
                                                                              list-ref x i
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
                                                          total_count 0
                                                        )
                                                      )
                                                       (
                                                        begin (
                                                          set! i 0
                                                        )
                                                         (
                                                          call/cc (
                                                            lambda (
                                                              break49
                                                            )
                                                             (
                                                              letrec (
                                                                (
                                                                  loop48 (
                                                                    lambda (
                                                                      
                                                                    )
                                                                     (
                                                                      if (
                                                                        < i (
                                                                          _len counts
                                                                        )
                                                                      )
                                                                       (
                                                                        begin (
                                                                          set! total_count (
                                                                            + total_count (
                                                                              list-ref counts i
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          set! i (
                                                                            + i 1
                                                                          )
                                                                        )
                                                                         (
                                                                          loop48
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
                                                                loop48
                                                              )
                                                            )
                                                          )
                                                        )
                                                         (
                                                          let (
                                                            (
                                                              probabilities (
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
                                                                  break51
                                                                )
                                                                 (
                                                                  letrec (
                                                                    (
                                                                      loop50 (
                                                                        lambda (
                                                                          
                                                                        )
                                                                         (
                                                                          if (
                                                                            < i (
                                                                              _len counts
                                                                            )
                                                                          )
                                                                           (
                                                                            begin (
                                                                              set! probabilities (
                                                                                append probabilities (
                                                                                  _list (
                                                                                    calculate_probabilities (
                                                                                      list-ref counts i
                                                                                    )
                                                                                     total_count
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
                                                                              loop50
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
                                                                    loop50
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             (
                                                              let (
                                                                (
                                                                  variance (
                                                                    calculate_variance x actual_means total_count
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                begin (
                                                                  let (
                                                                    (
                                                                      predicted (
                                                                        predict_y_values x actual_means variance probabilities
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    begin (
                                                                      _display (
                                                                        if (
                                                                          string? predicted
                                                                        )
                                                                         predicted (
                                                                          to-str predicted
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
                                                                            accuracy y predicted
                                                                          )
                                                                        )
                                                                         (
                                                                          accuracy y predicted
                                                                        )
                                                                         (
                                                                          to-str (
                                                                            accuracy y predicted
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
                                  )
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
              )
            )
          )
        )
      )
    )
     (
      let (
        (
          end53 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur54 (
              quotient (
                * (
                  - end53 start52
                )
                 1000000
              )
               jps55
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur54
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
