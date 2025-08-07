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
      start27 (
        current-jiffy
      )
    )
     (
      jps30 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        key x y
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            ret1 (
              string-append (
                string-append (
                  to-str-space x
                )
                 ","
              )
               (
                to-str-space y
              )
            )
          )
        )
      )
    )
     (
      define (
        joint_probability_distribution x_values y_values x_probabilities y_probabilities
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            let (
              (
                result (
                  alist->hash-table (
                    _list
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
                    call/cc (
                      lambda (
                        break4
                      )
                       (
                        letrec (
                          (
                            loop3 (
                              lambda (
                                
                              )
                               (
                                if (
                                  < i (
                                    _len x_values
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
                                            break6
                                          )
                                           (
                                            letrec (
                                              (
                                                loop5 (
                                                  lambda (
                                                    
                                                  )
                                                   (
                                                    if (
                                                      < j (
                                                        _len y_values
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            k (
                                                              key (
                                                                list-ref x_values i
                                                              )
                                                               (
                                                                list-ref y_values j
                                                              )
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            hash-table-set! result k (
                                                              * (
                                                                list-ref x_probabilities i
                                                              )
                                                               (
                                                                list-ref y_probabilities j
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
                                                        loop5
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
                                              loop5
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
                                    loop3
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
                          loop3
                        )
                      )
                    )
                  )
                   (
                    ret2 result
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
        expectation values probabilities
      )
       (
        call/cc (
          lambda (
            ret7
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
                                  < i (
                                    _len values
                                  )
                                )
                                 (
                                  begin (
                                    set! total (
                                      _add total (
                                        * (
                                          + 0.0 (
                                            list-ref values i
                                          )
                                        )
                                         (
                                          list-ref probabilities i
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
                    ret7 total
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
        variance values probabilities
      )
       (
        call/cc (
          lambda (
            ret10
          )
           (
            let (
              (
                mean (
                  expectation values probabilities
                )
              )
            )
             (
              begin (
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
                                      < i (
                                        _len values
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            diff (
                                              - (
                                                + 0.0 (
                                                  list-ref values i
                                                )
                                              )
                                               mean
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            set! total (
                                              _add total (
                                                * (
                                                  * diff diff
                                                )
                                                 (
                                                  list-ref probabilities i
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
                                        loop11
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
                              loop11
                            )
                          )
                        )
                      )
                       (
                        ret10 total
                      )
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
        covariance x_values y_values x_probabilities y_probabilities
      )
       (
        call/cc (
          lambda (
            ret13
          )
           (
            let (
              (
                mean_x (
                  expectation x_values x_probabilities
                )
              )
            )
             (
              begin (
                let (
                  (
                    mean_y (
                      expectation y_values y_probabilities
                    )
                  )
                )
                 (
                  begin (
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
                                            _len x_values
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
                                                                _len y_values
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                let (
                                                                  (
                                                                    diff_x (
                                                                      - (
                                                                        + 0.0 (
                                                                          list-ref x_values i
                                                                        )
                                                                      )
                                                                       mean_x
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        diff_y (
                                                                          - (
                                                                            + 0.0 (
                                                                              list-ref y_values j
                                                                            )
                                                                          )
                                                                           mean_y
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        set! total (
                                                                          _add total (
                                                                            * (
                                                                              * (
                                                                                * diff_x diff_y
                                                                              )
                                                                               (
                                                                                list-ref x_probabilities i
                                                                              )
                                                                            )
                                                                             (
                                                                              list-ref y_probabilities j
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
                            ret13 total
                          )
                        )
                      )
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
            ret18
          )
           (
            begin (
              if (
                <= x 0.0
              )
               (
                begin (
                  ret18 0.0
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
                  guess (
                    _div x 2.0
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
                      ret18 guess
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
        standard_deviation v
      )
       (
        call/cc (
          lambda (
            ret21
          )
           (
            ret21 (
              sqrtApprox v
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
            ret22
          )
           (
            let (
              (
                x_values (
                  _list 1 2
                )
              )
            )
             (
              begin (
                let (
                  (
                    y_values (
                      _list (
                        - 2
                      )
                       5 8
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        x_probabilities (
                          _list 0.7 0.3
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            y_probabilities (
                              _list 0.3 0.5 0.2
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                jpd (
                                  joint_probability_distribution x_values y_values x_probabilities y_probabilities
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
                                                  < i (
                                                    _len x_values
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
                                                                      < j (
                                                                        _len y_values
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            k (
                                                                              key (
                                                                                list-ref x_values i
                                                                              )
                                                                               (
                                                                                list-ref y_values j
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                prob (
                                                                                  cond (
                                                                                    (
                                                                                      string? jpd
                                                                                    )
                                                                                     (
                                                                                      _substring jpd k (
                                                                                        + k 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? jpd
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref jpd k
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref jpd k
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
                                                                                      string-append (
                                                                                        string-append k "="
                                                                                      )
                                                                                       (
                                                                                        to-str-space prob
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    string-append (
                                                                                      string-append k "="
                                                                                    )
                                                                                     (
                                                                                      to-str-space prob
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    to-str (
                                                                                      string-append (
                                                                                        string-append k "="
                                                                                      )
                                                                                       (
                                                                                        to-str-space prob
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
                                                        set! i (
                                                          + i 1
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    loop23
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
                                          loop23
                                        )
                                      )
                                    )
                                  )
                                   (
                                    let (
                                      (
                                        ex (
                                          expectation x_values x_probabilities
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            ey (
                                              expectation y_values y_probabilities
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                vx (
                                                  variance x_values x_probabilities
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    vy (
                                                      variance y_values y_probabilities
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        cov (
                                                          covariance x_values y_values x_probabilities y_probabilities
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        _display (
                                                          if (
                                                            string? (
                                                              string-append "Ex=" (
                                                                to-str-space ex
                                                              )
                                                            )
                                                          )
                                                           (
                                                            string-append "Ex=" (
                                                              to-str-space ex
                                                            )
                                                          )
                                                           (
                                                            to-str (
                                                              string-append "Ex=" (
                                                                to-str-space ex
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
                                                              string-append "Ey=" (
                                                                to-str-space ey
                                                              )
                                                            )
                                                          )
                                                           (
                                                            string-append "Ey=" (
                                                              to-str-space ey
                                                            )
                                                          )
                                                           (
                                                            to-str (
                                                              string-append "Ey=" (
                                                                to-str-space ey
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
                                                              string-append "Vx=" (
                                                                to-str-space vx
                                                              )
                                                            )
                                                          )
                                                           (
                                                            string-append "Vx=" (
                                                              to-str-space vx
                                                            )
                                                          )
                                                           (
                                                            to-str (
                                                              string-append "Vx=" (
                                                                to-str-space vx
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
                                                              string-append "Vy=" (
                                                                to-str-space vy
                                                              )
                                                            )
                                                          )
                                                           (
                                                            string-append "Vy=" (
                                                              to-str-space vy
                                                            )
                                                          )
                                                           (
                                                            to-str (
                                                              string-append "Vy=" (
                                                                to-str-space vy
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
                                                              string-append "Cov=" (
                                                                to-str-space cov
                                                              )
                                                            )
                                                          )
                                                           (
                                                            string-append "Cov=" (
                                                              to-str-space cov
                                                            )
                                                          )
                                                           (
                                                            to-str (
                                                              string-append "Cov=" (
                                                                to-str-space cov
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
                                                              string-append "Sx=" (
                                                                to-str-space (
                                                                  standard_deviation vx
                                                                )
                                                              )
                                                            )
                                                          )
                                                           (
                                                            string-append "Sx=" (
                                                              to-str-space (
                                                                standard_deviation vx
                                                              )
                                                            )
                                                          )
                                                           (
                                                            to-str (
                                                              string-append "Sx=" (
                                                                to-str-space (
                                                                  standard_deviation vx
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
                                                              string-append "Sy=" (
                                                                to-str-space (
                                                                  standard_deviation vy
                                                                )
                                                              )
                                                            )
                                                          )
                                                           (
                                                            string-append "Sy=" (
                                                              to-str-space (
                                                                standard_deviation vy
                                                              )
                                                            )
                                                          )
                                                           (
                                                            to-str (
                                                              string-append "Sy=" (
                                                                to-str-space (
                                                                  standard_deviation vy
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
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
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
          end28 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur29 (
              quotient (
                * (
                  - end28 start27
                )
                 1000000
              )
               jps30
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur29
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
