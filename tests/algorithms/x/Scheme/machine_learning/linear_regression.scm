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
        dot x y
      )
       (
        call/cc (
          lambda (
            ret1
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
                                    _len x
                                  )
                                )
                                 (
                                  begin (
                                    set! sum (
                                      _add sum (
                                        * (
                                          list-ref x i
                                        )
                                         (
                                          list-ref y i
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
                    ret1 sum
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
        run_steep_gradient_descent data_x data_y len_data alpha theta
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            let (
              (
                gradients (
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
                                    _len theta
                                  )
                                )
                                 (
                                  begin (
                                    set! gradients (
                                      append gradients (
                                        _list 0.0
                                      )
                                    )
                                  )
                                   (
                                    set! j (
                                      + j 1
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
                                      < i len_data
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            prediction (
                                              dot theta (
                                                list-ref data_x i
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                error (
                                                  - prediction (
                                                    list-ref data_y i
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
                                                                  < k (
                                                                    _len theta
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    list-set! gradients k (
                                                                      _add (
                                                                        list-ref gradients k
                                                                      )
                                                                       (
                                                                        * error (
                                                                          cond (
                                                                            (
                                                                              string? (
                                                                                list-ref data_x i
                                                                              )
                                                                            )
                                                                             (
                                                                              _substring (
                                                                                list-ref data_x i
                                                                              )
                                                                               k (
                                                                                + k 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            (
                                                                              hash-table? (
                                                                                list-ref data_x i
                                                                              )
                                                                            )
                                                                             (
                                                                              hash-table-ref (
                                                                                list-ref data_x i
                                                                              )
                                                                               k
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref (
                                                                                list-ref data_x i
                                                                              )
                                                                               k
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
                        let (
                          (
                            t (
                              _list
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                g 0
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
                                              < g (
                                                _len theta
                                              )
                                            )
                                             (
                                              begin (
                                                set! t (
                                                  append t (
                                                    _list (
                                                      - (
                                                        list-ref theta g
                                                      )
                                                       (
                                                        * (
                                                          _div alpha len_data
                                                        )
                                                         (
                                                          list-ref gradients g
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                set! g (
                                                  + g 1
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
                                ret4 t
                              )
                            )
                          )
                        )
                      )
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
        sum_of_square_error data_x data_y len_data theta
      )
       (
        call/cc (
          lambda (
            ret13
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
                                  < i len_data
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        prediction (
                                          dot theta (
                                            list-ref data_x i
                                          )
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            diff (
                                              - prediction (
                                                list-ref data_y i
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            set! total (
                                              _add total (
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
                    ret13 (
                      _div total (
                        * 2.0 len_data
                      )
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
        run_linear_regression data_x data_y
      )
       (
        call/cc (
          lambda (
            ret16
          )
           (
            let (
              (
                iterations 10
              )
            )
             (
              begin (
                let (
                  (
                    alpha 0.01
                  )
                )
                 (
                  begin (
                    let (
                      (
                        no_features (
                          _len (
                            list-ref data_x 0
                          )
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            len_data (
                              _len data_x
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                theta (
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
                                                  < i no_features
                                                )
                                                 (
                                                  begin (
                                                    set! theta (
                                                      append theta (
                                                        _list 0.0
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
                                    let (
                                      (
                                        iter 0
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
                                                      < iter iterations
                                                    )
                                                     (
                                                      begin (
                                                        set! theta (
                                                          run_steep_gradient_descent data_x data_y len_data alpha theta
                                                        )
                                                      )
                                                       (
                                                        let (
                                                          (
                                                            error (
                                                              sum_of_square_error data_x data_y len_data theta
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            _display (
                                                              if (
                                                                string? (
                                                                  string-append (
                                                                    string-append (
                                                                      string-append "At Iteration " (
                                                                        to-str-space (
                                                                          + iter 1
                                                                        )
                                                                      )
                                                                    )
                                                                     " - Error is "
                                                                  )
                                                                   (
                                                                    to-str-space error
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                string-append (
                                                                  string-append (
                                                                    string-append "At Iteration " (
                                                                      to-str-space (
                                                                        + iter 1
                                                                      )
                                                                    )
                                                                  )
                                                                   " - Error is "
                                                                )
                                                                 (
                                                                  to-str-space error
                                                                )
                                                              )
                                                               (
                                                                to-str (
                                                                  string-append (
                                                                    string-append (
                                                                      string-append "At Iteration " (
                                                                        to-str-space (
                                                                          + iter 1
                                                                        )
                                                                      )
                                                                    )
                                                                     " - Error is "
                                                                  )
                                                                   (
                                                                    to-str-space error
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                           (
                                                            newline
                                                          )
                                                           (
                                                            set! iter (
                                                              + iter 1
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
                                        ret16 theta
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
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
        absf x
      )
       (
        call/cc (
          lambda (
            ret21
          )
           (
            if (
              < x 0.0
            )
             (
              begin (
                ret21 (
                  - x
                )
              )
            )
             (
              begin (
                ret21 x
              )
            )
          )
        )
      )
    )
     (
      define (
        mean_absolute_error predicted_y original_y
      )
       (
        call/cc (
          lambda (
            ret22
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
                                    _len predicted_y
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        diff (
                                          absf (
                                            - (
                                              list-ref predicted_y i
                                            )
                                             (
                                              list-ref original_y i
                                            )
                                          )
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        set! total (
                                          _add total diff
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
                    ret22 (
                      _div total (
                        _len predicted_y
                      )
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
          data_x (
            _list (
              _list 1.0 1.0
            )
             (
              _list 1.0 2.0
            )
             (
              _list 1.0 3.0
            )
          )
        )
      )
       (
        begin (
          let (
            (
              data_y (
                _list 1.0 2.0 3.0
              )
            )
          )
           (
            begin (
              let (
                (
                  theta (
                    run_linear_regression data_x data_y
                  )
                )
              )
               (
                begin (
                  _display (
                    if (
                      string? "Resultant Feature vector :"
                    )
                     "Resultant Feature vector :" (
                      to-str "Resultant Feature vector :"
                    )
                  )
                )
                 (
                  newline
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
                                      _len theta
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
                                                  string? theta
                                                )
                                                 (
                                                  _substring theta i (
                                                    + i 1
                                                  )
                                                )
                                              )
                                               (
                                                (
                                                  hash-table? theta
                                                )
                                                 (
                                                  hash-table-ref theta i
                                                )
                                              )
                                               (
                                                else (
                                                  list-ref theta i
                                                )
                                              )
                                            )
                                          )
                                        )
                                         (
                                          to-str-space (
                                            cond (
                                              (
                                                string? theta
                                              )
                                               (
                                                _substring theta i (
                                                  + i 1
                                                )
                                              )
                                            )
                                             (
                                              (
                                                hash-table? theta
                                              )
                                               (
                                                hash-table-ref theta i
                                              )
                                            )
                                             (
                                              else (
                                                list-ref theta i
                                              )
                                            )
                                          )
                                        )
                                         (
                                          to-str (
                                            to-str-space (
                                              cond (
                                                (
                                                  string? theta
                                                )
                                                 (
                                                  _substring theta i (
                                                    + i 1
                                                  )
                                                )
                                              )
                                               (
                                                (
                                                  hash-table? theta
                                                )
                                                 (
                                                  hash-table-ref theta i
                                                )
                                              )
                                               (
                                                else (
                                                  list-ref theta i
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
                      let (
                        (
                          predicted_y (
                            _list 3.0 (
                              - 0.5
                            )
                             2.0 7.0
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              original_y (
                                _list 2.5 0.0 2.0 8.0
                              )
                            )
                          )
                           (
                            begin (
                              let (
                                (
                                  mae (
                                    mean_absolute_error predicted_y original_y
                                  )
                                )
                              )
                               (
                                begin (
                                  _display (
                                    if (
                                      string? (
                                        string-append "Mean Absolute Error : " (
                                          to-str-space mae
                                        )
                                      )
                                    )
                                     (
                                      string-append "Mean Absolute Error : " (
                                        to-str-space mae
                                      )
                                    )
                                     (
                                      to-str (
                                        string-append "Mean Absolute Error : " (
                                          to-str-space mae
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
