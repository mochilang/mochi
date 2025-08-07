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
      start21 (
        current-jiffy
      )
    )
     (
      jps24 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        absf x
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            begin (
              if (
                < x 0.0
              )
               (
                begin (
                  ret1 (
                    - x
                  )
                )
              )
               (
                quote (
                  
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
     (
      define (
        hypothesis_value input params
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            let (
              (
                value (
                  list-ref params 0
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
                                    _len input
                                  )
                                )
                                 (
                                  begin (
                                    set! value (
                                      _add value (
                                        * (
                                          list-ref input i
                                        )
                                         (
                                          list-ref params (
                                            + i 1
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
                    ret2 value
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
        calc_error dp params
      )
       (
        call/cc (
          lambda (
            ret5
          )
           (
            ret5 (
              - (
                hypothesis_value (
                  hash-table-ref dp "x"
                )
                 params
              )
               (
                hash-table-ref dp "y"
              )
            )
          )
        )
      )
    )
     (
      define (
        summation_of_cost_derivative index params data
      )
       (
        call/cc (
          lambda (
            ret6
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
                                    _len data
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        dp (
                                          list-ref data i
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            e (
                                              calc_error dp params
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              equal? index (
                                                - 1
                                              )
                                            )
                                             (
                                              begin (
                                                set! sum (
                                                  _add sum e
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                set! sum (
                                                  _add sum (
                                                    * e (
                                                      list-ref (
                                                        hash-table-ref dp "x"
                                                      )
                                                       index
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
                    ret6 sum
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
        get_cost_derivative index params data
      )
       (
        call/cc (
          lambda (
            ret9
          )
           (
            ret9 (
              _div (
                summation_of_cost_derivative index params data
              )
               (
                + 0.0 (
                  _len data
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        allclose a b atol rtol
      )
       (
        call/cc (
          lambda (
            ret10
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
                                _len a
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    diff (
                                      absf (
                                        - (
                                          list-ref a i
                                        )
                                         (
                                          list-ref b i
                                        )
                                      )
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        limit (
                                          _add atol (
                                            * rtol (
                                              absf (
                                                list-ref b i
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        if (
                                          _gt diff limit
                                        )
                                         (
                                          begin (
                                            ret10 #f
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
                ret10 #t
              )
            )
          )
        )
      )
    )
     (
      define (
        run_gradient_descent train_data initial_params
      )
       (
        call/cc (
          lambda (
            ret13
          )
           (
            let (
              (
                learning_rate 0.009
              )
            )
             (
              begin (
                let (
                  (
                    absolute_error_limit 2e-06
                  )
                )
                 (
                  begin (
                    let (
                      (
                        relative_error_limit 0.0
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
                                params initial_params
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
                                            if #t (
                                              begin (
                                                set! j (
                                                  + j 1
                                                )
                                              )
                                               (
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
                                                                      < i (
                                                                        _len params
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            deriv (
                                                                              get_cost_derivative (
                                                                                - i 1
                                                                              )
                                                                               params train_data
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            set! temp (
                                                                              append temp (
                                                                                _list (
                                                                                  - (
                                                                                    list-ref params i
                                                                                  )
                                                                                   (
                                                                                    * learning_rate deriv
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
                                                        if (
                                                          allclose params temp absolute_error_limit relative_error_limit
                                                        )
                                                         (
                                                          begin (
                                                            _display (
                                                              if (
                                                                string? (
                                                                  string-append "Number of iterations:" (
                                                                    to-str-space j
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                string-append "Number of iterations:" (
                                                                  to-str-space j
                                                                )
                                                              )
                                                               (
                                                                to-str (
                                                                  string-append "Number of iterations:" (
                                                                    to-str-space j
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                           (
                                                            newline
                                                          )
                                                           (
                                                            break15 (
                                                              quote (
                                                                
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
                                                        set! params temp
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
                                ret13 params
                              )
                            )
                          )
                        )
                      )
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
        test_gradient_descent test_data params
      )
       (
        call/cc (
          lambda (
            ret18
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
                              < i (
                                _len test_data
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    dp (
                                      list-ref test_data i
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    _display (
                                      if (
                                        string? (
                                          string-append "Actual output value:" (
                                            to-str-space (
                                              hash-table-ref dp "y"
                                            )
                                          )
                                        )
                                      )
                                       (
                                        string-append "Actual output value:" (
                                          to-str-space (
                                            hash-table-ref dp "y"
                                          )
                                        )
                                      )
                                       (
                                        to-str (
                                          string-append "Actual output value:" (
                                            to-str-space (
                                              hash-table-ref dp "y"
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
                                          string-append "Hypothesis output:" (
                                            to-str-space (
                                              hypothesis_value (
                                                hash-table-ref dp "x"
                                              )
                                               params
                                            )
                                          )
                                        )
                                      )
                                       (
                                        string-append "Hypothesis output:" (
                                          to-str-space (
                                            hypothesis_value (
                                              hash-table-ref dp "x"
                                            )
                                             params
                                          )
                                        )
                                      )
                                       (
                                        to-str (
                                          string-append "Hypothesis output:" (
                                            to-str-space (
                                              hypothesis_value (
                                                hash-table-ref dp "x"
                                              )
                                               params
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
            )
          )
        )
      )
    )
     (
      let (
        (
          train_data (
            _list (
              alist->hash-table (
                _list (
                  cons "x" (
                    _list 5.0 2.0 3.0
                  )
                )
                 (
                  cons "y" 15.0
                )
              )
            )
             (
              alist->hash-table (
                _list (
                  cons "x" (
                    _list 6.0 5.0 9.0
                  )
                )
                 (
                  cons "y" 25.0
                )
              )
            )
             (
              alist->hash-table (
                _list (
                  cons "x" (
                    _list 11.0 12.0 13.0
                  )
                )
                 (
                  cons "y" 41.0
                )
              )
            )
             (
              alist->hash-table (
                _list (
                  cons "x" (
                    _list 1.0 1.0 1.0
                  )
                )
                 (
                  cons "y" 8.0
                )
              )
            )
             (
              alist->hash-table (
                _list (
                  cons "x" (
                    _list 11.0 12.0 13.0
                  )
                )
                 (
                  cons "y" 41.0
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
              test_data (
                _list (
                  alist->hash-table (
                    _list (
                      cons "x" (
                        _list 515.0 22.0 13.0
                      )
                    )
                     (
                      cons "y" 555.0
                    )
                  )
                )
                 (
                  alist->hash-table (
                    _list (
                      cons "x" (
                        _list 61.0 35.0 49.0
                      )
                    )
                     (
                      cons "y" 150.0
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
                  parameter_vector (
                    _list 2.0 4.0 1.0 5.0
                  )
                )
              )
               (
                begin (
                  set! parameter_vector (
                    run_gradient_descent train_data parameter_vector
                  )
                )
                 (
                  _display (
                    if (
                      string? "\nTesting gradient descent for a linear hypothesis function.\n"
                    )
                     "\nTesting gradient descent for a linear hypothesis function.\n" (
                      to-str "\nTesting gradient descent for a linear hypothesis function.\n"
                    )
                  )
                )
                 (
                  newline
                )
                 (
                  test_gradient_descent test_data parameter_vector
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
          end22 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur23 (
              quotient (
                * (
                  - end22 start21
                )
                 1000000
              )
               jps24
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur23
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
