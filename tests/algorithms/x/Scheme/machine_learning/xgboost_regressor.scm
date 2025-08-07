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
      start26 (
        current-jiffy
      )
    )
     (
      jps29 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        data_handling dataset
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            ret1 dataset
          )
        )
      )
    )
     (
      define (
        xgboost features target test_features
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            let (
              (
                learning_rate 0.5
              )
            )
             (
              begin (
                let (
                  (
                    n_estimators 3
                  )
                )
                 (
                  begin (
                    let (
                      (
                        trees (
                          _list
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            predictions (
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
                                                _len target
                                              )
                                            )
                                             (
                                              begin (
                                                set! predictions (
                                                  append predictions (
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
                                let (
                                  (
                                    est 0
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
                                                  < est n_estimators
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        residuals (
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
                                                                          < j (
                                                                            _len target
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            set! residuals (
                                                                              append residuals (
                                                                                _list (
                                                                                  - (
                                                                                    list-ref target j
                                                                                  )
                                                                                   (
                                                                                    list-ref predictions j
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
                                                                sum_feat 0.0
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                set! j 0
                                                              )
                                                               (
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
                                                                              < j (
                                                                                _len features
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                set! sum_feat (
                                                                                  + sum_feat (
                                                                                    cond (
                                                                                      (
                                                                                        string? (
                                                                                          list-ref features j
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        _substring (
                                                                                          list-ref features j
                                                                                        )
                                                                                         0 (
                                                                                          + 0 1
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      (
                                                                                        hash-table? (
                                                                                          list-ref features j
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        hash-table-ref (
                                                                                          list-ref features j
                                                                                        )
                                                                                         0
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      else (
                                                                                        list-ref (
                                                                                          list-ref features j
                                                                                        )
                                                                                         0
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
                                                                let (
                                                                  (
                                                                    threshold (
                                                                      _div sum_feat (
                                                                        + 0.0 (
                                                                          _len features
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        left_sum 0.0
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            left_count 0
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                right_sum 0.0
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                let (
                                                                                  (
                                                                                    right_count 0
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    set! j 0
                                                                                  )
                                                                                   (
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
                                                                                                  < j (
                                                                                                    _len features
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  begin (
                                                                                                    if (
                                                                                                      <= (
                                                                                                        cond (
                                                                                                          (
                                                                                                            string? (
                                                                                                              list-ref features j
                                                                                                            )
                                                                                                          )
                                                                                                           (
                                                                                                            _substring (
                                                                                                              list-ref features j
                                                                                                            )
                                                                                                             0 (
                                                                                                              + 0 1
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          (
                                                                                                            hash-table? (
                                                                                                              list-ref features j
                                                                                                            )
                                                                                                          )
                                                                                                           (
                                                                                                            hash-table-ref (
                                                                                                              list-ref features j
                                                                                                            )
                                                                                                             0
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          else (
                                                                                                            list-ref (
                                                                                                              list-ref features j
                                                                                                            )
                                                                                                             0
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       threshold
                                                                                                    )
                                                                                                     (
                                                                                                      begin (
                                                                                                        set! left_sum (
                                                                                                          + left_sum (
                                                                                                            list-ref residuals j
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        set! left_count (
                                                                                                          + left_count 1
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      begin (
                                                                                                        set! right_sum (
                                                                                                          + right_sum (
                                                                                                            list-ref residuals j
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        set! right_count (
                                                                                                          + right_count 1
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
                                                                                    let (
                                                                                      (
                                                                                        left_value 0.0
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        if (
                                                                                          > left_count 0
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            set! left_value (
                                                                                              _div left_sum (
                                                                                                + 0.0 left_count
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
                                                                                            right_value 0.0
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            if (
                                                                                              > right_count 0
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                set! right_value (
                                                                                                  _div right_sum (
                                                                                                    + 0.0 right_count
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
                                                                                            set! j 0
                                                                                          )
                                                                                           (
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
                                                                                                          < j (
                                                                                                            _len features
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          begin (
                                                                                                            if (
                                                                                                              <= (
                                                                                                                cond (
                                                                                                                  (
                                                                                                                    string? (
                                                                                                                      list-ref features j
                                                                                                                    )
                                                                                                                  )
                                                                                                                   (
                                                                                                                    _substring (
                                                                                                                      list-ref features j
                                                                                                                    )
                                                                                                                     0 (
                                                                                                                      + 0 1
                                                                                                                    )
                                                                                                                  )
                                                                                                                )
                                                                                                                 (
                                                                                                                  (
                                                                                                                    hash-table? (
                                                                                                                      list-ref features j
                                                                                                                    )
                                                                                                                  )
                                                                                                                   (
                                                                                                                    hash-table-ref (
                                                                                                                      list-ref features j
                                                                                                                    )
                                                                                                                     0
                                                                                                                  )
                                                                                                                )
                                                                                                                 (
                                                                                                                  else (
                                                                                                                    list-ref (
                                                                                                                      list-ref features j
                                                                                                                    )
                                                                                                                     0
                                                                                                                  )
                                                                                                                )
                                                                                                              )
                                                                                                               threshold
                                                                                                            )
                                                                                                             (
                                                                                                              begin (
                                                                                                                list-set! predictions j (
                                                                                                                  _add (
                                                                                                                    list-ref predictions j
                                                                                                                  )
                                                                                                                   (
                                                                                                                    * learning_rate left_value
                                                                                                                  )
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              begin (
                                                                                                                list-set! predictions j (
                                                                                                                  _add (
                                                                                                                    list-ref predictions j
                                                                                                                  )
                                                                                                                   (
                                                                                                                    * learning_rate right_value
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
                                                                                            set! trees (
                                                                                              append trees (
                                                                                                _list (
                                                                                                  alist->hash-table (
                                                                                                    _list (
                                                                                                      cons "threshold" threshold
                                                                                                    )
                                                                                                     (
                                                                                                      cons "left_value" left_value
                                                                                                    )
                                                                                                     (
                                                                                                      cons "right_value" right_value
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            set! est (
                                                                                              + est 1
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
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
                                        preds (
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
                                                          < t (
                                                            _len test_features
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                pred 0.0
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
                                                                                  < k (
                                                                                    _len trees
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    if (
                                                                                      <= (
                                                                                        cond (
                                                                                          (
                                                                                            string? (
                                                                                              list-ref test_features t
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            _substring (
                                                                                              list-ref test_features t
                                                                                            )
                                                                                             0 (
                                                                                              + 0 1
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          (
                                                                                            hash-table? (
                                                                                              list-ref test_features t
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            hash-table-ref (
                                                                                              list-ref test_features t
                                                                                            )
                                                                                             0
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          else (
                                                                                            list-ref (
                                                                                              list-ref test_features t
                                                                                            )
                                                                                             0
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        hash-table-ref (
                                                                                          list-ref trees k
                                                                                        )
                                                                                         "threshold"
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        set! pred (
                                                                                          _add pred (
                                                                                            * learning_rate (
                                                                                              hash-table-ref (
                                                                                                list-ref trees k
                                                                                              )
                                                                                               "left_value"
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        set! pred (
                                                                                          _add pred (
                                                                                            * learning_rate (
                                                                                              hash-table-ref (
                                                                                                list-ref trees k
                                                                                              )
                                                                                               "right_value"
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
                                                                    set! preds (
                                                                      append preds (
                                                                        _list pred
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    set! t (
                                                                      + t 1
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
                                            ret2 preds
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
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
        mean_absolute_error y_true y_pred
      )
       (
        call/cc (
          lambda (
            ret19
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
                                  < i (
                                    _len y_true
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        diff (
                                          - (
                                            list-ref y_true i
                                          )
                                           (
                                            list-ref y_pred i
                                          )
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        if (
                                          < diff 0.0
                                        )
                                         (
                                          begin (
                                            set! diff (
                                              - diff
                                            )
                                          )
                                        )
                                         (
                                          quote (
                                            
                                          )
                                        )
                                      )
                                       (
                                        set! sum (
                                          + sum diff
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
                    ret19 (
                      _div sum (
                        + 0.0 (
                          _len y_true
                        )
                      )
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
        mean_squared_error y_true y_pred
      )
       (
        call/cc (
          lambda (
            ret22
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
                                    _len y_true
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        diff (
                                          - (
                                            list-ref y_true i
                                          )
                                           (
                                            list-ref y_pred i
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
                      _div sum (
                        + 0.0 (
                          _len y_true
                        )
                      )
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
            ret25
          )
           (
            let (
              (
                california (
                  alist->hash-table (
                    _list (
                      cons "data" (
                        _list (
                          _list 1.0
                        )
                         (
                          _list 2.0
                        )
                         (
                          _list 3.0
                        )
                         (
                          _list 4.0
                        )
                      )
                    )
                     (
                      cons "target" (
                        _list 2.0 3.0 4.0 5.0
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
                    ds (
                      data_handling california
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        x_train (
                          hash-table-ref ds "data"
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            y_train (
                              hash-table-ref ds "target"
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                x_test (
                                  _list (
                                    _list 1.5
                                  )
                                   (
                                    _list 3.5
                                  )
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    y_test (
                                      _list 2.5 4.5
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        predictions (
                                          xgboost x_train y_train x_test
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        _display (
                                          if (
                                            string? "Predictions:"
                                          )
                                           "Predictions:" (
                                            to-str "Predictions:"
                                          )
                                        )
                                      )
                                       (
                                        newline
                                      )
                                       (
                                        _display (
                                          if (
                                            string? predictions
                                          )
                                           predictions (
                                            to-str predictions
                                          )
                                        )
                                      )
                                       (
                                        newline
                                      )
                                       (
                                        _display (
                                          if (
                                            string? "Mean Absolute Error:"
                                          )
                                           "Mean Absolute Error:" (
                                            to-str "Mean Absolute Error:"
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
                                              mean_absolute_error y_test predictions
                                            )
                                          )
                                           (
                                            mean_absolute_error y_test predictions
                                          )
                                           (
                                            to-str (
                                              mean_absolute_error y_test predictions
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
                                            string? "Mean Square Error:"
                                          )
                                           "Mean Square Error:" (
                                            to-str "Mean Square Error:"
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
                                              mean_squared_error y_test predictions
                                            )
                                          )
                                           (
                                            mean_squared_error y_test predictions
                                          )
                                           (
                                            to-str (
                                              mean_squared_error y_test predictions
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
     (
      main
    )
     (
      let (
        (
          end27 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur28 (
              quotient (
                * (
                  - end27 start26
                )
                 1000000
              )
               jps29
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur28
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
