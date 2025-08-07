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
      start29 (
        current-jiffy
      )
    )
     (
      jps32 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        mean xs
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
                                    _len xs
                                  )
                                )
                                 (
                                  begin (
                                    set! sum (
                                      + sum (
                                        list-ref xs i
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
                      _div sum (
                        * (
                          _len xs
                        )
                         1.0
                      )
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
        stump_predict s x
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            begin (
              if (
                < (
                  list-ref x (
                    hash-table-ref s "feature"
                  )
                )
                 (
                  hash-table-ref s "threshold"
                )
              )
               (
                begin (
                  ret4 (
                    hash-table-ref s "left"
                  )
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              ret4 (
                hash-table-ref s "right"
              )
            )
          )
        )
      )
    )
     (
      define (
        train_stump features residuals
      )
       (
        call/cc (
          lambda (
            ret5
          )
           (
            let (
              (
                best_feature 0
              )
            )
             (
              begin (
                let (
                  (
                    best_threshold 0.0
                  )
                )
                 (
                  begin (
                    let (
                      (
                        best_error 1000000000.0
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            best_left 0.0
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                best_right 0.0
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    num_features (
                                      _len (
                                        list-ref features 0
                                      )
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        f 0
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
                                                      < f num_features
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
                                                                            _len features
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                threshold (
                                                                                  cond (
                                                                                    (
                                                                                      string? (
                                                                                        list-ref features i
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref features i
                                                                                      )
                                                                                       f (
                                                                                        + f 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref features i
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref features i
                                                                                      )
                                                                                       f
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref features i
                                                                                      )
                                                                                       f
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                let (
                                                                                  (
                                                                                    left (
                                                                                      _list
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    let (
                                                                                      (
                                                                                        right (
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
                                                                                                          < j (
                                                                                                            _len features
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          begin (
                                                                                                            if (
                                                                                                              < (
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
                                                                                                                     f (
                                                                                                                      + f 1
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
                                                                                                                     f
                                                                                                                  )
                                                                                                                )
                                                                                                                 (
                                                                                                                  else (
                                                                                                                    list-ref (
                                                                                                                      list-ref features j
                                                                                                                    )
                                                                                                                     f
                                                                                                                  )
                                                                                                                )
                                                                                                              )
                                                                                                               threshold
                                                                                                            )
                                                                                                             (
                                                                                                              begin (
                                                                                                                set! left (
                                                                                                                  append left (
                                                                                                                    _list (
                                                                                                                      list-ref residuals j
                                                                                                                    )
                                                                                                                  )
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              begin (
                                                                                                                set! right (
                                                                                                                  append right (
                                                                                                                    _list (
                                                                                                                      list-ref residuals j
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
                                                                                            if (
                                                                                              and (
                                                                                                not (
                                                                                                  equal? (
                                                                                                    _len left
                                                                                                  )
                                                                                                   0
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                not (
                                                                                                  equal? (
                                                                                                    _len right
                                                                                                  )
                                                                                                   0
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                let (
                                                                                                  (
                                                                                                    left_mean (
                                                                                                      mean left
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  begin (
                                                                                                    let (
                                                                                                      (
                                                                                                        right_mean (
                                                                                                          mean right
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      begin (
                                                                                                        let (
                                                                                                          (
                                                                                                            err 0.0
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          begin (
                                                                                                            set! j 0
                                                                                                          )
                                                                                                           (
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
                                                                                                                          < j (
                                                                                                                            _len features
                                                                                                                          )
                                                                                                                        )
                                                                                                                         (
                                                                                                                          begin (
                                                                                                                            let (
                                                                                                                              (
                                                                                                                                pred (
                                                                                                                                  if (
                                                                                                                                    < (
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
                                                                                                                                           f (
                                                                                                                                            + f 1
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
                                                                                                                                           f
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                       (
                                                                                                                                        else (
                                                                                                                                          list-ref (
                                                                                                                                            list-ref features j
                                                                                                                                          )
                                                                                                                                           f
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                     threshold
                                                                                                                                  )
                                                                                                                                   left_mean right_mean
                                                                                                                                )
                                                                                                                              )
                                                                                                                            )
                                                                                                                             (
                                                                                                                              begin (
                                                                                                                                let (
                                                                                                                                  (
                                                                                                                                    diff (
                                                                                                                                      - (
                                                                                                                                        list-ref residuals j
                                                                                                                                      )
                                                                                                                                       pred
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                )
                                                                                                                                 (
                                                                                                                                  begin (
                                                                                                                                    set! err (
                                                                                                                                      _add err (
                                                                                                                                        * diff diff
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
                                                                                                            if (
                                                                                                              < err best_error
                                                                                                            )
                                                                                                             (
                                                                                                              begin (
                                                                                                                set! best_error err
                                                                                                              )
                                                                                                               (
                                                                                                                set! best_feature f
                                                                                                              )
                                                                                                               (
                                                                                                                set! best_threshold threshold
                                                                                                              )
                                                                                                               (
                                                                                                                set! best_left left_mean
                                                                                                              )
                                                                                                               (
                                                                                                                set! best_right right_mean
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
                                                                                        )
                                                                                      )
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
                                                            set! f (
                                                              + f 1
                                                            )
                                                          )
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
                                        ret5 (
                                          alist->hash-table (
                                            _list (
                                              cons "feature" best_feature
                                            )
                                             (
                                              cons "threshold" best_threshold
                                            )
                                             (
                                              cons "left" best_left
                                            )
                                             (
                                              cons "right" best_right
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
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
        boost features targets rounds
      )
       (
        call/cc (
          lambda (
            ret14
          )
           (
            let (
              (
                model (
                  _list
                )
              )
            )
             (
              begin (
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
                                      < i (
                                        _len targets
                                      )
                                    )
                                     (
                                      begin (
                                        set! preds (
                                          append preds (
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
                        let (
                          (
                            r 0
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
                                          < r rounds
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
                                                                  < j (
                                                                    _len targets
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! residuals (
                                                                      append residuals (
                                                                        _list (
                                                                          - (
                                                                            list-ref targets j
                                                                          )
                                                                           (
                                                                            list-ref preds j
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
                                                    let (
                                                      (
                                                        stump (
                                                          train_stump features residuals
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        set! model (
                                                          append model (
                                                            _list stump
                                                          )
                                                        )
                                                      )
                                                       (
                                                        set! j 0
                                                      )
                                                       (
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
                                                                      < j (
                                                                        _len preds
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        list-set! preds j (
                                                                          _add (
                                                                            list-ref preds j
                                                                          )
                                                                           (
                                                                            stump_predict stump (
                                                                              list-ref features j
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
                                                        set! r (
                                                          + r 1
                                                        )
                                                      )
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
                            ret14 model
                          )
                        )
                      )
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
        predict model x
      )
       (
        call/cc (
          lambda (
            ret23
          )
           (
            let (
              (
                score 0.0
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
                                  < i (
                                    _len model
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        s (
                                          list-ref model i
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        if (
                                          < (
                                            list-ref x (
                                              hash-table-ref s "feature"
                                            )
                                          )
                                           (
                                            hash-table-ref s "threshold"
                                          )
                                        )
                                         (
                                          begin (
                                            set! score (
                                              + score (
                                                hash-table-ref s "left"
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            set! score (
                                              + score (
                                                hash-table-ref s "right"
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
                    ret23 score
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
            ret26
          )
           (
            let (
              (
                features (
                  _list (
                    _list 5.1 3.5
                  )
                   (
                    _list 4.9 3.0
                  )
                   (
                    _list 6.2 3.4
                  )
                   (
                    _list 5.9 3.0
                  )
                )
              )
            )
             (
              begin (
                let (
                  (
                    targets (
                      _list 0 0 1 1
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        model (
                          boost features targets 3
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            out ""
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
                                              < i (
                                                _len features
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    s (
                                                      predict model (
                                                        list-ref features i
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        label (
                                                          if (
                                                            >= s 0.5
                                                          )
                                                           1 0
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        if (
                                                          equal? i 0
                                                        )
                                                         (
                                                          begin (
                                                            set! out (
                                                              to-str-space label
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            set! out (
                                                              string-append (
                                                                string-append out " "
                                                              )
                                                               (
                                                                to-str-space label
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
                                _display (
                                  if (
                                    string? out
                                  )
                                   out (
                                    to-str out
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
     (
      main
    )
     (
      let (
        (
          end30 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur31 (
              quotient (
                * (
                  - end30 start29
                )
                 1000000
              )
               jps32
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur31
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
