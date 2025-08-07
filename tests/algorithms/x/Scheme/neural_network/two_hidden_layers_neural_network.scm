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
      define (
        exp_approx x
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                sum 1.0
              )
            )
             (
              begin (
                let (
                  (
                    term 1.0
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
                                      < i 10
                                    )
                                     (
                                      begin (
                                        set! term (
                                          _div (
                                            * term x
                                          )
                                           (
                                            exact->inexact i
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
                        ret1 sum
                      )
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
        sigmoid x
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            ret4 (
              _div 1.0 (
                _add 1.0 (
                  exp_approx (
                    - x
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
        sigmoid_derivative x
      )
       (
        call/cc (
          lambda (
            ret5
          )
           (
            ret5 (
              * x (
                - 1.0 x
              )
            )
          )
        )
      )
    )
     (
      define (
        new_network
      )
       (
        call/cc (
          lambda (
            ret6
          )
           (
            ret6 (
              alist->hash-table (
                _list (
                  cons "w1" (
                    _list (
                      _list 0.1 0.2 0.3 0.4
                    )
                     (
                      _list 0.5 0.6 0.7 0.8
                    )
                     (
                      _list 0.9 1.0 1.1 1.2
                    )
                  )
                )
                 (
                  cons "w2" (
                    _list (
                      _list 0.1 0.2 0.3
                    )
                     (
                      _list 0.4 0.5 0.6
                    )
                     (
                      _list 0.7 0.8 0.9
                    )
                     (
                      _list 1.0 1.1 1.2
                    )
                  )
                )
                 (
                  cons "w3" (
                    _list (
                      _list 0.1
                    )
                     (
                      _list 0.2
                    )
                     (
                      _list 0.3
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
        feedforward net input
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            let (
              (
                hidden1 (
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
                                  < j 4
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        sum1 0.0
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
                                                          < i 3
                                                        )
                                                         (
                                                          begin (
                                                            set! sum1 (
                                                              _add sum1 (
                                                                * (
                                                                  list-ref-safe input i
                                                                )
                                                                 (
                                                                  cond (
                                                                    (
                                                                      string? (
                                                                        list-ref-safe (
                                                                          hash-table-ref net "w1"
                                                                        )
                                                                         i
                                                                      )
                                                                    )
                                                                     (
                                                                      _substring (
                                                                        list-ref-safe (
                                                                          hash-table-ref net "w1"
                                                                        )
                                                                         i
                                                                      )
                                                                       j (
                                                                        + j 1
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    (
                                                                      hash-table? (
                                                                        list-ref-safe (
                                                                          hash-table-ref net "w1"
                                                                        )
                                                                         i
                                                                      )
                                                                    )
                                                                     (
                                                                      hash-table-ref (
                                                                        list-ref-safe (
                                                                          hash-table-ref net "w1"
                                                                        )
                                                                         i
                                                                      )
                                                                       j
                                                                    )
                                                                  )
                                                                   (
                                                                    else (
                                                                      list-ref-safe (
                                                                        list-ref-safe (
                                                                          hash-table-ref net "w1"
                                                                        )
                                                                         i
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
                                                            loop10
                                                          )
                                                        )
                                                         '(
                                                          
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
                                            set! hidden1 (
                                              append hidden1 (
                                                _list (
                                                  sigmoid sum1
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
                                    loop8
                                  )
                                )
                                 '(
                                  
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
                    let (
                      (
                        hidden2 (
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
                                          < k 3
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                sum2 0.0
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    j2 0
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
                                                                  < j2 4
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! sum2 (
                                                                      _add sum2 (
                                                                        * (
                                                                          list-ref-safe hidden1 j2
                                                                        )
                                                                         (
                                                                          cond (
                                                                            (
                                                                              string? (
                                                                                list-ref-safe (
                                                                                  hash-table-ref net "w2"
                                                                                )
                                                                                 j2
                                                                              )
                                                                            )
                                                                             (
                                                                              _substring (
                                                                                list-ref-safe (
                                                                                  hash-table-ref net "w2"
                                                                                )
                                                                                 j2
                                                                              )
                                                                               k (
                                                                                + k 1
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            (
                                                                              hash-table? (
                                                                                list-ref-safe (
                                                                                  hash-table-ref net "w2"
                                                                                )
                                                                                 j2
                                                                              )
                                                                            )
                                                                             (
                                                                              hash-table-ref (
                                                                                list-ref-safe (
                                                                                  hash-table-ref net "w2"
                                                                                )
                                                                                 j2
                                                                              )
                                                                               k
                                                                            )
                                                                          )
                                                                           (
                                                                            else (
                                                                              list-ref-safe (
                                                                                list-ref-safe (
                                                                                  hash-table-ref net "w2"
                                                                                )
                                                                                 j2
                                                                              )
                                                                               k
                                                                            )
                                                                          )
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
                                                    set! hidden2 (
                                                      append hidden2 (
                                                        _list (
                                                          sigmoid sum2
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
                                            )
                                          )
                                           (
                                            loop12
                                          )
                                        )
                                         '(
                                          
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
                            let (
                              (
                                sum3 0.0
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
                                                  < k2 3
                                                )
                                                 (
                                                  begin (
                                                    set! sum3 (
                                                      _add sum3 (
                                                        * (
                                                          list-ref-safe hidden2 k2
                                                        )
                                                         (
                                                          cond (
                                                            (
                                                              string? (
                                                                list-ref-safe (
                                                                  hash-table-ref net "w3"
                                                                )
                                                                 k2
                                                              )
                                                            )
                                                             (
                                                              _substring (
                                                                list-ref-safe (
                                                                  hash-table-ref net "w3"
                                                                )
                                                                 k2
                                                              )
                                                               0 (
                                                                + 0 1
                                                              )
                                                            )
                                                          )
                                                           (
                                                            (
                                                              hash-table? (
                                                                list-ref-safe (
                                                                  hash-table-ref net "w3"
                                                                )
                                                                 k2
                                                              )
                                                            )
                                                             (
                                                              hash-table-ref (
                                                                list-ref-safe (
                                                                  hash-table-ref net "w3"
                                                                )
                                                                 k2
                                                              )
                                                               0
                                                            )
                                                          )
                                                           (
                                                            else (
                                                              list-ref-safe (
                                                                list-ref-safe (
                                                                  hash-table-ref net "w3"
                                                                )
                                                                 k2
                                                              )
                                                               0
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    set! k2 (
                                                      + k2 1
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
                                    let (
                                      (
                                        out (
                                          sigmoid sum3
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        ret7 out
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
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
        train net inputs outputs iterations
      )
       (
        call/cc (
          lambda (
            ret18
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
                                let (
                                  (
                                    s 0
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
                                                  < s (
                                                    _len inputs
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        inp (
                                                          list-ref-safe inputs s
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            target (
                                                              list-ref-safe outputs s
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                hidden1 (
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
                                                                                  < j 4
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    let (
                                                                                      (
                                                                                        sum1 0.0
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
                                                                                                          < i 3
                                                                                                        )
                                                                                                         (
                                                                                                          begin (
                                                                                                            set! sum1 (
                                                                                                              _add sum1 (
                                                                                                                * (
                                                                                                                  list-ref-safe inp i
                                                                                                                )
                                                                                                                 (
                                                                                                                  cond (
                                                                                                                    (
                                                                                                                      string? (
                                                                                                                        list-ref-safe (
                                                                                                                          hash-table-ref net "w1"
                                                                                                                        )
                                                                                                                         i
                                                                                                                      )
                                                                                                                    )
                                                                                                                     (
                                                                                                                      _substring (
                                                                                                                        list-ref-safe (
                                                                                                                          hash-table-ref net "w1"
                                                                                                                        )
                                                                                                                         i
                                                                                                                      )
                                                                                                                       j (
                                                                                                                        + j 1
                                                                                                                      )
                                                                                                                    )
                                                                                                                  )
                                                                                                                   (
                                                                                                                    (
                                                                                                                      hash-table? (
                                                                                                                        list-ref-safe (
                                                                                                                          hash-table-ref net "w1"
                                                                                                                        )
                                                                                                                         i
                                                                                                                      )
                                                                                                                    )
                                                                                                                     (
                                                                                                                      hash-table-ref (
                                                                                                                        list-ref-safe (
                                                                                                                          hash-table-ref net "w1"
                                                                                                                        )
                                                                                                                         i
                                                                                                                      )
                                                                                                                       j
                                                                                                                    )
                                                                                                                  )
                                                                                                                   (
                                                                                                                    else (
                                                                                                                      list-ref-safe (
                                                                                                                        list-ref-safe (
                                                                                                                          hash-table-ref net "w1"
                                                                                                                        )
                                                                                                                         i
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
                                                                                                            loop25
                                                                                                          )
                                                                                                        )
                                                                                                         '(
                                                                                                          
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
                                                                                            set! hidden1 (
                                                                                              append hidden1 (
                                                                                                _list (
                                                                                                  sigmoid sum1
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
                                                                    let (
                                                                      (
                                                                        hidden2 (
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
                                                                                          < k 3
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            let (
                                                                                              (
                                                                                                sum2 0.0
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                let (
                                                                                                  (
                                                                                                    j2 0
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
                                                                                                                  < j2 4
                                                                                                                )
                                                                                                                 (
                                                                                                                  begin (
                                                                                                                    set! sum2 (
                                                                                                                      _add sum2 (
                                                                                                                        * (
                                                                                                                          list-ref-safe hidden1 j2
                                                                                                                        )
                                                                                                                         (
                                                                                                                          cond (
                                                                                                                            (
                                                                                                                              string? (
                                                                                                                                list-ref-safe (
                                                                                                                                  hash-table-ref net "w2"
                                                                                                                                )
                                                                                                                                 j2
                                                                                                                              )
                                                                                                                            )
                                                                                                                             (
                                                                                                                              _substring (
                                                                                                                                list-ref-safe (
                                                                                                                                  hash-table-ref net "w2"
                                                                                                                                )
                                                                                                                                 j2
                                                                                                                              )
                                                                                                                               k (
                                                                                                                                + k 1
                                                                                                                              )
                                                                                                                            )
                                                                                                                          )
                                                                                                                           (
                                                                                                                            (
                                                                                                                              hash-table? (
                                                                                                                                list-ref-safe (
                                                                                                                                  hash-table-ref net "w2"
                                                                                                                                )
                                                                                                                                 j2
                                                                                                                              )
                                                                                                                            )
                                                                                                                             (
                                                                                                                              hash-table-ref (
                                                                                                                                list-ref-safe (
                                                                                                                                  hash-table-ref net "w2"
                                                                                                                                )
                                                                                                                                 j2
                                                                                                                              )
                                                                                                                               k
                                                                                                                            )
                                                                                                                          )
                                                                                                                           (
                                                                                                                            else (
                                                                                                                              list-ref-safe (
                                                                                                                                list-ref-safe (
                                                                                                                                  hash-table-ref net "w2"
                                                                                                                                )
                                                                                                                                 j2
                                                                                                                              )
                                                                                                                               k
                                                                                                                            )
                                                                                                                          )
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
                                                                                                                    loop29
                                                                                                                  )
                                                                                                                )
                                                                                                                 '(
                                                                                                                  
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
                                                                                                    set! hidden2 (
                                                                                                      append hidden2 (
                                                                                                        _list (
                                                                                                          sigmoid sum2
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
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            loop27
                                                                                          )
                                                                                        )
                                                                                         '(
                                                                                          
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
                                                                            let (
                                                                              (
                                                                                sum3 0.0
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                let (
                                                                                  (
                                                                                    k3 0
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
                                                                                                  < k3 3
                                                                                                )
                                                                                                 (
                                                                                                  begin (
                                                                                                    set! sum3 (
                                                                                                      _add sum3 (
                                                                                                        * (
                                                                                                          list-ref-safe hidden2 k3
                                                                                                        )
                                                                                                         (
                                                                                                          cond (
                                                                                                            (
                                                                                                              string? (
                                                                                                                list-ref-safe (
                                                                                                                  hash-table-ref net "w3"
                                                                                                                )
                                                                                                                 k3
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              _substring (
                                                                                                                list-ref-safe (
                                                                                                                  hash-table-ref net "w3"
                                                                                                                )
                                                                                                                 k3
                                                                                                              )
                                                                                                               0 (
                                                                                                                + 0 1
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                           (
                                                                                                            (
                                                                                                              hash-table? (
                                                                                                                list-ref-safe (
                                                                                                                  hash-table-ref net "w3"
                                                                                                                )
                                                                                                                 k3
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              hash-table-ref (
                                                                                                                list-ref-safe (
                                                                                                                  hash-table-ref net "w3"
                                                                                                                )
                                                                                                                 k3
                                                                                                              )
                                                                                                               0
                                                                                                            )
                                                                                                          )
                                                                                                           (
                                                                                                            else (
                                                                                                              list-ref-safe (
                                                                                                                list-ref-safe (
                                                                                                                  hash-table-ref net "w3"
                                                                                                                )
                                                                                                                 k3
                                                                                                              )
                                                                                                               0
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    set! k3 (
                                                                                                      + k3 1
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    loop31
                                                                                                  )
                                                                                                )
                                                                                                 '(
                                                                                                  
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
                                                                                    let (
                                                                                      (
                                                                                        output (
                                                                                          sigmoid sum3
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        let (
                                                                                          (
                                                                                            error (
                                                                                              - target output
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            let (
                                                                                              (
                                                                                                delta_output (
                                                                                                  * error (
                                                                                                    sigmoid_derivative output
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                let (
                                                                                                  (
                                                                                                    new_w3 (
                                                                                                      _list
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  begin (
                                                                                                    let (
                                                                                                      (
                                                                                                        k4 0
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
                                                                                                                      < k4 3
                                                                                                                    )
                                                                                                                     (
                                                                                                                      begin (
                                                                                                                        let (
                                                                                                                          (
                                                                                                                            w3row (
                                                                                                                              list-ref-safe (
                                                                                                                                hash-table-ref net "w3"
                                                                                                                              )
                                                                                                                               k4
                                                                                                                            )
                                                                                                                          )
                                                                                                                        )
                                                                                                                         (
                                                                                                                          begin (
                                                                                                                            list-set! w3row 0 (
                                                                                                                              _add (
                                                                                                                                list-ref-safe w3row 0
                                                                                                                              )
                                                                                                                               (
                                                                                                                                * (
                                                                                                                                  list-ref-safe hidden2 k4
                                                                                                                                )
                                                                                                                                 delta_output
                                                                                                                              )
                                                                                                                            )
                                                                                                                          )
                                                                                                                           (
                                                                                                                            set! new_w3 (
                                                                                                                              append new_w3 (
                                                                                                                                _list w3row
                                                                                                                              )
                                                                                                                            )
                                                                                                                          )
                                                                                                                           (
                                                                                                                            set! k4 (
                                                                                                                              + k4 1
                                                                                                                            )
                                                                                                                          )
                                                                                                                        )
                                                                                                                      )
                                                                                                                       (
                                                                                                                        loop33
                                                                                                                      )
                                                                                                                    )
                                                                                                                     '(
                                                                                                                      
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
                                                                                                        hash-table-set! net "w3" new_w3
                                                                                                      )
                                                                                                       (
                                                                                                        let (
                                                                                                          (
                                                                                                            delta_hidden2 (
                                                                                                              _list
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          begin (
                                                                                                            let (
                                                                                                              (
                                                                                                                k5 0
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
                                                                                                                              < k5 3
                                                                                                                            )
                                                                                                                             (
                                                                                                                              begin (
                                                                                                                                let (
                                                                                                                                  (
                                                                                                                                    row (
                                                                                                                                      list-ref-safe (
                                                                                                                                        hash-table-ref net "w3"
                                                                                                                                      )
                                                                                                                                       k5
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                )
                                                                                                                                 (
                                                                                                                                  begin (
                                                                                                                                    let (
                                                                                                                                      (
                                                                                                                                        dh2 (
                                                                                                                                          * (
                                                                                                                                            * (
                                                                                                                                              list-ref-safe row 0
                                                                                                                                            )
                                                                                                                                             delta_output
                                                                                                                                          )
                                                                                                                                           (
                                                                                                                                            sigmoid_derivative (
                                                                                                                                              list-ref-safe hidden2 k5
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      begin (
                                                                                                                                        set! delta_hidden2 (
                                                                                                                                          append delta_hidden2 (
                                                                                                                                            _list dh2
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                       (
                                                                                                                                        set! k5 (
                                                                                                                                          + k5 1
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
                                                                                                                             '(
                                                                                                                              
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
                                                                                                                    new_w2 (
                                                                                                                      _list
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
                                                                                                                                  < j 4
                                                                                                                                )
                                                                                                                                 (
                                                                                                                                  begin (
                                                                                                                                    let (
                                                                                                                                      (
                                                                                                                                        w2row (
                                                                                                                                          list-ref-safe (
                                                                                                                                            hash-table-ref net "w2"
                                                                                                                                          )
                                                                                                                                           j
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      begin (
                                                                                                                                        let (
                                                                                                                                          (
                                                                                                                                            k6 0
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
                                                                                                                                                          < k6 3
                                                                                                                                                        )
                                                                                                                                                         (
                                                                                                                                                          begin (
                                                                                                                                                            list-set! w2row k6 (
                                                                                                                                                              _add (
                                                                                                                                                                list-ref-safe w2row k6
                                                                                                                                                              )
                                                                                                                                                               (
                                                                                                                                                                * (
                                                                                                                                                                  list-ref-safe hidden1 j
                                                                                                                                                                )
                                                                                                                                                                 (
                                                                                                                                                                  list-ref-safe delta_hidden2 k6
                                                                                                                                                                )
                                                                                                                                                              )
                                                                                                                                                            )
                                                                                                                                                          )
                                                                                                                                                           (
                                                                                                                                                            set! k6 (
                                                                                                                                                              + k6 1
                                                                                                                                                            )
                                                                                                                                                          )
                                                                                                                                                           (
                                                                                                                                                            loop39
                                                                                                                                                          )
                                                                                                                                                        )
                                                                                                                                                         '(
                                                                                                                                                          
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
                                                                                                                                            set! new_w2 (
                                                                                                                                              append new_w2 (
                                                                                                                                                _list w2row
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
                                                                                                                                    loop37
                                                                                                                                  )
                                                                                                                                )
                                                                                                                                 '(
                                                                                                                                  
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
                                                                                                                    hash-table-set! net "w2" new_w2
                                                                                                                  )
                                                                                                                   (
                                                                                                                    let (
                                                                                                                      (
                                                                                                                        delta_hidden1 (
                                                                                                                          _list
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
                                                                                                                                      < j 4
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      begin (
                                                                                                                                        let (
                                                                                                                                          (
                                                                                                                                            sumdh 0.0
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                         (
                                                                                                                                          begin (
                                                                                                                                            let (
                                                                                                                                              (
                                                                                                                                                k7 0
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
                                                                                                                                                              < k7 3
                                                                                                                                                            )
                                                                                                                                                             (
                                                                                                                                                              begin (
                                                                                                                                                                let (
                                                                                                                                                                  (
                                                                                                                                                                    row2 (
                                                                                                                                                                      list-ref-safe (
                                                                                                                                                                        hash-table-ref net "w2"
                                                                                                                                                                      )
                                                                                                                                                                       j
                                                                                                                                                                    )
                                                                                                                                                                  )
                                                                                                                                                                )
                                                                                                                                                                 (
                                                                                                                                                                  begin (
                                                                                                                                                                    set! sumdh (
                                                                                                                                                                      _add sumdh (
                                                                                                                                                                        * (
                                                                                                                                                                          list-ref-safe row2 k7
                                                                                                                                                                        )
                                                                                                                                                                         (
                                                                                                                                                                          list-ref-safe delta_hidden2 k7
                                                                                                                                                                        )
                                                                                                                                                                      )
                                                                                                                                                                    )
                                                                                                                                                                  )
                                                                                                                                                                   (
                                                                                                                                                                    set! k7 (
                                                                                                                                                                      + k7 1
                                                                                                                                                                    )
                                                                                                                                                                  )
                                                                                                                                                                )
                                                                                                                                                              )
                                                                                                                                                               (
                                                                                                                                                                loop43
                                                                                                                                                              )
                                                                                                                                                            )
                                                                                                                                                             '(
                                                                                                                                                              
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
                                                                                                                                                set! delta_hidden1 (
                                                                                                                                                  append delta_hidden1 (
                                                                                                                                                    _list (
                                                                                                                                                      * sumdh (
                                                                                                                                                        sigmoid_derivative (
                                                                                                                                                          list-ref-safe hidden1 j
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
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                       (
                                                                                                                                        loop41
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                     '(
                                                                                                                                      
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
                                                                                                                        let (
                                                                                                                          (
                                                                                                                            new_w1 (
                                                                                                                              _list
                                                                                                                            )
                                                                                                                          )
                                                                                                                        )
                                                                                                                         (
                                                                                                                          begin (
                                                                                                                            let (
                                                                                                                              (
                                                                                                                                i2 0
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
                                                                                                                                              < i2 3
                                                                                                                                            )
                                                                                                                                             (
                                                                                                                                              begin (
                                                                                                                                                let (
                                                                                                                                                  (
                                                                                                                                                    w1row (
                                                                                                                                                      list-ref-safe (
                                                                                                                                                        hash-table-ref net "w1"
                                                                                                                                                      )
                                                                                                                                                       i2
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
                                                                                                                                                                  < j 4
                                                                                                                                                                )
                                                                                                                                                                 (
                                                                                                                                                                  begin (
                                                                                                                                                                    list-set! w1row j (
                                                                                                                                                                      _add (
                                                                                                                                                                        list-ref-safe w1row j
                                                                                                                                                                      )
                                                                                                                                                                       (
                                                                                                                                                                        * (
                                                                                                                                                                          list-ref-safe inp i2
                                                                                                                                                                        )
                                                                                                                                                                         (
                                                                                                                                                                          list-ref-safe delta_hidden1 j
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
                                                                                                                                                    set! new_w1 (
                                                                                                                                                      append new_w1 (
                                                                                                                                                        _list w1row
                                                                                                                                                      )
                                                                                                                                                    )
                                                                                                                                                  )
                                                                                                                                                   (
                                                                                                                                                    set! i2 (
                                                                                                                                                      + i2 1
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
                                                                                                                               (
                                                                                                                                hash-table-set! net "w1" new_w1
                                                                                                                              )
                                                                                                                               (
                                                                                                                                set! s (
                                                                                                                                  + s 1
                                                                                                                                )
                                                                                                                              )
                                                                                                                            )
                                                                                                                          )
                                                                                                                        )
                                                                                                                      )
                                                                                                                    )
                                                                                                                  )
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
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
            )
          )
        )
      )
    )
     (
      define (
        predict net input
      )
       (
        call/cc (
          lambda (
            ret49
          )
           (
            let (
              (
                out (
                  feedforward net input
                )
              )
            )
             (
              begin (
                if (
                  _gt out 0.6
                )
                 (
                  begin (
                    ret49 1
                  )
                )
                 '(
                  
                )
              )
               (
                ret49 0
              )
            )
          )
        )
      )
    )
     (
      define (
        example
      )
       (
        call/cc (
          lambda (
            ret50
          )
           (
            let (
              (
                inputs (
                  _list (
                    _list 0.0 0.0 0.0
                  )
                   (
                    _list 0.0 0.0 1.0
                  )
                   (
                    _list 0.0 1.0 0.0
                  )
                   (
                    _list 0.0 1.0 1.0
                  )
                   (
                    _list 1.0 0.0 0.0
                  )
                   (
                    _list 1.0 0.0 1.0
                  )
                   (
                    _list 1.0 1.0 0.0
                  )
                   (
                    _list 1.0 1.0 1.0
                  )
                )
              )
            )
             (
              begin (
                let (
                  (
                    outputs (
                      _list 0.0 1.0 1.0 0.0 1.0 0.0 0.0 1.0
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        net (
                          new_network
                        )
                      )
                    )
                     (
                      begin (
                        train net inputs outputs 10
                      )
                       (
                        let (
                          (
                            result (
                              predict net (
                                _list 1.0 1.0 1.0
                              )
                            )
                          )
                        )
                         (
                          begin (
                            _display (
                              if (
                                string? (
                                  to-str-space result
                                )
                              )
                               (
                                to-str-space result
                              )
                               (
                                to-str (
                                  to-str-space result
                                )
                              )
                            )
                          )
                           (
                            newline
                          )
                           (
                            ret50 result
                          )
                        )
                      )
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
            ret51
          )
           (
            example
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
