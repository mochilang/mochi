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
      start71 (
        current-jiffy
      )
    )
     (
      jps74 (
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
        maxf a b
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            begin (
              if (
                > a b
              )
               (
                begin (
                  ret2 a
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              ret2 b
            )
          )
        )
      )
    )
     (
      define (
        minf a b
      )
       (
        call/cc (
          lambda (
            ret3
          )
           (
            begin (
              if (
                < a b
              )
               (
                begin (
                  ret3 a
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              ret3 b
            )
          )
        )
      )
    )
     (
      define (
        clip x lo hi
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            ret4 (
              maxf lo (
                minf x hi
              )
            )
          )
        )
      )
    )
     (
      define (
        to_float x
      )
       (
        call/cc (
          lambda (
            ret5
          )
           (
            ret5 (
              * x 1.0
            )
          )
        )
      )
    )
     (
      define (
        powf base exp
      )
       (
        call/cc (
          lambda (
            ret6
          )
           (
            let (
              (
                result 1.0
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
                        n (
                          let (
                            (
                              v7 exp
                            )
                          )
                           (
                            cond (
                              (
                                string? v7
                              )
                               (
                                exact (
                                  floor (
                                    string->number v7
                                  )
                                )
                              )
                            )
                             (
                              (
                                boolean? v7
                              )
                               (
                                if v7 1 0
                              )
                            )
                             (
                              else (
                                exact (
                                  floor v7
                                )
                              )
                            )
                          )
                        )
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
                                      < i n
                                    )
                                     (
                                      begin (
                                        set! result (
                                          * result base
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
     (
      define (
        ln x
      )
       (
        call/cc (
          lambda (
            ret10
          )
           (
            begin (
              if (
                <= x 0.0
              )
               (
                begin (
                  panic "ln domain error"
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
                  y (
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
                      y2 (
                        * y y
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          term y
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
                                  k 0
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
                                                < k 10
                                              )
                                               (
                                                begin (
                                                  let (
                                                    (
                                                      denom (
                                                        to_float (
                                                          + (
                                                            * 2 k
                                                          )
                                                           1
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      set! sum (
                                                        _add sum (
                                                          _div term denom
                                                        )
                                                      )
                                                    )
                                                     (
                                                      set! term (
                                                        * term y2
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
                                  ret10 (
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
        )
      )
    )
     (
      define (
        exp x
      )
       (
        call/cc (
          lambda (
            ret13
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
                        n 1
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
                                      < n 20
                                    )
                                     (
                                      begin (
                                        set! term (
                                          _div (
                                            * term x
                                          )
                                           (
                                            to_float n
                                          )
                                        )
                                      )
                                       (
                                        set! sum (
                                          + sum term
                                        )
                                      )
                                       (
                                        set! n (
                                          + n 1
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
                        ret13 sum
                      )
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
        mean v
      )
       (
        call/cc (
          lambda (
            ret16
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
                                    _len v
                                  )
                                )
                                 (
                                  begin (
                                    set! total (
                                      + total (
                                        list-ref v i
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
                      _div total (
                        to_float (
                          _len v
                        )
                      )
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
        binary_cross_entropy y_true y_pred epsilon
      )
       (
        call/cc (
          lambda (
            ret19
          )
           (
            begin (
              if (
                not (
                  equal? (
                    _len y_true
                  )
                   (
                    _len y_pred
                  )
                )
              )
               (
                begin (
                  panic "Input arrays must have the same length."
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
                  losses (
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
                                          yt (
                                            list-ref y_true i
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          let (
                                            (
                                              yp (
                                                clip (
                                                  list-ref y_pred i
                                                )
                                                 epsilon (
                                                  - 1.0 epsilon
                                                )
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              let (
                                                (
                                                  loss (
                                                    - (
                                                      _add (
                                                        * yt (
                                                          ln yp
                                                        )
                                                      )
                                                       (
                                                        * (
                                                          - 1.0 yt
                                                        )
                                                         (
                                                          ln (
                                                            - 1.0 yp
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                begin (
                                                  set! losses (
                                                    append losses (
                                                      _list loss
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
                        mean losses
                      )
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
        binary_focal_cross_entropy y_true y_pred gamma alpha epsilon
      )
       (
        call/cc (
          lambda (
            ret22
          )
           (
            begin (
              if (
                not (
                  equal? (
                    _len y_true
                  )
                   (
                    _len y_pred
                  )
                )
              )
               (
                begin (
                  panic "Input arrays must have the same length."
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
                  losses (
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
                                          yt (
                                            list-ref y_true i
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          let (
                                            (
                                              yp (
                                                clip (
                                                  list-ref y_pred i
                                                )
                                                 epsilon (
                                                  - 1.0 epsilon
                                                )
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              let (
                                                (
                                                  term1 (
                                                    * (
                                                      * (
                                                        * alpha (
                                                          powf (
                                                            - 1.0 yp
                                                          )
                                                           gamma
                                                        )
                                                      )
                                                       yt
                                                    )
                                                     (
                                                      ln yp
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                begin (
                                                  let (
                                                    (
                                                      term2 (
                                                        * (
                                                          * (
                                                            * (
                                                              - 1.0 alpha
                                                            )
                                                             (
                                                              powf yp gamma
                                                            )
                                                          )
                                                           (
                                                            - 1.0 yt
                                                          )
                                                        )
                                                         (
                                                          ln (
                                                            - 1.0 yp
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      set! losses (
                                                        append losses (
                                                          _list (
                                                            - (
                                                              _add term1 term2
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
                        mean losses
                      )
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
        categorical_cross_entropy y_true y_pred epsilon
      )
       (
        call/cc (
          lambda (
            ret25
          )
           (
            begin (
              if (
                not (
                  equal? (
                    _len y_true
                  )
                   (
                    _len y_pred
                  )
                )
              )
               (
                begin (
                  panic "Input arrays must have the same shape."
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
                  rows (
                    _len y_true
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
                                        < i rows
                                      )
                                       (
                                        begin (
                                          if (
                                            not (
                                              equal? (
                                                _len (
                                                  list-ref y_true i
                                                )
                                              )
                                               (
                                                _len (
                                                  list-ref y_pred i
                                                )
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              panic "Input arrays must have the same shape."
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
                                              sum_true 0.0
                                            )
                                          )
                                           (
                                            begin (
                                              let (
                                                (
                                                  sum_pred 0.0
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
                                                                    < j (
                                                                      _len (
                                                                        list-ref y_true i
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    begin (
                                                                      let (
                                                                        (
                                                                          yt (
                                                                            cond (
                                                                              (
                                                                                string? (
                                                                                  list-ref y_true i
                                                                                )
                                                                              )
                                                                               (
                                                                                _substring (
                                                                                  list-ref y_true i
                                                                                )
                                                                                 j (
                                                                                  + j 1
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              (
                                                                                hash-table? (
                                                                                  list-ref y_true i
                                                                                )
                                                                              )
                                                                               (
                                                                                hash-table-ref (
                                                                                  list-ref y_true i
                                                                                )
                                                                                 j
                                                                              )
                                                                            )
                                                                             (
                                                                              else (
                                                                                list-ref (
                                                                                  list-ref y_true i
                                                                                )
                                                                                 j
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        begin (
                                                                          let (
                                                                            (
                                                                              yp (
                                                                                cond (
                                                                                  (
                                                                                    string? (
                                                                                      list-ref y_pred i
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    _substring (
                                                                                      list-ref y_pred i
                                                                                    )
                                                                                     j (
                                                                                      + j 1
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  (
                                                                                    hash-table? (
                                                                                      list-ref y_pred i
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    hash-table-ref (
                                                                                      list-ref y_pred i
                                                                                    )
                                                                                     j
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  else (
                                                                                    list-ref (
                                                                                      list-ref y_pred i
                                                                                    )
                                                                                     j
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            begin (
                                                                              if (
                                                                                and (
                                                                                  not (
                                                                                    equal? yt 0.0
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  not (
                                                                                    equal? yt 1.0
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                begin (
                                                                                  panic "y_true must be one-hot encoded."
                                                                                )
                                                                              )
                                                                               (
                                                                                quote (
                                                                                  
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              set! sum_true (
                                                                                + sum_true yt
                                                                              )
                                                                            )
                                                                             (
                                                                              set! sum_pred (
                                                                                + sum_pred yp
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
                                                      if (
                                                        not (
                                                          equal? sum_true 1.0
                                                        )
                                                      )
                                                       (
                                                        begin (
                                                          panic "y_true must be one-hot encoded."
                                                        )
                                                      )
                                                       (
                                                        quote (
                                                          
                                                        )
                                                      )
                                                    )
                                                     (
                                                      if (
                                                        _gt (
                                                          absf (
                                                            - sum_pred 1.0
                                                          )
                                                        )
                                                         epsilon
                                                      )
                                                       (
                                                        begin (
                                                          panic "Predicted probabilities must sum to approximately 1."
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
                                                                    < j (
                                                                      _len (
                                                                        list-ref y_true i
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    begin (
                                                                      let (
                                                                        (
                                                                          yp (
                                                                            clip (
                                                                              cond (
                                                                                (
                                                                                  string? (
                                                                                    list-ref y_pred i
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  _substring (
                                                                                    list-ref y_pred i
                                                                                  )
                                                                                   j (
                                                                                    + j 1
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                (
                                                                                  hash-table? (
                                                                                    list-ref y_pred i
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  hash-table-ref (
                                                                                    list-ref y_pred i
                                                                                  )
                                                                                   j
                                                                                )
                                                                              )
                                                                               (
                                                                                else (
                                                                                  list-ref (
                                                                                    list-ref y_pred i
                                                                                  )
                                                                                   j
                                                                                )
                                                                              )
                                                                            )
                                                                             epsilon 1.0
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        begin (
                                                                          set! total (
                                                                            - total (
                                                                              * (
                                                                                cond (
                                                                                  (
                                                                                    string? (
                                                                                      list-ref y_true i
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    _substring (
                                                                                      list-ref y_true i
                                                                                    )
                                                                                     j (
                                                                                      + j 1
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  (
                                                                                    hash-table? (
                                                                                      list-ref y_true i
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    hash-table-ref (
                                                                                      list-ref y_true i
                                                                                    )
                                                                                     j
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  else (
                                                                                    list-ref (
                                                                                      list-ref y_true i
                                                                                    )
                                                                                     j
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                ln yp
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
                          ret25 total
                        )
                      )
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
        categorical_focal_cross_entropy y_true y_pred alpha gamma epsilon
      )
       (
        call/cc (
          lambda (
            ret32
          )
           (
            begin (
              if (
                not (
                  equal? (
                    _len y_true
                  )
                   (
                    _len y_pred
                  )
                )
              )
               (
                begin (
                  panic "Shape of y_true and y_pred must be the same."
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
                  rows (
                    _len y_true
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      cols (
                        _len (
                          list-ref y_true 0
                        )
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          a alpha
                        )
                      )
                       (
                        begin (
                          if (
                            equal? (
                              _len a
                            )
                             0
                          )
                           (
                            begin (
                              let (
                                (
                                  tmp (
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
                                                    < j cols
                                                  )
                                                   (
                                                    begin (
                                                      set! tmp (
                                                        append tmp (
                                                          _list 1.0
                                                        )
                                                      )
                                                    )
                                                     (
                                                      set! j (
                                                        + j 1
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
                                      set! a tmp
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
                          if (
                            not (
                              equal? (
                                _len a
                              )
                               cols
                            )
                          )
                           (
                            begin (
                              panic "Length of alpha must match the number of classes."
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
                                                < i rows
                                              )
                                               (
                                                begin (
                                                  if (
                                                    or (
                                                      not (
                                                        equal? (
                                                          _len (
                                                            list-ref y_true i
                                                          )
                                                        )
                                                         cols
                                                      )
                                                    )
                                                     (
                                                      not (
                                                        equal? (
                                                          _len (
                                                            list-ref y_pred i
                                                          )
                                                        )
                                                         cols
                                                      )
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      panic "Shape of y_true and y_pred must be the same."
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
                                                      sum_true 0.0
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      let (
                                                        (
                                                          sum_pred 0.0
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
                                                                            < j cols
                                                                          )
                                                                           (
                                                                            begin (
                                                                              let (
                                                                                (
                                                                                  yt (
                                                                                    cond (
                                                                                      (
                                                                                        string? (
                                                                                          list-ref y_true i
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        _substring (
                                                                                          list-ref y_true i
                                                                                        )
                                                                                         j (
                                                                                          + j 1
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      (
                                                                                        hash-table? (
                                                                                          list-ref y_true i
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        hash-table-ref (
                                                                                          list-ref y_true i
                                                                                        )
                                                                                         j
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      else (
                                                                                        list-ref (
                                                                                          list-ref y_true i
                                                                                        )
                                                                                         j
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                begin (
                                                                                  let (
                                                                                    (
                                                                                      yp (
                                                                                        cond (
                                                                                          (
                                                                                            string? (
                                                                                              list-ref y_pred i
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            _substring (
                                                                                              list-ref y_pred i
                                                                                            )
                                                                                             j (
                                                                                              + j 1
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          (
                                                                                            hash-table? (
                                                                                              list-ref y_pred i
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            hash-table-ref (
                                                                                              list-ref y_pred i
                                                                                            )
                                                                                             j
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          else (
                                                                                            list-ref (
                                                                                              list-ref y_pred i
                                                                                            )
                                                                                             j
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    begin (
                                                                                      if (
                                                                                        and (
                                                                                          not (
                                                                                            equal? yt 0.0
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          not (
                                                                                            equal? yt 1.0
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        begin (
                                                                                          panic "y_true must be one-hot encoded."
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        quote (
                                                                                          
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      set! sum_true (
                                                                                        + sum_true yt
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      set! sum_pred (
                                                                                        + sum_pred yp
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
                                                              if (
                                                                not (
                                                                  equal? sum_true 1.0
                                                                )
                                                              )
                                                               (
                                                                begin (
                                                                  panic "y_true must be one-hot encoded."
                                                                )
                                                              )
                                                               (
                                                                quote (
                                                                  
                                                                )
                                                              )
                                                            )
                                                             (
                                                              if (
                                                                _gt (
                                                                  absf (
                                                                    - sum_pred 1.0
                                                                  )
                                                                )
                                                                 epsilon
                                                              )
                                                               (
                                                                begin (
                                                                  panic "Predicted probabilities must sum to approximately 1."
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
                                                                  row_loss 0.0
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
                                                                                < j cols
                                                                              )
                                                                               (
                                                                                begin (
                                                                                  let (
                                                                                    (
                                                                                      yp (
                                                                                        clip (
                                                                                          cond (
                                                                                            (
                                                                                              string? (
                                                                                                list-ref y_pred i
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              _substring (
                                                                                                list-ref y_pred i
                                                                                              )
                                                                                               j (
                                                                                                + j 1
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            (
                                                                                              hash-table? (
                                                                                                list-ref y_pred i
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              hash-table-ref (
                                                                                                list-ref y_pred i
                                                                                              )
                                                                                               j
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            else (
                                                                                              list-ref (
                                                                                                list-ref y_pred i
                                                                                              )
                                                                                               j
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         epsilon 1.0
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    begin (
                                                                                      set! row_loss (
                                                                                        _add row_loss (
                                                                                          * (
                                                                                            * (
                                                                                              * (
                                                                                                list-ref a j
                                                                                              )
                                                                                               (
                                                                                                powf (
                                                                                                  - 1.0 yp
                                                                                                )
                                                                                                 gamma
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              cond (
                                                                                                (
                                                                                                  string? (
                                                                                                    list-ref y_true i
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  _substring (
                                                                                                    list-ref y_true i
                                                                                                  )
                                                                                                   j (
                                                                                                    + j 1
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                (
                                                                                                  hash-table? (
                                                                                                    list-ref y_true i
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  hash-table-ref (
                                                                                                    list-ref y_true i
                                                                                                  )
                                                                                                   j
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                else (
                                                                                                  list-ref (
                                                                                                    list-ref y_true i
                                                                                                  )
                                                                                                   j
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            ln yp
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
                                                                  set! total (
                                                                    - total row_loss
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
                                  ret32 (
                                    _div total (
                                      to_float rows
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
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
        hinge_loss y_true y_pred
      )
       (
        call/cc (
          lambda (
            ret41
          )
           (
            begin (
              if (
                not (
                  equal? (
                    _len y_true
                  )
                   (
                    _len y_pred
                  )
                )
              )
               (
                begin (
                  panic "Length of predicted and actual array must be same."
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
                  losses (
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
                          break43
                        )
                         (
                          letrec (
                            (
                              loop42 (
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
                                          yt (
                                            list-ref y_true i
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          if (
                                            and (
                                              not (
                                                equal? yt (
                                                  - 1.0
                                                )
                                              )
                                            )
                                             (
                                              not (
                                                equal? yt 1.0
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              panic "y_true can have values -1 or 1 only."
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
                                              pred (
                                                list-ref y_pred i
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              let (
                                                (
                                                  l (
                                                    maxf 0.0 (
                                                      - 1.0 (
                                                        * yt pred
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                begin (
                                                  set! losses (
                                                    append losses (
                                                      _list l
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
                                      loop42
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
                            loop42
                          )
                        )
                      )
                    )
                     (
                      ret41 (
                        mean losses
                      )
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
        huber_loss y_true y_pred delta
      )
       (
        call/cc (
          lambda (
            ret44
          )
           (
            begin (
              if (
                not (
                  equal? (
                    _len y_true
                  )
                   (
                    _len y_pred
                  )
                )
              )
               (
                begin (
                  panic "Input arrays must have the same length."
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
                                          let (
                                            (
                                              adiff (
                                                absf diff
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              if (
                                                _le adiff delta
                                              )
                                               (
                                                begin (
                                                  set! total (
                                                    _add total (
                                                      * (
                                                        * 0.5 diff
                                                      )
                                                       diff
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                begin (
                                                  set! total (
                                                    _add total (
                                                      * delta (
                                                        - adiff (
                                                          * 0.5 delta
                                                        )
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
                                      loop45
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
                            loop45
                          )
                        )
                      )
                    )
                     (
                      ret44 (
                        _div total (
                          to_float (
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
    )
     (
      define (
        mean_squared_error y_true y_pred
      )
       (
        call/cc (
          lambda (
            ret47
          )
           (
            begin (
              if (
                not (
                  equal? (
                    _len y_true
                  )
                   (
                    _len y_pred
                  )
                )
              )
               (
                begin (
                  panic "Input arrays must have the same length."
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
                  losses (
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
                                          set! losses (
                                            append losses (
                                              _list (
                                                * diff diff
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
                      ret47 (
                        mean losses
                      )
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
            ret50
          )
           (
            begin (
              if (
                not (
                  equal? (
                    _len y_true
                  )
                   (
                    _len y_pred
                  )
                )
              )
               (
                begin (
                  panic "Input arrays must have the same length."
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
                          break52
                        )
                         (
                          letrec (
                            (
                              loop51 (
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
                                      set! total (
                                        _add total (
                                          absf (
                                            - (
                                              list-ref y_true i
                                            )
                                             (
                                              list-ref y_pred i
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
                                      loop51
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
                            loop51
                          )
                        )
                      )
                    )
                     (
                      ret50 (
                        _div total (
                          to_float (
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
    )
     (
      define (
        mean_squared_logarithmic_error y_true y_pred
      )
       (
        call/cc (
          lambda (
            ret53
          )
           (
            begin (
              if (
                not (
                  equal? (
                    _len y_true
                  )
                   (
                    _len y_pred
                  )
                )
              )
               (
                begin (
                  panic "Input arrays must have the same length."
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
                                      _len y_true
                                    )
                                  )
                                   (
                                    begin (
                                      let (
                                        (
                                          a (
                                            ln (
                                              + 1.0 (
                                                list-ref y_true i
                                              )
                                            )
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          let (
                                            (
                                              b (
                                                ln (
                                                  + 1.0 (
                                                    list-ref y_pred i
                                                  )
                                                )
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              let (
                                                (
                                                  diff (
                                                    - a b
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
                      ret53 (
                        _div total (
                          to_float (
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
    )
     (
      define (
        mean_absolute_percentage_error y_true y_pred epsilon
      )
       (
        call/cc (
          lambda (
            ret56
          )
           (
            begin (
              if (
                not (
                  equal? (
                    _len y_true
                  )
                   (
                    _len y_pred
                  )
                )
              )
               (
                begin (
                  panic "The length of the two arrays should be the same."
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
                                      _len y_true
                                    )
                                  )
                                   (
                                    begin (
                                      let (
                                        (
                                          yt (
                                            list-ref y_true i
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          if (
                                            equal? yt 0.0
                                          )
                                           (
                                            begin (
                                              set! yt epsilon
                                            )
                                          )
                                           (
                                            quote (
                                              
                                            )
                                          )
                                        )
                                         (
                                          set! total (
                                            _add total (
                                              absf (
                                                _div (
                                                  - yt (
                                                    list-ref y_pred i
                                                  )
                                                )
                                                 yt
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
                      ret56 (
                        _div total (
                          to_float (
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
    )
     (
      define (
        perplexity_loss y_true y_pred epsilon
      )
       (
        call/cc (
          lambda (
            ret59
          )
           (
            let (
              (
                batch (
                  _len y_true
                )
              )
            )
             (
              begin (
                if (
                  not (
                    equal? batch (
                      _len y_pred
                    )
                  )
                )
                 (
                  begin (
                    panic "Batch size of y_true and y_pred must be equal."
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
                    sentence_len (
                      _len (
                        list-ref y_true 0
                      )
                    )
                  )
                )
                 (
                  begin (
                    if (
                      not (
                        equal? sentence_len (
                          _len (
                            list-ref y_pred 0
                          )
                        )
                      )
                    )
                     (
                      begin (
                        panic "Sentence length of y_true and y_pred must be equal."
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
                        vocab_size (
                          _len (
                            cond (
                              (
                                string? (
                                  list-ref y_pred 0
                                )
                              )
                               (
                                _substring (
                                  list-ref y_pred 0
                                )
                                 0 (
                                  + 0 1
                                )
                              )
                            )
                             (
                              (
                                hash-table? (
                                  list-ref y_pred 0
                                )
                              )
                               (
                                hash-table-ref (
                                  list-ref y_pred 0
                                )
                                 0
                              )
                            )
                             (
                              else (
                                list-ref (
                                  list-ref y_pred 0
                                )
                                 0
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
                            b 0
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                total_perp 0.0
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
                                              < b batch
                                            )
                                             (
                                              begin (
                                                if (
                                                  or (
                                                    not (
                                                      equal? (
                                                        _len (
                                                          list-ref y_true b
                                                        )
                                                      )
                                                       sentence_len
                                                    )
                                                  )
                                                   (
                                                    not (
                                                      equal? (
                                                        _len (
                                                          list-ref y_pred b
                                                        )
                                                      )
                                                       sentence_len
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    panic "Sentence length of y_true and y_pred must be equal."
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
                                                    sum_log 0.0
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
                                                                      < j sentence_len
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            label (
                                                                              cond (
                                                                                (
                                                                                  string? (
                                                                                    list-ref y_true b
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  _substring (
                                                                                    list-ref y_true b
                                                                                  )
                                                                                   j (
                                                                                    + j 1
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                (
                                                                                  hash-table? (
                                                                                    list-ref y_true b
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  hash-table-ref (
                                                                                    list-ref y_true b
                                                                                  )
                                                                                   j
                                                                                )
                                                                              )
                                                                               (
                                                                                else (
                                                                                  list-ref (
                                                                                    list-ref y_true b
                                                                                  )
                                                                                   j
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            if (
                                                                              >= label vocab_size
                                                                            )
                                                                             (
                                                                              begin (
                                                                                panic "Label value must not be greater than vocabulary size."
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
                                                                                prob (
                                                                                  clip (
                                                                                    cond (
                                                                                      (
                                                                                        string? (
                                                                                          cond (
                                                                                            (
                                                                                              string? (
                                                                                                list-ref y_pred b
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              _substring (
                                                                                                list-ref y_pred b
                                                                                              )
                                                                                               j (
                                                                                                + j 1
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            (
                                                                                              hash-table? (
                                                                                                list-ref y_pred b
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              hash-table-ref (
                                                                                                list-ref y_pred b
                                                                                              )
                                                                                               j
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            else (
                                                                                              list-ref (
                                                                                                list-ref y_pred b
                                                                                              )
                                                                                               j
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        _substring (
                                                                                          cond (
                                                                                            (
                                                                                              string? (
                                                                                                list-ref y_pred b
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              _substring (
                                                                                                list-ref y_pred b
                                                                                              )
                                                                                               j (
                                                                                                + j 1
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            (
                                                                                              hash-table? (
                                                                                                list-ref y_pred b
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              hash-table-ref (
                                                                                                list-ref y_pred b
                                                                                              )
                                                                                               j
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            else (
                                                                                              list-ref (
                                                                                                list-ref y_pred b
                                                                                              )
                                                                                               j
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         label (
                                                                                          + label 1
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      (
                                                                                        hash-table? (
                                                                                          cond (
                                                                                            (
                                                                                              string? (
                                                                                                list-ref y_pred b
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              _substring (
                                                                                                list-ref y_pred b
                                                                                              )
                                                                                               j (
                                                                                                + j 1
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            (
                                                                                              hash-table? (
                                                                                                list-ref y_pred b
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              hash-table-ref (
                                                                                                list-ref y_pred b
                                                                                              )
                                                                                               j
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            else (
                                                                                              list-ref (
                                                                                                list-ref y_pred b
                                                                                              )
                                                                                               j
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        hash-table-ref (
                                                                                          cond (
                                                                                            (
                                                                                              string? (
                                                                                                list-ref y_pred b
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              _substring (
                                                                                                list-ref y_pred b
                                                                                              )
                                                                                               j (
                                                                                                + j 1
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            (
                                                                                              hash-table? (
                                                                                                list-ref y_pred b
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              hash-table-ref (
                                                                                                list-ref y_pred b
                                                                                              )
                                                                                               j
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            else (
                                                                                              list-ref (
                                                                                                list-ref y_pred b
                                                                                              )
                                                                                               j
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         label
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      else (
                                                                                        list-ref (
                                                                                          cond (
                                                                                            (
                                                                                              string? (
                                                                                                list-ref y_pred b
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              _substring (
                                                                                                list-ref y_pred b
                                                                                              )
                                                                                               j (
                                                                                                + j 1
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            (
                                                                                              hash-table? (
                                                                                                list-ref y_pred b
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              hash-table-ref (
                                                                                                list-ref y_pred b
                                                                                              )
                                                                                               j
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            else (
                                                                                              list-ref (
                                                                                                list-ref y_pred b
                                                                                              )
                                                                                               j
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         label
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   epsilon 1.0
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                set! sum_log (
                                                                                  _add sum_log (
                                                                                    ln prob
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
                                                        let (
                                                          (
                                                            mean_log (
                                                              _div sum_log (
                                                                to_float sentence_len
                                                              )
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                perp (
                                                                  exp (
                                                                    - mean_log
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                set! total_perp (
                                                                  _add total_perp perp
                                                                )
                                                              )
                                                               (
                                                                set! b (
                                                                  + b 1
                                                                )
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
                                ret59 (
                                  _div total_perp (
                                    to_float batch
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
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
        smooth_l1_loss y_true y_pred beta
      )
       (
        call/cc (
          lambda (
            ret64
          )
           (
            begin (
              if (
                not (
                  equal? (
                    _len y_true
                  )
                   (
                    _len y_pred
                  )
                )
              )
               (
                begin (
                  panic "The length of the two arrays should be the same."
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
                          break66
                        )
                         (
                          letrec (
                            (
                              loop65 (
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
                                            absf (
                                              - (
                                                list-ref y_true i
                                              )
                                               (
                                                list-ref y_pred i
                                              )
                                            )
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          if (
                                            _lt diff beta
                                          )
                                           (
                                            begin (
                                              set! total (
                                                _add total (
                                                  _div (
                                                    * (
                                                      * 0.5 diff
                                                    )
                                                     diff
                                                  )
                                                   beta
                                                )
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              set! total (
                                                - (
                                                  _add total diff
                                                )
                                                 (
                                                  * 0.5 beta
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
                                      loop65
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
                            loop65
                          )
                        )
                      )
                    )
                     (
                      ret64 (
                        _div total (
                          to_float (
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
    )
     (
      define (
        kullback_leibler_divergence y_true y_pred
      )
       (
        call/cc (
          lambda (
            ret67
          )
           (
            begin (
              if (
                not (
                  equal? (
                    _len y_true
                  )
                   (
                    _len y_pred
                  )
                )
              )
               (
                begin (
                  panic "Input arrays must have the same length."
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
                          break69
                        )
                         (
                          letrec (
                            (
                              loop68 (
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
                                      set! total (
                                        _add total (
                                          * (
                                            list-ref y_true i
                                          )
                                           (
                                            ln (
                                              _div (
                                                list-ref y_true i
                                              )
                                               (
                                                list-ref y_pred i
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
                                      loop68
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
                            loop68
                          )
                        )
                      )
                    )
                     (
                      ret67 total
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
            ret70
          )
           (
            let (
              (
                y_true_bc (
                  _list 0.0 1.0 1.0 0.0 1.0
                )
              )
            )
             (
              begin (
                let (
                  (
                    y_pred_bc (
                      _list 0.2 0.7 0.9 0.3 0.8
                    )
                  )
                )
                 (
                  begin (
                    _display (
                      if (
                        string? (
                          binary_cross_entropy y_true_bc y_pred_bc 1e-15
                        )
                      )
                       (
                        binary_cross_entropy y_true_bc y_pred_bc 1e-15
                      )
                       (
                        to-str (
                          binary_cross_entropy y_true_bc y_pred_bc 1e-15
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
                          binary_focal_cross_entropy y_true_bc y_pred_bc 2.0 0.25 1e-15
                        )
                      )
                       (
                        binary_focal_cross_entropy y_true_bc y_pred_bc 2.0 0.25 1e-15
                      )
                       (
                        to-str (
                          binary_focal_cross_entropy y_true_bc y_pred_bc 2.0 0.25 1e-15
                        )
                      )
                    )
                  )
                   (
                    newline
                  )
                   (
                    let (
                      (
                        y_true_cce (
                          _list (
                            _list 1.0 0.0 0.0
                          )
                           (
                            _list 0.0 1.0 0.0
                          )
                           (
                            _list 0.0 0.0 1.0
                          )
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            y_pred_cce (
                              _list (
                                _list 0.9 0.1 0.0
                              )
                               (
                                _list 0.2 0.7 0.1
                              )
                               (
                                _list 0.0 0.1 0.9
                              )
                            )
                          )
                        )
                         (
                          begin (
                            _display (
                              if (
                                string? (
                                  categorical_cross_entropy y_true_cce y_pred_cce 1e-15
                                )
                              )
                               (
                                categorical_cross_entropy y_true_cce y_pred_cce 1e-15
                              )
                               (
                                to-str (
                                  categorical_cross_entropy y_true_cce y_pred_cce 1e-15
                                )
                              )
                            )
                          )
                           (
                            newline
                          )
                           (
                            let (
                              (
                                alpha (
                                  _list 0.6 0.2 0.7
                                )
                              )
                            )
                             (
                              begin (
                                _display (
                                  if (
                                    string? (
                                      categorical_focal_cross_entropy y_true_cce y_pred_cce alpha 2.0 1e-15
                                    )
                                  )
                                   (
                                    categorical_focal_cross_entropy y_true_cce y_pred_cce alpha 2.0 1e-15
                                  )
                                   (
                                    to-str (
                                      categorical_focal_cross_entropy y_true_cce y_pred_cce alpha 2.0 1e-15
                                    )
                                  )
                                )
                              )
                               (
                                newline
                              )
                               (
                                let (
                                  (
                                    y_true_hinge (
                                      _list (
                                        - 1.0
                                      )
                                       1.0 1.0 (
                                        - 1.0
                                      )
                                       1.0
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        y_pred_hinge (
                                          _list (
                                            - 4.0
                                          )
                                           (
                                            - 0.3
                                          )
                                           0.7 5.0 10.0
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        _display (
                                          if (
                                            string? (
                                              hinge_loss y_true_hinge y_pred_hinge
                                            )
                                          )
                                           (
                                            hinge_loss y_true_hinge y_pred_hinge
                                          )
                                           (
                                            to-str (
                                              hinge_loss y_true_hinge y_pred_hinge
                                            )
                                          )
                                        )
                                      )
                                       (
                                        newline
                                      )
                                       (
                                        let (
                                          (
                                            y_true_huber (
                                              _list 0.9 10.0 2.0 1.0 5.2
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                y_pred_huber (
                                                  _list 0.8 2.1 2.9 4.2 5.2
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                _display (
                                                  if (
                                                    string? (
                                                      huber_loss y_true_huber y_pred_huber 1.0
                                                    )
                                                  )
                                                   (
                                                    huber_loss y_true_huber y_pred_huber 1.0
                                                  )
                                                   (
                                                    to-str (
                                                      huber_loss y_true_huber y_pred_huber 1.0
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
                                                      mean_squared_error y_true_huber y_pred_huber
                                                    )
                                                  )
                                                   (
                                                    mean_squared_error y_true_huber y_pred_huber
                                                  )
                                                   (
                                                    to-str (
                                                      mean_squared_error y_true_huber y_pred_huber
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
                                                      mean_absolute_error y_true_huber y_pred_huber
                                                    )
                                                  )
                                                   (
                                                    mean_absolute_error y_true_huber y_pred_huber
                                                  )
                                                   (
                                                    to-str (
                                                      mean_absolute_error y_true_huber y_pred_huber
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
                                                      mean_squared_logarithmic_error y_true_huber y_pred_huber
                                                    )
                                                  )
                                                   (
                                                    mean_squared_logarithmic_error y_true_huber y_pred_huber
                                                  )
                                                   (
                                                    to-str (
                                                      mean_squared_logarithmic_error y_true_huber y_pred_huber
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                newline
                                              )
                                               (
                                                let (
                                                  (
                                                    y_true_mape (
                                                      _list 10.0 20.0 30.0 40.0
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        y_pred_mape (
                                                          _list 12.0 18.0 33.0 45.0
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        _display (
                                                          if (
                                                            string? (
                                                              mean_absolute_percentage_error y_true_mape y_pred_mape 1e-15
                                                            )
                                                          )
                                                           (
                                                            mean_absolute_percentage_error y_true_mape y_pred_mape 1e-15
                                                          )
                                                           (
                                                            to-str (
                                                              mean_absolute_percentage_error y_true_mape y_pred_mape 1e-15
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        newline
                                                      )
                                                       (
                                                        let (
                                                          (
                                                            y_true_perp (
                                                              _list (
                                                                _list 1 4
                                                              )
                                                               (
                                                                _list 2 3
                                                              )
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                y_pred_perp (
                                                                  _list (
                                                                    _list (
                                                                      _list 0.28 0.19 0.21 0.15 0.17
                                                                    )
                                                                     (
                                                                      _list 0.24 0.19 0.09 0.18 0.3
                                                                    )
                                                                  )
                                                                   (
                                                                    _list (
                                                                      _list 0.03 0.26 0.21 0.18 0.32
                                                                    )
                                                                     (
                                                                      _list 0.28 0.1 0.33 0.15 0.14
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
                                                                      perplexity_loss y_true_perp y_pred_perp 1e-07
                                                                    )
                                                                  )
                                                                   (
                                                                    perplexity_loss y_true_perp y_pred_perp 1e-07
                                                                  )
                                                                   (
                                                                    to-str (
                                                                      perplexity_loss y_true_perp y_pred_perp 1e-07
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                newline
                                                              )
                                                               (
                                                                let (
                                                                  (
                                                                    y_true_smooth (
                                                                      _list 3.0 5.0 2.0 7.0
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        y_pred_smooth (
                                                                          _list 2.9 4.8 2.1 7.2
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        _display (
                                                                          if (
                                                                            string? (
                                                                              smooth_l1_loss y_true_smooth y_pred_smooth 1.0
                                                                            )
                                                                          )
                                                                           (
                                                                            smooth_l1_loss y_true_smooth y_pred_smooth 1.0
                                                                          )
                                                                           (
                                                                            to-str (
                                                                              smooth_l1_loss y_true_smooth y_pred_smooth 1.0
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        newline
                                                                      )
                                                                       (
                                                                        let (
                                                                          (
                                                                            y_true_kl (
                                                                              _list 0.2 0.3 0.5
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                y_pred_kl (
                                                                                  _list 0.3 0.3 0.4
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                _display (
                                                                                  if (
                                                                                    string? (
                                                                                      kullback_leibler_divergence y_true_kl y_pred_kl
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    kullback_leibler_divergence y_true_kl y_pred_kl
                                                                                  )
                                                                                   (
                                                                                    to-str (
                                                                                      kullback_leibler_divergence y_true_kl y_pred_kl
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
                        )
                      )
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
          end72 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur73 (
              quotient (
                * (
                  - end72 start71
                )
                 1000000
              )
               jps74
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur73
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
