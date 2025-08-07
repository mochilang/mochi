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
      start33 (
        current-jiffy
      )
    )
     (
      jps36 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        distance_sq a b
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
                call/cc (
                  lambda (
                    break3
                  )
                   (
                    letrec (
                      (
                        loop2 (
                          lambda (
                            i
                          )
                           (
                            if (
                              < i (
                                _len a
                              )
                            )
                             (
                              begin (
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
                                  )
                                )
                              )
                               (
                                loop2 (
                                  + i 1
                                )
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
                      loop2 0
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
     (
      define (
        assign_clusters data centroids
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            let (
              (
                assignments (
                  _list
                )
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
                            i
                          )
                           (
                            if (
                              < i (
                                _len data
                              )
                            )
                             (
                              begin (
                                begin (
                                  let (
                                    (
                                      best_idx 0
                                    )
                                  )
                                   (
                                    begin (
                                      let (
                                        (
                                          best (
                                            distance_sq (
                                              list-ref data i
                                            )
                                             (
                                              list-ref centroids 0
                                            )
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
                                                      j
                                                    )
                                                     (
                                                      if (
                                                        < j (
                                                          _len centroids
                                                        )
                                                      )
                                                       (
                                                        begin (
                                                          begin (
                                                            let (
                                                              (
                                                                dist (
                                                                  distance_sq (
                                                                    list-ref data i
                                                                  )
                                                                   (
                                                                    list-ref centroids j
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                if (
                                                                  _lt dist best
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! best dist
                                                                  )
                                                                   (
                                                                    set! best_idx j
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
                                                          loop7 (
                                                            + j 1
                                                          )
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
                                                loop7 1
                                              )
                                            )
                                          )
                                        )
                                         (
                                          set! assignments (
                                            append assignments (
                                              _list best_idx
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                               (
                                loop5 (
                                  + i 1
                                )
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
                      loop5 0
                    )
                  )
                )
              )
               (
                ret4 assignments
              )
            )
          )
        )
      )
    )
     (
      define (
        revise_centroids data k assignment
      )
       (
        call/cc (
          lambda (
            ret9
          )
           (
            let (
              (
                dim (
                  _len (
                    list-ref data 0
                  )
                )
              )
            )
             (
              begin (
                let (
                  (
                    sums (
                      _list
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        counts (
                          _list
                        )
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
                                    i
                                  )
                                   (
                                    if (
                                      < i k
                                    )
                                     (
                                      begin (
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
                                              call/cc (
                                                lambda (
                                                  break13
                                                )
                                                 (
                                                  letrec (
                                                    (
                                                      loop12 (
                                                        lambda (
                                                          j
                                                        )
                                                         (
                                                          if (
                                                            < j dim
                                                          )
                                                           (
                                                            begin (
                                                              begin (
                                                                set! row (
                                                                  append row (
                                                                    _list 0.0
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             (
                                                              loop12 (
                                                                + j 1
                                                              )
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
                                                    loop12 0
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              set! sums (
                                                append sums (
                                                  _list row
                                                )
                                              )
                                            )
                                             (
                                              set! counts (
                                                append counts (
                                                  _list 0
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                       (
                                        loop10 (
                                          + i 1
                                        )
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
                              loop10 0
                            )
                          )
                        )
                      )
                       (
                        call/cc (
                          lambda (
                            break15
                          )
                           (
                            letrec (
                              (
                                loop14 (
                                  lambda (
                                    i
                                  )
                                   (
                                    if (
                                      < i (
                                        _len data
                                      )
                                    )
                                     (
                                      begin (
                                        begin (
                                          let (
                                            (
                                              c (
                                                list-ref assignment i
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              list-set! counts c (
                                                + (
                                                  list-ref counts c
                                                )
                                                 1
                                              )
                                            )
                                             (
                                              call/cc (
                                                lambda (
                                                  break17
                                                )
                                                 (
                                                  letrec (
                                                    (
                                                      loop16 (
                                                        lambda (
                                                          j
                                                        )
                                                         (
                                                          if (
                                                            < j dim
                                                          )
                                                           (
                                                            begin (
                                                              begin (
                                                                list-set! (
                                                                  list-ref sums c
                                                                )
                                                                 j (
                                                                  + (
                                                                    cond (
                                                                      (
                                                                        string? (
                                                                          list-ref sums c
                                                                        )
                                                                      )
                                                                       (
                                                                        _substring (
                                                                          list-ref sums c
                                                                        )
                                                                         j (
                                                                          + j 1
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      (
                                                                        hash-table? (
                                                                          list-ref sums c
                                                                        )
                                                                      )
                                                                       (
                                                                        hash-table-ref (
                                                                          list-ref sums c
                                                                        )
                                                                         j
                                                                      )
                                                                    )
                                                                     (
                                                                      else (
                                                                        list-ref (
                                                                          list-ref sums c
                                                                        )
                                                                         j
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    cond (
                                                                      (
                                                                        string? (
                                                                          list-ref data i
                                                                        )
                                                                      )
                                                                       (
                                                                        _substring (
                                                                          list-ref data i
                                                                        )
                                                                         j (
                                                                          + j 1
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      (
                                                                        hash-table? (
                                                                          list-ref data i
                                                                        )
                                                                      )
                                                                       (
                                                                        hash-table-ref (
                                                                          list-ref data i
                                                                        )
                                                                         j
                                                                      )
                                                                    )
                                                                     (
                                                                      else (
                                                                        list-ref (
                                                                          list-ref data i
                                                                        )
                                                                         j
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             (
                                                              loop16 (
                                                                + j 1
                                                              )
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
                                                    loop16 0
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                       (
                                        loop14 (
                                          + i 1
                                        )
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
                              loop14 0
                            )
                          )
                        )
                      )
                       (
                        let (
                          (
                            centroids (
                              _list
                            )
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
                                        i
                                      )
                                       (
                                        if (
                                          < i k
                                        )
                                         (
                                          begin (
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
                                                  if (
                                                    > (
                                                      list-ref counts i
                                                    )
                                                     0
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
                                                                  j
                                                                )
                                                                 (
                                                                  if (
                                                                    < j dim
                                                                  )
                                                                   (
                                                                    begin (
                                                                      begin (
                                                                        set! row (
                                                                          append row (
                                                                            _list (
                                                                              _div (
                                                                                cond (
                                                                                  (
                                                                                    string? (
                                                                                      list-ref sums i
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    _substring (
                                                                                      list-ref sums i
                                                                                    )
                                                                                     j (
                                                                                      + j 1
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  (
                                                                                    hash-table? (
                                                                                      list-ref sums i
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    hash-table-ref (
                                                                                      list-ref sums i
                                                                                    )
                                                                                     j
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  else (
                                                                                    list-ref (
                                                                                      list-ref sums i
                                                                                    )
                                                                                     j
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                + 0.0 (
                                                                                  list-ref counts i
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      loop20 (
                                                                        + j 1
                                                                      )
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
                                                            loop20 0
                                                          )
                                                        )
                                                      )
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
                                                                  j
                                                                )
                                                                 (
                                                                  if (
                                                                    < j dim
                                                                  )
                                                                   (
                                                                    begin (
                                                                      begin (
                                                                        set! row (
                                                                          append row (
                                                                            _list 0.0
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      loop22 (
                                                                        + j 1
                                                                      )
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
                                                            loop22 0
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  set! centroids (
                                                    append centroids (
                                                      _list row
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                           (
                                            loop18 (
                                              + i 1
                                            )
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
                                  loop18 0
                                )
                              )
                            )
                          )
                           (
                            ret9 centroids
                          )
                        )
                      )
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
        compute_heterogeneity data centroids assignment
      )
       (
        call/cc (
          lambda (
            ret24
          )
           (
            let (
              (
                total 0.0
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
                            i
                          )
                           (
                            if (
                              < i (
                                _len data
                              )
                            )
                             (
                              begin (
                                begin (
                                  let (
                                    (
                                      c (
                                        list-ref assignment i
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      set! total (
                                        _add total (
                                          distance_sq (
                                            list-ref data i
                                          )
                                           (
                                            list-ref centroids c
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                               (
                                loop25 (
                                  + i 1
                                )
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
                      loop25 0
                    )
                  )
                )
              )
               (
                ret24 total
              )
            )
          )
        )
      )
    )
     (
      define (
        lists_equal a b
      )
       (
        call/cc (
          lambda (
            ret27
          )
           (
            begin (
              if (
                not (
                  equal? (
                    _len a
                  )
                   (
                    _len b
                  )
                )
              )
               (
                begin (
                  ret27 #f
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              call/cc (
                lambda (
                  break29
                )
                 (
                  letrec (
                    (
                      loop28 (
                        lambda (
                          i
                        )
                         (
                          if (
                            < i (
                              _len a
                            )
                          )
                           (
                            begin (
                              begin (
                                if (
                                  not (
                                    equal? (
                                      list-ref a i
                                    )
                                     (
                                      list-ref b i
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    ret27 #f
                                  )
                                )
                                 (
                                  quote (
                                    
                                  )
                                )
                              )
                            )
                             (
                              loop28 (
                                + i 1
                              )
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
                    loop28 0
                  )
                )
              )
            )
             (
              ret27 #t
            )
          )
        )
      )
    )
     (
      define (
        kmeans data k initial_centroids max_iter
      )
       (
        call/cc (
          lambda (
            ret30
          )
           (
            let (
              (
                centroids initial_centroids
              )
            )
             (
              begin (
                let (
                  (
                    assignment (
                      _list
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        prev (
                          _list
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            heterogeneity (
                              _list
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                iter 0
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
                                              < iter max_iter
                                            )
                                             (
                                              begin (
                                                set! assignment (
                                                  assign_clusters data centroids
                                                )
                                              )
                                               (
                                                set! centroids (
                                                  revise_centroids data k assignment
                                                )
                                              )
                                               (
                                                let (
                                                  (
                                                    h (
                                                      compute_heterogeneity data centroids assignment
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    set! heterogeneity (
                                                      append heterogeneity (
                                                        _list h
                                                      )
                                                    )
                                                  )
                                                   (
                                                    if (
                                                      and (
                                                        > iter 0
                                                      )
                                                       (
                                                        lists_equal prev assignment
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        break32 (
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
                                                    set! prev assignment
                                                  )
                                                   (
                                                    set! iter (
                                                      + iter 1
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                loop31
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
                                      loop31
                                    )
                                  )
                                )
                              )
                               (
                                ret30 (
                                  alist->hash-table (
                                    _list (
                                      cons "centroids" centroids
                                    )
                                     (
                                      cons "assignments" assignment
                                    )
                                     (
                                      cons "heterogeneity" heterogeneity
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
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
          data (
            _list (
              _list 1.0 2.0
            )
             (
              _list 1.5 1.8
            )
             (
              _list 5.0 8.0
            )
             (
              _list 8.0 8.0
            )
             (
              _list 1.0 0.6
            )
             (
              _list 9.0 11.0
            )
          )
        )
      )
       (
        begin (
          let (
            (
              k 3
            )
          )
           (
            begin (
              let (
                (
                  initial_centroids (
                    _list (
                      list-ref data 0
                    )
                     (
                      list-ref data 2
                    )
                     (
                      list-ref data 5
                    )
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      result (
                        kmeans data k initial_centroids 10
                      )
                    )
                  )
                   (
                    begin (
                      _display (
                        if (
                          string? (
                            to-str-space (
                              hash-table-ref result "centroids"
                            )
                          )
                        )
                         (
                          to-str-space (
                            hash-table-ref result "centroids"
                          )
                        )
                         (
                          to-str (
                            to-str-space (
                              hash-table-ref result "centroids"
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
                              hash-table-ref result "assignments"
                            )
                          )
                        )
                         (
                          to-str-space (
                            hash-table-ref result "assignments"
                          )
                        )
                         (
                          to-str (
                            to-str-space (
                              hash-table-ref result "assignments"
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
                              hash-table-ref result "heterogeneity"
                            )
                          )
                        )
                         (
                          to-str-space (
                            hash-table-ref result "heterogeneity"
                          )
                        )
                         (
                          to-str (
                            to-str-space (
                              hash-table-ref result "heterogeneity"
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
     (
      let (
        (
          end34 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur35 (
              quotient (
                * (
                  - end34 start33
                )
                 1000000
              )
               jps36
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur35
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
