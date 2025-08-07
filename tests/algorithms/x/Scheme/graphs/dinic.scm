;; Generated on 2025-08-07 08:56 +0700
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
      start31 (
        current-jiffy
      )
    )
     (
      jps34 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      let (
        (
          INF 1000000000
        )
      )
       (
        begin (
          define (
            pow2 k
          )
           (
            call/cc (
              lambda (
                ret1
              )
               (
                let (
                  (
                    res 1
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
                                      < i k
                                    )
                                     (
                                      begin (
                                        set! res (
                                          * res 2
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
                        ret1 res
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
            min2 a b
          )
           (
            call/cc (
              lambda (
                ret4
              )
               (
                begin (
                  if (
                    < a b
                  )
                   (
                    begin (
                      ret4 a
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  ret4 b
                )
              )
            )
          )
        )
         (
          define (
            new_dinic n
          )
           (
            call/cc (
              lambda (
                ret5
              )
               (
                let (
                  (
                    lvl (
                      _list
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        ptr (
                          _list
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            q (
                              _list
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                adj (
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
                                                  < i n
                                                )
                                                 (
                                                  begin (
                                                    set! lvl (
                                                      append lvl (
                                                        _list 0
                                                      )
                                                    )
                                                  )
                                                   (
                                                    set! ptr (
                                                      append ptr (
                                                        _list 0
                                                      )
                                                    )
                                                  )
                                                   (
                                                    set! q (
                                                      append q (
                                                        _list 0
                                                      )
                                                    )
                                                  )
                                                   (
                                                    let (
                                                      (
                                                        edges (
                                                          _list
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        set! adj (
                                                          append adj (
                                                            _list edges
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
                                          cons "n" n
                                        )
                                         (
                                          cons "lvl" lvl
                                        )
                                         (
                                          cons "ptr" ptr
                                        )
                                         (
                                          cons "q" q
                                        )
                                         (
                                          cons "adj" adj
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
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
            add_edge g a b c rcap
          )
           (
            call/cc (
              lambda (
                ret8
              )
               (
                let (
                  (
                    adj (
                      hash-table-ref g "adj"
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        list_a (
                          list-ref adj a
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            list_b (
                              list-ref adj b
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                e1 (
                                  _list b (
                                    _len list_b
                                  )
                                   c 0
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    e2 (
                                      _list a (
                                        _len list_a
                                      )
                                       rcap 0
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    set! list_a (
                                      append list_a (
                                        _list e1
                                      )
                                    )
                                  )
                                   (
                                    set! list_b (
                                      append list_b (
                                        _list e2
                                      )
                                    )
                                  )
                                   (
                                    list-set! adj a list_a
                                  )
                                   (
                                    list-set! adj b list_b
                                  )
                                   (
                                    hash-table-set! g "adj" adj
                                  )
                                )
                              )
                            )
                          )
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
            dfs g v sink flow
          )
           (
            call/cc (
              lambda (
                ret9
              )
               (
                begin (
                  if (
                    or (
                      equal? v sink
                    )
                     (
                      equal? flow 0
                    )
                  )
                   (
                    begin (
                      ret9 flow
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
                      ptr (
                        hash-table-ref g "ptr"
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          i (
                            list-ref ptr v
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              adj_all (
                                hash-table-ref g "adj"
                              )
                            )
                          )
                           (
                            begin (
                              let (
                                (
                                  adj_v (
                                    list-ref adj_all v
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
                                              
                                            )
                                             (
                                              if (
                                                < i (
                                                  _len adj_v
                                                )
                                              )
                                               (
                                                begin (
                                                  let (
                                                    (
                                                      e (
                                                        list-ref adj_v i
                                                      )
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      let (
                                                        (
                                                          to (
                                                            list-ref e 0
                                                          )
                                                        )
                                                      )
                                                       (
                                                        begin (
                                                          if (
                                                            equal? (
                                                              list-ref (
                                                                hash-table-ref g "lvl"
                                                              )
                                                               to
                                                            )
                                                             (
                                                              + (
                                                                list-ref (
                                                                  hash-table-ref g "lvl"
                                                                )
                                                                 v
                                                              )
                                                               1
                                                            )
                                                          )
                                                           (
                                                            begin (
                                                              let (
                                                                (
                                                                  avail (
                                                                    - (
                                                                      list-ref e 2
                                                                    )
                                                                     (
                                                                      list-ref e 3
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                begin (
                                                                  let (
                                                                    (
                                                                      pushed (
                                                                        dfs g to sink (
                                                                          min2 flow avail
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    begin (
                                                                      if (
                                                                        _gt pushed 0
                                                                      )
                                                                       (
                                                                        begin (
                                                                          list-set! e 3 (
                                                                            _add (
                                                                              list-ref e 3
                                                                            )
                                                                             pushed
                                                                          )
                                                                        )
                                                                         (
                                                                          list-set! adj_v i e
                                                                        )
                                                                         (
                                                                          let (
                                                                            (
                                                                              adj_to (
                                                                                list-ref adj_all to
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            begin (
                                                                              let (
                                                                                (
                                                                                  back (
                                                                                    list-ref adj_to (
                                                                                      list-ref e 1
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                begin (
                                                                                  list-set! back 3 (
                                                                                    - (
                                                                                      list-ref back 3
                                                                                    )
                                                                                     pushed
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  list-set! adj_to (
                                                                                    list-ref e 1
                                                                                  )
                                                                                   back
                                                                                )
                                                                                 (
                                                                                  list-set! adj_all to adj_to
                                                                                )
                                                                                 (
                                                                                  list-set! adj_all v adj_v
                                                                                )
                                                                                 (
                                                                                  hash-table-set! g "adj" adj_all
                                                                                )
                                                                                 (
                                                                                  ret9 pushed
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
                                                         (
                                                          list-set! ptr v i
                                                        )
                                                      )
                                                    )
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
                                  hash-table-set! g "ptr" ptr
                                )
                                 (
                                  list-set! adj_all v adj_v
                                )
                                 (
                                  hash-table-set! g "adj" adj_all
                                )
                                 (
                                  ret9 0
                                )
                              )
                            )
                          )
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
            max_flow g source sink
          )
           (
            call/cc (
              lambda (
                ret12
              )
               (
                let (
                  (
                    flow 0
                  )
                )
                 (
                  begin (
                    let (
                      (
                        l 0
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
                                      < l 31
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            threshold (
                                              pow2 (
                                                - 30 l
                                              )
                                            )
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
                                                        if #t (
                                                          begin (
                                                            let (
                                                              (
                                                                lvl (
                                                                  _list
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                let (
                                                                  (
                                                                    ptr (
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
                                                                                      < i (
                                                                                        hash-table-ref g "n"
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        set! lvl (
                                                                                          append lvl (
                                                                                            _list 0
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        set! ptr (
                                                                                          append ptr (
                                                                                            _list 0
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
                                                                        hash-table-set! g "lvl" lvl
                                                                      )
                                                                       (
                                                                        hash-table-set! g "ptr" ptr
                                                                      )
                                                                       (
                                                                        let (
                                                                          (
                                                                            qi 0
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                qe 1
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                list-set! lvl source 1
                                                                              )
                                                                               (
                                                                                hash-table-set! g "lvl" lvl
                                                                              )
                                                                               (
                                                                                let (
                                                                                  (
                                                                                    q (
                                                                                      hash-table-ref g "q"
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    list-set! q 0 source
                                                                                  )
                                                                                   (
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
                                                                                                  and (
                                                                                                    < qi qe
                                                                                                  )
                                                                                                   (
                                                                                                    equal? (
                                                                                                      list-ref (
                                                                                                        hash-table-ref g "lvl"
                                                                                                      )
                                                                                                       sink
                                                                                                    )
                                                                                                     0
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  begin (
                                                                                                    let (
                                                                                                      (
                                                                                                        v (
                                                                                                          list-ref q qi
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      begin (
                                                                                                        set! qi (
                                                                                                          + qi 1
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        let (
                                                                                                          (
                                                                                                            edges (
                                                                                                              list-ref (
                                                                                                                hash-table-ref g "adj"
                                                                                                              )
                                                                                                               v
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
                                                                                                                                _len edges
                                                                                                                              )
                                                                                                                            )
                                                                                                                             (
                                                                                                                              begin (
                                                                                                                                let (
                                                                                                                                  (
                                                                                                                                    e (
                                                                                                                                      list-ref edges j
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                )
                                                                                                                                 (
                                                                                                                                  begin (
                                                                                                                                    let (
                                                                                                                                      (
                                                                                                                                        to (
                                                                                                                                          list-ref e 0
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      begin (
                                                                                                                                        let (
                                                                                                                                          (
                                                                                                                                            residual (
                                                                                                                                              - (
                                                                                                                                                list-ref e 2
                                                                                                                                              )
                                                                                                                                               (
                                                                                                                                                list-ref e 3
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                         (
                                                                                                                                          begin (
                                                                                                                                            let (
                                                                                                                                              (
                                                                                                                                                lvl_inner (
                                                                                                                                                  hash-table-ref g "lvl"
                                                                                                                                                )
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                             (
                                                                                                                                              begin (
                                                                                                                                                if (
                                                                                                                                                  and (
                                                                                                                                                    equal? (
                                                                                                                                                      list-ref lvl_inner to
                                                                                                                                                    )
                                                                                                                                                     0
                                                                                                                                                  )
                                                                                                                                                   (
                                                                                                                                                    _ge residual threshold
                                                                                                                                                  )
                                                                                                                                                )
                                                                                                                                                 (
                                                                                                                                                  begin (
                                                                                                                                                    list-set! q qe to
                                                                                                                                                  )
                                                                                                                                                   (
                                                                                                                                                    set! qe (
                                                                                                                                                      + qe 1
                                                                                                                                                    )
                                                                                                                                                  )
                                                                                                                                                   (
                                                                                                                                                    list-set! lvl_inner to (
                                                                                                                                                      + (
                                                                                                                                                        list-ref lvl_inner v
                                                                                                                                                      )
                                                                                                                                                       1
                                                                                                                                                    )
                                                                                                                                                  )
                                                                                                                                                   (
                                                                                                                                                    hash-table-set! g "lvl" lvl_inner
                                                                                                                                                  )
                                                                                                                                                )
                                                                                                                                                 (
                                                                                                                                                  quote (
                                                                                                                                                    
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
                                                                                    let (
                                                                                      (
                                                                                        p (
                                                                                          dfs g source sink INF
                                                                                        )
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
                                                                                                      _gt p 0
                                                                                                    )
                                                                                                     (
                                                                                                      begin (
                                                                                                        set! flow (
                                                                                                          _add flow p
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        set! p (
                                                                                                          dfs g source sink INF
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
                                                                                        if (
                                                                                          equal? (
                                                                                            list-ref (
                                                                                              hash-table-ref g "lvl"
                                                                                            )
                                                                                             sink
                                                                                          )
                                                                                           0
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            break16 (
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
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
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
                                            set! l (
                                              + l 1
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
                        ret12 flow
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
              graph (
                new_dinic 10
              )
            )
          )
           (
            begin (
              let (
                (
                  source 0
                )
              )
               (
                begin (
                  let (
                    (
                      sink 9
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          v 1
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
                                        < v 5
                                      )
                                       (
                                        begin (
                                          add_edge graph source v 1 0
                                        )
                                         (
                                          set! v (
                                            + v 1
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
                          set! v 5
                        )
                         (
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
                                        < v 9
                                      )
                                       (
                                        begin (
                                          add_edge graph v sink 1 0
                                        )
                                         (
                                          set! v (
                                            + v 1
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
                          set! v 1
                        )
                         (
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
                                        < v 5
                                      )
                                       (
                                        begin (
                                          add_edge graph v (
                                            + v 4
                                          )
                                           1 0
                                        )
                                         (
                                          set! v (
                                            + v 1
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
                          _display (
                            if (
                              string? (
                                to-str-space (
                                  max_flow graph source sink
                                )
                              )
                            )
                             (
                              to-str-space (
                                max_flow graph source sink
                              )
                            )
                             (
                              to-str (
                                to-str-space (
                                  max_flow graph source sink
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
     (
      let (
        (
          end32 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur33 (
              quotient (
                * (
                  - end32 start31
                )
                 1000000
              )
               jps34
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur33
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
