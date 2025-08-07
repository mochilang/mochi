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
      start23 (
        current-jiffy
      )
    )
     (
      jps26 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        expand_search graph queue head parents visited opposite_visited
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            begin (
              if (
                >= head (
                  _len queue
                )
              )
               (
                begin (
                  ret1 (
                    alist->hash-table (
                      _list (
                        cons "queue" queue
                      )
                       (
                        cons "head" head
                      )
                       (
                        cons "parents" parents
                      )
                       (
                        cons "visited" visited
                      )
                       (
                        cons "intersection" (
                          - 0 1
                        )
                      )
                       (
                        cons "found" #f
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
              let (
                (
                  current (
                    list-ref queue head
                  )
                )
              )
               (
                begin (
                  set! head (
                    + head 1
                  )
                )
                 (
                  let (
                    (
                      neighbors (
                        hash-table-ref/default graph current (
                          quote (
                            
                          )
                        )
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          q queue
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              p parents
                            )
                          )
                           (
                            begin (
                              let (
                                (
                                  v visited
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
                                                      _len neighbors
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      let (
                                                        (
                                                          neighbor (
                                                            list-ref neighbors i
                                                          )
                                                        )
                                                      )
                                                       (
                                                        begin (
                                                          if (
                                                            hash-table-ref/default v neighbor (
                                                              quote (
                                                                
                                                              )
                                                            )
                                                          )
                                                           (
                                                            begin (
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
                                                         (
                                                          hash-table-set! v neighbor #t
                                                        )
                                                         (
                                                          hash-table-set! p neighbor current
                                                        )
                                                         (
                                                          set! q (
                                                            append q (
                                                              _list neighbor
                                                            )
                                                          )
                                                        )
                                                         (
                                                          if (
                                                            hash-table-ref/default opposite_visited neighbor (
                                                              quote (
                                                                
                                                              )
                                                            )
                                                          )
                                                           (
                                                            begin (
                                                              ret1 (
                                                                alist->hash-table (
                                                                  _list (
                                                                    cons "queue" q
                                                                  )
                                                                   (
                                                                    cons "head" head
                                                                  )
                                                                   (
                                                                    cons "parents" p
                                                                  )
                                                                   (
                                                                    cons "visited" v
                                                                  )
                                                                   (
                                                                    cons "intersection" neighbor
                                                                  )
                                                                   (
                                                                    cons "found" #t
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
                                        alist->hash-table (
                                          _list (
                                            cons "queue" q
                                          )
                                           (
                                            cons "head" head
                                          )
                                           (
                                            cons "parents" p
                                          )
                                           (
                                            cons "visited" v
                                          )
                                           (
                                            cons "intersection" (
                                              - 0 1
                                            )
                                          )
                                           (
                                            cons "found" #f
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
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
        construct_path current parents
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            let (
              (
                path (
                  _list
                )
              )
            )
             (
              begin (
                let (
                  (
                    node current
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
                                  not (
                                    equal? node (
                                      - 0 1
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    set! path (
                                      append path (
                                        _list node
                                      )
                                    )
                                  )
                                   (
                                    set! node (
                                      hash-table-ref/default parents node (
                                        quote (
                                          
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
                    ret4 path
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
        reverse_list xs
      )
       (
        call/cc (
          lambda (
            ret7
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
                    i (
                      _len xs
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
                                  > i 0
                                )
                                 (
                                  begin (
                                    set! i (
                                      - i 1
                                    )
                                  )
                                   (
                                    set! res (
                                      append res (
                                        _list (
                                          list-ref xs i
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
                    ret7 res
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
        bidirectional_search g start goal
      )
       (
        call/cc (
          lambda (
            ret10
          )
           (
            begin (
              if (
                equal? start goal
              )
               (
                begin (
                  ret10 (
                    alist->hash-table (
                      _list (
                        cons "path" (
                          _list start
                        )
                      )
                       (
                        cons "ok" #t
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
              let (
                (
                  forward_parents (
                    alist->hash-table (
                      _list
                    )
                  )
                )
              )
               (
                begin (
                  hash-table-set! forward_parents start (
                    - 0 1
                  )
                )
                 (
                  let (
                    (
                      backward_parents (
                        alist->hash-table (
                          _list
                        )
                      )
                    )
                  )
                   (
                    begin (
                      hash-table-set! backward_parents goal (
                        - 0 1
                      )
                    )
                     (
                      let (
                        (
                          forward_visited (
                            alist->hash-table (
                              _list
                            )
                          )
                        )
                      )
                       (
                        begin (
                          hash-table-set! forward_visited start #t
                        )
                         (
                          let (
                            (
                              backward_visited (
                                alist->hash-table (
                                  _list
                                )
                              )
                            )
                          )
                           (
                            begin (
                              hash-table-set! backward_visited goal #t
                            )
                             (
                              let (
                                (
                                  forward_queue (
                                    _list start
                                  )
                                )
                              )
                               (
                                begin (
                                  let (
                                    (
                                      backward_queue (
                                        _list goal
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      let (
                                        (
                                          forward_head 0
                                        )
                                      )
                                       (
                                        begin (
                                          let (
                                            (
                                              backward_head 0
                                            )
                                          )
                                           (
                                            begin (
                                              let (
                                                (
                                                  intersection (
                                                    - 0 1
                                                  )
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
                                                                and (
                                                                  and (
                                                                    < forward_head (
                                                                      _len forward_queue
                                                                    )
                                                                  )
                                                                   (
                                                                    < backward_head (
                                                                      _len backward_queue
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  equal? intersection (
                                                                    - 0 1
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                begin (
                                                                  let (
                                                                    (
                                                                      res (
                                                                        expand_search g forward_queue forward_head forward_parents forward_visited backward_visited
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    begin (
                                                                      set! forward_queue (
                                                                        hash-table-ref res "queue"
                                                                      )
                                                                    )
                                                                     (
                                                                      set! forward_head (
                                                                        hash-table-ref res "head"
                                                                      )
                                                                    )
                                                                     (
                                                                      set! forward_parents (
                                                                        hash-table-ref res "parents"
                                                                      )
                                                                    )
                                                                     (
                                                                      set! forward_visited (
                                                                        hash-table-ref res "visited"
                                                                      )
                                                                    )
                                                                     (
                                                                      if (
                                                                        hash-table-ref res "found"
                                                                      )
                                                                       (
                                                                        begin (
                                                                          set! intersection (
                                                                            hash-table-ref res "intersection"
                                                                          )
                                                                        )
                                                                         (
                                                                          break12 (
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
                                                                      set! res (
                                                                        expand_search g backward_queue backward_head backward_parents backward_visited forward_visited
                                                                      )
                                                                    )
                                                                     (
                                                                      set! backward_queue (
                                                                        hash-table-ref res "queue"
                                                                      )
                                                                    )
                                                                     (
                                                                      set! backward_head (
                                                                        hash-table-ref res "head"
                                                                      )
                                                                    )
                                                                     (
                                                                      set! backward_parents (
                                                                        hash-table-ref res "parents"
                                                                      )
                                                                    )
                                                                     (
                                                                      set! backward_visited (
                                                                        hash-table-ref res "visited"
                                                                      )
                                                                    )
                                                                     (
                                                                      if (
                                                                        hash-table-ref res "found"
                                                                      )
                                                                       (
                                                                        begin (
                                                                          set! intersection (
                                                                            hash-table-ref res "intersection"
                                                                          )
                                                                        )
                                                                         (
                                                                          break12 (
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
                                                  if (
                                                    equal? intersection (
                                                      - 0 1
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      ret10 (
                                                        alist->hash-table (
                                                          _list (
                                                            cons "path" (
                                                              _list
                                                            )
                                                          )
                                                           (
                                                            cons "ok" #f
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
                                                  let (
                                                    (
                                                      forward_path (
                                                        construct_path intersection forward_parents
                                                      )
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      set! forward_path (
                                                        reverse_list forward_path
                                                      )
                                                    )
                                                     (
                                                      let (
                                                        (
                                                          back_start (
                                                            hash-table-ref/default backward_parents intersection (
                                                              quote (
                                                                
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        begin (
                                                          let (
                                                            (
                                                              backward_path (
                                                                construct_path back_start backward_parents
                                                              )
                                                            )
                                                          )
                                                           (
                                                            begin (
                                                              let (
                                                                (
                                                                  result forward_path
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
                                                                                      _len backward_path
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    begin (
                                                                                      set! result (
                                                                                        append result (
                                                                                          _list (
                                                                                            cond (
                                                                                              (
                                                                                                string? backward_path
                                                                                              )
                                                                                               (
                                                                                                _substring backward_path j (
                                                                                                  + j 1
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              (
                                                                                                hash-table? backward_path
                                                                                              )
                                                                                               (
                                                                                                hash-table-ref backward_path j
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              else (
                                                                                                list-ref backward_path j
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
                                                                      ret10 (
                                                                        alist->hash-table (
                                                                          _list (
                                                                            cons "path" result
                                                                          )
                                                                           (
                                                                            cons "ok" #t
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
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
        is_edge g u v
      )
       (
        call/cc (
          lambda (
            ret15
          )
           (
            let (
              (
                neighbors (
                  hash-table-ref/default g u (
                    quote (
                      
                    )
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
                                    _len neighbors
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      equal? (
                                        list-ref neighbors i
                                      )
                                       v
                                    )
                                     (
                                      begin (
                                        ret15 #t
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
                    ret15 #f
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
        path_exists g path
      )
       (
        call/cc (
          lambda (
            ret18
          )
           (
            begin (
              if (
                equal? (
                  _len path
                )
                 0
              )
               (
                begin (
                  ret18 #f
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
                                < (
                                  + i 1
                                )
                                 (
                                  _len path
                                )
                              )
                               (
                                begin (
                                  if (
                                    not (
                                      is_edge g (
                                        list-ref path i
                                      )
                                       (
                                        list-ref path (
                                          + i 1
                                        )
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      ret18 #f
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
                  ret18 #t
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        print_path g s t
      )
       (
        call/cc (
          lambda (
            ret21
          )
           (
            let (
              (
                res (
                  bidirectional_search g s t
                )
              )
            )
             (
              begin (
                if (
                  and (
                    hash-table-ref res "ok"
                  )
                   (
                    path_exists g (
                      hash-table-ref res "path"
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
                              string-append (
                                string-append (
                                  string-append "Path from " (
                                    to-str-space s
                                  )
                                )
                                 " to "
                              )
                               (
                                to-str-space t
                              )
                            )
                             ": "
                          )
                           (
                            to-str-space (
                              hash-table-ref res "path"
                            )
                          )
                        )
                      )
                       (
                        string-append (
                          string-append (
                            string-append (
                              string-append (
                                string-append "Path from " (
                                  to-str-space s
                                )
                              )
                               " to "
                            )
                             (
                              to-str-space t
                            )
                          )
                           ": "
                        )
                         (
                          to-str-space (
                            hash-table-ref res "path"
                          )
                        )
                      )
                       (
                        to-str (
                          string-append (
                            string-append (
                              string-append (
                                string-append (
                                  string-append "Path from " (
                                    to-str-space s
                                  )
                                )
                                 " to "
                              )
                               (
                                to-str-space t
                              )
                            )
                             ": "
                          )
                           (
                            to-str-space (
                              hash-table-ref res "path"
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
                 (
                  begin (
                    _display (
                      if (
                        string? (
                          string-append (
                            string-append (
                              string-append (
                                string-append "Path from " (
                                  to-str-space s
                                )
                              )
                               " to "
                            )
                             (
                              to-str-space t
                            )
                          )
                           ": None"
                        )
                      )
                       (
                        string-append (
                          string-append (
                            string-append (
                              string-append "Path from " (
                                to-str-space s
                              )
                            )
                             " to "
                          )
                           (
                            to-str-space t
                          )
                        )
                         ": None"
                      )
                       (
                        to-str (
                          string-append (
                            string-append (
                              string-append (
                                string-append "Path from " (
                                  to-str-space s
                                )
                              )
                               " to "
                            )
                             (
                              to-str-space t
                            )
                          )
                           ": None"
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
                graph (
                  alist->hash-table (
                    _list (
                      cons 0 (
                        _list 1 2
                      )
                    )
                     (
                      cons 1 (
                        _list 0 3 4
                      )
                    )
                     (
                      cons 2 (
                        _list 0 5 6
                      )
                    )
                     (
                      cons 3 (
                        _list 1 7
                      )
                    )
                     (
                      cons 4 (
                        _list 1 8
                      )
                    )
                     (
                      cons 5 (
                        _list 2 9
                      )
                    )
                     (
                      cons 6 (
                        _list 2 10
                      )
                    )
                     (
                      cons 7 (
                        _list 3 11
                      )
                    )
                     (
                      cons 8 (
                        _list 4 11
                      )
                    )
                     (
                      cons 9 (
                        _list 5 11
                      )
                    )
                     (
                      cons 10 (
                        _list 6 11
                      )
                    )
                     (
                      cons 11 (
                        _list 7 8 9 10
                      )
                    )
                  )
                )
              )
            )
             (
              begin (
                print_path graph 0 11
              )
               (
                print_path graph 5 5
              )
               (
                let (
                  (
                    disconnected (
                      alist->hash-table (
                        _list (
                          cons 0 (
                            _list 1 2
                          )
                        )
                         (
                          cons 1 (
                            _list 0
                          )
                        )
                         (
                          cons 2 (
                            _list 0
                          )
                        )
                         (
                          cons 3 (
                            _list 4
                          )
                        )
                         (
                          cons 4 (
                            _list 3
                          )
                        )
                      )
                    )
                  )
                )
                 (
                  begin (
                    print_path disconnected 0 3
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
          end24 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur25 (
              quotient (
                * (
                  - end24 start23
                )
                 1000000
              )
               jps26
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur25
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
