;; Generated on 2025-08-07 08:56 +0700
(import (scheme base))
(import (scheme time))
(import (chibi string))
(import (only (scheme char) string-upcase string-downcase))
(import (srfi 69))
(import (srfi 1))
(define _list list)
(import (chibi time) (srfi 98))
(define _now_seeded #f)
(define _now_seed 0)
(define (now)
  (when (not _now_seeded)
    (let ((s (get-environment-variable "MOCHI_NOW_SEED")))
      (when (and s (string->number s))
        (set! _now_seed (string->number s))
        (set! _now_seeded #t))))
  (if _now_seeded
      (begin
        (set! _now_seed (modulo (+ (* _now_seed 1664525) 1013904223) 2147483647))
        _now_seed)
      (exact (floor (* (current-second) 1000000000))))
)
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
      start101 (
        current-jiffy
      )
    )
     (
      jps104 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        list_contains_int xs x
      )
       (
        call/cc (
          lambda (
            ret1
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
                                if (
                                  equal? (
                                    list-ref xs i
                                  )
                                   x
                                )
                                 (
                                  begin (
                                    ret1 #t
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
                ret1 #f
              )
            )
          )
        )
      )
    )
     (
      define (
        edge_exists edges w v
      )
       (
        call/cc (
          lambda (
            ret4
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
                              < i (
                                _len edges
                              )
                            )
                             (
                              begin (
                                if (
                                  and (
                                    equal? (
                                      cond (
                                        (
                                          string? (
                                            list-ref edges i
                                          )
                                        )
                                         (
                                          _substring (
                                            list-ref edges i
                                          )
                                           0 (
                                            + 0 1
                                          )
                                        )
                                      )
                                       (
                                        (
                                          hash-table? (
                                            list-ref edges i
                                          )
                                        )
                                         (
                                          hash-table-ref (
                                            list-ref edges i
                                          )
                                           0
                                        )
                                      )
                                       (
                                        else (
                                          list-ref (
                                            list-ref edges i
                                          )
                                           0
                                        )
                                      )
                                    )
                                     w
                                  )
                                   (
                                    equal? (
                                      cond (
                                        (
                                          string? (
                                            list-ref edges i
                                          )
                                        )
                                         (
                                          _substring (
                                            list-ref edges i
                                          )
                                           1 (
                                            + 1 1
                                          )
                                        )
                                      )
                                       (
                                        (
                                          hash-table? (
                                            list-ref edges i
                                          )
                                        )
                                         (
                                          hash-table-ref (
                                            list-ref edges i
                                          )
                                           1
                                        )
                                      )
                                       (
                                        else (
                                          list-ref (
                                            list-ref edges i
                                          )
                                           1
                                        )
                                      )
                                    )
                                     v
                                  )
                                )
                                 (
                                  begin (
                                    ret4 #t
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
                ret4 #f
              )
            )
          )
        )
      )
    )
     (
      define (
        first_key m
      )
       (
        call/cc (
          lambda (
            ret7
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
                          xs
                        )
                         (
                          if (
                            null? xs
                          )
                           (
                            quote (
                              
                            )
                          )
                           (
                            begin (
                              let (
                                (
                                  k (
                                    car xs
                                  )
                                )
                              )
                               (
                                begin (
                                  ret7 k
                                )
                              )
                            )
                             (
                              loop8 (
                                cdr xs
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                   (
                    loop8 (
                      hash-table-keys m
                    )
                  )
                )
              )
            )
             (
              ret7 0
            )
          )
        )
      )
    )
     (
      define (
        rand_range low high
      )
       (
        call/cc (
          lambda (
            ret10
          )
           (
            ret10 (
              + (
                _mod (
                  now
                )
                 (
                  - high low
                )
              )
               low
            )
          )
        )
      )
    )
     (
      define (
        dg_make_graph
      )
       (
        call/cc (
          lambda (
            ret11
          )
           (
            ret11 (
              alist->hash-table (
                _list (
                  cons "graph" (
                    alist->hash-table (
                      _list
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
        dg_add_pair g u v w
      )
       (
        call/cc (
          lambda (
            ret12
          )
           (
            begin (
              if (
                cond (
                  (
                    string? (
                      hash-table-ref g "graph"
                    )
                  )
                   (
                    if (
                      string-contains (
                        hash-table-ref g "graph"
                      )
                       u
                    )
                     #t #f
                  )
                )
                 (
                  (
                    hash-table? (
                      hash-table-ref g "graph"
                    )
                  )
                   (
                    if (
                      hash-table-exists? (
                        hash-table-ref g "graph"
                      )
                       u
                    )
                     #t #f
                  )
                )
                 (
                  else (
                    if (
                      member u (
                        hash-table-ref g "graph"
                      )
                    )
                     #t #f
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      edges (
                        hash-table-ref/default (
                          hash-table-ref g "graph"
                        )
                         u (
                          quote (
                            
                          )
                        )
                      )
                    )
                  )
                   (
                    begin (
                      if (
                        not (
                          edge_exists edges w v
                        )
                      )
                       (
                        begin (
                          set! edges (
                            append edges (
                              _list (
                                _list w v
                              )
                            )
                          )
                        )
                         (
                          let (
                            (
                              m (
                                hash-table-ref g "graph"
                              )
                            )
                          )
                           (
                            begin (
                              hash-table-set! m u edges
                            )
                             (
                              hash-table-set! g "graph" m
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
               (
                begin (
                  let (
                    (
                      m0 (
                        hash-table-ref g "graph"
                      )
                    )
                  )
                   (
                    begin (
                      hash-table-set! m0 u (
                        _list (
                          _list w v
                        )
                      )
                    )
                     (
                      hash-table-set! g "graph" m0
                    )
                  )
                )
              )
            )
             (
              if (
                not (
                  cond (
                    (
                      string? (
                        hash-table-ref g "graph"
                      )
                    )
                     (
                      if (
                        string-contains (
                          hash-table-ref g "graph"
                        )
                         v
                      )
                       #t #f
                    )
                  )
                   (
                    (
                      hash-table? (
                        hash-table-ref g "graph"
                      )
                    )
                     (
                      if (
                        hash-table-exists? (
                          hash-table-ref g "graph"
                        )
                         v
                      )
                       #t #f
                    )
                  )
                   (
                    else (
                      if (
                        member v (
                          hash-table-ref g "graph"
                        )
                      )
                       #t #f
                    )
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      m1 (
                        hash-table-ref g "graph"
                      )
                    )
                  )
                   (
                    begin (
                      hash-table-set! m1 v (
                        _list
                      )
                    )
                     (
                      hash-table-set! g "graph" m1
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
     (
      define (
        dg_remove_pair g u v
      )
       (
        call/cc (
          lambda (
            ret13
          )
           (
            if (
              cond (
                (
                  string? (
                    hash-table-ref g "graph"
                  )
                )
                 (
                  if (
                    string-contains (
                      hash-table-ref g "graph"
                    )
                     u
                  )
                   #t #f
                )
              )
               (
                (
                  hash-table? (
                    hash-table-ref g "graph"
                  )
                )
                 (
                  if (
                    hash-table-exists? (
                      hash-table-ref g "graph"
                    )
                     u
                  )
                   #t #f
                )
              )
               (
                else (
                  if (
                    member u (
                      hash-table-ref g "graph"
                    )
                  )
                   #t #f
                )
              )
            )
             (
              begin (
                let (
                  (
                    edges (
                      hash-table-ref/default (
                        hash-table-ref g "graph"
                      )
                       u (
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
                        new_edges (
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
                                            _len edges
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              not (
                                                equal? (
                                                  cond (
                                                    (
                                                      string? (
                                                        list-ref edges i
                                                      )
                                                    )
                                                     (
                                                      _substring (
                                                        list-ref edges i
                                                      )
                                                       1 (
                                                        + 1 1
                                                      )
                                                    )
                                                  )
                                                   (
                                                    (
                                                      hash-table? (
                                                        list-ref edges i
                                                      )
                                                    )
                                                     (
                                                      hash-table-ref (
                                                        list-ref edges i
                                                      )
                                                       1
                                                    )
                                                  )
                                                   (
                                                    else (
                                                      list-ref (
                                                        list-ref edges i
                                                      )
                                                       1
                                                    )
                                                  )
                                                )
                                                 v
                                              )
                                            )
                                             (
                                              begin (
                                                set! new_edges (
                                                  append new_edges (
                                                    _list (
                                                      list-ref edges i
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
                            let (
                              (
                                m (
                                  hash-table-ref g "graph"
                                )
                              )
                            )
                             (
                              begin (
                                hash-table-set! m u new_edges
                              )
                               (
                                hash-table-set! g "graph" m
                              )
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
        )
      )
    )
     (
      define (
        dg_all_nodes g
      )
       (
        call/cc (
          lambda (
            ret16
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
                call/cc (
                  lambda (
                    break18
                  )
                   (
                    letrec (
                      (
                        loop17 (
                          lambda (
                            xs
                          )
                           (
                            if (
                              null? xs
                            )
                             (
                              quote (
                                
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    k (
                                      car xs
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    set! res (
                                      append res (
                                        _list k
                                      )
                                    )
                                  )
                                )
                              )
                               (
                                loop17 (
                                  cdr xs
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                     (
                      loop17 (
                        hash-table-keys (
                          hash-table-ref g "graph"
                        )
                      )
                    )
                  )
                )
              )
               (
                ret16 res
              )
            )
          )
        )
      )
    )
     (
      define (
        dg_dfs_util g node visited order d
      )
       (
        call/cc (
          lambda (
            ret19
          )
           (
            begin (
              hash-table-set! visited node #t
            )
             (
              set! order (
                append order (
                  _list node
                )
              )
            )
             (
              if (
                and (
                  not (
                    equal? d (
                      - 1
                    )
                  )
                )
                 (
                  equal? node d
                )
              )
               (
                begin (
                  ret19 order
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
                  edges (
                    hash-table-ref/default (
                      hash-table-ref g "graph"
                    )
                     node (
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
                                      _len edges
                                    )
                                  )
                                   (
                                    begin (
                                      let (
                                        (
                                          neigh (
                                            cond (
                                              (
                                                string? (
                                                  list-ref edges i
                                                )
                                              )
                                               (
                                                _substring (
                                                  list-ref edges i
                                                )
                                                 1 (
                                                  + 1 1
                                                )
                                              )
                                            )
                                             (
                                              (
                                                hash-table? (
                                                  list-ref edges i
                                                )
                                              )
                                               (
                                                hash-table-ref (
                                                  list-ref edges i
                                                )
                                                 1
                                              )
                                            )
                                             (
                                              else (
                                                list-ref (
                                                  list-ref edges i
                                                )
                                                 1
                                              )
                                            )
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          if (
                                            not (
                                              cond (
                                                (
                                                  string? visited
                                                )
                                                 (
                                                  if (
                                                    string-contains visited neigh
                                                  )
                                                   #t #f
                                                )
                                              )
                                               (
                                                (
                                                  hash-table? visited
                                                )
                                                 (
                                                  if (
                                                    hash-table-exists? visited neigh
                                                  )
                                                   #t #f
                                                )
                                              )
                                               (
                                                else (
                                                  if (
                                                    member neigh visited
                                                  )
                                                   #t #f
                                                )
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              set! order (
                                                dg_dfs_util g neigh visited order d
                                              )
                                            )
                                             (
                                              if (
                                                and (
                                                  not (
                                                    equal? d (
                                                      - 1
                                                    )
                                                  )
                                                )
                                                 (
                                                  equal? (
                                                    list-ref order (
                                                      - (
                                                        _len order
                                                      )
                                                       1
                                                    )
                                                  )
                                                   d
                                                )
                                              )
                                               (
                                                begin (
                                                  ret19 order
                                                )
                                              )
                                               (
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
                      ret19 order
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
        dg_dfs g s d
      )
       (
        call/cc (
          lambda (
            ret22
          )
           (
            begin (
              if (
                equal? s d
              )
               (
                begin (
                  ret22 (
                    _list
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
                  start (
                    if (
                      equal? s (
                        - 2
                      )
                    )
                     (
                      first_key (
                        hash-table-ref g "graph"
                      )
                    )
                     s
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      visited (
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
                          order (
                            _list
                          )
                        )
                      )
                       (
                        begin (
                          set! order (
                            dg_dfs_util g start visited order d
                          )
                        )
                         (
                          ret22 order
                        )
                      )
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
        dg_bfs g s
      )
       (
        call/cc (
          lambda (
            ret23
          )
           (
            let (
              (
                queue (
                  _list
                )
              )
            )
             (
              begin (
                let (
                  (
                    visited (
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
                        order (
                          _list
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            start (
                              if (
                                equal? s (
                                  - 2
                                )
                              )
                               (
                                first_key (
                                  hash-table-ref g "graph"
                                )
                              )
                               s
                            )
                          )
                        )
                         (
                          begin (
                            set! queue (
                              append queue (
                                _list start
                              )
                            )
                          )
                           (
                            hash-table-set! visited start #t
                          )
                           (
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
                                          > (
                                            _len queue
                                          )
                                           0
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                node (
                                                  list-ref queue 0
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                set! queue (
                                                  take (
                                                    drop queue 1
                                                  )
                                                   (
                                                    - (
                                                      _len queue
                                                    )
                                                     1
                                                  )
                                                )
                                              )
                                               (
                                                set! order (
                                                  append order (
                                                    _list node
                                                  )
                                                )
                                              )
                                               (
                                                let (
                                                  (
                                                    edges (
                                                      hash-table-ref/default (
                                                        hash-table-ref g "graph"
                                                      )
                                                       node (
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
                                                                      < i (
                                                                        _len edges
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            neigh (
                                                                              cond (
                                                                                (
                                                                                  string? (
                                                                                    list-ref edges i
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  _substring (
                                                                                    list-ref edges i
                                                                                  )
                                                                                   1 (
                                                                                    + 1 1
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                (
                                                                                  hash-table? (
                                                                                    list-ref edges i
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  hash-table-ref (
                                                                                    list-ref edges i
                                                                                  )
                                                                                   1
                                                                                )
                                                                              )
                                                                               (
                                                                                else (
                                                                                  list-ref (
                                                                                    list-ref edges i
                                                                                  )
                                                                                   1
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            if (
                                                                              not (
                                                                                cond (
                                                                                  (
                                                                                    string? visited
                                                                                  )
                                                                                   (
                                                                                    if (
                                                                                      string-contains visited neigh
                                                                                    )
                                                                                     #t #f
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  (
                                                                                    hash-table? visited
                                                                                  )
                                                                                   (
                                                                                    if (
                                                                                      hash-table-exists? visited neigh
                                                                                    )
                                                                                     #t #f
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  else (
                                                                                    if (
                                                                                      member neigh visited
                                                                                    )
                                                                                     #t #f
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                set! queue (
                                                                                  append queue (
                                                                                    _list neigh
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                hash-table-set! visited neigh #t
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
                            ret23 order
                          )
                        )
                      )
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
        dg_in_degree g u
      )
       (
        call/cc (
          lambda (
            ret28
          )
           (
            let (
              (
                count 0
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
                            xs
                          )
                           (
                            if (
                              null? xs
                            )
                             (
                              quote (
                                
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    k (
                                      car xs
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        edges (
                                          hash-table-ref/default (
                                            hash-table-ref g "graph"
                                          )
                                           k (
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
                                                          < i (
                                                            _len edges
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            if (
                                                              equal? (
                                                                cond (
                                                                  (
                                                                    string? (
                                                                      list-ref edges i
                                                                    )
                                                                  )
                                                                   (
                                                                    _substring (
                                                                      list-ref edges i
                                                                    )
                                                                     1 (
                                                                      + 1 1
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  (
                                                                    hash-table? (
                                                                      list-ref edges i
                                                                    )
                                                                  )
                                                                   (
                                                                    hash-table-ref (
                                                                      list-ref edges i
                                                                    )
                                                                     1
                                                                  )
                                                                )
                                                                 (
                                                                  else (
                                                                    list-ref (
                                                                      list-ref edges i
                                                                    )
                                                                     1
                                                                  )
                                                                )
                                                              )
                                                               u
                                                            )
                                                             (
                                                              begin (
                                                                set! count (
                                                                  + count 1
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
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                               (
                                loop29 (
                                  cdr xs
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                     (
                      loop29 (
                        hash-table-keys (
                          hash-table-ref g "graph"
                        )
                      )
                    )
                  )
                )
              )
               (
                ret28 count
              )
            )
          )
        )
      )
    )
     (
      define (
        dg_out_degree g u
      )
       (
        call/cc (
          lambda (
            ret33
          )
           (
            begin (
              if (
                cond (
                  (
                    string? (
                      hash-table-ref g "graph"
                    )
                  )
                   (
                    if (
                      string-contains (
                        hash-table-ref g "graph"
                      )
                       u
                    )
                     #t #f
                  )
                )
                 (
                  (
                    hash-table? (
                      hash-table-ref g "graph"
                    )
                  )
                   (
                    if (
                      hash-table-exists? (
                        hash-table-ref g "graph"
                      )
                       u
                    )
                     #t #f
                  )
                )
                 (
                  else (
                    if (
                      member u (
                        hash-table-ref g "graph"
                      )
                    )
                     #t #f
                  )
                )
              )
               (
                begin (
                  ret33 (
                    _len (
                      hash-table-ref/default (
                        hash-table-ref g "graph"
                      )
                       u (
                        quote (
                          
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
              ret33 0
            )
          )
        )
      )
    )
     (
      define (
        dg_topo_util g node visited stack
      )
       (
        call/cc (
          lambda (
            ret34
          )
           (
            begin (
              hash-table-set! visited node #t
            )
             (
              let (
                (
                  edges (
                    hash-table-ref/default (
                      hash-table-ref g "graph"
                    )
                     node (
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
                                    < i (
                                      _len edges
                                    )
                                  )
                                   (
                                    begin (
                                      let (
                                        (
                                          neigh (
                                            cond (
                                              (
                                                string? (
                                                  list-ref edges i
                                                )
                                              )
                                               (
                                                _substring (
                                                  list-ref edges i
                                                )
                                                 1 (
                                                  + 1 1
                                                )
                                              )
                                            )
                                             (
                                              (
                                                hash-table? (
                                                  list-ref edges i
                                                )
                                              )
                                               (
                                                hash-table-ref (
                                                  list-ref edges i
                                                )
                                                 1
                                              )
                                            )
                                             (
                                              else (
                                                list-ref (
                                                  list-ref edges i
                                                )
                                                 1
                                              )
                                            )
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          if (
                                            not (
                                              cond (
                                                (
                                                  string? visited
                                                )
                                                 (
                                                  if (
                                                    string-contains visited neigh
                                                  )
                                                   #t #f
                                                )
                                              )
                                               (
                                                (
                                                  hash-table? visited
                                                )
                                                 (
                                                  if (
                                                    hash-table-exists? visited neigh
                                                  )
                                                   #t #f
                                                )
                                              )
                                               (
                                                else (
                                                  if (
                                                    member neigh visited
                                                  )
                                                   #t #f
                                                )
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              set! stack (
                                                dg_topo_util g neigh visited stack
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
                      set! stack (
                        append stack (
                          _list node
                        )
                      )
                    )
                     (
                      ret34 stack
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
        dg_topological_sort g
      )
       (
        call/cc (
          lambda (
            ret37
          )
           (
            let (
              (
                visited (
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
                    stack (
                      _list
                    )
                  )
                )
                 (
                  begin (
                    call/cc (
                      lambda (
                        break39
                      )
                       (
                        letrec (
                          (
                            loop38 (
                              lambda (
                                xs
                              )
                               (
                                if (
                                  null? xs
                                )
                                 (
                                  quote (
                                    
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        k (
                                          car xs
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        if (
                                          not (
                                            cond (
                                              (
                                                string? visited
                                              )
                                               (
                                                if (
                                                  string-contains visited k
                                                )
                                                 #t #f
                                              )
                                            )
                                             (
                                              (
                                                hash-table? visited
                                              )
                                               (
                                                if (
                                                  hash-table-exists? visited k
                                                )
                                                 #t #f
                                              )
                                            )
                                             (
                                              else (
                                                if (
                                                  member k visited
                                                )
                                                 #t #f
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            set! stack (
                                              dg_topo_util g k visited stack
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
                                    loop38 (
                                      cdr xs
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                         (
                          loop38 (
                            hash-table-keys (
                              hash-table-ref g "graph"
                            )
                          )
                        )
                      )
                    )
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
                              - (
                                _len stack
                              )
                               1
                            )
                          )
                        )
                         (
                          begin (
                            call/cc (
                              lambda (
                                break41
                              )
                               (
                                letrec (
                                  (
                                    loop40 (
                                      lambda (
                                        
                                      )
                                       (
                                        if (
                                          >= i 0
                                        )
                                         (
                                          begin (
                                            set! res (
                                              append res (
                                                _list (
                                                  list-ref stack i
                                                )
                                              )
                                            )
                                          )
                                           (
                                            set! i (
                                              - i 1
                                            )
                                          )
                                           (
                                            loop40
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
                                  loop40
                                )
                              )
                            )
                          )
                           (
                            ret37 res
                          )
                        )
                      )
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
        dg_cycle_util g node visited rec res
      )
       (
        call/cc (
          lambda (
            ret42
          )
           (
            begin (
              hash-table-set! visited node #t
            )
             (
              hash-table-set! rec node #t
            )
             (
              let (
                (
                  edges (
                    hash-table-ref/default (
                      hash-table-ref g "graph"
                    )
                     node (
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
                                    < i (
                                      _len edges
                                    )
                                  )
                                   (
                                    begin (
                                      let (
                                        (
                                          neigh (
                                            cond (
                                              (
                                                string? (
                                                  list-ref edges i
                                                )
                                              )
                                               (
                                                _substring (
                                                  list-ref edges i
                                                )
                                                 1 (
                                                  + 1 1
                                                )
                                              )
                                            )
                                             (
                                              (
                                                hash-table? (
                                                  list-ref edges i
                                                )
                                              )
                                               (
                                                hash-table-ref (
                                                  list-ref edges i
                                                )
                                                 1
                                              )
                                            )
                                             (
                                              else (
                                                list-ref (
                                                  list-ref edges i
                                                )
                                                 1
                                              )
                                            )
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          if (
                                            not (
                                              cond (
                                                (
                                                  string? visited
                                                )
                                                 (
                                                  if (
                                                    string-contains visited neigh
                                                  )
                                                   #t #f
                                                )
                                              )
                                               (
                                                (
                                                  hash-table? visited
                                                )
                                                 (
                                                  if (
                                                    hash-table-exists? visited neigh
                                                  )
                                                   #t #f
                                                )
                                              )
                                               (
                                                else (
                                                  if (
                                                    member neigh visited
                                                  )
                                                   #t #f
                                                )
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              set! res (
                                                dg_cycle_util g neigh visited rec res
                                              )
                                            )
                                          )
                                           (
                                            if (
                                              hash-table-ref/default rec neigh (
                                                quote (
                                                  
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                if (
                                                  not (
                                                    list_contains_int res neigh
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    set! res (
                                                      append res (
                                                        _list neigh
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
                                                    list_contains_int res node
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    set! res (
                                                      append res (
                                                        _list node
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
                                             (
                                              quote (
                                                
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
                                      loop43
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
                            loop43
                          )
                        )
                      )
                    )
                     (
                      hash-table-set! rec node #f
                    )
                     (
                      ret42 res
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
        dg_cycle_nodes g
      )
       (
        call/cc (
          lambda (
            ret45
          )
           (
            let (
              (
                visited (
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
                    rec (
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
                        res (
                          _list
                        )
                      )
                    )
                     (
                      begin (
                        call/cc (
                          lambda (
                            break47
                          )
                           (
                            letrec (
                              (
                                loop46 (
                                  lambda (
                                    xs
                                  )
                                   (
                                    if (
                                      null? xs
                                    )
                                     (
                                      quote (
                                        
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            k (
                                              car xs
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              not (
                                                cond (
                                                  (
                                                    string? visited
                                                  )
                                                   (
                                                    if (
                                                      string-contains visited k
                                                    )
                                                     #t #f
                                                  )
                                                )
                                                 (
                                                  (
                                                    hash-table? visited
                                                  )
                                                   (
                                                    if (
                                                      hash-table-exists? visited k
                                                    )
                                                     #t #f
                                                  )
                                                )
                                                 (
                                                  else (
                                                    if (
                                                      member k visited
                                                    )
                                                     #t #f
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                set! res (
                                                  dg_cycle_util g k visited rec res
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
                                        loop46 (
                                          cdr xs
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                             (
                              loop46 (
                                hash-table-keys (
                                  hash-table-ref g "graph"
                                )
                              )
                            )
                          )
                        )
                      )
                       (
                        ret45 res
                      )
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
        dg_has_cycle_util g node visited rec
      )
       (
        call/cc (
          lambda (
            ret48
          )
           (
            begin (
              hash-table-set! visited node #t
            )
             (
              hash-table-set! rec node #t
            )
             (
              let (
                (
                  edges (
                    hash-table-ref/default (
                      hash-table-ref g "graph"
                    )
                     node (
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
                          break50
                        )
                         (
                          letrec (
                            (
                              loop49 (
                                lambda (
                                  
                                )
                                 (
                                  if (
                                    < i (
                                      _len edges
                                    )
                                  )
                                   (
                                    begin (
                                      let (
                                        (
                                          neigh (
                                            cond (
                                              (
                                                string? (
                                                  list-ref edges i
                                                )
                                              )
                                               (
                                                _substring (
                                                  list-ref edges i
                                                )
                                                 1 (
                                                  + 1 1
                                                )
                                              )
                                            )
                                             (
                                              (
                                                hash-table? (
                                                  list-ref edges i
                                                )
                                              )
                                               (
                                                hash-table-ref (
                                                  list-ref edges i
                                                )
                                                 1
                                              )
                                            )
                                             (
                                              else (
                                                list-ref (
                                                  list-ref edges i
                                                )
                                                 1
                                              )
                                            )
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          if (
                                            not (
                                              cond (
                                                (
                                                  string? visited
                                                )
                                                 (
                                                  if (
                                                    string-contains visited neigh
                                                  )
                                                   #t #f
                                                )
                                              )
                                               (
                                                (
                                                  hash-table? visited
                                                )
                                                 (
                                                  if (
                                                    hash-table-exists? visited neigh
                                                  )
                                                   #t #f
                                                )
                                              )
                                               (
                                                else (
                                                  if (
                                                    member neigh visited
                                                  )
                                                   #t #f
                                                )
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              if (
                                                dg_has_cycle_util g neigh visited rec
                                              )
                                               (
                                                begin (
                                                  ret48 #t
                                                )
                                              )
                                               (
                                                quote (
                                                  
                                                )
                                              )
                                            )
                                          )
                                           (
                                            if (
                                              hash-table-ref/default rec neigh (
                                                quote (
                                                  
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                ret48 #t
                                              )
                                            )
                                             (
                                              quote (
                                                
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
                                      loop49
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
                            loop49
                          )
                        )
                      )
                    )
                     (
                      hash-table-set! rec node #f
                    )
                     (
                      ret48 #f
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
        dg_has_cycle g
      )
       (
        call/cc (
          lambda (
            ret51
          )
           (
            let (
              (
                visited (
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
                    rec (
                      alist->hash-table (
                        _list
                      )
                    )
                  )
                )
                 (
                  begin (
                    call/cc (
                      lambda (
                        break53
                      )
                       (
                        letrec (
                          (
                            loop52 (
                              lambda (
                                xs
                              )
                               (
                                if (
                                  null? xs
                                )
                                 (
                                  quote (
                                    
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        k (
                                          car xs
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        if (
                                          not (
                                            cond (
                                              (
                                                string? visited
                                              )
                                               (
                                                if (
                                                  string-contains visited k
                                                )
                                                 #t #f
                                              )
                                            )
                                             (
                                              (
                                                hash-table? visited
                                              )
                                               (
                                                if (
                                                  hash-table-exists? visited k
                                                )
                                                 #t #f
                                              )
                                            )
                                             (
                                              else (
                                                if (
                                                  member k visited
                                                )
                                                 #t #f
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              dg_has_cycle_util g k visited rec
                                            )
                                             (
                                              begin (
                                                ret51 #t
                                              )
                                            )
                                             (
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
                                    loop52 (
                                      cdr xs
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                         (
                          loop52 (
                            hash-table-keys (
                              hash-table-ref g "graph"
                            )
                          )
                        )
                      )
                    )
                  )
                   (
                    ret51 #f
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
        dg_fill_graph_randomly g c
      )
       (
        call/cc (
          lambda (
            ret54
          )
           (
            let (
              (
                count c
              )
            )
             (
              begin (
                if (
                  equal? count (
                    - 1
                  )
                )
                 (
                  begin (
                    set! count (
                      rand_range 10 10010
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
                    i 0
                  )
                )
                 (
                  begin (
                    call/cc (
                      lambda (
                        break56
                      )
                       (
                        letrec (
                          (
                            loop55 (
                              lambda (
                                
                              )
                               (
                                if (
                                  < i count
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        edge_count (
                                          rand_range 1 103
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
                                                          _lt j edge_count
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                n (
                                                                  rand_range 0 count
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                if (
                                                                  not (
                                                                    equal? n i
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    dg_add_pair g i n 1
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
                                            set! i (
                                              + i 1
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                   (
                                    loop55
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
                          loop55
                        )
                      )
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
        dg_dfs_time g s e
      )
       (
        call/cc (
          lambda (
            ret59
          )
           (
            let (
              (
                begin (
                  now
                )
              )
            )
             (
              begin (
                dg_dfs g s e
              )
               (
                let (
                  (
                    end (
                      now
                    )
                  )
                )
                 (
                  begin (
                    ret59 (
                      - end begin
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
        dg_bfs_time g s
      )
       (
        call/cc (
          lambda (
            ret60
          )
           (
            let (
              (
                begin (
                  now
                )
              )
            )
             (
              begin (
                dg_bfs g s
              )
               (
                let (
                  (
                    end (
                      now
                    )
                  )
                )
                 (
                  begin (
                    ret60 (
                      - end begin
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
        g_make_graph
      )
       (
        call/cc (
          lambda (
            ret61
          )
           (
            ret61 (
              alist->hash-table (
                _list (
                  cons "graph" (
                    alist->hash-table (
                      _list
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
        g_add_pair g u v w
      )
       (
        call/cc (
          lambda (
            ret62
          )
           (
            begin (
              if (
                cond (
                  (
                    string? (
                      hash-table-ref g "graph"
                    )
                  )
                   (
                    if (
                      string-contains (
                        hash-table-ref g "graph"
                      )
                       u
                    )
                     #t #f
                  )
                )
                 (
                  (
                    hash-table? (
                      hash-table-ref g "graph"
                    )
                  )
                   (
                    if (
                      hash-table-exists? (
                        hash-table-ref g "graph"
                      )
                       u
                    )
                     #t #f
                  )
                )
                 (
                  else (
                    if (
                      member u (
                        hash-table-ref g "graph"
                      )
                    )
                     #t #f
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      edges (
                        hash-table-ref/default (
                          hash-table-ref g "graph"
                        )
                         u (
                          quote (
                            
                          )
                        )
                      )
                    )
                  )
                   (
                    begin (
                      if (
                        not (
                          edge_exists edges w v
                        )
                      )
                       (
                        begin (
                          set! edges (
                            append edges (
                              _list (
                                _list w v
                              )
                            )
                          )
                        )
                         (
                          let (
                            (
                              m (
                                hash-table-ref g "graph"
                              )
                            )
                          )
                           (
                            begin (
                              hash-table-set! m u edges
                            )
                             (
                              hash-table-set! g "graph" m
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
               (
                begin (
                  let (
                    (
                      m0 (
                        hash-table-ref g "graph"
                      )
                    )
                  )
                   (
                    begin (
                      hash-table-set! m0 u (
                        _list (
                          _list w v
                        )
                      )
                    )
                     (
                      hash-table-set! g "graph" m0
                    )
                  )
                )
              )
            )
             (
              if (
                cond (
                  (
                    string? (
                      hash-table-ref g "graph"
                    )
                  )
                   (
                    if (
                      string-contains (
                        hash-table-ref g "graph"
                      )
                       v
                    )
                     #t #f
                  )
                )
                 (
                  (
                    hash-table? (
                      hash-table-ref g "graph"
                    )
                  )
                   (
                    if (
                      hash-table-exists? (
                        hash-table-ref g "graph"
                      )
                       v
                    )
                     #t #f
                  )
                )
                 (
                  else (
                    if (
                      member v (
                        hash-table-ref g "graph"
                      )
                    )
                     #t #f
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      edges2 (
                        hash-table-ref/default (
                          hash-table-ref g "graph"
                        )
                         v (
                          quote (
                            
                          )
                        )
                      )
                    )
                  )
                   (
                    begin (
                      if (
                        not (
                          edge_exists edges2 w u
                        )
                      )
                       (
                        begin (
                          set! edges2 (
                            append edges2 (
                              _list (
                                _list w u
                              )
                            )
                          )
                        )
                         (
                          let (
                            (
                              m2 (
                                hash-table-ref g "graph"
                              )
                            )
                          )
                           (
                            begin (
                              hash-table-set! m2 v edges2
                            )
                             (
                              hash-table-set! g "graph" m2
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
               (
                begin (
                  let (
                    (
                      m3 (
                        hash-table-ref g "graph"
                      )
                    )
                  )
                   (
                    begin (
                      hash-table-set! m3 v (
                        _list (
                          _list w u
                        )
                      )
                    )
                     (
                      hash-table-set! g "graph" m3
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
        g_remove_pair g u v
      )
       (
        call/cc (
          lambda (
            ret63
          )
           (
            begin (
              if (
                cond (
                  (
                    string? (
                      hash-table-ref g "graph"
                    )
                  )
                   (
                    if (
                      string-contains (
                        hash-table-ref g "graph"
                      )
                       u
                    )
                     #t #f
                  )
                )
                 (
                  (
                    hash-table? (
                      hash-table-ref g "graph"
                    )
                  )
                   (
                    if (
                      hash-table-exists? (
                        hash-table-ref g "graph"
                      )
                       u
                    )
                     #t #f
                  )
                )
                 (
                  else (
                    if (
                      member u (
                        hash-table-ref g "graph"
                      )
                    )
                     #t #f
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      edges (
                        hash-table-ref/default (
                          hash-table-ref g "graph"
                        )
                         u (
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
                          new_edges (
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
                                  break65
                                )
                                 (
                                  letrec (
                                    (
                                      loop64 (
                                        lambda (
                                          
                                        )
                                         (
                                          if (
                                            < i (
                                              _len edges
                                            )
                                          )
                                           (
                                            begin (
                                              if (
                                                not (
                                                  equal? (
                                                    cond (
                                                      (
                                                        string? (
                                                          list-ref edges i
                                                        )
                                                      )
                                                       (
                                                        _substring (
                                                          list-ref edges i
                                                        )
                                                         1 (
                                                          + 1 1
                                                        )
                                                      )
                                                    )
                                                     (
                                                      (
                                                        hash-table? (
                                                          list-ref edges i
                                                        )
                                                      )
                                                       (
                                                        hash-table-ref (
                                                          list-ref edges i
                                                        )
                                                         1
                                                      )
                                                    )
                                                     (
                                                      else (
                                                        list-ref (
                                                          list-ref edges i
                                                        )
                                                         1
                                                      )
                                                    )
                                                  )
                                                   v
                                                )
                                              )
                                               (
                                                begin (
                                                  set! new_edges (
                                                    append new_edges (
                                                      _list (
                                                        list-ref edges i
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
                                              loop64
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
                                    loop64
                                  )
                                )
                              )
                            )
                             (
                              let (
                                (
                                  m (
                                    hash-table-ref g "graph"
                                  )
                                )
                              )
                               (
                                begin (
                                  hash-table-set! m u new_edges
                                )
                                 (
                                  hash-table-set! g "graph" m
                                )
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
              if (
                cond (
                  (
                    string? (
                      hash-table-ref g "graph"
                    )
                  )
                   (
                    if (
                      string-contains (
                        hash-table-ref g "graph"
                      )
                       v
                    )
                     #t #f
                  )
                )
                 (
                  (
                    hash-table? (
                      hash-table-ref g "graph"
                    )
                  )
                   (
                    if (
                      hash-table-exists? (
                        hash-table-ref g "graph"
                      )
                       v
                    )
                     #t #f
                  )
                )
                 (
                  else (
                    if (
                      member v (
                        hash-table-ref g "graph"
                      )
                    )
                     #t #f
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      edges2 (
                        hash-table-ref/default (
                          hash-table-ref g "graph"
                        )
                         v (
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
                          new_edges2 (
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
                                  break67
                                )
                                 (
                                  letrec (
                                    (
                                      loop66 (
                                        lambda (
                                          
                                        )
                                         (
                                          if (
                                            < j (
                                              _len edges2
                                            )
                                          )
                                           (
                                            begin (
                                              if (
                                                not (
                                                  equal? (
                                                    cond (
                                                      (
                                                        string? (
                                                          list-ref edges2 j
                                                        )
                                                      )
                                                       (
                                                        _substring (
                                                          list-ref edges2 j
                                                        )
                                                         1 (
                                                          + 1 1
                                                        )
                                                      )
                                                    )
                                                     (
                                                      (
                                                        hash-table? (
                                                          list-ref edges2 j
                                                        )
                                                      )
                                                       (
                                                        hash-table-ref (
                                                          list-ref edges2 j
                                                        )
                                                         1
                                                      )
                                                    )
                                                     (
                                                      else (
                                                        list-ref (
                                                          list-ref edges2 j
                                                        )
                                                         1
                                                      )
                                                    )
                                                  )
                                                   u
                                                )
                                              )
                                               (
                                                begin (
                                                  set! new_edges2 (
                                                    append new_edges2 (
                                                      _list (
                                                        list-ref edges2 j
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
                                              set! j (
                                                + j 1
                                              )
                                            )
                                             (
                                              loop66
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
                                    loop66
                                  )
                                )
                              )
                            )
                             (
                              let (
                                (
                                  m2 (
                                    hash-table-ref g "graph"
                                  )
                                )
                              )
                               (
                                begin (
                                  hash-table-set! m2 v new_edges2
                                )
                                 (
                                  hash-table-set! g "graph" m2
                                )
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
          )
        )
      )
    )
     (
      define (
        g_all_nodes g
      )
       (
        call/cc (
          lambda (
            ret68
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
                call/cc (
                  lambda (
                    break70
                  )
                   (
                    letrec (
                      (
                        loop69 (
                          lambda (
                            xs
                          )
                           (
                            if (
                              null? xs
                            )
                             (
                              quote (
                                
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    k (
                                      car xs
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    set! res (
                                      append res (
                                        _list k
                                      )
                                    )
                                  )
                                )
                              )
                               (
                                loop69 (
                                  cdr xs
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                     (
                      loop69 (
                        hash-table-keys (
                          hash-table-ref g "graph"
                        )
                      )
                    )
                  )
                )
              )
               (
                ret68 res
              )
            )
          )
        )
      )
    )
     (
      define (
        g_dfs_util g node visited order d
      )
       (
        call/cc (
          lambda (
            ret71
          )
           (
            begin (
              hash-table-set! visited node #t
            )
             (
              set! order (
                append order (
                  _list node
                )
              )
            )
             (
              if (
                and (
                  not (
                    equal? d (
                      - 1
                    )
                  )
                )
                 (
                  equal? node d
                )
              )
               (
                begin (
                  ret71 order
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
                  edges (
                    hash-table-ref/default (
                      hash-table-ref g "graph"
                    )
                     node (
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
                          break73
                        )
                         (
                          letrec (
                            (
                              loop72 (
                                lambda (
                                  
                                )
                                 (
                                  if (
                                    < i (
                                      _len edges
                                    )
                                  )
                                   (
                                    begin (
                                      let (
                                        (
                                          neigh (
                                            cond (
                                              (
                                                string? (
                                                  list-ref edges i
                                                )
                                              )
                                               (
                                                _substring (
                                                  list-ref edges i
                                                )
                                                 1 (
                                                  + 1 1
                                                )
                                              )
                                            )
                                             (
                                              (
                                                hash-table? (
                                                  list-ref edges i
                                                )
                                              )
                                               (
                                                hash-table-ref (
                                                  list-ref edges i
                                                )
                                                 1
                                              )
                                            )
                                             (
                                              else (
                                                list-ref (
                                                  list-ref edges i
                                                )
                                                 1
                                              )
                                            )
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          if (
                                            not (
                                              cond (
                                                (
                                                  string? visited
                                                )
                                                 (
                                                  if (
                                                    string-contains visited neigh
                                                  )
                                                   #t #f
                                                )
                                              )
                                               (
                                                (
                                                  hash-table? visited
                                                )
                                                 (
                                                  if (
                                                    hash-table-exists? visited neigh
                                                  )
                                                   #t #f
                                                )
                                              )
                                               (
                                                else (
                                                  if (
                                                    member neigh visited
                                                  )
                                                   #t #f
                                                )
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              set! order (
                                                g_dfs_util g neigh visited order d
                                              )
                                            )
                                             (
                                              if (
                                                and (
                                                  not (
                                                    equal? d (
                                                      - 1
                                                    )
                                                  )
                                                )
                                                 (
                                                  equal? (
                                                    list-ref order (
                                                      - (
                                                        _len order
                                                      )
                                                       1
                                                    )
                                                  )
                                                   d
                                                )
                                              )
                                               (
                                                begin (
                                                  ret71 order
                                                )
                                              )
                                               (
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
                                          set! i (
                                            + i 1
                                          )
                                        )
                                      )
                                    )
                                     (
                                      loop72
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
                            loop72
                          )
                        )
                      )
                    )
                     (
                      ret71 order
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
        g_dfs g s d
      )
       (
        call/cc (
          lambda (
            ret74
          )
           (
            begin (
              if (
                equal? s d
              )
               (
                begin (
                  ret74 (
                    _list
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
                  start (
                    if (
                      equal? s (
                        - 2
                      )
                    )
                     (
                      first_key (
                        hash-table-ref g "graph"
                      )
                    )
                     s
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      visited (
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
                          order (
                            _list
                          )
                        )
                      )
                       (
                        begin (
                          set! order (
                            g_dfs_util g start visited order d
                          )
                        )
                         (
                          ret74 order
                        )
                      )
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
        g_bfs g s
      )
       (
        call/cc (
          lambda (
            ret75
          )
           (
            let (
              (
                queue (
                  _list
                )
              )
            )
             (
              begin (
                let (
                  (
                    visited (
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
                        order (
                          _list
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            start (
                              if (
                                equal? s (
                                  - 2
                                )
                              )
                               (
                                first_key (
                                  hash-table-ref g "graph"
                                )
                              )
                               s
                            )
                          )
                        )
                         (
                          begin (
                            set! queue (
                              append queue (
                                _list start
                              )
                            )
                          )
                           (
                            hash-table-set! visited start #t
                          )
                           (
                            call/cc (
                              lambda (
                                break77
                              )
                               (
                                letrec (
                                  (
                                    loop76 (
                                      lambda (
                                        
                                      )
                                       (
                                        if (
                                          > (
                                            _len queue
                                          )
                                           0
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                node (
                                                  list-ref queue 0
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                set! queue (
                                                  take (
                                                    drop queue 1
                                                  )
                                                   (
                                                    - (
                                                      _len queue
                                                    )
                                                     1
                                                  )
                                                )
                                              )
                                               (
                                                set! order (
                                                  append order (
                                                    _list node
                                                  )
                                                )
                                              )
                                               (
                                                let (
                                                  (
                                                    edges (
                                                      hash-table-ref/default (
                                                        hash-table-ref g "graph"
                                                      )
                                                       node (
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
                                                            break79
                                                          )
                                                           (
                                                            letrec (
                                                              (
                                                                loop78 (
                                                                  lambda (
                                                                    
                                                                  )
                                                                   (
                                                                    if (
                                                                      < i (
                                                                        _len edges
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            neigh (
                                                                              cond (
                                                                                (
                                                                                  string? (
                                                                                    list-ref edges i
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  _substring (
                                                                                    list-ref edges i
                                                                                  )
                                                                                   1 (
                                                                                    + 1 1
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                (
                                                                                  hash-table? (
                                                                                    list-ref edges i
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  hash-table-ref (
                                                                                    list-ref edges i
                                                                                  )
                                                                                   1
                                                                                )
                                                                              )
                                                                               (
                                                                                else (
                                                                                  list-ref (
                                                                                    list-ref edges i
                                                                                  )
                                                                                   1
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            if (
                                                                              not (
                                                                                cond (
                                                                                  (
                                                                                    string? visited
                                                                                  )
                                                                                   (
                                                                                    if (
                                                                                      string-contains visited neigh
                                                                                    )
                                                                                     #t #f
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  (
                                                                                    hash-table? visited
                                                                                  )
                                                                                   (
                                                                                    if (
                                                                                      hash-table-exists? visited neigh
                                                                                    )
                                                                                     #t #f
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  else (
                                                                                    if (
                                                                                      member neigh visited
                                                                                    )
                                                                                     #t #f
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                set! queue (
                                                                                  append queue (
                                                                                    _list neigh
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                hash-table-set! visited neigh #t
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
                                                                        loop78
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
                                                              loop78
                                                            )
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
                                            loop76
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
                                  loop76
                                )
                              )
                            )
                          )
                           (
                            ret75 order
                          )
                        )
                      )
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
        g_degree g u
      )
       (
        call/cc (
          lambda (
            ret80
          )
           (
            begin (
              if (
                cond (
                  (
                    string? (
                      hash-table-ref g "graph"
                    )
                  )
                   (
                    if (
                      string-contains (
                        hash-table-ref g "graph"
                      )
                       u
                    )
                     #t #f
                  )
                )
                 (
                  (
                    hash-table? (
                      hash-table-ref g "graph"
                    )
                  )
                   (
                    if (
                      hash-table-exists? (
                        hash-table-ref g "graph"
                      )
                       u
                    )
                     #t #f
                  )
                )
                 (
                  else (
                    if (
                      member u (
                        hash-table-ref g "graph"
                      )
                    )
                     #t #f
                  )
                )
              )
               (
                begin (
                  ret80 (
                    _len (
                      hash-table-ref/default (
                        hash-table-ref g "graph"
                      )
                       u (
                        quote (
                          
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
              ret80 0
            )
          )
        )
      )
    )
     (
      define (
        g_cycle_util g node visited parent res
      )
       (
        call/cc (
          lambda (
            ret81
          )
           (
            begin (
              hash-table-set! visited node #t
            )
             (
              let (
                (
                  edges (
                    hash-table-ref/default (
                      hash-table-ref g "graph"
                    )
                     node (
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
                          break83
                        )
                         (
                          letrec (
                            (
                              loop82 (
                                lambda (
                                  
                                )
                                 (
                                  if (
                                    < i (
                                      _len edges
                                    )
                                  )
                                   (
                                    begin (
                                      let (
                                        (
                                          neigh (
                                            cond (
                                              (
                                                string? (
                                                  list-ref edges i
                                                )
                                              )
                                               (
                                                _substring (
                                                  list-ref edges i
                                                )
                                                 1 (
                                                  + 1 1
                                                )
                                              )
                                            )
                                             (
                                              (
                                                hash-table? (
                                                  list-ref edges i
                                                )
                                              )
                                               (
                                                hash-table-ref (
                                                  list-ref edges i
                                                )
                                                 1
                                              )
                                            )
                                             (
                                              else (
                                                list-ref (
                                                  list-ref edges i
                                                )
                                                 1
                                              )
                                            )
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          if (
                                            not (
                                              cond (
                                                (
                                                  string? visited
                                                )
                                                 (
                                                  if (
                                                    string-contains visited neigh
                                                  )
                                                   #t #f
                                                )
                                              )
                                               (
                                                (
                                                  hash-table? visited
                                                )
                                                 (
                                                  if (
                                                    hash-table-exists? visited neigh
                                                  )
                                                   #t #f
                                                )
                                              )
                                               (
                                                else (
                                                  if (
                                                    member neigh visited
                                                  )
                                                   #t #f
                                                )
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              set! res (
                                                g_cycle_util g neigh visited node res
                                              )
                                            )
                                          )
                                           (
                                            if (
                                              not (
                                                equal? neigh parent
                                              )
                                            )
                                             (
                                              begin (
                                                if (
                                                  not (
                                                    list_contains_int res neigh
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    set! res (
                                                      append res (
                                                        _list neigh
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
                                                    list_contains_int res node
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    set! res (
                                                      append res (
                                                        _list node
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
                                             (
                                              quote (
                                                
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
                                      loop82
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
                            loop82
                          )
                        )
                      )
                    )
                     (
                      ret81 res
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
        g_cycle_nodes g
      )
       (
        call/cc (
          lambda (
            ret84
          )
           (
            let (
              (
                visited (
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
                    res (
                      _list
                    )
                  )
                )
                 (
                  begin (
                    call/cc (
                      lambda (
                        break86
                      )
                       (
                        letrec (
                          (
                            loop85 (
                              lambda (
                                xs
                              )
                               (
                                if (
                                  null? xs
                                )
                                 (
                                  quote (
                                    
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        k (
                                          car xs
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        if (
                                          not (
                                            cond (
                                              (
                                                string? visited
                                              )
                                               (
                                                if (
                                                  string-contains visited k
                                                )
                                                 #t #f
                                              )
                                            )
                                             (
                                              (
                                                hash-table? visited
                                              )
                                               (
                                                if (
                                                  hash-table-exists? visited k
                                                )
                                                 #t #f
                                              )
                                            )
                                             (
                                              else (
                                                if (
                                                  member k visited
                                                )
                                                 #t #f
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            set! res (
                                              g_cycle_util g k visited (
                                                - 1
                                              )
                                               res
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
                                    loop85 (
                                      cdr xs
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                         (
                          loop85 (
                            hash-table-keys (
                              hash-table-ref g "graph"
                            )
                          )
                        )
                      )
                    )
                  )
                   (
                    ret84 res
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
        g_has_cycle_util g node visited parent
      )
       (
        call/cc (
          lambda (
            ret87
          )
           (
            begin (
              hash-table-set! visited node #t
            )
             (
              let (
                (
                  edges (
                    hash-table-ref/default (
                      hash-table-ref g "graph"
                    )
                     node (
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
                          break89
                        )
                         (
                          letrec (
                            (
                              loop88 (
                                lambda (
                                  
                                )
                                 (
                                  if (
                                    < i (
                                      _len edges
                                    )
                                  )
                                   (
                                    begin (
                                      let (
                                        (
                                          neigh (
                                            cond (
                                              (
                                                string? (
                                                  list-ref edges i
                                                )
                                              )
                                               (
                                                _substring (
                                                  list-ref edges i
                                                )
                                                 1 (
                                                  + 1 1
                                                )
                                              )
                                            )
                                             (
                                              (
                                                hash-table? (
                                                  list-ref edges i
                                                )
                                              )
                                               (
                                                hash-table-ref (
                                                  list-ref edges i
                                                )
                                                 1
                                              )
                                            )
                                             (
                                              else (
                                                list-ref (
                                                  list-ref edges i
                                                )
                                                 1
                                              )
                                            )
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          if (
                                            not (
                                              cond (
                                                (
                                                  string? visited
                                                )
                                                 (
                                                  if (
                                                    string-contains visited neigh
                                                  )
                                                   #t #f
                                                )
                                              )
                                               (
                                                (
                                                  hash-table? visited
                                                )
                                                 (
                                                  if (
                                                    hash-table-exists? visited neigh
                                                  )
                                                   #t #f
                                                )
                                              )
                                               (
                                                else (
                                                  if (
                                                    member neigh visited
                                                  )
                                                   #t #f
                                                )
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              if (
                                                g_has_cycle_util g neigh visited node
                                              )
                                               (
                                                begin (
                                                  ret87 #t
                                                )
                                              )
                                               (
                                                quote (
                                                  
                                                )
                                              )
                                            )
                                          )
                                           (
                                            if (
                                              not (
                                                equal? neigh parent
                                              )
                                            )
                                             (
                                              begin (
                                                ret87 #t
                                              )
                                            )
                                             (
                                              quote (
                                                
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
                                      loop88
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
                            loop88
                          )
                        )
                      )
                    )
                     (
                      ret87 #f
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
        g_has_cycle g
      )
       (
        call/cc (
          lambda (
            ret90
          )
           (
            let (
              (
                visited (
                  alist->hash-table (
                    _list
                  )
                )
              )
            )
             (
              begin (
                call/cc (
                  lambda (
                    break92
                  )
                   (
                    letrec (
                      (
                        loop91 (
                          lambda (
                            xs
                          )
                           (
                            if (
                              null? xs
                            )
                             (
                              quote (
                                
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    k (
                                      car xs
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      not (
                                        cond (
                                          (
                                            string? visited
                                          )
                                           (
                                            if (
                                              string-contains visited k
                                            )
                                             #t #f
                                          )
                                        )
                                         (
                                          (
                                            hash-table? visited
                                          )
                                           (
                                            if (
                                              hash-table-exists? visited k
                                            )
                                             #t #f
                                          )
                                        )
                                         (
                                          else (
                                            if (
                                              member k visited
                                            )
                                             #t #f
                                          )
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        if (
                                          g_has_cycle_util g k visited (
                                            - 1
                                          )
                                        )
                                         (
                                          begin (
                                            ret90 #t
                                          )
                                        )
                                         (
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
                                loop91 (
                                  cdr xs
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                     (
                      loop91 (
                        hash-table-keys (
                          hash-table-ref g "graph"
                        )
                      )
                    )
                  )
                )
              )
               (
                ret90 #f
              )
            )
          )
        )
      )
    )
     (
      define (
        g_fill_graph_randomly g c
      )
       (
        call/cc (
          lambda (
            ret93
          )
           (
            let (
              (
                count c
              )
            )
             (
              begin (
                if (
                  equal? count (
                    - 1
                  )
                )
                 (
                  begin (
                    set! count (
                      rand_range 10 10010
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
                    i 0
                  )
                )
                 (
                  begin (
                    call/cc (
                      lambda (
                        break95
                      )
                       (
                        letrec (
                          (
                            loop94 (
                              lambda (
                                
                              )
                               (
                                if (
                                  < i count
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        edge_count (
                                          rand_range 1 103
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
                                                break97
                                              )
                                               (
                                                letrec (
                                                  (
                                                    loop96 (
                                                      lambda (
                                                        
                                                      )
                                                       (
                                                        if (
                                                          _lt j edge_count
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                n (
                                                                  rand_range 0 count
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                if (
                                                                  not (
                                                                    equal? n i
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    g_add_pair g i n 1
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
                                                           (
                                                            loop96
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
                                                  loop96
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
                                    loop94
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
                          loop94
                        )
                      )
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
        g_dfs_time g s e
      )
       (
        call/cc (
          lambda (
            ret98
          )
           (
            let (
              (
                begin (
                  now
                )
              )
            )
             (
              begin (
                g_dfs g s e
              )
               (
                let (
                  (
                    end (
                      now
                    )
                  )
                )
                 (
                  begin (
                    ret98 (
                      - end begin
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
        g_bfs_time g s
      )
       (
        call/cc (
          lambda (
            ret99
          )
           (
            let (
              (
                begin (
                  now
                )
              )
            )
             (
              begin (
                g_bfs g s
              )
               (
                let (
                  (
                    end (
                      now
                    )
                  )
                )
                 (
                  begin (
                    ret99 (
                      - end begin
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
            ret100
          )
           (
            let (
              (
                dg (
                  dg_make_graph
                )
              )
            )
             (
              begin (
                dg_add_pair dg 0 1 5
              )
               (
                dg_add_pair dg 0 2 3
              )
               (
                dg_add_pair dg 1 3 2
              )
               (
                dg_add_pair dg 2 3 4
              )
               (
                _display (
                  if (
                    string? (
                      to-str-space (
                        dg_dfs dg (
                          - 2
                        )
                         (
                          - 1
                        )
                      )
                    )
                  )
                   (
                    to-str-space (
                      dg_dfs dg (
                        - 2
                      )
                       (
                        - 1
                      )
                    )
                  )
                   (
                    to-str (
                      to-str-space (
                        dg_dfs dg (
                          - 2
                        )
                         (
                          - 1
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
                      to-str-space (
                        dg_bfs dg (
                          - 2
                        )
                      )
                    )
                  )
                   (
                    to-str-space (
                      dg_bfs dg (
                        - 2
                      )
                    )
                  )
                   (
                    to-str (
                      to-str-space (
                        dg_bfs dg (
                          - 2
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
                      to-str-space (
                        dg_in_degree dg 3
                      )
                    )
                  )
                   (
                    to-str-space (
                      dg_in_degree dg 3
                    )
                  )
                   (
                    to-str (
                      to-str-space (
                        dg_in_degree dg 3
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
                        dg_out_degree dg 0
                      )
                    )
                  )
                   (
                    to-str-space (
                      dg_out_degree dg 0
                    )
                  )
                   (
                    to-str (
                      to-str-space (
                        dg_out_degree dg 0
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
                        dg_topological_sort dg
                      )
                    )
                  )
                   (
                    to-str-space (
                      dg_topological_sort dg
                    )
                  )
                   (
                    to-str (
                      to-str-space (
                        dg_topological_sort dg
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
                        dg_has_cycle dg
                      )
                    )
                  )
                   (
                    to-str-space (
                      dg_has_cycle dg
                    )
                  )
                   (
                    to-str (
                      to-str-space (
                        dg_has_cycle dg
                      )
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
                    ug (
                      g_make_graph
                    )
                  )
                )
                 (
                  begin (
                    g_add_pair ug 0 1 1
                  )
                   (
                    g_add_pair ug 1 2 1
                  )
                   (
                    g_add_pair ug 2 0 1
                  )
                   (
                    _display (
                      if (
                        string? (
                          to-str-space (
                            g_dfs ug (
                              - 2
                            )
                             (
                              - 1
                            )
                          )
                        )
                      )
                       (
                        to-str-space (
                          g_dfs ug (
                            - 2
                          )
                           (
                            - 1
                          )
                        )
                      )
                       (
                        to-str (
                          to-str-space (
                            g_dfs ug (
                              - 2
                            )
                             (
                              - 1
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
                          to-str-space (
                            g_bfs ug (
                              - 2
                            )
                          )
                        )
                      )
                       (
                        to-str-space (
                          g_bfs ug (
                            - 2
                          )
                        )
                      )
                       (
                        to-str (
                          to-str-space (
                            g_bfs ug (
                              - 2
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
                          to-str-space (
                            g_degree ug 1
                          )
                        )
                      )
                       (
                        to-str-space (
                          g_degree ug 1
                        )
                      )
                       (
                        to-str (
                          to-str-space (
                            g_degree ug 1
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
                            g_has_cycle ug
                          )
                        )
                      )
                       (
                        to-str-space (
                          g_has_cycle ug
                        )
                      )
                       (
                        to-str (
                          to-str-space (
                            g_has_cycle ug
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
     (
      main
    )
     (
      let (
        (
          end102 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur103 (
              quotient (
                * (
                  - end102 start101
                )
                 1000000
              )
               jps104
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur103
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
