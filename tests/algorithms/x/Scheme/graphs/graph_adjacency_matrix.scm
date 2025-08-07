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
        make_graph vertices edges directed
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                g (
                  alist->hash-table (
                    _list (
                      cons "directed" directed
                    )
                     (
                      cons "vertex_to_index" (
                        alist->hash-table (
                          _list
                        )
                      )
                    )
                     (
                      cons "adj_matrix" (
                        _list
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
                                    _len vertices
                                  )
                                )
                                 (
                                  begin (
                                    add_vertex g (
                                      list-ref vertices i
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
                    let (
                      (
                        j 0
                      )
                    )
                     (
                      begin (
                        call/cc (
                          lambda (
                            break5
                          )
                           (
                            letrec (
                              (
                                loop4 (
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
                                            add_edge g (
                                              list-ref e 0
                                            )
                                             (
                                              list-ref e 1
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
                                        loop4
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
                              loop4
                            )
                          )
                        )
                      )
                       (
                        ret1 g
                      )
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
        contains_vertex g v
      )
       (
        call/cc (
          lambda (
            ret6
          )
           (
            ret6 (
              cond (
                (
                  string? (
                    hash-table-ref g "vertex_to_index"
                  )
                )
                 (
                  if (
                    string-contains (
                      hash-table-ref g "vertex_to_index"
                    )
                     v
                  )
                   #t #f
                )
              )
               (
                (
                  hash-table? (
                    hash-table-ref g "vertex_to_index"
                  )
                )
                 (
                  if (
                    hash-table-exists? (
                      hash-table-ref g "vertex_to_index"
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
                      hash-table-ref g "vertex_to_index"
                    )
                  )
                   #t #f
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        add_vertex g v
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            begin (
              if (
                contains_vertex g v
              )
               (
                begin (
                  panic "vertex already exists"
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
                  matrix (
                    hash-table-ref g "adj_matrix"
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
                                      _len matrix
                                    )
                                  )
                                   (
                                    begin (
                                      list-set! matrix i (
                                        append (
                                          list-ref matrix i
                                        )
                                         (
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
                      let (
                        (
                          row (
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
                                              + (
                                                _len matrix
                                              )
                                               1
                                            )
                                          )
                                           (
                                            begin (
                                              set! row (
                                                append row (
                                                  _list 0
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
                              set! matrix (
                                append matrix (
                                  _list row
                                )
                              )
                            )
                             (
                              hash-table-set! g "adj_matrix" matrix
                            )
                             (
                              let (
                                (
                                  idx_map (
                                    hash-table-ref g "vertex_to_index"
                                  )
                                )
                              )
                               (
                                begin (
                                  hash-table-set! idx_map v (
                                    - (
                                      _len matrix
                                    )
                                     1
                                  )
                                )
                                 (
                                  hash-table-set! g "vertex_to_index" idx_map
                                )
                              )
                            )
                          )
                        )
                      )
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
        remove_key m k
      )
       (
        call/cc (
          lambda (
            ret12
          )
           (
            let (
              (
                out (
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
                    break14
                  )
                   (
                    letrec (
                      (
                        loop13 (
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
                                    key (
                                      car xs
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      not (
                                        equal? key k
                                      )
                                    )
                                     (
                                      begin (
                                        hash-table-set! out key (
                                          hash-table-ref/default m key (
                                            quote (
                                              
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
                               (
                                loop13 (
                                  cdr xs
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                     (
                      loop13 (
                        hash-table-keys m
                      )
                    )
                  )
                )
              )
               (
                ret12 out
              )
            )
          )
        )
      )
    )
     (
      define (
        decrement_indices m start
      )
       (
        call/cc (
          lambda (
            ret15
          )
           (
            let (
              (
                out (
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
                    break17
                  )
                   (
                    letrec (
                      (
                        loop16 (
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
                                    key (
                                      car xs
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        idx (
                                          hash-table-ref/default m key (
                                            quote (
                                              
                                            )
                                          )
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        if (
                                          > idx start
                                        )
                                         (
                                          begin (
                                            hash-table-set! out key (
                                              - idx 1
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            hash-table-set! out key idx
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                               (
                                loop16 (
                                  cdr xs
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                     (
                      loop16 (
                        hash-table-keys m
                      )
                    )
                  )
                )
              )
               (
                ret15 out
              )
            )
          )
        )
      )
    )
     (
      define (
        remove_vertex g v
      )
       (
        call/cc (
          lambda (
            ret18
          )
           (
            begin (
              if (
                not (
                  contains_vertex g v
                )
              )
               (
                begin (
                  panic "vertex does not exist"
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
                  idx (
                    hash-table-ref/default (
                      hash-table-ref g "vertex_to_index"
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
                      new_matrix (
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
                                          _len (
                                            hash-table-ref g "adj_matrix"
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          if (
                                            not (
                                              equal? i idx
                                            )
                                          )
                                           (
                                            begin (
                                              let (
                                                (
                                                  row (
                                                    list-ref (
                                                      hash-table-ref g "adj_matrix"
                                                    )
                                                     i
                                                  )
                                                )
                                              )
                                               (
                                                begin (
                                                  let (
                                                    (
                                                      new_row (
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
                                                                          _len row
                                                                        )
                                                                      )
                                                                       (
                                                                        begin (
                                                                          if (
                                                                            not (
                                                                              equal? j idx
                                                                            )
                                                                          )
                                                                           (
                                                                            begin (
                                                                              set! new_row (
                                                                                append new_row (
                                                                                  _list (
                                                                                    list-ref row j
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
                                                          set! new_matrix (
                                                            append new_matrix (
                                                              _list new_row
                                                            )
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
                          hash-table-set! g "adj_matrix" new_matrix
                        )
                         (
                          let (
                            (
                              m (
                                remove_key (
                                  hash-table-ref g "vertex_to_index"
                                )
                                 v
                              )
                            )
                          )
                           (
                            begin (
                              hash-table-set! g "vertex_to_index" (
                                decrement_indices m idx
                              )
                            )
                          )
                        )
                      )
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
        add_edge g u v
      )
       (
        call/cc (
          lambda (
            ret23
          )
           (
            begin (
              if (
                not (
                  and (
                    contains_vertex g u
                  )
                   (
                    contains_vertex g v
                  )
                )
              )
               (
                begin (
                  panic "missing vertex"
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
                  i (
                    hash-table-ref/default (
                      hash-table-ref g "vertex_to_index"
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
                      j (
                        hash-table-ref/default (
                          hash-table-ref g "vertex_to_index"
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
                          matrix (
                            hash-table-ref g "adj_matrix"
                          )
                        )
                      )
                       (
                        begin (
                          list-set! (
                            list-ref matrix i
                          )
                           j 1
                        )
                         (
                          if (
                            not (
                              hash-table-ref g "directed"
                            )
                          )
                           (
                            begin (
                              list-set! (
                                list-ref matrix j
                              )
                               i 1
                            )
                          )
                           (
                            quote (
                              
                            )
                          )
                        )
                         (
                          hash-table-set! g "adj_matrix" matrix
                        )
                      )
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
        remove_edge g u v
      )
       (
        call/cc (
          lambda (
            ret24
          )
           (
            begin (
              if (
                not (
                  and (
                    contains_vertex g u
                  )
                   (
                    contains_vertex g v
                  )
                )
              )
               (
                begin (
                  panic "missing vertex"
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
                  i (
                    hash-table-ref/default (
                      hash-table-ref g "vertex_to_index"
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
                      j (
                        hash-table-ref/default (
                          hash-table-ref g "vertex_to_index"
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
                          matrix (
                            hash-table-ref g "adj_matrix"
                          )
                        )
                      )
                       (
                        begin (
                          list-set! (
                            list-ref matrix i
                          )
                           j 0
                        )
                         (
                          if (
                            not (
                              hash-table-ref g "directed"
                            )
                          )
                           (
                            begin (
                              list-set! (
                                list-ref matrix j
                              )
                               i 0
                            )
                          )
                           (
                            quote (
                              
                            )
                          )
                        )
                         (
                          hash-table-set! g "adj_matrix" matrix
                        )
                      )
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
        contains_edge g u v
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
                  and (
                    contains_vertex g u
                  )
                   (
                    contains_vertex g v
                  )
                )
              )
               (
                begin (
                  panic "missing vertex"
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
                  i (
                    hash-table-ref/default (
                      hash-table-ref g "vertex_to_index"
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
                      j (
                        hash-table-ref/default (
                          hash-table-ref g "vertex_to_index"
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
                          matrix (
                            hash-table-ref g "adj_matrix"
                          )
                        )
                      )
                       (
                        begin (
                          ret25 (
                            equal? (
                              cond (
                                (
                                  string? (
                                    list-ref matrix i
                                  )
                                )
                                 (
                                  _substring (
                                    list-ref matrix i
                                  )
                                   j (
                                    + j 1
                                  )
                                )
                              )
                               (
                                (
                                  hash-table? (
                                    list-ref matrix i
                                  )
                                )
                                 (
                                  hash-table-ref (
                                    list-ref matrix i
                                  )
                                   j
                                )
                              )
                               (
                                else (
                                  list-ref (
                                    list-ref matrix i
                                  )
                                   j
                                )
                              )
                            )
                             1
                          )
                        )
                      )
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
        clear_graph g
      )
       (
        call/cc (
          lambda (
            ret26
          )
           (
            begin (
              hash-table-set! g "vertex_to_index" (
                alist->hash-table (
                  _list
                )
              )
            )
             (
              hash-table-set! g "adj_matrix" (
                _list
              )
            )
          )
        )
      )
    )
     (
      let (
        (
          g (
            make_graph (
              _list 1 2 3
            )
             (
              _list (
                _list 1 2
              )
               (
                _list 2 3
              )
            )
             #f
          )
        )
      )
       (
        begin (
          _display (
            if (
              string? (
                to-str-space (
                  hash-table-ref g "adj_matrix"
                )
              )
            )
             (
              to-str-space (
                hash-table-ref g "adj_matrix"
              )
            )
             (
              to-str (
                to-str-space (
                  hash-table-ref g "adj_matrix"
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
                  contains_edge g 1 2
                )
              )
            )
             (
              to-str-space (
                contains_edge g 1 2
              )
            )
             (
              to-str (
                to-str-space (
                  contains_edge g 1 2
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
                  contains_edge g 2 1
                )
              )
            )
             (
              to-str-space (
                contains_edge g 2 1
              )
            )
             (
              to-str (
                to-str-space (
                  contains_edge g 2 1
                )
              )
            )
          )
        )
         (
          newline
        )
         (
          remove_edge g 1 2
        )
         (
          _display (
            if (
              string? (
                to-str-space (
                  contains_edge g 1 2
                )
              )
            )
             (
              to-str-space (
                contains_edge g 1 2
              )
            )
             (
              to-str (
                to-str-space (
                  contains_edge g 1 2
                )
              )
            )
          )
        )
         (
          newline
        )
         (
          remove_vertex g 2
        )
         (
          _display (
            if (
              string? (
                to-str-space (
                  hash-table-ref g "adj_matrix"
                )
              )
            )
             (
              to-str-space (
                hash-table-ref g "adj_matrix"
              )
            )
             (
              to-str (
                to-str-space (
                  hash-table-ref g "adj_matrix"
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
