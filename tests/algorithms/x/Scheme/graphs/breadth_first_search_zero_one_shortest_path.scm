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
      start19 (
        current-jiffy
      )
    )
     (
      jps22 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        new_adjacency_list size
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
                                  < i size
                                )
                                 (
                                  begin (
                                    set! g (
                                      append g (
                                        _list (
                                          _list
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
                          cons "graph" g
                        )
                         (
                          cons "size" size
                        )
                      )
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
        add_edge al from_vertex to_vertex weight
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            begin (
              if (
                not (
                  or (
                    equal? weight 0
                  )
                   (
                    equal? weight 1
                  )
                )
              )
               (
                begin (
                  panic "Edge weight must be either 0 or 1."
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              if (
                or (
                  < to_vertex 0
                )
                 (
                  >= to_vertex (
                    hash-table-ref al "size"
                  )
                )
              )
               (
                begin (
                  panic "Vertex indexes must be in [0; size)."
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
                  g (
                    hash-table-ref al "graph"
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      edges (
                        list-ref g from_vertex
                      )
                    )
                  )
                   (
                    begin (
                      list-set! g from_vertex (
                        append edges (
                          _list (
                            alist->hash-table (
                              _list (
                                cons "destination_vertex" to_vertex
                              )
                               (
                                cons "weight" weight
                              )
                            )
                          )
                        )
                      )
                    )
                     (
                      hash-table-set! al "graph" g
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
        push_front q v
      )
       (
        call/cc (
          lambda (
            ret5
          )
           (
            let (
              (
                res (
                  _list v
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
                                  < i (
                                    _len q
                                  )
                                )
                                 (
                                  begin (
                                    set! res (
                                      append res (
                                        _list (
                                          list-ref q i
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
                    ret5 res
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
        pop_front q
      )
       (
        call/cc (
          lambda (
            ret8
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
                    i 1
                  )
                )
                 (
                  begin (
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
                                  < i (
                                    _len q
                                  )
                                )
                                 (
                                  begin (
                                    set! res (
                                      append res (
                                        _list (
                                          list-ref q i
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
                    ret8 res
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
        front q
      )
       (
        call/cc (
          lambda (
            ret11
          )
           (
            ret11 (
              list-ref q 0
            )
          )
        )
      )
    )
     (
      define (
        get_shortest_path al start_vertex finish_vertex
      )
       (
        call/cc (
          lambda (
            ret12
          )
           (
            let (
              (
                queue (
                  _list start_vertex
                )
              )
            )
             (
              begin (
                let (
                  (
                    distances (
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
                                      < i (
                                        hash-table-ref al "size"
                                      )
                                    )
                                     (
                                      begin (
                                        set! distances (
                                          append distances (
                                            _list (
                                              - 1
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
                        list-set! distances start_vertex 0
                      )
                       (
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
                                      > (
                                        _len queue
                                      )
                                       0
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            current_vertex (
                                              front queue
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            set! queue (
                                              pop_front queue
                                            )
                                          )
                                           (
                                            let (
                                              (
                                                current_distance (
                                                  list-ref distances current_vertex
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    edges (
                                                      list-ref (
                                                        hash-table-ref al "graph"
                                                      )
                                                       current_vertex
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
                                                                      < j (
                                                                        _len edges
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            edge (
                                                                              list-ref edges j
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                new_distance (
                                                                                  + current_distance (
                                                                                    hash-table-ref edge "weight"
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                let (
                                                                                  (
                                                                                    dest (
                                                                                      hash-table-ref edge "destination_vertex"
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    let (
                                                                                      (
                                                                                        dest_distance (
                                                                                          list-ref distances dest
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        if (
                                                                                          and (
                                                                                            >= dest_distance 0
                                                                                          )
                                                                                           (
                                                                                            >= new_distance dest_distance
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            set! j (
                                                                                              + j 1
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
                                                                                       (
                                                                                        list-set! distances dest new_distance
                                                                                      )
                                                                                       (
                                                                                        if (
                                                                                          equal? (
                                                                                            hash-table-ref edge "weight"
                                                                                          )
                                                                                           0
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            set! queue (
                                                                                              push_front queue dest
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            set! queue (
                                                                                              append queue (
                                                                                                _list dest
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
                        let (
                          (
                            result (
                              list-ref distances finish_vertex
                            )
                          )
                        )
                         (
                          begin (
                            if (
                              < result 0
                            )
                             (
                              begin (
                                panic "No path from start_vertex to finish_vertex."
                              )
                            )
                             (
                              quote (
                                
                              )
                            )
                          )
                           (
                            ret12 result
                          )
                        )
                      )
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
          g (
            new_adjacency_list 11
          )
        )
      )
       (
        begin (
          add_edge g 0 1 0
        )
         (
          add_edge g 0 3 1
        )
         (
          add_edge g 1 2 0
        )
         (
          add_edge g 2 3 0
        )
         (
          add_edge g 4 2 1
        )
         (
          add_edge g 4 5 1
        )
         (
          add_edge g 4 6 1
        )
         (
          add_edge g 5 9 0
        )
         (
          add_edge g 6 7 1
        )
         (
          add_edge g 7 8 1
        )
         (
          add_edge g 8 10 1
        )
         (
          add_edge g 9 7 0
        )
         (
          add_edge g 9 10 1
        )
         (
          _display (
            if (
              string? (
                to-str-space (
                  get_shortest_path g 0 3
                )
              )
            )
             (
              to-str-space (
                get_shortest_path g 0 3
              )
            )
             (
              to-str (
                to-str-space (
                  get_shortest_path g 0 3
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
                  get_shortest_path g 4 10
                )
              )
            )
             (
              to-str-space (
                get_shortest_path g 4 10
              )
            )
             (
              to-str (
                to-str-space (
                  get_shortest_path g 4 10
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
                  get_shortest_path g 4 8
                )
              )
            )
             (
              to-str-space (
                get_shortest_path g 4 8
              )
            )
             (
              to-str (
                to-str-space (
                  get_shortest_path g 4 8
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
                  get_shortest_path g 0 1
                )
              )
            )
             (
              to-str-space (
                get_shortest_path g 0 1
              )
            )
             (
              to-str (
                to-str-space (
                  get_shortest_path g 0 1
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
          end20 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur21 (
              quotient (
                * (
                  - end20 start19
                )
                 1000000
              )
               jps22
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur21
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
