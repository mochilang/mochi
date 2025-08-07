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
        add_edge g from_vertex to_vertex
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                v (
                  hash-table-ref g "vertex"
                )
              )
            )
             (
              begin (
                if (
                  cond (
                    (
                      string? v
                    )
                     (
                      if (
                        string-contains v from_vertex
                      )
                       #t #f
                    )
                  )
                   (
                    (
                      hash-table? v
                    )
                     (
                      if (
                        hash-table-exists? v from_vertex
                      )
                       #t #f
                    )
                  )
                   (
                    else (
                      if (
                        member from_vertex v
                      )
                       #t #f
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        lst (
                          hash-table-ref/default v from_vertex (
                            quote (
                              
                            )
                          )
                        )
                      )
                    )
                     (
                      begin (
                        set! lst (
                          append lst (
                            _list to_vertex
                          )
                        )
                      )
                       (
                        hash-table-set! v from_vertex lst
                      )
                    )
                  )
                )
                 (
                  begin (
                    hash-table-set! v from_vertex (
                      _list to_vertex
                    )
                  )
                )
              )
               (
                hash-table-set! g "vertex" v
              )
               (
                if (
                  > (
                    + from_vertex 1
                  )
                   (
                    hash-table-ref g "size"
                  )
                )
                 (
                  begin (
                    hash-table-set! g "size" (
                      + from_vertex 1
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
                  > (
                    + to_vertex 1
                  )
                   (
                    hash-table-ref g "size"
                  )
                )
                 (
                  begin (
                    hash-table-set! g "size" (
                      + to_vertex 1
                    )
                  )
                )
                 (
                  quote (
                    
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
     (
      define (
        list_to_string lst
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            let (
              (
                res ""
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
                                    _len lst
                                  )
                                )
                                 (
                                  begin (
                                    set! res (
                                      string-append res (
                                        to-str-space (
                                          list-ref lst i
                                        )
                                      )
                                    )
                                  )
                                   (
                                    if (
                                      < i (
                                        - (
                                          _len lst
                                        )
                                         1
                                      )
                                    )
                                     (
                                      begin (
                                        set! res (
                                          string-append res " "
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
                    ret2 res
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
        list_to_arrow lst
      )
       (
        call/cc (
          lambda (
            ret5
          )
           (
            let (
              (
                res ""
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
                                    _len lst
                                  )
                                )
                                 (
                                  begin (
                                    set! res (
                                      string-append res (
                                        to-str-space (
                                          list-ref lst i
                                        )
                                      )
                                    )
                                  )
                                   (
                                    if (
                                      < i (
                                        - (
                                          _len lst
                                        )
                                         1
                                      )
                                    )
                                     (
                                      begin (
                                        set! res (
                                          string-append res " -> "
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
        print_graph g
      )
       (
        call/cc (
          lambda (
            ret8
          )
           (
            begin (
              _display (
                if (
                  string? (
                    to-str-space (
                      hash-table-ref g "vertex"
                    )
                  )
                )
                 (
                  to-str-space (
                    hash-table-ref g "vertex"
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      hash-table-ref g "vertex"
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
                  i 0
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
                                  hash-table-ref g "size"
                                )
                              )
                               (
                                begin (
                                  let (
                                    (
                                      edges (
                                        _list
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      if (
                                        cond (
                                          (
                                            string? (
                                              hash-table-ref g "vertex"
                                            )
                                          )
                                           (
                                            if (
                                              string-contains (
                                                hash-table-ref g "vertex"
                                              )
                                               i
                                            )
                                             #t #f
                                          )
                                        )
                                         (
                                          (
                                            hash-table? (
                                              hash-table-ref g "vertex"
                                            )
                                          )
                                           (
                                            if (
                                              hash-table-exists? (
                                                hash-table-ref g "vertex"
                                              )
                                               i
                                            )
                                             #t #f
                                          )
                                        )
                                         (
                                          else (
                                            if (
                                              member i (
                                                hash-table-ref g "vertex"
                                              )
                                            )
                                             #t #f
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          set! edges (
                                            hash-table-ref/default (
                                              hash-table-ref g "vertex"
                                            )
                                             i (
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
                                     (
                                      let (
                                        (
                                          line (
                                            string-append (
                                              string-append (
                                                to-str-space i
                                              )
                                               "  ->  "
                                            )
                                             (
                                              list_to_arrow edges
                                            )
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          _display (
                                            if (
                                              string? line
                                            )
                                             line (
                                              to-str line
                                            )
                                          )
                                        )
                                         (
                                          newline
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
              )
            )
          )
        )
      )
    )
     (
      define (
        dfs_recursive g start_vertex visited order
      )
       (
        call/cc (
          lambda (
            ret11
          )
           (
            begin (
              list-set! visited start_vertex #t
            )
             (
              set! order (
                append order (
                  _list start_vertex
                )
              )
            )
             (
              if (
                cond (
                  (
                    string? (
                      hash-table-ref g "vertex"
                    )
                  )
                   (
                    if (
                      string-contains (
                        hash-table-ref g "vertex"
                      )
                       start_vertex
                    )
                     #t #f
                  )
                )
                 (
                  (
                    hash-table? (
                      hash-table-ref g "vertex"
                    )
                  )
                   (
                    if (
                      hash-table-exists? (
                        hash-table-ref g "vertex"
                      )
                       start_vertex
                    )
                     #t #f
                  )
                )
                 (
                  else (
                    if (
                      member start_vertex (
                        hash-table-ref g "vertex"
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
                      neighbors (
                        hash-table-ref/default (
                          hash-table-ref g "vertex"
                        )
                         start_vertex (
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
                                        < i (
                                          _len neighbors
                                        )
                                      )
                                       (
                                        begin (
                                          let (
                                            (
                                              nb (
                                                list-ref neighbors i
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              if (
                                                not (
                                                  list-ref visited nb
                                                )
                                              )
                                               (
                                                begin (
                                                  set! order (
                                                    dfs_recursive g nb visited order
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
              ret11 order
            )
          )
        )
      )
    )
     (
      define (
        dfs g
      )
       (
        call/cc (
          lambda (
            ret14
          )
           (
            let (
              (
                n (
                  hash-table-ref g "size"
                )
              )
            )
             (
              begin (
                let (
                  (
                    visited (
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
                                      < i n
                                    )
                                     (
                                      begin (
                                        set! visited (
                                          append visited (
                                            _list #f
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
                            order (
                              _list
                            )
                          )
                        )
                         (
                          begin (
                            set! i 0
                          )
                           (
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
                                          < i n
                                        )
                                         (
                                          begin (
                                            if (
                                              not (
                                                list-ref visited i
                                              )
                                            )
                                             (
                                              begin (
                                                set! order (
                                                  dfs_recursive g i visited order
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
                            ret14 order
                          )
                        )
                      )
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
            alist->hash-table (
              _list (
                cons "vertex" (
                  alist->hash-table (
                    _list
                  )
                )
              )
               (
                cons "size" 0
              )
            )
          )
        )
      )
       (
        begin (
          set! g (
            add_edge g 0 1
          )
        )
         (
          set! g (
            add_edge g 0 2
          )
        )
         (
          set! g (
            add_edge g 1 2
          )
        )
         (
          set! g (
            add_edge g 2 0
          )
        )
         (
          set! g (
            add_edge g 2 3
          )
        )
         (
          set! g (
            add_edge g 3 3
          )
        )
         (
          print_graph g
        )
         (
          _display (
            if (
              string? "DFS:"
            )
             "DFS:" (
              to-str "DFS:"
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
                list_to_string (
                  dfs g
                )
              )
            )
             (
              list_to_string (
                dfs g
              )
            )
             (
              to-str (
                list_to_string (
                  dfs g
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
