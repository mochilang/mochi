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
      start13 (
        current-jiffy
      )
    )
     (
      jps16 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        make_matrix n
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                matrix (
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
                                  <= i n
                                )
                                 (
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
                                                          <= j n
                                                        )
                                                         (
                                                          begin (
                                                            set! row (
                                                              append row (
                                                                _list #f
                                                              )
                                                            )
                                                          )
                                                           (
                                                            set! j (
                                                              + j 1
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
                                            set! matrix (
                                              append matrix (
                                                _list row
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
                    ret1 matrix
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
        dfs u graph visited_edge path
      )
       (
        call/cc (
          lambda (
            ret6
          )
           (
            begin (
              set! path (
                append path (
                  _list u
                )
              )
            )
             (
              if (
                cond (
                  (
                    string? graph
                  )
                   (
                    if (
                      string-contains graph u
                    )
                     #t #f
                  )
                )
                 (
                  (
                    hash-table? graph
                  )
                   (
                    if (
                      hash-table-exists? graph u
                    )
                     #t #f
                  )
                )
                 (
                  else (
                    if (
                      member u graph
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
                        hash-table-ref/default graph u (
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
                              break8
                            )
                             (
                              letrec (
                                (
                                  loop7 (
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
                                              v (
                                                list-ref neighbors i
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              if (
                                                eq? (
                                                  cond (
                                                    (
                                                      string? (
                                                        list-ref visited_edge u
                                                      )
                                                    )
                                                     (
                                                      _substring (
                                                        list-ref visited_edge u
                                                      )
                                                       v (
                                                        + v 1
                                                      )
                                                    )
                                                  )
                                                   (
                                                    (
                                                      hash-table? (
                                                        list-ref visited_edge u
                                                      )
                                                    )
                                                     (
                                                      hash-table-ref (
                                                        list-ref visited_edge u
                                                      )
                                                       v
                                                    )
                                                  )
                                                   (
                                                    else (
                                                      list-ref (
                                                        list-ref visited_edge u
                                                      )
                                                       v
                                                    )
                                                  )
                                                )
                                                 #f
                                              )
                                               (
                                                begin (
                                                  list-set! (
                                                    list-ref visited_edge u
                                                  )
                                                   v #t
                                                )
                                                 (
                                                  list-set! (
                                                    list-ref visited_edge v
                                                  )
                                                   u #t
                                                )
                                                 (
                                                  set! path (
                                                    dfs v graph visited_edge path
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
                                          loop7
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
                                loop7
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
              ret6 path
            )
          )
        )
      )
    )
     (
      define (
        check_circuit_or_path graph max_node
      )
       (
        call/cc (
          lambda (
            ret9
          )
           (
            let (
              (
                odd_degree_nodes 0
              )
            )
             (
              begin (
                let (
                  (
                    odd_node (
                      - 1
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
                                      < i max_node
                                    )
                                     (
                                      begin (
                                        if (
                                          cond (
                                            (
                                              string? graph
                                            )
                                             (
                                              if (
                                                string-contains graph i
                                              )
                                               #t #f
                                            )
                                          )
                                           (
                                            (
                                              hash-table? graph
                                            )
                                             (
                                              if (
                                                hash-table-exists? graph i
                                              )
                                               #t #f
                                            )
                                          )
                                           (
                                            else (
                                              if (
                                                member i graph
                                              )
                                               #t #f
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              equal? (
                                                _mod (
                                                  _len (
                                                    hash-table-ref/default graph i (
                                                      quote (
                                                        
                                                      )
                                                    )
                                                  )
                                                )
                                                 2
                                              )
                                               1
                                            )
                                             (
                                              begin (
                                                set! odd_degree_nodes (
                                                  + odd_degree_nodes 1
                                                )
                                              )
                                               (
                                                set! odd_node i
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
                          equal? odd_degree_nodes 0
                        )
                         (
                          begin (
                            ret9 (
                              alist->hash-table (
                                _list (
                                  cons "status" 1
                                )
                                 (
                                  cons "odd_node" odd_node
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
                          equal? odd_degree_nodes 2
                        )
                         (
                          begin (
                            ret9 (
                              alist->hash-table (
                                _list (
                                  cons "status" 2
                                )
                                 (
                                  cons "odd_node" odd_node
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
                        ret9 (
                          alist->hash-table (
                            _list (
                              cons "status" 3
                            )
                             (
                              cons "odd_node" odd_node
                            )
                          )
                        )
                      )
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
        check_euler graph max_node
      )
       (
        call/cc (
          lambda (
            ret12
          )
           (
            let (
              (
                visited_edge (
                  make_matrix max_node
                )
              )
            )
             (
              begin (
                let (
                  (
                    res (
                      check_circuit_or_path graph max_node
                    )
                  )
                )
                 (
                  begin (
                    if (
                      equal? (
                        hash-table-ref res "status"
                      )
                       3
                    )
                     (
                      begin (
                        _display (
                          if (
                            string? "graph is not Eulerian"
                          )
                           "graph is not Eulerian" (
                            to-str "graph is not Eulerian"
                          )
                        )
                      )
                       (
                        newline
                      )
                       (
                        _display (
                          if (
                            string? "no path"
                          )
                           "no path" (
                            to-str "no path"
                          )
                        )
                      )
                       (
                        newline
                      )
                       (
                        ret12 (
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
                    let (
                      (
                        start_node 1
                      )
                    )
                     (
                      begin (
                        if (
                          equal? (
                            hash-table-ref res "status"
                          )
                           2
                        )
                         (
                          begin (
                            set! start_node (
                              hash-table-ref res "odd_node"
                            )
                          )
                           (
                            _display (
                              if (
                                string? "graph has a Euler path"
                              )
                               "graph has a Euler path" (
                                to-str "graph has a Euler path"
                              )
                            )
                          )
                           (
                            newline
                          )
                        )
                         (
                          quote (
                            
                          )
                        )
                      )
                       (
                        if (
                          equal? (
                            hash-table-ref res "status"
                          )
                           1
                        )
                         (
                          begin (
                            _display (
                              if (
                                string? "graph has a Euler cycle"
                              )
                               "graph has a Euler cycle" (
                                to-str "graph has a Euler cycle"
                              )
                            )
                          )
                           (
                            newline
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
                            path (
                              dfs start_node graph visited_edge (
                                _list
                              )
                            )
                          )
                        )
                         (
                          begin (
                            _display (
                              if (
                                string? (
                                  to-str-space path
                                )
                              )
                               (
                                to-str-space path
                              )
                               (
                                to-str (
                                  to-str-space path
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
     (
      let (
        (
          g1 (
            alist->hash-table (
              _list (
                cons 1 (
                  _list 2 3 4
                )
              )
               (
                cons 2 (
                  _list 1 3
                )
              )
               (
                cons 3 (
                  _list 1 2
                )
              )
               (
                cons 4 (
                  _list 1 5
                )
              )
               (
                cons 5 (
                  _list 4
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
              g2 (
                alist->hash-table (
                  _list (
                    cons 1 (
                      _list 2 3 4 5
                    )
                  )
                   (
                    cons 2 (
                      _list 1 3
                    )
                  )
                   (
                    cons 3 (
                      _list 1 2
                    )
                  )
                   (
                    cons 4 (
                      _list 1 5
                    )
                  )
                   (
                    cons 5 (
                      _list 1 4
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
                  g3 (
                    alist->hash-table (
                      _list (
                        cons 1 (
                          _list 2 3 4
                        )
                      )
                       (
                        cons 2 (
                          _list 1 3 4
                        )
                      )
                       (
                        cons 3 (
                          _list 1 2
                        )
                      )
                       (
                        cons 4 (
                          _list 1 2 5
                        )
                      )
                       (
                        cons 5 (
                          _list 4
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
                      g4 (
                        alist->hash-table (
                          _list (
                            cons 1 (
                              _list 2 3
                            )
                          )
                           (
                            cons 2 (
                              _list 1 3
                            )
                          )
                           (
                            cons 3 (
                              _list 1 2
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
                          g5 (
                            alist->hash-table (
                              _list (
                                cons 1 (
                                  _list
                                )
                              )
                               (
                                cons 2 (
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
                              max_node 10
                            )
                          )
                           (
                            begin (
                              check_euler g1 max_node
                            )
                             (
                              check_euler g2 max_node
                            )
                             (
                              check_euler g3 max_node
                            )
                             (
                              check_euler g4 max_node
                            )
                             (
                              check_euler g5 max_node
                            )
                          )
                        )
                      )
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
          end14 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur15 (
              quotient (
                * (
                  - end14 start13
                )
                 1000000
              )
               jps16
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur15
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
