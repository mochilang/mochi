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
      start15 (
        current-jiffy
      )
    )
     (
      jps18 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        sort_edges edges
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                es edges
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
                                    _len es
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
                                                      < j (
                                                        - (
                                                          - (
                                                            _len es
                                                          )
                                                           i
                                                        )
                                                         1
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        if (
                                                          > (
                                                            cond (
                                                              (
                                                                string? (
                                                                  list-ref es j
                                                                )
                                                              )
                                                               (
                                                                _substring (
                                                                  list-ref es j
                                                                )
                                                                 2 (
                                                                  + 2 1
                                                                )
                                                              )
                                                            )
                                                             (
                                                              (
                                                                hash-table? (
                                                                  list-ref es j
                                                                )
                                                              )
                                                               (
                                                                hash-table-ref (
                                                                  list-ref es j
                                                                )
                                                                 2
                                                              )
                                                            )
                                                             (
                                                              else (
                                                                list-ref (
                                                                  list-ref es j
                                                                )
                                                                 2
                                                              )
                                                            )
                                                          )
                                                           (
                                                            cond (
                                                              (
                                                                string? (
                                                                  list-ref es (
                                                                    + j 1
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                _substring (
                                                                  list-ref es (
                                                                    + j 1
                                                                  )
                                                                )
                                                                 2 (
                                                                  + 2 1
                                                                )
                                                              )
                                                            )
                                                             (
                                                              (
                                                                hash-table? (
                                                                  list-ref es (
                                                                    + j 1
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                hash-table-ref (
                                                                  list-ref es (
                                                                    + j 1
                                                                  )
                                                                )
                                                                 2
                                                              )
                                                            )
                                                             (
                                                              else (
                                                                list-ref (
                                                                  list-ref es (
                                                                    + j 1
                                                                  )
                                                                )
                                                                 2
                                                              )
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                temp (
                                                                  list-ref es j
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                list-set! es j (
                                                                  list-ref es (
                                                                    + j 1
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                list-set! es (
                                                                  + j 1
                                                                )
                                                                 temp
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
                    ret1 es
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
        find_parent parent i
      )
       (
        call/cc (
          lambda (
            ret6
          )
           (
            begin (
              if (
                not (
                  equal? (
                    list-ref parent i
                  )
                   i
                )
              )
               (
                begin (
                  list-set! parent i (
                    find_parent parent (
                      list-ref parent i
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
              ret6 (
                list-ref parent i
              )
            )
          )
        )
      )
    )
     (
      define (
        kruskal num_nodes edges
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            let (
              (
                es (
                  sort_edges edges
                )
              )
            )
             (
              begin (
                let (
                  (
                    parent (
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
                                      < i num_nodes
                                    )
                                     (
                                      begin (
                                        set! parent (
                                          append parent (
                                            _list i
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
                            mst (
                              _list
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                idx 0
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
                                              < idx (
                                                _len es
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    e (
                                                      cond (
                                                        (
                                                          string? es
                                                        )
                                                         (
                                                          _substring es idx (
                                                            + idx 1
                                                          )
                                                        )
                                                      )
                                                       (
                                                        (
                                                          hash-table? es
                                                        )
                                                         (
                                                          hash-table-ref es idx
                                                        )
                                                      )
                                                       (
                                                        else (
                                                          list-ref es idx
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        pa (
                                                          find_parent parent (
                                                            cond (
                                                              (
                                                                string? e
                                                              )
                                                               (
                                                                _substring e 0 (
                                                                  + 0 1
                                                                )
                                                              )
                                                            )
                                                             (
                                                              (
                                                                hash-table? e
                                                              )
                                                               (
                                                                hash-table-ref e 0
                                                              )
                                                            )
                                                             (
                                                              else (
                                                                list-ref e 0
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
                                                            pb (
                                                              find_parent parent (
                                                                cond (
                                                                  (
                                                                    string? e
                                                                  )
                                                                   (
                                                                    _substring e 1 (
                                                                      + 1 1
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  (
                                                                    hash-table? e
                                                                  )
                                                                   (
                                                                    hash-table-ref e 1
                                                                  )
                                                                )
                                                                 (
                                                                  else (
                                                                    list-ref e 1
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            if (
                                                              not (
                                                                equal? pa pb
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                set! mst (
                                                                  append mst (
                                                                    _list e
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                list-set! parent pa pb
                                                              )
                                                            )
                                                             (
                                                              quote (
                                                                
                                                              )
                                                            )
                                                          )
                                                           (
                                                            set! idx (
                                                              + idx 1
                                                            )
                                                          )
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
                                ret7 mst
                              )
                            )
                          )
                        )
                      )
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
        edges_to_string es
      )
       (
        call/cc (
          lambda (
            ret12
          )
           (
            let (
              (
                s "["
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
                                    _len es
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        e (
                                          list-ref es i
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        set! s (
                                          string-append (
                                            string-append (
                                              string-append (
                                                string-append (
                                                  string-append (
                                                    string-append (
                                                      string-append s "("
                                                    )
                                                     (
                                                      to-str-space (
                                                        list-ref e 0
                                                      )
                                                    )
                                                  )
                                                   ", "
                                                )
                                                 (
                                                  to-str-space (
                                                    list-ref e 1
                                                  )
                                                )
                                              )
                                               ", "
                                            )
                                             (
                                              to-str-space (
                                                list-ref e 2
                                              )
                                            )
                                          )
                                           ")"
                                        )
                                      )
                                       (
                                        if (
                                          < i (
                                            - (
                                              _len es
                                            )
                                             1
                                          )
                                        )
                                         (
                                          begin (
                                            set! s (
                                              string-append s ", "
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
                    set! s (
                      string-append s "]"
                    )
                  )
                   (
                    ret12 s
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
          edges1 (
            _list (
              _list 0 1 3
            )
             (
              _list 1 2 5
            )
             (
              _list 2 3 1
            )
          )
        )
      )
       (
        begin (
          _display (
            if (
              string? (
                edges_to_string (
                  kruskal 4 edges1
                )
              )
            )
             (
              edges_to_string (
                kruskal 4 edges1
              )
            )
             (
              to-str (
                edges_to_string (
                  kruskal 4 edges1
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
              edges2 (
                _list (
                  _list 0 1 3
                )
                 (
                  _list 1 2 5
                )
                 (
                  _list 2 3 1
                )
                 (
                  _list 0 2 1
                )
                 (
                  _list 0 3 2
                )
              )
            )
          )
           (
            begin (
              _display (
                if (
                  string? (
                    edges_to_string (
                      kruskal 4 edges2
                    )
                  )
                )
                 (
                  edges_to_string (
                    kruskal 4 edges2
                  )
                )
                 (
                  to-str (
                    edges_to_string (
                      kruskal 4 edges2
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
                  edges3 (
                    _list (
                      _list 0 1 3
                    )
                     (
                      _list 1 2 5
                    )
                     (
                      _list 2 3 1
                    )
                     (
                      _list 0 2 1
                    )
                     (
                      _list 0 3 2
                    )
                     (
                      _list 2 1 1
                    )
                  )
                )
              )
               (
                begin (
                  _display (
                    if (
                      string? (
                        edges_to_string (
                          kruskal 4 edges3
                        )
                      )
                    )
                     (
                      edges_to_string (
                        kruskal 4 edges3
                      )
                    )
                     (
                      to-str (
                        edges_to_string (
                          kruskal 4 edges3
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
     (
      let (
        (
          end16 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur17 (
              quotient (
                * (
                  - end16 start15
                )
                 1000000
              )
               jps18
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur17
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
