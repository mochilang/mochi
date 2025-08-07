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
      start36 (
        current-jiffy
      )
    )
     (
      jps39 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      let (
        (
          seed 1
        )
      )
       (
        begin (
          define (
            rand_int n
          )
           (
            call/cc (
              lambda (
                ret1
              )
               (
                begin (
                  set! seed (
                    _mod (
                      + (
                        * seed 1103515245
                      )
                       12345
                    )
                     2147483648
                  )
                )
                 (
                  ret1 (
                    _mod seed n
                  )
                )
              )
            )
          )
        )
         (
          define (
            contains list value
          )
           (
            call/cc (
              lambda (
                ret2
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
                                    _len list
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      string=? (
                                        list-ref list i
                                      )
                                       value
                                    )
                                     (
                                      begin (
                                        ret2 #t
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
                    ret2 #f
                  )
                )
              )
            )
          )
        )
         (
          define (
            remove_all list value
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
                                      < i (
                                        _len list
                                      )
                                    )
                                     (
                                      begin (
                                        if (
                                          not (
                                            string=? (
                                              list-ref list i
                                            )
                                             value
                                          )
                                        )
                                         (
                                          begin (
                                            set! res (
                                              append res (
                                                _list (
                                                  list-ref list i
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
            partition_graph graph
          )
           (
            call/cc (
              lambda (
                ret8
              )
               (
                let (
                  (
                    contracted (
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
                        break10
                      )
                       (
                        letrec (
                          (
                            loop9 (
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
                                        node (
                                          car xs
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        hash-table-set! contracted node (
                                          _list node
                                        )
                                      )
                                    )
                                  )
                                   (
                                    loop9 (
                                      cdr xs
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                         (
                          loop9 (
                            hash-table-keys graph
                          )
                        )
                      )
                    )
                  )
                   (
                    let (
                      (
                        graph_copy (
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
                            break12
                          )
                           (
                            letrec (
                              (
                                loop11 (
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
                                            node (
                                              car xs
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                lst (
                                                  _list
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    neigh (
                                                      hash-table-ref/default graph node (
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
                                                                        _len neigh
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        set! lst (
                                                                          append lst (
                                                                            _list (
                                                                              list-ref neigh i
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
                                                        hash-table-set! graph_copy node lst
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
                                        loop11 (
                                          cdr xs
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                             (
                              loop11 (
                                hash-table-keys graph
                              )
                            )
                          )
                        )
                      )
                       (
                        let (
                          (
                            nodes (
                              hash-table-keys graph_copy
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
                                        if (
                                          > (
                                            _len nodes
                                          )
                                           2
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                u (
                                                  list-ref nodes (
                                                    rand_int (
                                                      _len nodes
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    u_neighbors (
                                                      hash-table-ref/default graph_copy u (
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
                                                        v (
                                                          list-ref u_neighbors (
                                                            rand_int (
                                                              _len u_neighbors
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            uv (
                                                              string-append u v
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                uv_neighbors (
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
                                                                                    _len (
                                                                                      hash-table-ref/default graph_copy u (
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
                                                                                        n (
                                                                                          cond (
                                                                                            (
                                                                                              string? (
                                                                                                hash-table-ref/default graph_copy u (
                                                                                                  quote (
                                                                                                    
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              _substring (
                                                                                                hash-table-ref/default graph_copy u (
                                                                                                  quote (
                                                                                                    
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               i (
                                                                                                + i 1
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            (
                                                                                              hash-table? (
                                                                                                hash-table-ref/default graph_copy u (
                                                                                                  quote (
                                                                                                    
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              hash-table-ref (
                                                                                                hash-table-ref/default graph_copy u (
                                                                                                  quote (
                                                                                                    
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               i
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            else (
                                                                                              list-ref (
                                                                                                hash-table-ref/default graph_copy u (
                                                                                                  quote (
                                                                                                    
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               i
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        if (
                                                                                          and (
                                                                                            and (
                                                                                              not (
                                                                                                string=? n u
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              not (
                                                                                                string=? n v
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            eq? (
                                                                                              contains uv_neighbors n
                                                                                            )
                                                                                             #f
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            set! uv_neighbors (
                                                                                              append uv_neighbors (
                                                                                                _list n
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
                                                                    set! i 0
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
                                                                                  < i (
                                                                                    _len (
                                                                                      hash-table-ref/default graph_copy v (
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
                                                                                        n (
                                                                                          cond (
                                                                                            (
                                                                                              string? (
                                                                                                hash-table-ref/default graph_copy v (
                                                                                                  quote (
                                                                                                    
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              _substring (
                                                                                                hash-table-ref/default graph_copy v (
                                                                                                  quote (
                                                                                                    
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               i (
                                                                                                + i 1
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            (
                                                                                              hash-table? (
                                                                                                hash-table-ref/default graph_copy v (
                                                                                                  quote (
                                                                                                    
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              hash-table-ref (
                                                                                                hash-table-ref/default graph_copy v (
                                                                                                  quote (
                                                                                                    
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               i
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            else (
                                                                                              list-ref (
                                                                                                hash-table-ref/default graph_copy v (
                                                                                                  quote (
                                                                                                    
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               i
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        if (
                                                                                          and (
                                                                                            and (
                                                                                              not (
                                                                                                string=? n u
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              not (
                                                                                                string=? n v
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            eq? (
                                                                                              contains uv_neighbors n
                                                                                            )
                                                                                             #f
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            set! uv_neighbors (
                                                                                              append uv_neighbors (
                                                                                                _list n
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
                                                                    hash-table-set! graph_copy uv uv_neighbors
                                                                  )
                                                                   (
                                                                    let (
                                                                      (
                                                                        k 0
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
                                                                                      < k (
                                                                                        _len uv_neighbors
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        let (
                                                                                          (
                                                                                            nb (
                                                                                              list-ref uv_neighbors k
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            hash-table-set! graph_copy nb (
                                                                                              append (
                                                                                                hash-table-ref/default graph_copy nb (
                                                                                                  quote (
                                                                                                    
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                _list uv
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            hash-table-set! graph_copy nb (
                                                                                              remove_all (
                                                                                                hash-table-ref/default graph_copy nb (
                                                                                                  quote (
                                                                                                    
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               u
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            hash-table-set! graph_copy nb (
                                                                                              remove_all (
                                                                                                hash-table-ref/default graph_copy nb (
                                                                                                  quote (
                                                                                                    
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               v
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
                                                                        let (
                                                                          (
                                                                            group (
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
                                                                                            _len (
                                                                                              hash-table-ref/default contracted u (
                                                                                                quote (
                                                                                                  
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            set! group (
                                                                                              append group (
                                                                                                _list (
                                                                                                  cond (
                                                                                                    (
                                                                                                      string? (
                                                                                                        hash-table-ref/default contracted u (
                                                                                                          quote (
                                                                                                            
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      _substring (
                                                                                                        hash-table-ref/default contracted u (
                                                                                                          quote (
                                                                                                            
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       i (
                                                                                                        + i 1
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    (
                                                                                                      hash-table? (
                                                                                                        hash-table-ref/default contracted u (
                                                                                                          quote (
                                                                                                            
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      hash-table-ref (
                                                                                                        hash-table-ref/default contracted u (
                                                                                                          quote (
                                                                                                            
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       i
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    else (
                                                                                                      list-ref (
                                                                                                        hash-table-ref/default contracted u (
                                                                                                          quote (
                                                                                                            
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       i
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
                                                                            set! i 0
                                                                          )
                                                                           (
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
                                                                                          < i (
                                                                                            _len (
                                                                                              hash-table-ref/default contracted v (
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
                                                                                                val (
                                                                                                  cond (
                                                                                                    (
                                                                                                      string? (
                                                                                                        hash-table-ref/default contracted v (
                                                                                                          quote (
                                                                                                            
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      _substring (
                                                                                                        hash-table-ref/default contracted v (
                                                                                                          quote (
                                                                                                            
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       i (
                                                                                                        + i 1
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    (
                                                                                                      hash-table? (
                                                                                                        hash-table-ref/default contracted v (
                                                                                                          quote (
                                                                                                            
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      hash-table-ref (
                                                                                                        hash-table-ref/default contracted v (
                                                                                                          quote (
                                                                                                            
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       i
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    else (
                                                                                                      list-ref (
                                                                                                        hash-table-ref/default contracted v (
                                                                                                          quote (
                                                                                                            
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       i
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                if (
                                                                                                  eq? (
                                                                                                    contains group val
                                                                                                  )
                                                                                                   #f
                                                                                                )
                                                                                                 (
                                                                                                  begin (
                                                                                                    set! group (
                                                                                                      append group (
                                                                                                        _list val
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
                                                                            hash-table-set! contracted uv group
                                                                          )
                                                                           (
                                                                            set! nodes (
                                                                              remove_all nodes u
                                                                            )
                                                                          )
                                                                           (
                                                                            set! nodes (
                                                                              remove_all nodes v
                                                                            )
                                                                          )
                                                                           (
                                                                            set! nodes (
                                                                              append nodes (
                                                                                _list uv
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
                                groups (
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
                                                  < j (
                                                    _len nodes
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        n (
                                                          list-ref nodes j
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        set! groups (
                                                          append groups (
                                                            _list (
                                                              hash-table-ref/default contracted n (
                                                                quote (
                                                                  
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
                                    let (
                                      (
                                        groupA (
                                          list-ref groups 0
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            groupB (
                                              list-ref groups 1
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                cut (
                                                  _list
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                set! j 0
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
                                                              < j (
                                                                _len groupA
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                let (
                                                                  (
                                                                    node (
                                                                      list-ref groupA j
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        neigh (
                                                                          hash-table-ref/default graph node (
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
                                                                            l 0
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
                                                                                          < l (
                                                                                            _len neigh
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            let (
                                                                                              (
                                                                                                nb (
                                                                                                  list-ref neigh l
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                if (
                                                                                                  contains groupB nb
                                                                                                )
                                                                                                 (
                                                                                                  begin (
                                                                                                    set! cut (
                                                                                                      append cut (
                                                                                                        _list (
                                                                                                          alist->hash-table (
                                                                                                            _list (
                                                                                                              cons "a" node
                                                                                                            )
                                                                                                             (
                                                                                                              cons "b" nb
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
                                                                                                set! l (
                                                                                                  + l 1
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
                                                ret8 cut
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
            cut_to_string cut
          )
           (
            call/cc (
              lambda (
                ret33
              )
               (
                let (
                  (
                    s "{"
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
                            break35
                          )
                           (
                            letrec (
                              (
                                loop34 (
                                  lambda (
                                    
                                  )
                                   (
                                    if (
                                      < i (
                                        _len cut
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            p (
                                              list-ref cut i
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
                                                      string-append s "("
                                                    )
                                                     (
                                                      hash-table-ref p "a"
                                                    )
                                                  )
                                                   ", "
                                                )
                                                 (
                                                  hash-table-ref p "b"
                                                )
                                              )
                                               ")"
                                            )
                                          )
                                           (
                                            if (
                                              < i (
                                                - (
                                                  _len cut
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
                                        loop34
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
                              loop34
                            )
                          )
                        )
                      )
                       (
                        set! s (
                          string-append s "}"
                        )
                      )
                       (
                        ret33 s
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
              TEST_GRAPH (
                alist->hash-table (
                  _list (
                    cons "1" (
                      _list "2" "3" "4" "5"
                    )
                  )
                   (
                    cons "2" (
                      _list "1" "3" "4" "5"
                    )
                  )
                   (
                    cons "3" (
                      _list "1" "2" "4" "5" "10"
                    )
                  )
                   (
                    cons "4" (
                      _list "1" "2" "3" "5" "6"
                    )
                  )
                   (
                    cons "5" (
                      _list "1" "2" "3" "4" "7"
                    )
                  )
                   (
                    cons "6" (
                      _list "7" "8" "9" "10" "4"
                    )
                  )
                   (
                    cons "7" (
                      _list "6" "8" "9" "10" "5"
                    )
                  )
                   (
                    cons "8" (
                      _list "6" "7" "9" "10"
                    )
                  )
                   (
                    cons "9" (
                      _list "6" "7" "8" "10"
                    )
                  )
                   (
                    cons "10" (
                      _list "6" "7" "8" "9" "3"
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
                  result (
                    partition_graph TEST_GRAPH
                  )
                )
              )
               (
                begin (
                  _display (
                    if (
                      string? (
                        cut_to_string result
                      )
                    )
                     (
                      cut_to_string result
                    )
                     (
                      to-str (
                        cut_to_string result
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
          end37 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur38 (
              quotient (
                * (
                  - end37 start36
                )
                 1000000
              )
               jps39
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur38
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
