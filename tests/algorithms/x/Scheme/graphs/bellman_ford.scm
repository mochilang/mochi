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
      start14 (
        current-jiffy
      )
    )
     (
      jps17 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      let (
        (
          INF 1000000000.0
        )
      )
       (
        begin (
          define (
            list_to_string arr
          )
           (
            call/cc (
              lambda (
                ret1
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
                                        _len arr
                                      )
                                    )
                                     (
                                      begin (
                                        set! s (
                                          string-append s (
                                            to-str-space (
                                              list-ref arr i
                                            )
                                          )
                                        )
                                      )
                                       (
                                        if (
                                          < i (
                                            - (
                                              _len arr
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
                          string-append s "]"
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
            check_negative_cycle graph distance edge_count
          )
           (
            call/cc (
              lambda (
                ret4
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
                                  < j edge_count
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        e (
                                          list-ref graph j
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            u (
                                              hash-table-ref e "src"
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                v (
                                                  hash-table-ref e "dst"
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    w (
                                                      + 0.0 (
                                                        hash-table-ref e "weight"
                                                      )
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    if (
                                                      and (
                                                        < (
                                                          list-ref distance u
                                                        )
                                                         INF
                                                      )
                                                       (
                                                        _lt (
                                                          + (
                                                            list-ref distance u
                                                          )
                                                           w
                                                        )
                                                         (
                                                          list-ref distance v
                                                        )
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
            bellman_ford graph vertex_count edge_count src
          )
           (
            call/cc (
              lambda (
                ret7
              )
               (
                let (
                  (
                    distance (
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
                                      < i vertex_count
                                    )
                                     (
                                      begin (
                                        set! distance (
                                          append distance (
                                            _list INF
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
                        list-set! distance src 0.0
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
                                          < k (
                                            - vertex_count 1
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
                                                              < j edge_count
                                                            )
                                                             (
                                                              begin (
                                                                let (
                                                                  (
                                                                    e (
                                                                      list-ref graph j
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        u (
                                                                          hash-table-ref e "src"
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            v (
                                                                              hash-table-ref e "dst"
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                w (
                                                                                  + 0.0 (
                                                                                    hash-table-ref e "weight"
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                if (
                                                                                  and (
                                                                                    < (
                                                                                      list-ref distance u
                                                                                    )
                                                                                     INF
                                                                                  )
                                                                                   (
                                                                                    _lt (
                                                                                      + (
                                                                                        list-ref distance u
                                                                                      )
                                                                                       w
                                                                                    )
                                                                                     (
                                                                                      list-ref distance v
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    list-set! distance v (
                                                                                      + (
                                                                                        list-ref distance u
                                                                                      )
                                                                                       w
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
                                                                            )
                                                                          )
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
                                               (
                                                set! k (
                                                  + k 1
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
                            if (
                              check_negative_cycle graph distance edge_count
                            )
                             (
                              begin (
                                panic "Negative cycle found"
                              )
                            )
                             (
                              quote (
                                
                              )
                            )
                          )
                           (
                            ret7 distance
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
              edges (
                _list (
                  alist->hash-table (
                    _list (
                      cons "src" 2
                    )
                     (
                      cons "dst" 1
                    )
                     (
                      cons "weight" (
                        - 10
                      )
                    )
                  )
                )
                 (
                  alist->hash-table (
                    _list (
                      cons "src" 3
                    )
                     (
                      cons "dst" 2
                    )
                     (
                      cons "weight" 3
                    )
                  )
                )
                 (
                  alist->hash-table (
                    _list (
                      cons "src" 0
                    )
                     (
                      cons "dst" 3
                    )
                     (
                      cons "weight" 5
                    )
                  )
                )
                 (
                  alist->hash-table (
                    _list (
                      cons "src" 0
                    )
                     (
                      cons "dst" 1
                    )
                     (
                      cons "weight" 4
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
                  distances (
                    bellman_ford edges 4 (
                      _len edges
                    )
                     0
                  )
                )
              )
               (
                begin (
                  _display (
                    if (
                      string? (
                        list_to_string distances
                      )
                    )
                     (
                      list_to_string distances
                    )
                     (
                      to-str (
                        list_to_string distances
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
          end15 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur16 (
              quotient (
                * (
                  - end15 start14
                )
                 1000000
              )
               jps17
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur16
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
