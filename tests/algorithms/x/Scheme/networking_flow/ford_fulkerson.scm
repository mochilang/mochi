;; Generated on 2025-08-07 16:11 +0700
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
        ((number? x)
         (if (integer? x)
             (number->string (inexact->exact x))
             (number->string x)))
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
(define (_div a b) (if (and (integer? a) (integer? b) (exact? a) (exact? b)) (quotient a b) (/ a b)))
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
(define (list-ref-safe lst idx) (if (and (integer? idx) (>= idx 0) (< idx (length lst))) (list-ref lst idx) '()))
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
      let (
        (
          INF 1000000000
        )
      )
       (
        begin (
          define (
            breadth_first_search graph source sink parent
          )
           (
            call/cc (
              lambda (
                ret1
              )
               (
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
                                        _len graph
                                      )
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
                                        loop2
                                      )
                                    )
                                     '(
                                      
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
                            queue (
                              _list
                            )
                          )
                        )
                         (
                          begin (
                            set! queue (
                              append queue (
                                _list source
                              )
                            )
                          )
                           (
                            list-set! visited source #t
                          )
                           (
                            let (
                              (
                                head 0
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
                                              < head (
                                                _len queue
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    u (
                                                      list-ref-safe queue head
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
                                                        row (
                                                          list-ref-safe graph u
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            ind 0
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
                                                                          < ind (
                                                                            _len row
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                capacity (
                                                                                  list-ref-safe row ind
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                if (
                                                                                  and (
                                                                                    eq? (
                                                                                      list-ref-safe visited ind
                                                                                    )
                                                                                     #f
                                                                                  )
                                                                                   (
                                                                                    > capacity 0
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    set! queue (
                                                                                      append queue (
                                                                                        _list ind
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    list-set! visited ind #t
                                                                                  )
                                                                                   (
                                                                                    list-set! parent ind u
                                                                                  )
                                                                                )
                                                                                 '(
                                                                                  
                                                                                )
                                                                              )
                                                                               (
                                                                                set! ind (
                                                                                  + ind 1
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            loop6
                                                                          )
                                                                        )
                                                                         '(
                                                                          
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
                                             '(
                                              
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
                                ret1 (
                                  list-ref-safe visited sink
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
            ford_fulkerson graph source sink
          )
           (
            call/cc (
              lambda (
                ret8
              )
               (
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
                                        _len graph
                                      )
                                    )
                                     (
                                      begin (
                                        set! parent (
                                          append parent (
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
                                        loop9
                                      )
                                    )
                                     '(
                                      
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
                        let (
                          (
                            max_flow 0
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
                                          breadth_first_search graph source sink parent
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                path_flow INF
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    s sink
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
                                                                  not (
                                                                    equal? s source
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        prev (
                                                                          list-ref-safe parent s
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            cap (
                                                                              cond (
                                                                                (
                                                                                  string? (
                                                                                    list-ref-safe graph prev
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  _substring (
                                                                                    list-ref-safe graph prev
                                                                                  )
                                                                                   s (
                                                                                    + s 1
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                (
                                                                                  hash-table? (
                                                                                    list-ref-safe graph prev
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  hash-table-ref (
                                                                                    list-ref-safe graph prev
                                                                                  )
                                                                                   s
                                                                                )
                                                                              )
                                                                               (
                                                                                else (
                                                                                  list-ref-safe (
                                                                                    list-ref-safe graph prev
                                                                                  )
                                                                                   s
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            if (
                                                                              < cap path_flow
                                                                            )
                                                                             (
                                                                              begin (
                                                                                set! path_flow cap
                                                                              )
                                                                            )
                                                                             '(
                                                                              
                                                                            )
                                                                          )
                                                                           (
                                                                            set! s prev
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    loop13
                                                                  )
                                                                )
                                                                 '(
                                                                  
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
                                                    set! max_flow (
                                                      + max_flow path_flow
                                                    )
                                                  )
                                                   (
                                                    let (
                                                      (
                                                        v sink
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
                                                                      not (
                                                                        equal? v source
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            u (
                                                                              list-ref-safe parent v
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            list-set! (
                                                                              list-ref-safe graph u
                                                                            )
                                                                             v (
                                                                              - (
                                                                                cond (
                                                                                  (
                                                                                    string? (
                                                                                      list-ref-safe graph u
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    _substring (
                                                                                      list-ref-safe graph u
                                                                                    )
                                                                                     v (
                                                                                      + v 1
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  (
                                                                                    hash-table? (
                                                                                      list-ref-safe graph u
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    hash-table-ref (
                                                                                      list-ref-safe graph u
                                                                                    )
                                                                                     v
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  else (
                                                                                    list-ref-safe (
                                                                                      list-ref-safe graph u
                                                                                    )
                                                                                     v
                                                                                  )
                                                                                )
                                                                              )
                                                                               path_flow
                                                                            )
                                                                          )
                                                                           (
                                                                            list-set! (
                                                                              list-ref-safe graph v
                                                                            )
                                                                             u (
                                                                              + (
                                                                                cond (
                                                                                  (
                                                                                    string? (
                                                                                      list-ref-safe graph v
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    _substring (
                                                                                      list-ref-safe graph v
                                                                                    )
                                                                                     u (
                                                                                      + u 1
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  (
                                                                                    hash-table? (
                                                                                      list-ref-safe graph v
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    hash-table-ref (
                                                                                      list-ref-safe graph v
                                                                                    )
                                                                                     u
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  else (
                                                                                    list-ref-safe (
                                                                                      list-ref-safe graph v
                                                                                    )
                                                                                     u
                                                                                  )
                                                                                )
                                                                              )
                                                                               path_flow
                                                                            )
                                                                          )
                                                                           (
                                                                            set! v u
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        loop15
                                                                      )
                                                                    )
                                                                     '(
                                                                      
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
                                                                            _len parent
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            list-set! parent j (
                                                                              - 1
                                                                            )
                                                                          )
                                                                           (
                                                                            set! j (
                                                                              + j 1
                                                                            )
                                                                          )
                                                                           (
                                                                            loop17
                                                                          )
                                                                        )
                                                                         '(
                                                                          
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
                                            loop11
                                          )
                                        )
                                         '(
                                          
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
                            ret8 max_flow
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
              graph (
                _list (
                  _list 0 16 13 0 0 0
                )
                 (
                  _list 0 0 10 12 0 0
                )
                 (
                  _list 0 4 0 0 14 0
                )
                 (
                  _list 0 0 9 0 0 20
                )
                 (
                  _list 0 0 0 7 0 4
                )
                 (
                  _list 0 0 0 0 0 0
                )
              )
            )
          )
           (
            begin (
              _display (
                if (
                  string? (
                    to-str-space (
                      ford_fulkerson graph 0 5
                    )
                  )
                )
                 (
                  to-str-space (
                    ford_fulkerson graph 0 5
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      ford_fulkerson graph 0 5
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
