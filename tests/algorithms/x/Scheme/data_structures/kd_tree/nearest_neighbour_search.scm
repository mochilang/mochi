;; Generated on 2025-08-06 23:57 +0700
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
      start8 (
        current-jiffy
      )
    )
     (
      jps11 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        square_distance a b
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
                let (
                  (
                    total 0.0
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
                                    _len a
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        diff (
                                          - (
                                            list-ref a i
                                          )
                                           (
                                            list-ref b i
                                          )
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        set! total (
                                          _add total (
                                            * diff diff
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
                    ret1 total
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
        search nodes index query_point depth best
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            begin (
              if (
                equal? index (
                  - 1
                )
              )
               (
                begin (
                  ret4 best
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
                  result best
                )
              )
               (
                begin (
                  hash-table-set! result "nodes_visited" (
                    + (
                      hash-table-ref result "nodes_visited"
                    )
                     1
                  )
                )
                 (
                  let (
                    (
                      node (
                        list-ref nodes index
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          current_point (
                            hash-table-ref node "point"
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              current_dist (
                                square_distance query_point current_point
                              )
                            )
                          )
                           (
                            begin (
                              if (
                                or (
                                  equal? (
                                    _len (
                                      hash-table-ref result "point"
                                    )
                                  )
                                   0
                                )
                                 (
                                  _lt current_dist (
                                    hash-table-ref result "distance"
                                  )
                                )
                              )
                               (
                                begin (
                                  hash-table-set! result "point" current_point
                                )
                                 (
                                  hash-table-set! result "distance" current_dist
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
                                  k (
                                    _len query_point
                                  )
                                )
                              )
                               (
                                begin (
                                  let (
                                    (
                                      axis (
                                        _mod depth k
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      let (
                                        (
                                          nearer (
                                            hash-table-ref node "left"
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          let (
                                            (
                                              further (
                                                hash-table-ref node "right"
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              if (
                                                > (
                                                  list-ref query_point axis
                                                )
                                                 (
                                                  list-ref current_point axis
                                                )
                                              )
                                               (
                                                begin (
                                                  set! nearer (
                                                    hash-table-ref node "right"
                                                  )
                                                )
                                                 (
                                                  set! further (
                                                    hash-table-ref node "left"
                                                  )
                                                )
                                              )
                                               (
                                                quote (
                                                  
                                                )
                                              )
                                            )
                                             (
                                              set! result (
                                                search nodes nearer query_point (
                                                  + depth 1
                                                )
                                                 result
                                              )
                                            )
                                             (
                                              let (
                                                (
                                                  diff (
                                                    - (
                                                      list-ref query_point axis
                                                    )
                                                     (
                                                      list-ref current_point axis
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                begin (
                                                  if (
                                                    _lt (
                                                      * diff diff
                                                    )
                                                     (
                                                      hash-table-ref result "distance"
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      set! result (
                                                        search nodes further query_point (
                                                          + depth 1
                                                        )
                                                         result
                                                      )
                                                    )
                                                  )
                                                   (
                                                    quote (
                                                      
                                                    )
                                                  )
                                                )
                                                 (
                                                  ret4 result
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
        nearest_neighbour_search nodes root query_point
      )
       (
        call/cc (
          lambda (
            ret5
          )
           (
            let (
              (
                initial (
                  alist->hash-table (
                    _list (
                      cons "point" (
                        _list
                      )
                    )
                     (
                      cons "distance" 1000000000000000019884624838656.0
                    )
                     (
                      cons "nodes_visited" 0
                    )
                  )
                )
              )
            )
             (
              begin (
                ret5 (
                  search nodes root query_point 0 initial
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
          nodes (
            _list (
              alist->hash-table (
                _list (
                  cons "point" (
                    _list 9.0 1.0
                  )
                )
                 (
                  cons "left" 1
                )
                 (
                  cons "right" 4
                )
              )
            )
             (
              alist->hash-table (
                _list (
                  cons "point" (
                    _list 2.0 7.0
                  )
                )
                 (
                  cons "left" 2
                )
                 (
                  cons "right" 3
                )
              )
            )
             (
              alist->hash-table (
                _list (
                  cons "point" (
                    _list 3.0 6.0
                  )
                )
                 (
                  cons "left" (
                    - 1
                  )
                )
                 (
                  cons "right" (
                    - 1
                  )
                )
              )
            )
             (
              alist->hash-table (
                _list (
                  cons "point" (
                    _list 6.0 12.0
                  )
                )
                 (
                  cons "left" (
                    - 1
                  )
                )
                 (
                  cons "right" (
                    - 1
                  )
                )
              )
            )
             (
              alist->hash-table (
                _list (
                  cons "point" (
                    _list 17.0 15.0
                  )
                )
                 (
                  cons "left" 5
                )
                 (
                  cons "right" 6
                )
              )
            )
             (
              alist->hash-table (
                _list (
                  cons "point" (
                    _list 13.0 15.0
                  )
                )
                 (
                  cons "left" (
                    - 1
                  )
                )
                 (
                  cons "right" (
                    - 1
                  )
                )
              )
            )
             (
              alist->hash-table (
                _list (
                  cons "point" (
                    _list 10.0 19.0
                  )
                )
                 (
                  cons "left" (
                    - 1
                  )
                )
                 (
                  cons "right" (
                    - 1
                  )
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
              queries (
                _list (
                  _list 9.0 2.0
                )
                 (
                  _list 12.0 15.0
                )
                 (
                  _list 1.0 3.0
                )
              )
            )
          )
           (
            begin (
              let (
                (
                  q 0
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
                                < q (
                                  _len queries
                                )
                              )
                               (
                                begin (
                                  let (
                                    (
                                      res (
                                        nearest_neighbour_search nodes 0 (
                                          list-ref queries q
                                        )
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
                                                    string-append (
                                                      to-str-space (
                                                        hash-table-ref res "point"
                                                      )
                                                    )
                                                     " "
                                                  )
                                                   (
                                                    to-str-space (
                                                      hash-table-ref res "distance"
                                                    )
                                                  )
                                                )
                                                 " "
                                              )
                                               (
                                                to-str-space (
                                                  hash-table-ref res "nodes_visited"
                                                )
                                              )
                                            )
                                             "\n"
                                          )
                                        )
                                         (
                                          string-append (
                                            string-append (
                                              string-append (
                                                string-append (
                                                  string-append (
                                                    to-str-space (
                                                      hash-table-ref res "point"
                                                    )
                                                  )
                                                   " "
                                                )
                                                 (
                                                  to-str-space (
                                                    hash-table-ref res "distance"
                                                  )
                                                )
                                              )
                                               " "
                                            )
                                             (
                                              to-str-space (
                                                hash-table-ref res "nodes_visited"
                                              )
                                            )
                                          )
                                           "\n"
                                        )
                                         (
                                          to-str (
                                            string-append (
                                              string-append (
                                                string-append (
                                                  string-append (
                                                    string-append (
                                                      to-str-space (
                                                        hash-table-ref res "point"
                                                      )
                                                    )
                                                     " "
                                                  )
                                                   (
                                                    to-str-space (
                                                      hash-table-ref res "distance"
                                                    )
                                                  )
                                                )
                                                 " "
                                              )
                                               (
                                                to-str-space (
                                                  hash-table-ref res "nodes_visited"
                                                )
                                              )
                                            )
                                             "\n"
                                          )
                                        )
                                      )
                                    )
                                     (
                                      newline
                                    )
                                     (
                                      set! q (
                                        + q 1
                                      )
                                    )
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
              )
            )
          )
        )
      )
    )
     (
      let (
        (
          end9 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur10 (
              quotient (
                * (
                  - end9 start8
                )
                 1000000
              )
               jps11
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur10
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
