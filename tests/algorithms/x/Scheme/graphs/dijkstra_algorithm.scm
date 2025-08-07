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
      define (
        make_int_list n value
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
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
                                  < i n
                                )
                                 (
                                  begin (
                                    set! lst (
                                      append lst (
                                        _list value
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
                    ret1 lst
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
        make_bool_list n
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
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
                                  < i n
                                )
                                 (
                                  begin (
                                    set! lst (
                                      append lst (
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
                    ret4 lst
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
        dijkstra graph src
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            let (
              (
                n (
                  _len graph
                )
              )
            )
             (
              begin (
                let (
                  (
                    dist (
                      make_int_list n 1000000000
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        visited (
                          make_bool_list n
                        )
                      )
                    )
                     (
                      begin (
                        list-set! dist src 0
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
                                          < count n
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                u (
                                                  - 1
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    min_dist 1000000000
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
                                                                      < i n
                                                                    )
                                                                     (
                                                                      begin (
                                                                        if (
                                                                          and (
                                                                            not (
                                                                              cond (
                                                                                (
                                                                                  string? visited
                                                                                )
                                                                                 (
                                                                                  _substring visited i (
                                                                                    + i 1
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                (
                                                                                  hash-table? visited
                                                                                )
                                                                                 (
                                                                                  hash-table-ref visited i
                                                                                )
                                                                              )
                                                                               (
                                                                                else (
                                                                                  list-ref visited i
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            _lt (
                                                                              cond (
                                                                                (
                                                                                  string? dist
                                                                                )
                                                                                 (
                                                                                  _substring dist i (
                                                                                    + i 1
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                (
                                                                                  hash-table? dist
                                                                                )
                                                                                 (
                                                                                  hash-table-ref dist i
                                                                                )
                                                                              )
                                                                               (
                                                                                else (
                                                                                  list-ref dist i
                                                                                )
                                                                              )
                                                                            )
                                                                             min_dist
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            set! min_dist (
                                                                              cond (
                                                                                (
                                                                                  string? dist
                                                                                )
                                                                                 (
                                                                                  _substring dist i (
                                                                                    + i 1
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                (
                                                                                  hash-table? dist
                                                                                )
                                                                                 (
                                                                                  hash-table-ref dist i
                                                                                )
                                                                              )
                                                                               (
                                                                                else (
                                                                                  list-ref dist i
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            set! u i
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
                                                          < u 0
                                                        )
                                                         (
                                                          begin (
                                                            break9 (
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
                                                        list-set! visited u #t
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
                                                                          < j (
                                                                            _len (
                                                                              list-ref graph u
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                e (
                                                                                  cond (
                                                                                    (
                                                                                      string? (
                                                                                        list-ref graph u
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      _substring (
                                                                                        list-ref graph u
                                                                                      )
                                                                                       j (
                                                                                        + j 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? (
                                                                                        list-ref graph u
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref (
                                                                                        list-ref graph u
                                                                                      )
                                                                                       j
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref (
                                                                                        list-ref graph u
                                                                                      )
                                                                                       j
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
                                                                                      hash-table-ref e "node"
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    let (
                                                                                      (
                                                                                        w (
                                                                                          hash-table-ref e "weight"
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
                                                                                                _substring visited v (
                                                                                                  + v 1
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              (
                                                                                                hash-table? visited
                                                                                              )
                                                                                               (
                                                                                                hash-table-ref visited v
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              else (
                                                                                                list-ref visited v
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            let (
                                                                                              (
                                                                                                new_dist (
                                                                                                  _add (
                                                                                                    cond (
                                                                                                      (
                                                                                                        string? dist
                                                                                                      )
                                                                                                       (
                                                                                                        _substring dist u (
                                                                                                          + u 1
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      (
                                                                                                        hash-table? dist
                                                                                                      )
                                                                                                       (
                                                                                                        hash-table-ref dist u
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      else (
                                                                                                        list-ref dist u
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                   w
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                if (
                                                                                                  _lt new_dist (
                                                                                                    cond (
                                                                                                      (
                                                                                                        string? dist
                                                                                                      )
                                                                                                       (
                                                                                                        _substring dist v (
                                                                                                          + v 1
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      (
                                                                                                        hash-table? dist
                                                                                                      )
                                                                                                       (
                                                                                                        hash-table-ref dist v
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      else (
                                                                                                        list-ref dist v
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  begin (
                                                                                                    list-set! dist v new_dist
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
                                                            set! count (
                                                              + count 1
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
                            ret7 dist
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
          graph (
            _list (
              _list (
                alist->hash-table (
                  _list (
                    cons "node" 1
                  )
                   (
                    cons "weight" 10
                  )
                )
              )
               (
                alist->hash-table (
                  _list (
                    cons "node" 3
                  )
                   (
                    cons "weight" 5
                  )
                )
              )
            )
             (
              _list (
                alist->hash-table (
                  _list (
                    cons "node" 2
                  )
                   (
                    cons "weight" 1
                  )
                )
              )
               (
                alist->hash-table (
                  _list (
                    cons "node" 3
                  )
                   (
                    cons "weight" 2
                  )
                )
              )
            )
             (
              _list (
                alist->hash-table (
                  _list (
                    cons "node" 4
                  )
                   (
                    cons "weight" 4
                  )
                )
              )
            )
             (
              _list (
                alist->hash-table (
                  _list (
                    cons "node" 1
                  )
                   (
                    cons "weight" 3
                  )
                )
              )
               (
                alist->hash-table (
                  _list (
                    cons "node" 2
                  )
                   (
                    cons "weight" 9
                  )
                )
              )
               (
                alist->hash-table (
                  _list (
                    cons "node" 4
                  )
                   (
                    cons "weight" 2
                  )
                )
              )
            )
             (
              _list (
                alist->hash-table (
                  _list (
                    cons "node" 0
                  )
                   (
                    cons "weight" 7
                  )
                )
              )
               (
                alist->hash-table (
                  _list (
                    cons "node" 2
                  )
                   (
                    cons "weight" 6
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
              dist (
                dijkstra graph 0
              )
            )
          )
           (
            begin (
              _display (
                if (
                  string? (
                    to-str-space (
                      cond (
                        (
                          string? dist
                        )
                         (
                          _substring dist 0 (
                            + 0 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? dist
                        )
                         (
                          hash-table-ref dist 0
                        )
                      )
                       (
                        else (
                          list-ref dist 0
                        )
                      )
                    )
                  )
                )
                 (
                  to-str-space (
                    cond (
                      (
                        string? dist
                      )
                       (
                        _substring dist 0 (
                          + 0 1
                        )
                      )
                    )
                     (
                      (
                        hash-table? dist
                      )
                       (
                        hash-table-ref dist 0
                      )
                    )
                     (
                      else (
                        list-ref dist 0
                      )
                    )
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      cond (
                        (
                          string? dist
                        )
                         (
                          _substring dist 0 (
                            + 0 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? dist
                        )
                         (
                          hash-table-ref dist 0
                        )
                      )
                       (
                        else (
                          list-ref dist 0
                        )
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
                      cond (
                        (
                          string? dist
                        )
                         (
                          _substring dist 1 (
                            + 1 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? dist
                        )
                         (
                          hash-table-ref dist 1
                        )
                      )
                       (
                        else (
                          list-ref dist 1
                        )
                      )
                    )
                  )
                )
                 (
                  to-str-space (
                    cond (
                      (
                        string? dist
                      )
                       (
                        _substring dist 1 (
                          + 1 1
                        )
                      )
                    )
                     (
                      (
                        hash-table? dist
                      )
                       (
                        hash-table-ref dist 1
                      )
                    )
                     (
                      else (
                        list-ref dist 1
                      )
                    )
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      cond (
                        (
                          string? dist
                        )
                         (
                          _substring dist 1 (
                            + 1 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? dist
                        )
                         (
                          hash-table-ref dist 1
                        )
                      )
                       (
                        else (
                          list-ref dist 1
                        )
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
                      cond (
                        (
                          string? dist
                        )
                         (
                          _substring dist 2 (
                            + 2 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? dist
                        )
                         (
                          hash-table-ref dist 2
                        )
                      )
                       (
                        else (
                          list-ref dist 2
                        )
                      )
                    )
                  )
                )
                 (
                  to-str-space (
                    cond (
                      (
                        string? dist
                      )
                       (
                        _substring dist 2 (
                          + 2 1
                        )
                      )
                    )
                     (
                      (
                        hash-table? dist
                      )
                       (
                        hash-table-ref dist 2
                      )
                    )
                     (
                      else (
                        list-ref dist 2
                      )
                    )
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      cond (
                        (
                          string? dist
                        )
                         (
                          _substring dist 2 (
                            + 2 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? dist
                        )
                         (
                          hash-table-ref dist 2
                        )
                      )
                       (
                        else (
                          list-ref dist 2
                        )
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
                      cond (
                        (
                          string? dist
                        )
                         (
                          _substring dist 3 (
                            + 3 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? dist
                        )
                         (
                          hash-table-ref dist 3
                        )
                      )
                       (
                        else (
                          list-ref dist 3
                        )
                      )
                    )
                  )
                )
                 (
                  to-str-space (
                    cond (
                      (
                        string? dist
                      )
                       (
                        _substring dist 3 (
                          + 3 1
                        )
                      )
                    )
                     (
                      (
                        hash-table? dist
                      )
                       (
                        hash-table-ref dist 3
                      )
                    )
                     (
                      else (
                        list-ref dist 3
                      )
                    )
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      cond (
                        (
                          string? dist
                        )
                         (
                          _substring dist 3 (
                            + 3 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? dist
                        )
                         (
                          hash-table-ref dist 3
                        )
                      )
                       (
                        else (
                          list-ref dist 3
                        )
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
                      cond (
                        (
                          string? dist
                        )
                         (
                          _substring dist 4 (
                            + 4 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? dist
                        )
                         (
                          hash-table-ref dist 4
                        )
                      )
                       (
                        else (
                          list-ref dist 4
                        )
                      )
                    )
                  )
                )
                 (
                  to-str-space (
                    cond (
                      (
                        string? dist
                      )
                       (
                        _substring dist 4 (
                          + 4 1
                        )
                      )
                    )
                     (
                      (
                        hash-table? dist
                      )
                       (
                        hash-table-ref dist 4
                      )
                    )
                     (
                      else (
                        list-ref dist 4
                      )
                    )
                  )
                )
                 (
                  to-str (
                    to-str-space (
                      cond (
                        (
                          string? dist
                        )
                         (
                          _substring dist 4 (
                            + 4 1
                          )
                        )
                      )
                       (
                        (
                          hash-table? dist
                        )
                         (
                          hash-table-ref dist 4
                        )
                      )
                       (
                        else (
                          list-ref dist 4
                        )
                      )
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
