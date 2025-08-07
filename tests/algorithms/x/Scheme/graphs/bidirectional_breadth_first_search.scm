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
      start34 (
        current-jiffy
      )
    )
     (
      jps37 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      let (
        (
          grid (
            _list (
              _list 0 0 0 0 0 0 0
            )
             (
              _list 0 1 0 0 0 0 0
            )
             (
              _list 0 0 0 0 0 0 0
            )
             (
              _list 0 0 1 0 0 0 0
            )
             (
              _list 1 0 1 0 0 0 0
            )
             (
              _list 0 0 0 0 0 0 0
            )
             (
              _list 0 0 0 0 1 0 0
            )
          )
        )
      )
       (
        begin (
          let (
            (
              delta (
                _list (
                  _list (
                    - 1
                  )
                   0
                )
                 (
                  _list 0 (
                    - 1
                  )
                )
                 (
                  _list 1 0
                )
                 (
                  _list 0 1
                )
              )
            )
          )
           (
            begin (
              define (
                key y x
              )
               (
                call/cc (
                  lambda (
                    ret1
                  )
                   (
                    ret1 (
                      string-append (
                        string-append (
                          to-str-space y
                        )
                         ","
                      )
                       (
                        to-str-space x
                      )
                    )
                  )
                )
              )
            )
             (
              define (
                parse_int s
              )
               (
                call/cc (
                  lambda (
                    ret2
                  )
                   (
                    let (
                      (
                        value 0
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
                                            _len s
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                c (
                                                  _substring s i (
                                                    + i 1
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                set! value (
                                                  + (
                                                    * value 10
                                                  )
                                                   (
                                                    let (
                                                      (
                                                        v5 c
                                                      )
                                                    )
                                                     (
                                                      cond (
                                                        (
                                                          string? v5
                                                        )
                                                         (
                                                          inexact->exact (
                                                            floor (
                                                              string->number v5
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        (
                                                          boolean? v5
                                                        )
                                                         (
                                                          if v5 1 0
                                                        )
                                                      )
                                                       (
                                                        else (
                                                          inexact->exact (
                                                            floor v5
                                                          )
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
                            ret2 value
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
                parse_key k
              )
               (
                call/cc (
                  lambda (
                    ret6
                  )
                   (
                    let (
                      (
                        idx 0
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
                                      and (
                                        < idx (
                                          _len k
                                        )
                                      )
                                       (
                                        not (
                                          string=? (
                                            _substring k idx (
                                              + idx 1
                                            )
                                          )
                                           ","
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        set! idx (
                                          + idx 1
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
                       (
                        let (
                          (
                            y (
                              parse_int (
                                _substring k 0 idx
                              )
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                x (
                                  parse_int (
                                    _substring k (
                                      + idx 1
                                    )
                                     (
                                      _len k
                                    )
                                  )
                                )
                              )
                            )
                             (
                              begin (
                                ret6 (
                                  _list y x
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
                neighbors pos
              )
               (
                call/cc (
                  lambda (
                    ret9
                  )
                   (
                    let (
                      (
                        coords (
                          parse_key pos
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            y (
                              cond (
                                (
                                  string? coords
                                )
                                 (
                                  _substring coords 0 (
                                    + 0 1
                                  )
                                )
                              )
                               (
                                (
                                  hash-table? coords
                                )
                                 (
                                  hash-table-ref coords 0
                                )
                              )
                               (
                                else (
                                  list-ref coords 0
                                )
                              )
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                x (
                                  cond (
                                    (
                                      string? coords
                                    )
                                     (
                                      _substring coords 1 (
                                        + 1 1
                                      )
                                    )
                                  )
                                   (
                                    (
                                      hash-table? coords
                                    )
                                     (
                                      hash-table-ref coords 1
                                    )
                                  )
                                   (
                                    else (
                                      list-ref coords 1
                                    )
                                  )
                                )
                              )
                            )
                             (
                              begin (
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
                                                      < i (
                                                        _len delta
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            ny (
                                                              _add y (
                                                                cond (
                                                                  (
                                                                    string? (
                                                                      list-ref delta i
                                                                    )
                                                                  )
                                                                   (
                                                                    _substring (
                                                                      list-ref delta i
                                                                    )
                                                                     0 (
                                                                      + 0 1
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  (
                                                                    hash-table? (
                                                                      list-ref delta i
                                                                    )
                                                                  )
                                                                   (
                                                                    hash-table-ref (
                                                                      list-ref delta i
                                                                    )
                                                                     0
                                                                  )
                                                                )
                                                                 (
                                                                  else (
                                                                    list-ref (
                                                                      list-ref delta i
                                                                    )
                                                                     0
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
                                                                nx (
                                                                  _add x (
                                                                    cond (
                                                                      (
                                                                        string? (
                                                                          list-ref delta i
                                                                        )
                                                                      )
                                                                       (
                                                                        _substring (
                                                                          list-ref delta i
                                                                        )
                                                                         1 (
                                                                          + 1 1
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      (
                                                                        hash-table? (
                                                                          list-ref delta i
                                                                        )
                                                                      )
                                                                       (
                                                                        hash-table-ref (
                                                                          list-ref delta i
                                                                        )
                                                                         1
                                                                      )
                                                                    )
                                                                     (
                                                                      else (
                                                                        list-ref (
                                                                          list-ref delta i
                                                                        )
                                                                         1
                                                                      )
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
                                                                      and (
                                                                        _ge ny 0
                                                                      )
                                                                       (
                                                                        _lt ny (
                                                                          _len grid
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      _ge nx 0
                                                                    )
                                                                  )
                                                                   (
                                                                    _lt nx (
                                                                      _len (
                                                                        list-ref grid 0
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    if (
                                                                      equal? (
                                                                        cond (
                                                                          (
                                                                            string? (
                                                                              list-ref grid ny
                                                                            )
                                                                          )
                                                                           (
                                                                            _substring (
                                                                              list-ref grid ny
                                                                            )
                                                                             nx (
                                                                              + nx 1
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          (
                                                                            hash-table? (
                                                                              list-ref grid ny
                                                                            )
                                                                          )
                                                                           (
                                                                            hash-table-ref (
                                                                              list-ref grid ny
                                                                            )
                                                                             nx
                                                                          )
                                                                        )
                                                                         (
                                                                          else (
                                                                            list-ref (
                                                                              list-ref grid ny
                                                                            )
                                                                             nx
                                                                          )
                                                                        )
                                                                      )
                                                                       0
                                                                    )
                                                                     (
                                                                      begin (
                                                                        set! res (
                                                                          append res (
                                                                            _list (
                                                                              key ny nx
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
                                        ret9 res
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
                reverse_list lst
              )
               (
                call/cc (
                  lambda (
                    ret12
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
                            i (
                              - (
                                _len lst
                              )
                               1
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
                                        
                                      )
                                       (
                                        if (
                                          >= i 0
                                        )
                                         (
                                          begin (
                                            set! res (
                                              append res (
                                                _list (
                                                  list-ref lst i
                                                )
                                              )
                                            )
                                          )
                                           (
                                            set! i (
                                              - i 1
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
                            ret12 res
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
                bfs start goal
              )
               (
                call/cc (
                  lambda (
                    ret15
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
                            _list (
                              alist->hash-table (
                                _list (
                                  cons "pos" start
                                )
                                 (
                                  cons "path" (
                                    _list start
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
                            head 0
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                visited (
                                  alist->hash-table (
                                    _list (
                                      cons "start" #t
                                    )
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
                                                    node (
                                                      list-ref queue head
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
                                                    if (
                                                      string=? (
                                                        hash-table-ref node "pos"
                                                      )
                                                       goal
                                                    )
                                                     (
                                                      begin (
                                                        ret15 (
                                                          hash-table-ref node "path"
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
                                                        neigh (
                                                          neighbors (
                                                            hash-table-ref node "pos"
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
                                                                break19
                                                              )
                                                               (
                                                                letrec (
                                                                  (
                                                                    loop18 (
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
                                                                            let (
                                                                              (
                                                                                npos (
                                                                                  cond (
                                                                                    (
                                                                                      string? neigh
                                                                                    )
                                                                                     (
                                                                                      _substring neigh i (
                                                                                        + i 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? neigh
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref neigh i
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref neigh i
                                                                                    )
                                                                                  )
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
                                                                                        if (
                                                                                          string-contains visited npos
                                                                                        )
                                                                                         #t #f
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      (
                                                                                        hash-table? visited
                                                                                      )
                                                                                       (
                                                                                        if (
                                                                                          hash-table-exists? visited npos
                                                                                        )
                                                                                         #t #f
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      else (
                                                                                        if (
                                                                                          member npos visited
                                                                                        )
                                                                                         #t #f
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    hash-table-set! visited npos #t
                                                                                  )
                                                                                   (
                                                                                    let (
                                                                                      (
                                                                                        new_path (
                                                                                          append (
                                                                                            hash-table-ref node "path"
                                                                                          )
                                                                                           (
                                                                                            _list npos
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        set! queue (
                                                                                          append queue (
                                                                                            _list (
                                                                                              alist->hash-table (
                                                                                                _list (
                                                                                                  cons "pos" npos
                                                                                                )
                                                                                                 (
                                                                                                  cons "path" new_path
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
                                                                            )
                                                                          )
                                                                           (
                                                                            loop18
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
                                                                  loop18
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
                                                loop16
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
                                      loop16
                                    )
                                  )
                                )
                              )
                               (
                                ret15 (
                                  _list
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
                bidirectional_bfs start goal
              )
               (
                call/cc (
                  lambda (
                    ret20
                  )
                   (
                    let (
                      (
                        queue_f (
                          _list
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            queue_b (
                              _list
                            )
                          )
                        )
                         (
                          begin (
                            set! queue_f (
                              append queue_f (
                                _list (
                                  alist->hash-table (
                                    _list (
                                      cons "pos" start
                                    )
                                     (
                                      cons "path" (
                                        _list start
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                           (
                            set! queue_b (
                              append queue_b (
                                _list (
                                  alist->hash-table (
                                    _list (
                                      cons "pos" goal
                                    )
                                     (
                                      cons "path" (
                                        _list goal
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
                                head_f 0
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    head_b 0
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        visited_f (
                                          alist->hash-table (
                                            _list (
                                              cons "start" (
                                                _list start
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
                                            visited_b (
                                              alist->hash-table (
                                                _list (
                                                  cons "goal" (
                                                    _list goal
                                                  )
                                                )
                                              )
                                            )
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
                                                          and (
                                                            < head_f (
                                                              _len queue_f
                                                            )
                                                          )
                                                           (
                                                            < head_b (
                                                              _len queue_b
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                node_f (
                                                                  list-ref queue_f head_f
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                set! head_f (
                                                                  + head_f 1
                                                                )
                                                              )
                                                               (
                                                                let (
                                                                  (
                                                                    neigh_f (
                                                                      neighbors (
                                                                        hash-table-ref node_f "pos"
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
                                                                                        _len neigh_f
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        let (
                                                                                          (
                                                                                            npos (
                                                                                              cond (
                                                                                                (
                                                                                                  string? neigh_f
                                                                                                )
                                                                                                 (
                                                                                                  _substring neigh_f i (
                                                                                                    + i 1
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                (
                                                                                                  hash-table? neigh_f
                                                                                                )
                                                                                                 (
                                                                                                  hash-table-ref neigh_f i
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                else (
                                                                                                  list-ref neigh_f i
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            if (
                                                                                              not (
                                                                                                cond (
                                                                                                  (
                                                                                                    string? visited_f
                                                                                                  )
                                                                                                   (
                                                                                                    if (
                                                                                                      string-contains visited_f npos
                                                                                                    )
                                                                                                     #t #f
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  (
                                                                                                    hash-table? visited_f
                                                                                                  )
                                                                                                   (
                                                                                                    if (
                                                                                                      hash-table-exists? visited_f npos
                                                                                                    )
                                                                                                     #t #f
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  else (
                                                                                                    if (
                                                                                                      member npos visited_f
                                                                                                    )
                                                                                                     #t #f
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                let (
                                                                                                  (
                                                                                                    new_path (
                                                                                                      append (
                                                                                                        hash-table-ref node_f "path"
                                                                                                      )
                                                                                                       (
                                                                                                        _list npos
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  begin (
                                                                                                    hash-table-set! visited_f npos new_path
                                                                                                  )
                                                                                                   (
                                                                                                    if (
                                                                                                      cond (
                                                                                                        (
                                                                                                          string? visited_b
                                                                                                        )
                                                                                                         (
                                                                                                          if (
                                                                                                            string-contains visited_b npos
                                                                                                          )
                                                                                                           #t #f
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        (
                                                                                                          hash-table? visited_b
                                                                                                        )
                                                                                                         (
                                                                                                          if (
                                                                                                            hash-table-exists? visited_b npos
                                                                                                          )
                                                                                                           #t #f
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        else (
                                                                                                          if (
                                                                                                            member npos visited_b
                                                                                                          )
                                                                                                           #t #f
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      begin (
                                                                                                        let (
                                                                                                          (
                                                                                                            rev (
                                                                                                              reverse_list (
                                                                                                                hash-table-ref/default visited_b npos (
                                                                                                                  quote (
                                                                                                                    
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
                                                                                                                j 1
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              begin (
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
                                                                                                                              < j (
                                                                                                                                _len rev
                                                                                                                              )
                                                                                                                            )
                                                                                                                             (
                                                                                                                              begin (
                                                                                                                                set! new_path (
                                                                                                                                  append new_path (
                                                                                                                                    _list (
                                                                                                                                      cond (
                                                                                                                                        (
                                                                                                                                          string? rev
                                                                                                                                        )
                                                                                                                                         (
                                                                                                                                          _substring rev j (
                                                                                                                                            + j 1
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                       (
                                                                                                                                        (
                                                                                                                                          hash-table? rev
                                                                                                                                        )
                                                                                                                                         (
                                                                                                                                          hash-table-ref rev j
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                       (
                                                                                                                                        else (
                                                                                                                                          list-ref rev j
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
                                                                                                                ret20 new_path
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
                                                                                                    set! queue_f (
                                                                                                      append queue_f (
                                                                                                        _list (
                                                                                                          alist->hash-table (
                                                                                                            _list (
                                                                                                              cons "pos" npos
                                                                                                            )
                                                                                                             (
                                                                                                              cons "path" new_path
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
                                                                        let (
                                                                          (
                                                                            node_b (
                                                                              list-ref queue_b head_b
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            set! head_b (
                                                                              + head_b 1
                                                                            )
                                                                          )
                                                                           (
                                                                            let (
                                                                              (
                                                                                neigh_b (
                                                                                  neighbors (
                                                                                    hash-table-ref node_b "pos"
                                                                                  )
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
                                                                                                    _len neigh_b
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  begin (
                                                                                                    let (
                                                                                                      (
                                                                                                        nposb (
                                                                                                          cond (
                                                                                                            (
                                                                                                              string? neigh_b
                                                                                                            )
                                                                                                             (
                                                                                                              _substring neigh_b j (
                                                                                                                + j 1
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                           (
                                                                                                            (
                                                                                                              hash-table? neigh_b
                                                                                                            )
                                                                                                             (
                                                                                                              hash-table-ref neigh_b j
                                                                                                            )
                                                                                                          )
                                                                                                           (
                                                                                                            else (
                                                                                                              list-ref neigh_b j
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      begin (
                                                                                                        if (
                                                                                                          not (
                                                                                                            cond (
                                                                                                              (
                                                                                                                string? visited_b
                                                                                                              )
                                                                                                               (
                                                                                                                if (
                                                                                                                  string-contains visited_b nposb
                                                                                                                )
                                                                                                                 #t #f
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              (
                                                                                                                hash-table? visited_b
                                                                                                              )
                                                                                                               (
                                                                                                                if (
                                                                                                                  hash-table-exists? visited_b nposb
                                                                                                                )
                                                                                                                 #t #f
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              else (
                                                                                                                if (
                                                                                                                  member nposb visited_b
                                                                                                                )
                                                                                                                 #t #f
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          begin (
                                                                                                            let (
                                                                                                              (
                                                                                                                new_path_b (
                                                                                                                  append (
                                                                                                                    hash-table-ref node_b "path"
                                                                                                                  )
                                                                                                                   (
                                                                                                                    _list nposb
                                                                                                                  )
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              begin (
                                                                                                                hash-table-set! visited_b nposb new_path_b
                                                                                                              )
                                                                                                               (
                                                                                                                if (
                                                                                                                  cond (
                                                                                                                    (
                                                                                                                      string? visited_f
                                                                                                                    )
                                                                                                                     (
                                                                                                                      if (
                                                                                                                        string-contains visited_f nposb
                                                                                                                      )
                                                                                                                       #t #f
                                                                                                                    )
                                                                                                                  )
                                                                                                                   (
                                                                                                                    (
                                                                                                                      hash-table? visited_f
                                                                                                                    )
                                                                                                                     (
                                                                                                                      if (
                                                                                                                        hash-table-exists? visited_f nposb
                                                                                                                      )
                                                                                                                       #t #f
                                                                                                                    )
                                                                                                                  )
                                                                                                                   (
                                                                                                                    else (
                                                                                                                      if (
                                                                                                                        member nposb visited_f
                                                                                                                      )
                                                                                                                       #t #f
                                                                                                                    )
                                                                                                                  )
                                                                                                                )
                                                                                                                 (
                                                                                                                  begin (
                                                                                                                    let (
                                                                                                                      (
                                                                                                                        path_f (
                                                                                                                          hash-table-ref/default visited_f nposb (
                                                                                                                            quote (
                                                                                                                              
                                                                                                                            )
                                                                                                                          )
                                                                                                                        )
                                                                                                                      )
                                                                                                                    )
                                                                                                                     (
                                                                                                                      begin (
                                                                                                                        set! new_path_b (
                                                                                                                          reverse_list new_path_b
                                                                                                                        )
                                                                                                                      )
                                                                                                                       (
                                                                                                                        let (
                                                                                                                          (
                                                                                                                            t 1
                                                                                                                          )
                                                                                                                        )
                                                                                                                         (
                                                                                                                          begin (
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
                                                                                                                                          < t (
                                                                                                                                            _len new_path_b
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                         (
                                                                                                                                          begin (
                                                                                                                                            set! path_f (
                                                                                                                                              append path_f (
                                                                                                                                                _list (
                                                                                                                                                  list-ref new_path_b t
                                                                                                                                                )
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                           (
                                                                                                                                            set! t (
                                                                                                                                              + t 1
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
                                                                                                                            ret20 path_f
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
                                                                                                                set! queue_b (
                                                                                                                  append queue_b (
                                                                                                                    _list (
                                                                                                                      alist->hash-table (
                                                                                                                        _list (
                                                                                                                          cons "pos" nposb
                                                                                                                        )
                                                                                                                         (
                                                                                                                          cons "path" new_path_b
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
                                            ret20 (
                                              _list start
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
                path_to_string path
              )
               (
                call/cc (
                  lambda (
                    ret31
                  )
                   (
                    begin (
                      if (
                        equal? (
                          _len path
                        )
                         0
                      )
                       (
                        begin (
                          ret31 "[]"
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
                          first (
                            parse_key (
                              list-ref path 0
                            )
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              s (
                                string-append (
                                  string-append (
                                    string-append (
                                      string-append "[(" (
                                        to-str-space (
                                          cond (
                                            (
                                              string? first
                                            )
                                             (
                                              _substring first 0 (
                                                + 0 1
                                              )
                                            )
                                          )
                                           (
                                            (
                                              hash-table? first
                                            )
                                             (
                                              hash-table-ref first 0
                                            )
                                          )
                                           (
                                            else (
                                              list-ref first 0
                                            )
                                          )
                                        )
                                      )
                                    )
                                     ", "
                                  )
                                   (
                                    to-str-space (
                                      cond (
                                        (
                                          string? first
                                        )
                                         (
                                          _substring first 1 (
                                            + 1 1
                                          )
                                        )
                                      )
                                       (
                                        (
                                          hash-table? first
                                        )
                                         (
                                          hash-table-ref first 1
                                        )
                                      )
                                       (
                                        else (
                                          list-ref first 1
                                        )
                                      )
                                    )
                                  )
                                )
                                 ")"
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
                                      break33
                                    )
                                     (
                                      letrec (
                                        (
                                          loop32 (
                                            lambda (
                                              
                                            )
                                             (
                                              if (
                                                < i (
                                                  _len path
                                                )
                                              )
                                               (
                                                begin (
                                                  let (
                                                    (
                                                      c (
                                                        parse_key (
                                                          list-ref path i
                                                        )
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
                                                                string-append s ", ("
                                                              )
                                                               (
                                                                to-str-space (
                                                                  cond (
                                                                    (
                                                                      string? c
                                                                    )
                                                                     (
                                                                      _substring c 0 (
                                                                        + 0 1
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    (
                                                                      hash-table? c
                                                                    )
                                                                     (
                                                                      hash-table-ref c 0
                                                                    )
                                                                  )
                                                                   (
                                                                    else (
                                                                      list-ref c 0
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             ", "
                                                          )
                                                           (
                                                            to-str-space (
                                                              cond (
                                                                (
                                                                  string? c
                                                                )
                                                                 (
                                                                  _substring c 1 (
                                                                    + 1 1
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                (
                                                                  hash-table? c
                                                                )
                                                                 (
                                                                  hash-table-ref c 1
                                                                )
                                                              )
                                                               (
                                                                else (
                                                                  list-ref c 1
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                         ")"
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
                                                  loop32
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
                                        loop32
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
                                  ret31 s
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
                  start (
                    key 0 0
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      goal (
                        key (
                          - (
                            _len grid
                          )
                           1
                        )
                         (
                          - (
                            _len (
                              list-ref grid 0
                            )
                          )
                           1
                        )
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          path1 (
                            bfs start goal
                          )
                        )
                      )
                       (
                        begin (
                          _display (
                            if (
                              string? (
                                path_to_string path1
                              )
                            )
                             (
                              path_to_string path1
                            )
                             (
                              to-str (
                                path_to_string path1
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
                              path2 (
                                bidirectional_bfs start goal
                              )
                            )
                          )
                           (
                            begin (
                              _display (
                                if (
                                  string? (
                                    path_to_string path2
                                  )
                                )
                                 (
                                  path_to_string path2
                                )
                                 (
                                  to-str (
                                    path_to_string path2
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
    )
     (
      let (
        (
          end35 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur36 (
              quotient (
                * (
                  - end35 start34
                )
                 1000000
              )
               jps37
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur36
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
