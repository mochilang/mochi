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
      start51 (
        current-jiffy
      )
    )
     (
      jps54 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      let (
        (
          HEURISTIC 0
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
                    abs x
                  )
                   (
                    call/cc (
                      lambda (
                        ret1
                      )
                       (
                        begin (
                          if (
                            < x 0
                          )
                           (
                            begin (
                              ret1 (
                                - x
                              )
                            )
                          )
                           (
                            quote (
                              
                            )
                          )
                        )
                         (
                          ret1 x
                        )
                      )
                    )
                  )
                )
                 (
                  define (
                    sqrtApprox x
                  )
                   (
                    call/cc (
                      lambda (
                        ret2
                      )
                       (
                        begin (
                          if (
                            <= x 0.0
                          )
                           (
                            begin (
                              ret2 0.0
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
                              guess x
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
                                                < i 10
                                              )
                                               (
                                                begin (
                                                  set! guess (
                                                    _div (
                                                      _add guess (
                                                        _div x guess
                                                      )
                                                    )
                                                     2.0
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
                                  ret2 guess
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
                    heuristic a b
                  )
                   (
                    call/cc (
                      lambda (
                        ret5
                      )
                       (
                        let (
                          (
                            dy (
                              - (
                                hash-table-ref a "y"
                              )
                               (
                                hash-table-ref b "y"
                              )
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                dx (
                                  - (
                                    hash-table-ref a "x"
                                  )
                                   (
                                    hash-table-ref b "x"
                                  )
                                )
                              )
                            )
                             (
                              begin (
                                if (
                                  equal? HEURISTIC 1
                                )
                                 (
                                  begin (
                                    ret5 (
                                      + 0.0 (
                                        _add (
                                          abs dy
                                        )
                                         (
                                          abs dx
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
                                    dyf (
                                      + 0.0 dy
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        dxf (
                                          + 0.0 dx
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        ret5 (
                                          sqrtApprox (
                                            _add (
                                              * dyf dyf
                                            )
                                             (
                                              * dxf dxf
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
                    pos_equal a b
                  )
                   (
                    call/cc (
                      lambda (
                        ret6
                      )
                       (
                        ret6 (
                          and (
                            equal? (
                              hash-table-ref a "y"
                            )
                             (
                              hash-table-ref b "y"
                            )
                          )
                           (
                            equal? (
                              hash-table-ref a "x"
                            )
                             (
                              hash-table-ref b "x"
                            )
                          )
                        )
                      )
                    )
                  )
                )
                 (
                  define (
                    contains_pos lst p
                  )
                   (
                    call/cc (
                      lambda (
                        ret7
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
                                          < i (
                                            _len lst
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              pos_equal (
                                                list-ref lst i
                                              )
                                               p
                                            )
                                             (
                                              begin (
                                                ret7 #t
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
                            ret7 #f
                          )
                        )
                      )
                    )
                  )
                )
                 (
                  define (
                    open_index_of_pos open p
                  )
                   (
                    call/cc (
                      lambda (
                        ret10
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
                                          < i (
                                            _len open
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              pos_equal (
                                                hash-table-ref (
                                                  list-ref open i
                                                )
                                                 "pos"
                                              )
                                               p
                                            )
                                             (
                                              begin (
                                                ret10 i
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
                                            loop11
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
                                  loop11
                                )
                              )
                            )
                          )
                           (
                            ret10 (
                              - 0 1
                            )
                          )
                        )
                      )
                    )
                  )
                )
                 (
                  define (
                    remove_node_at nodes idx
                  )
                   (
                    call/cc (
                      lambda (
                        ret13
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
                                    break15
                                  )
                                   (
                                    letrec (
                                      (
                                        loop14 (
                                          lambda (
                                            
                                          )
                                           (
                                            if (
                                              < i (
                                                _len nodes
                                              )
                                            )
                                             (
                                              begin (
                                                if (
                                                  not (
                                                    equal? i idx
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    set! res (
                                                      append res (
                                                        _list (
                                                          list-ref nodes i
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
                                                loop14
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
                                      loop14
                                    )
                                  )
                                )
                              )
                               (
                                ret13 res
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
                    append_pos_list path p
                  )
                   (
                    call/cc (
                      lambda (
                        ret16
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
                                                _len path
                                              )
                                            )
                                             (
                                              begin (
                                                set! res (
                                                  append res (
                                                    _list (
                                                      list-ref path i
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
                                set! res (
                                  append res (
                                    _list p
                                  )
                                )
                              )
                               (
                                ret16 res
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
                    reverse_pos_list lst
                  )
                   (
                    call/cc (
                      lambda (
                        ret19
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
                                    break21
                                  )
                                   (
                                    letrec (
                                      (
                                        loop20 (
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
                                                loop20
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
                                      loop20
                                    )
                                  )
                                )
                              )
                               (
                                ret19 res
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
                    concat_pos_lists a b
                  )
                   (
                    call/cc (
                      lambda (
                        ret22
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
                                                _len a
                                              )
                                            )
                                             (
                                              begin (
                                                set! res (
                                                  append res (
                                                    _list (
                                                      list-ref a i
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
                                let (
                                  (
                                    j 0
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
                                                    _len b
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    set! res (
                                                      append res (
                                                        _list (
                                                          list-ref b j
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
                                    ret22 res
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
                    get_successors p
                  )
                   (
                    call/cc (
                      lambda (
                        ret27
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
                                    break29
                                  )
                                   (
                                    letrec (
                                      (
                                        loop28 (
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
                                                    nx (
                                                      + (
                                                        hash-table-ref p "x"
                                                      )
                                                       (
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
                                                    let (
                                                      (
                                                        ny (
                                                          + (
                                                            hash-table-ref p "y"
                                                          )
                                                           (
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
                                                        if (
                                                          and (
                                                            and (
                                                              and (
                                                                >= nx 0
                                                              )
                                                               (
                                                                >= ny 0
                                                              )
                                                            )
                                                             (
                                                              < nx (
                                                                _len (
                                                                  list-ref grid 0
                                                                )
                                                              )
                                                            )
                                                          )
                                                           (
                                                            < ny (
                                                              _len grid
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
                                                                      alist->hash-table (
                                                                        _list (
                                                                          cons "y" ny
                                                                        )
                                                                         (
                                                                          cons "x" nx
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
                                                loop28
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
                                      loop28
                                    )
                                  )
                                )
                              )
                               (
                                ret27 res
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
                    find_lowest_f open
                  )
                   (
                    call/cc (
                      lambda (
                        ret30
                      )
                       (
                        let (
                          (
                            best 0
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
                                              < i (
                                                _len open
                                              )
                                            )
                                             (
                                              begin (
                                                if (
                                                  < (
                                                    hash-table-ref (
                                                      list-ref open i
                                                    )
                                                     "f_cost"
                                                  )
                                                   (
                                                    hash-table-ref (
                                                      list-ref open best
                                                    )
                                                     "f_cost"
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    set! best i
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
                                ret30 best
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
                    astar start goal
                  )
                   (
                    call/cc (
                      lambda (
                        ret33
                      )
                       (
                        let (
                          (
                            h0 (
                              heuristic start goal
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                open (
                                  _list (
                                    alist->hash-table (
                                      _list (
                                        cons "pos" start
                                      )
                                       (
                                        cons "g_cost" 0
                                      )
                                       (
                                        cons "h_cost" h0
                                      )
                                       (
                                        cons "f_cost" h0
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
                              begin (
                                let (
                                  (
                                    closed (
                                      _list
                                    )
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
                                                  > (
                                                    _len open
                                                  )
                                                   0
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        idx (
                                                          find_lowest_f open
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            current (
                                                              list-ref open idx
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            set! open (
                                                              remove_node_at open idx
                                                            )
                                                          )
                                                           (
                                                            if (
                                                              pos_equal (
                                                                hash-table-ref current "pos"
                                                              )
                                                               goal
                                                            )
                                                             (
                                                              begin (
                                                                ret33 (
                                                                  hash-table-ref current "path"
                                                                )
                                                              )
                                                            )
                                                             (
                                                              quote (
                                                                
                                                              )
                                                            )
                                                          )
                                                           (
                                                            set! closed (
                                                              append closed (
                                                                _list (
                                                                  hash-table-ref current "pos"
                                                                )
                                                              )
                                                            )
                                                          )
                                                           (
                                                            let (
                                                              (
                                                                succ (
                                                                  get_successors (
                                                                    hash-table-ref current "pos"
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
                                                                        break37
                                                                      )
                                                                       (
                                                                        letrec (
                                                                          (
                                                                            loop36 (
                                                                              lambda (
                                                                                
                                                                              )
                                                                               (
                                                                                if (
                                                                                  < i (
                                                                                    _len succ
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    let (
                                                                                      (
                                                                                        pos (
                                                                                          cond (
                                                                                            (
                                                                                              string? succ
                                                                                            )
                                                                                             (
                                                                                              _substring succ i (
                                                                                                + i 1
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            (
                                                                                              hash-table? succ
                                                                                            )
                                                                                             (
                                                                                              hash-table-ref succ i
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            else (
                                                                                              list-ref succ i
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        if (
                                                                                          contains_pos closed pos
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            set! i (
                                                                                              + i 1
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            loop36
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
                                                                                            tentative_g (
                                                                                              + (
                                                                                                hash-table-ref current "g_cost"
                                                                                              )
                                                                                               1
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            let (
                                                                                              (
                                                                                                idx_open (
                                                                                                  open_index_of_pos open pos
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                if (
                                                                                                  or (
                                                                                                    equal? idx_open (
                                                                                                      - 0 1
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    < tentative_g (
                                                                                                      hash-table-ref (
                                                                                                        list-ref open idx_open
                                                                                                      )
                                                                                                       "g_cost"
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  begin (
                                                                                                    let (
                                                                                                      (
                                                                                                        new_path (
                                                                                                          append_pos_list (
                                                                                                            hash-table-ref current "path"
                                                                                                          )
                                                                                                           pos
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      begin (
                                                                                                        let (
                                                                                                          (
                                                                                                            h (
                                                                                                              heuristic pos goal
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          begin (
                                                                                                            let (
                                                                                                              (
                                                                                                                f (
                                                                                                                  _add (
                                                                                                                    + 0.0 tentative_g
                                                                                                                  )
                                                                                                                   h
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              begin (
                                                                                                                if (
                                                                                                                  not (
                                                                                                                    equal? idx_open (
                                                                                                                      - 0 1
                                                                                                                    )
                                                                                                                  )
                                                                                                                )
                                                                                                                 (
                                                                                                                  begin (
                                                                                                                    set! open (
                                                                                                                      remove_node_at open idx_open
                                                                                                                    )
                                                                                                                  )
                                                                                                                )
                                                                                                                 (
                                                                                                                  quote (
                                                                                                                    
                                                                                                                  )
                                                                                                                )
                                                                                                              )
                                                                                                               (
                                                                                                                set! open (
                                                                                                                  append open (
                                                                                                                    _list (
                                                                                                                      alist->hash-table (
                                                                                                                        _list (
                                                                                                                          cons "pos" pos
                                                                                                                        )
                                                                                                                         (
                                                                                                                          cons "g_cost" tentative_g
                                                                                                                        )
                                                                                                                         (
                                                                                                                          cons "h_cost" h
                                                                                                                        )
                                                                                                                         (
                                                                                                                          cons "f_cost" f
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
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    loop36
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
                                                                          loop36
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
                                    ret33 (
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
                 (
                  define (
                    combine_paths fwd bwd
                  )
                   (
                    call/cc (
                      lambda (
                        ret38
                      )
                       (
                        let (
                          (
                            bwd_copy (
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
                                    break40
                                  )
                                   (
                                    letrec (
                                      (
                                        loop39 (
                                          lambda (
                                            
                                          )
                                           (
                                            if (
                                              < i (
                                                - (
                                                  _len (
                                                    hash-table-ref bwd "path"
                                                  )
                                                )
                                                 1
                                              )
                                            )
                                             (
                                              begin (
                                                set! bwd_copy (
                                                  append bwd_copy (
                                                    _list (
                                                      list-ref (
                                                        hash-table-ref bwd "path"
                                                      )
                                                       i
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
                                                loop39
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
                                      loop39
                                    )
                                  )
                                )
                              )
                               (
                                set! bwd_copy (
                                  reverse_pos_list bwd_copy
                                )
                              )
                               (
                                ret38 (
                                  concat_pos_lists (
                                    hash-table-ref fwd "path"
                                  )
                                   bwd_copy
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
                    bidirectional_astar start goal
                  )
                   (
                    call/cc (
                      lambda (
                        ret41
                      )
                       (
                        let (
                          (
                            hf (
                              heuristic start goal
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                hb (
                                  heuristic goal start
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    open_f (
                                      _list (
                                        alist->hash-table (
                                          _list (
                                            cons "pos" start
                                          )
                                           (
                                            cons "g_cost" 0
                                          )
                                           (
                                            cons "h_cost" hf
                                          )
                                           (
                                            cons "f_cost" hf
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
                                  begin (
                                    let (
                                      (
                                        open_b (
                                          _list (
                                            alist->hash-table (
                                              _list (
                                                cons "pos" goal
                                              )
                                               (
                                                cons "g_cost" 0
                                              )
                                               (
                                                cons "h_cost" hb
                                              )
                                               (
                                                cons "f_cost" hb
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
                                      begin (
                                        let (
                                          (
                                            closed_f (
                                              _list
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                closed_b (
                                                  _list
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                call/cc (
                                                  lambda (
                                                    break43
                                                  )
                                                   (
                                                    letrec (
                                                      (
                                                        loop42 (
                                                          lambda (
                                                            
                                                          )
                                                           (
                                                            if (
                                                              and (
                                                                > (
                                                                  _len open_f
                                                                )
                                                                 0
                                                              )
                                                               (
                                                                > (
                                                                  _len open_b
                                                                )
                                                                 0
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                let (
                                                                  (
                                                                    idx_f (
                                                                      find_lowest_f open_f
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        current_f (
                                                                          list-ref open_f idx_f
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        set! open_f (
                                                                          remove_node_at open_f idx_f
                                                                        )
                                                                      )
                                                                       (
                                                                        let (
                                                                          (
                                                                            idx_b (
                                                                              find_lowest_f open_b
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                current_b (
                                                                                  list-ref open_b idx_b
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                set! open_b (
                                                                                  remove_node_at open_b idx_b
                                                                                )
                                                                              )
                                                                               (
                                                                                if (
                                                                                  pos_equal (
                                                                                    hash-table-ref current_f "pos"
                                                                                  )
                                                                                   (
                                                                                    hash-table-ref current_b "pos"
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    ret41 (
                                                                                      combine_paths current_f current_b
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  quote (
                                                                                    
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                set! closed_f (
                                                                                  append closed_f (
                                                                                    _list (
                                                                                      hash-table-ref current_f "pos"
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                set! closed_b (
                                                                                  append closed_b (
                                                                                    _list (
                                                                                      hash-table-ref current_b "pos"
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                let (
                                                                                  (
                                                                                    succ_f (
                                                                                      get_successors (
                                                                                        hash-table-ref current_f "pos"
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
                                                                                            break45
                                                                                          )
                                                                                           (
                                                                                            letrec (
                                                                                              (
                                                                                                loop44 (
                                                                                                  lambda (
                                                                                                    
                                                                                                  )
                                                                                                   (
                                                                                                    if (
                                                                                                      < i (
                                                                                                        _len succ_f
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      begin (
                                                                                                        let (
                                                                                                          (
                                                                                                            pos (
                                                                                                              cond (
                                                                                                                (
                                                                                                                  string? succ_f
                                                                                                                )
                                                                                                                 (
                                                                                                                  _substring succ_f i (
                                                                                                                    + i 1
                                                                                                                  )
                                                                                                                )
                                                                                                              )
                                                                                                               (
                                                                                                                (
                                                                                                                  hash-table? succ_f
                                                                                                                )
                                                                                                                 (
                                                                                                                  hash-table-ref succ_f i
                                                                                                                )
                                                                                                              )
                                                                                                               (
                                                                                                                else (
                                                                                                                  list-ref succ_f i
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          begin (
                                                                                                            if (
                                                                                                              contains_pos closed_f pos
                                                                                                            )
                                                                                                             (
                                                                                                              begin (
                                                                                                                set! i (
                                                                                                                  + i 1
                                                                                                                )
                                                                                                              )
                                                                                                               (
                                                                                                                loop44
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
                                                                                                                tentative_g (
                                                                                                                  + (
                                                                                                                    hash-table-ref current_f "g_cost"
                                                                                                                  )
                                                                                                                   1
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              begin (
                                                                                                                let (
                                                                                                                  (
                                                                                                                    h (
                                                                                                                      heuristic pos (
                                                                                                                        hash-table-ref current_b "pos"
                                                                                                                      )
                                                                                                                    )
                                                                                                                  )
                                                                                                                )
                                                                                                                 (
                                                                                                                  begin (
                                                                                                                    let (
                                                                                                                      (
                                                                                                                        f (
                                                                                                                          _add (
                                                                                                                            + 0.0 tentative_g
                                                                                                                          )
                                                                                                                           h
                                                                                                                        )
                                                                                                                      )
                                                                                                                    )
                                                                                                                     (
                                                                                                                      begin (
                                                                                                                        let (
                                                                                                                          (
                                                                                                                            idx_open (
                                                                                                                              open_index_of_pos open_f pos
                                                                                                                            )
                                                                                                                          )
                                                                                                                        )
                                                                                                                         (
                                                                                                                          begin (
                                                                                                                            if (
                                                                                                                              or (
                                                                                                                                equal? idx_open (
                                                                                                                                  - 0 1
                                                                                                                                )
                                                                                                                              )
                                                                                                                               (
                                                                                                                                < tentative_g (
                                                                                                                                  hash-table-ref (
                                                                                                                                    list-ref open_f idx_open
                                                                                                                                  )
                                                                                                                                   "g_cost"
                                                                                                                                )
                                                                                                                              )
                                                                                                                            )
                                                                                                                             (
                                                                                                                              begin (
                                                                                                                                let (
                                                                                                                                  (
                                                                                                                                    new_path (
                                                                                                                                      append_pos_list (
                                                                                                                                        hash-table-ref current_f "path"
                                                                                                                                      )
                                                                                                                                       pos
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                )
                                                                                                                                 (
                                                                                                                                  begin (
                                                                                                                                    if (
                                                                                                                                      not (
                                                                                                                                        equal? idx_open (
                                                                                                                                          - 0 1
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      begin (
                                                                                                                                        set! open_f (
                                                                                                                                          remove_node_at open_f idx_open
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      quote (
                                                                                                                                        
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    set! open_f (
                                                                                                                                      append open_f (
                                                                                                                                        _list (
                                                                                                                                          alist->hash-table (
                                                                                                                                            _list (
                                                                                                                                              cons "pos" pos
                                                                                                                                            )
                                                                                                                                             (
                                                                                                                                              cons "g_cost" tentative_g
                                                                                                                                            )
                                                                                                                                             (
                                                                                                                                              cons "h_cost" h
                                                                                                                                            )
                                                                                                                                             (
                                                                                                                                              cons "f_cost" f
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
                                                                                                                    )
                                                                                                                  )
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        loop44
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
                                                                                              loop44
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        let (
                                                                                          (
                                                                                            succ_b (
                                                                                              get_successors (
                                                                                                hash-table-ref current_b "pos"
                                                                                              )
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
                                                                                                break47
                                                                                              )
                                                                                               (
                                                                                                letrec (
                                                                                                  (
                                                                                                    loop46 (
                                                                                                      lambda (
                                                                                                        
                                                                                                      )
                                                                                                       (
                                                                                                        if (
                                                                                                          < i (
                                                                                                            _len succ_b
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          begin (
                                                                                                            let (
                                                                                                              (
                                                                                                                pos (
                                                                                                                  cond (
                                                                                                                    (
                                                                                                                      string? succ_b
                                                                                                                    )
                                                                                                                     (
                                                                                                                      _substring succ_b i (
                                                                                                                        + i 1
                                                                                                                      )
                                                                                                                    )
                                                                                                                  )
                                                                                                                   (
                                                                                                                    (
                                                                                                                      hash-table? succ_b
                                                                                                                    )
                                                                                                                     (
                                                                                                                      hash-table-ref succ_b i
                                                                                                                    )
                                                                                                                  )
                                                                                                                   (
                                                                                                                    else (
                                                                                                                      list-ref succ_b i
                                                                                                                    )
                                                                                                                  )
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              begin (
                                                                                                                if (
                                                                                                                  contains_pos closed_b pos
                                                                                                                )
                                                                                                                 (
                                                                                                                  begin (
                                                                                                                    set! i (
                                                                                                                      + i 1
                                                                                                                    )
                                                                                                                  )
                                                                                                                   (
                                                                                                                    loop46
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
                                                                                                                    tentative_g (
                                                                                                                      + (
                                                                                                                        hash-table-ref current_b "g_cost"
                                                                                                                      )
                                                                                                                       1
                                                                                                                    )
                                                                                                                  )
                                                                                                                )
                                                                                                                 (
                                                                                                                  begin (
                                                                                                                    let (
                                                                                                                      (
                                                                                                                        h (
                                                                                                                          heuristic pos (
                                                                                                                            hash-table-ref current_f "pos"
                                                                                                                          )
                                                                                                                        )
                                                                                                                      )
                                                                                                                    )
                                                                                                                     (
                                                                                                                      begin (
                                                                                                                        let (
                                                                                                                          (
                                                                                                                            f (
                                                                                                                              _add (
                                                                                                                                + 0.0 tentative_g
                                                                                                                              )
                                                                                                                               h
                                                                                                                            )
                                                                                                                          )
                                                                                                                        )
                                                                                                                         (
                                                                                                                          begin (
                                                                                                                            let (
                                                                                                                              (
                                                                                                                                idx_open (
                                                                                                                                  open_index_of_pos open_b pos
                                                                                                                                )
                                                                                                                              )
                                                                                                                            )
                                                                                                                             (
                                                                                                                              begin (
                                                                                                                                if (
                                                                                                                                  or (
                                                                                                                                    equal? idx_open (
                                                                                                                                      - 0 1
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    < tentative_g (
                                                                                                                                      hash-table-ref (
                                                                                                                                        list-ref open_b idx_open
                                                                                                                                      )
                                                                                                                                       "g_cost"
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                )
                                                                                                                                 (
                                                                                                                                  begin (
                                                                                                                                    let (
                                                                                                                                      (
                                                                                                                                        new_path (
                                                                                                                                          append_pos_list (
                                                                                                                                            hash-table-ref current_b "path"
                                                                                                                                          )
                                                                                                                                           pos
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                     (
                                                                                                                                      begin (
                                                                                                                                        if (
                                                                                                                                          not (
                                                                                                                                            equal? idx_open (
                                                                                                                                              - 0 1
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                         (
                                                                                                                                          begin (
                                                                                                                                            set! open_b (
                                                                                                                                              remove_node_at open_b idx_open
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                         (
                                                                                                                                          quote (
                                                                                                                                            
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                       (
                                                                                                                                        set! open_b (
                                                                                                                                          append open_b (
                                                                                                                                            _list (
                                                                                                                                              alist->hash-table (
                                                                                                                                                _list (
                                                                                                                                                  cons "pos" pos
                                                                                                                                                )
                                                                                                                                                 (
                                                                                                                                                  cons "g_cost" tentative_g
                                                                                                                                                )
                                                                                                                                                 (
                                                                                                                                                  cons "h_cost" h
                                                                                                                                                )
                                                                                                                                                 (
                                                                                                                                                  cons "f_cost" f
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
                                                                                                                        )
                                                                                                                      )
                                                                                                                    )
                                                                                                                  )
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                           (
                                                                                                            loop46
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
                                                                                                  loop46
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
                                                                loop42
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
                                                      loop42
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                ret41 (
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
                        ret48
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
                              ret48 "[]"
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
                              s (
                                string-append (
                                  string-append (
                                    string-append (
                                      string-append "[(" (
                                        to-str-space (
                                          hash-table-ref (
                                            list-ref path 0
                                          )
                                           "y"
                                        )
                                      )
                                    )
                                     ", "
                                  )
                                   (
                                    to-str-space (
                                      hash-table-ref (
                                        list-ref path 0
                                      )
                                       "x"
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
                                      break50
                                    )
                                     (
                                      letrec (
                                        (
                                          loop49 (
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
                                                  set! s (
                                                    string-append (
                                                      string-append (
                                                        string-append (
                                                          string-append (
                                                            string-append s ", ("
                                                          )
                                                           (
                                                            to-str-space (
                                                              hash-table-ref (
                                                                list-ref path i
                                                              )
                                                               "y"
                                                            )
                                                          )
                                                        )
                                                         ", "
                                                      )
                                                       (
                                                        to-str-space (
                                                          hash-table-ref (
                                                            list-ref path i
                                                          )
                                                           "x"
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
                                                 (
                                                  loop49
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
                                        loop49
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
                                  ret48 s
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
                        alist->hash-table (
                          _list (
                            cons "y" 0
                          )
                           (
                            cons "x" 0
                          )
                        )
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          goal (
                            alist->hash-table (
                              _list (
                                cons "y" (
                                  - (
                                    _len grid
                                  )
                                   1
                                )
                              )
                               (
                                cons "x" (
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
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              path1 (
                                astar start goal
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
                                    bidirectional_astar start goal
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
      )
    )
     (
      let (
        (
          end52 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur53 (
              quotient (
                * (
                  - end52 start51
                )
                 1000000
              )
               jps54
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur53
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
