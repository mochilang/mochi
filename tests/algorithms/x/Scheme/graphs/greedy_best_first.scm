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
                    - 0 x
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
        manhattan x1 y1 x2 y2
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            ret2 (
              _add (
                abs (
                  - x1 x2
                )
              )
               (
                abs (
                  - y1 y2
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        clone_path p
      )
       (
        call/cc (
          lambda (
            ret3
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
                                  < i (
                                    _len p
                                  )
                                )
                                 (
                                  begin (
                                    set! res (
                                      append res (
                                        _list (
                                          list-ref p i
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
                    ret3 res
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
        make_node pos_x pos_y goal_x goal_y g_cost path
      )
       (
        call/cc (
          lambda (
            ret6
          )
           (
            let (
              (
                f (
                  manhattan pos_x pos_y goal_x goal_y
                )
              )
            )
             (
              begin (
                ret6 (
                  alist->hash-table (
                    _list (
                      cons "pos_x" pos_x
                    )
                     (
                      cons "pos_y" pos_y
                    )
                     (
                      cons "goal_x" goal_x
                    )
                     (
                      cons "goal_y" goal_y
                    )
                     (
                      cons "g_cost" g_cost
                    )
                     (
                      cons "f_cost" f
                    )
                     (
                      cons "path" path
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
          delta (
            _list (
              alist->hash-table (
                _list (
                  cons "y" (
                    - 1
                  )
                )
                 (
                  cons "x" 0
                )
              )
            )
             (
              alist->hash-table (
                _list (
                  cons "y" 0
                )
                 (
                  cons "x" (
                    - 1
                  )
                )
              )
            )
             (
              alist->hash-table (
                _list (
                  cons "y" 1
                )
                 (
                  cons "x" 0
                )
              )
            )
             (
              alist->hash-table (
                _list (
                  cons "y" 0
                )
                 (
                  cons "x" 1
                )
              )
            )
          )
        )
      )
       (
        begin (
          define (
            node_equal a b
          )
           (
            call/cc (
              lambda (
                ret7
              )
               (
                ret7 (
                  and (
                    equal? (
                      hash-table-ref a "pos_x"
                    )
                     (
                      hash-table-ref b "pos_x"
                    )
                  )
                   (
                    equal? (
                      hash-table-ref a "pos_y"
                    )
                     (
                      hash-table-ref b "pos_y"
                    )
                  )
                )
              )
            )
          )
        )
         (
          define (
            contains nodes node
          )
           (
            call/cc (
              lambda (
                ret8
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
                                    _len nodes
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      node_equal (
                                        list-ref nodes i
                                      )
                                       node
                                    )
                                     (
                                      begin (
                                        ret8 #t
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
                   (
                    ret8 #f
                  )
                )
              )
            )
          )
        )
         (
          define (
            sort_nodes nodes
          )
           (
            call/cc (
              lambda (
                ret11
              )
               (
                let (
                  (
                    arr nodes
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
                                        _len arr
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            key_node (
                                              list-ref arr i
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                j (
                                                  - i 1
                                                )
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
                                                              >= j 0
                                                            )
                                                             (
                                                              begin (
                                                                let (
                                                                  (
                                                                    temp (
                                                                      list-ref arr j
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    if (
                                                                      > (
                                                                        hash-table-ref temp "f_cost"
                                                                      )
                                                                       (
                                                                        hash-table-ref key_node "f_cost"
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        list-set! arr (
                                                                          + j 1
                                                                        )
                                                                         temp
                                                                      )
                                                                       (
                                                                        set! j (
                                                                          - j 1
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        break15 (
                                                                          quote (
                                                                            
                                                                          )
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
                                                list-set! arr (
                                                  + j 1
                                                )
                                                 key_node
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
                        ret11 arr
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
            get_successors grid parent target
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
                                        _len delta
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            d (
                                              list-ref delta i
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                pos_x (
                                                  + (
                                                    hash-table-ref parent "pos_x"
                                                  )
                                                   (
                                                    hash-table-ref d "x"
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    pos_y (
                                                      + (
                                                        hash-table-ref parent "pos_y"
                                                      )
                                                       (
                                                        hash-table-ref d "y"
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
                                                            and (
                                                              >= pos_x 0
                                                            )
                                                             (
                                                              < pos_x (
                                                                _len (
                                                                  list-ref grid 0
                                                                )
                                                              )
                                                            )
                                                          )
                                                           (
                                                            >= pos_y 0
                                                          )
                                                        )
                                                         (
                                                          < pos_y (
                                                            _len grid
                                                          )
                                                        )
                                                      )
                                                       (
                                                        equal? (
                                                          cond (
                                                            (
                                                              string? (
                                                                list-ref grid pos_y
                                                              )
                                                            )
                                                             (
                                                              _substring (
                                                                list-ref grid pos_y
                                                              )
                                                               pos_x (
                                                                + pos_x 1
                                                              )
                                                            )
                                                          )
                                                           (
                                                            (
                                                              hash-table? (
                                                                list-ref grid pos_y
                                                              )
                                                            )
                                                             (
                                                              hash-table-ref (
                                                                list-ref grid pos_y
                                                              )
                                                               pos_x
                                                            )
                                                          )
                                                           (
                                                            else (
                                                              list-ref (
                                                                list-ref grid pos_y
                                                              )
                                                               pos_x
                                                            )
                                                          )
                                                        )
                                                         0
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            new_path (
                                                              clone_path (
                                                                hash-table-ref parent "path"
                                                              )
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            set! new_path (
                                                              append new_path (
                                                                _list (
                                                                  alist->hash-table (
                                                                    _list (
                                                                      cons "y" pos_y
                                                                    )
                                                                     (
                                                                      cons "x" pos_x
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                           (
                                                            set! res (
                                                              append res (
                                                                _list (
                                                                  make_node pos_x pos_y (
                                                                    hash-table-ref target "x"
                                                                  )
                                                                   (
                                                                    hash-table-ref target "y"
                                                                  )
                                                                   (
                                                                    + (
                                                                      hash-table-ref parent "g_cost"
                                                                    )
                                                                     1
                                                                  )
                                                                   new_path
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
            greedy_best_first grid init goal
          )
           (
            call/cc (
              lambda (
                ret19
              )
               (
                let (
                  (
                    start_path (
                      _list init
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        start (
                          make_node (
                            hash-table-ref init "x"
                          )
                           (
                            hash-table-ref init "y"
                          )
                           (
                            hash-table-ref goal "x"
                          )
                           (
                            hash-table-ref goal "y"
                          )
                           0 start_path
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            open_nodes (
                              _list start
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                closed_nodes (
                                  _list
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
                                              > (
                                                _len open_nodes
                                              )
                                               0
                                            )
                                             (
                                              begin (
                                                set! open_nodes (
                                                  sort_nodes open_nodes
                                                )
                                              )
                                               (
                                                let (
                                                  (
                                                    current (
                                                      list-ref open_nodes 0
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        new_open (
                                                          _list
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            idx 1
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            call/cc (
                                                              lambda (
                                                                break23
                                                              )
                                                               (
                                                                letrec (
                                                                  (
                                                                    loop22 (
                                                                      lambda (
                                                                        
                                                                      )
                                                                       (
                                                                        if (
                                                                          < idx (
                                                                            _len open_nodes
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            set! new_open (
                                                                              append new_open (
                                                                                _list (
                                                                                  list-ref open_nodes idx
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            set! idx (
                                                                              + idx 1
                                                                            )
                                                                          )
                                                                           (
                                                                            loop22
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
                                                                  loop22
                                                                )
                                                              )
                                                            )
                                                          )
                                                           (
                                                            set! open_nodes new_open
                                                          )
                                                           (
                                                            if (
                                                              and (
                                                                equal? (
                                                                  hash-table-ref current "pos_x"
                                                                )
                                                                 (
                                                                  hash-table-ref goal "x"
                                                                )
                                                              )
                                                               (
                                                                equal? (
                                                                  hash-table-ref current "pos_y"
                                                                )
                                                                 (
                                                                  hash-table-ref goal "y"
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                ret19 (
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
                                                            set! closed_nodes (
                                                              append closed_nodes (
                                                                _list current
                                                              )
                                                            )
                                                          )
                                                           (
                                                            let (
                                                              (
                                                                successors (
                                                                  get_successors grid current goal
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
                                                                        break25
                                                                      )
                                                                       (
                                                                        letrec (
                                                                          (
                                                                            loop24 (
                                                                              lambda (
                                                                                
                                                                              )
                                                                               (
                                                                                if (
                                                                                  < i (
                                                                                    _len successors
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    let (
                                                                                      (
                                                                                        child (
                                                                                          cond (
                                                                                            (
                                                                                              string? successors
                                                                                            )
                                                                                             (
                                                                                              _substring successors i (
                                                                                                + i 1
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            (
                                                                                              hash-table? successors
                                                                                            )
                                                                                             (
                                                                                              hash-table-ref successors i
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            else (
                                                                                              list-ref successors i
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        if (
                                                                                          and (
                                                                                            not (
                                                                                              contains closed_nodes child
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            not (
                                                                                              contains open_nodes child
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            set! open_nodes (
                                                                                              append open_nodes (
                                                                                                _list child
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
                                                                                    loop24
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
                                                                          loop24
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
                                let (
                                  (
                                    r (
                                      _list init
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    ret19 r
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
              TEST_GRIDS (
                _list (
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
                 (
                  _list (
                    _list 0 0 0 1 1 0 0
                  )
                   (
                    _list 0 0 0 0 1 0 1
                  )
                   (
                    _list 0 0 0 1 1 0 0
                  )
                   (
                    _list 0 1 0 0 1 0 0
                  )
                   (
                    _list 1 0 0 1 1 0 1
                  )
                   (
                    _list 0 0 0 0 0 0 0
                  )
                )
                 (
                  _list (
                    _list 0 0 1 0 0
                  )
                   (
                    _list 0 1 0 0 0
                  )
                   (
                    _list 0 0 1 0 1
                  )
                   (
                    _list 1 0 0 1 1
                  )
                   (
                    _list 0 0 0 0 0
                  )
                )
              )
            )
          )
           (
            begin (
              define (
                print_grid grid
              )
               (
                call/cc (
                  lambda (
                    ret26
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
                                      < i (
                                        _len grid
                                      )
                                    )
                                     (
                                      begin (
                                        _display (
                                          if (
                                            string? (
                                              to-str-space (
                                                list-ref grid i
                                              )
                                            )
                                          )
                                           (
                                            to-str-space (
                                              list-ref grid i
                                            )
                                          )
                                           (
                                            to-str (
                                              to-str-space (
                                                list-ref grid i
                                              )
                                            )
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
             (
              define (
                main
              )
               (
                call/cc (
                  lambda (
                    ret29
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
                            break31
                          )
                           (
                            letrec (
                              (
                                loop30 (
                                  lambda (
                                    
                                  )
                                   (
                                    if (
                                      < idx (
                                        _len TEST_GRIDS
                                      )
                                    )
                                     (
                                      begin (
                                        _display (
                                          if (
                                            string? (
                                              string-append (
                                                string-append "==grid-" (
                                                  to-str-space (
                                                    + idx 1
                                                  )
                                                )
                                              )
                                               "=="
                                            )
                                          )
                                           (
                                            string-append (
                                              string-append "==grid-" (
                                                to-str-space (
                                                  + idx 1
                                                )
                                              )
                                            )
                                             "=="
                                          )
                                           (
                                            to-str (
                                              string-append (
                                                string-append "==grid-" (
                                                  to-str-space (
                                                    + idx 1
                                                  )
                                                )
                                              )
                                               "=="
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
                                            grid (
                                              list-ref TEST_GRIDS idx
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                init (
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
                                                    print_grid grid
                                                  )
                                                   (
                                                    _display (
                                                      if (
                                                        string? "------"
                                                      )
                                                       "------" (
                                                        to-str "------"
                                                      )
                                                    )
                                                  )
                                                   (
                                                    newline
                                                  )
                                                   (
                                                    let (
                                                      (
                                                        path (
                                                          greedy_best_first grid init goal
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
                                                                          < j (
                                                                            _len path
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                p (
                                                                                  cond (
                                                                                    (
                                                                                      string? path
                                                                                    )
                                                                                     (
                                                                                      _substring path j (
                                                                                        + j 1
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    (
                                                                                      hash-table? path
                                                                                    )
                                                                                     (
                                                                                      hash-table-ref path j
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    else (
                                                                                      list-ref path j
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                list-set! (
                                                                                  list-ref grid (
                                                                                    hash-table-ref p "y"
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  hash-table-ref p "x"
                                                                                )
                                                                                 2
                                                                              )
                                                                               (
                                                                                set! j (
                                                                                  + j 1
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
                                                            print_grid grid
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
                                            )
                                          )
                                        )
                                      )
                                       (
                                        loop30
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
                              loop30
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
              main
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
