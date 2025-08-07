;; Generated on 2025-08-07 08:40 +0700
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
      start24 (
        current-jiffy
      )
    )
     (
      jps27 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        sort_nodes nodes
      )
       (
        call/cc (
          lambda (
            ret1
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
                                                                    hash-table-ref temp "key"
                                                                  )
                                                                   (
                                                                    hash-table-ref key_node "key"
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
                                                                    break5 (
                                                                      quote (
                                                                        
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
                    ret1 arr
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
        print_node n
      )
       (
        call/cc (
          lambda (
            ret6
          )
           (
            begin (
              _display (
                if (
                  string? (
                    string-append (
                      string-append (
                        string-append (
                          string-append "Node(key=" (
                            to-str-space (
                              hash-table-ref n "key"
                            )
                          )
                        )
                         ", freq="
                      )
                       (
                        to-str-space (
                          hash-table-ref n "freq"
                        )
                      )
                    )
                     ")"
                  )
                )
                 (
                  string-append (
                    string-append (
                      string-append (
                        string-append "Node(key=" (
                          to-str-space (
                            hash-table-ref n "key"
                          )
                        )
                      )
                       ", freq="
                    )
                     (
                      to-str-space (
                        hash-table-ref n "freq"
                      )
                    )
                  )
                   ")"
                )
                 (
                  to-str (
                    string-append (
                      string-append (
                        string-append (
                          string-append "Node(key=" (
                            to-str-space (
                              hash-table-ref n "key"
                            )
                          )
                        )
                         ", freq="
                      )
                       (
                        to-str-space (
                          hash-table-ref n "freq"
                        )
                      )
                    )
                     ")"
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
      define (
        print_binary_search_tree root keys i j parent is_left
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            begin (
              if (
                or (
                  or (
                    > i j
                  )
                   (
                    < i 0
                  )
                )
                 (
                  > j (
                    - (
                      _len root
                    )
                     1
                  )
                )
              )
               (
                begin (
                  ret7 (
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
                  node (
                    cond (
                      (
                        string? (
                          list-ref root i
                        )
                      )
                       (
                        _substring (
                          list-ref root i
                        )
                         j (
                          + j 1
                        )
                      )
                    )
                     (
                      (
                        hash-table? (
                          list-ref root i
                        )
                      )
                       (
                        hash-table-ref (
                          list-ref root i
                        )
                         j
                      )
                    )
                     (
                      else (
                        list-ref (
                          list-ref root i
                        )
                         j
                      )
                    )
                  )
                )
              )
               (
                begin (
                  if (
                    equal? parent (
                      - 1
                    )
                  )
                   (
                    begin (
                      _display (
                        if (
                          string? (
                            string-append (
                              to-str-space (
                                list-ref keys node
                              )
                            )
                             " is the root of the binary search tree."
                          )
                        )
                         (
                          string-append (
                            to-str-space (
                              list-ref keys node
                            )
                          )
                           " is the root of the binary search tree."
                        )
                         (
                          to-str (
                            string-append (
                              to-str-space (
                                list-ref keys node
                              )
                            )
                             " is the root of the binary search tree."
                          )
                        )
                      )
                    )
                     (
                      newline
                    )
                  )
                   (
                    begin (
                      if is_left (
                        begin (
                          _display (
                            if (
                              string? (
                                string-append (
                                  string-append (
                                    string-append (
                                      to-str-space (
                                        list-ref keys node
                                      )
                                    )
                                     " is the left child of key "
                                  )
                                   (
                                    to-str-space parent
                                  )
                                )
                                 "."
                              )
                            )
                             (
                              string-append (
                                string-append (
                                  string-append (
                                    to-str-space (
                                      list-ref keys node
                                    )
                                  )
                                   " is the left child of key "
                                )
                                 (
                                  to-str-space parent
                                )
                              )
                               "."
                            )
                             (
                              to-str (
                                string-append (
                                  string-append (
                                    string-append (
                                      to-str-space (
                                        list-ref keys node
                                      )
                                    )
                                     " is the left child of key "
                                  )
                                   (
                                    to-str-space parent
                                  )
                                )
                                 "."
                              )
                            )
                          )
                        )
                         (
                          newline
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
                                      to-str-space (
                                        list-ref keys node
                                      )
                                    )
                                     " is the right child of key "
                                  )
                                   (
                                    to-str-space parent
                                  )
                                )
                                 "."
                              )
                            )
                             (
                              string-append (
                                string-append (
                                  string-append (
                                    to-str-space (
                                      list-ref keys node
                                    )
                                  )
                                   " is the right child of key "
                                )
                                 (
                                  to-str-space parent
                                )
                              )
                               "."
                            )
                             (
                              to-str (
                                string-append (
                                  string-append (
                                    string-append (
                                      to-str-space (
                                        list-ref keys node
                                      )
                                    )
                                     " is the right child of key "
                                  )
                                   (
                                    to-str-space parent
                                  )
                                )
                                 "."
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
                  print_binary_search_tree root keys i (
                    - node 1
                  )
                   (
                    list-ref keys node
                  )
                   #t
                )
                 (
                  print_binary_search_tree root keys (
                    + node 1
                  )
                   j (
                    list-ref keys node
                  )
                   #f
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        find_optimal_binary_search_tree original_nodes
      )
       (
        call/cc (
          lambda (
            ret8
          )
           (
            let (
              (
                nodes (
                  sort_nodes original_nodes
                )
              )
            )
             (
              begin (
                let (
                  (
                    n (
                      _len nodes
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        keys (
                          _list
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            freqs (
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
                                              < i n
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    node (
                                                      list-ref nodes i
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    set! keys (
                                                      append keys (
                                                        _list (
                                                          hash-table-ref node "key"
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    set! freqs (
                                                      append freqs (
                                                        _list (
                                                          hash-table-ref node "freq"
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
                                let (
                                  (
                                    dp (
                                      _list
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        total (
                                          _list
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            root (
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
                                                          < i n
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                dp_row (
                                                                  _list
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                let (
                                                                  (
                                                                    total_row (
                                                                      _list
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        root_row (
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
                                                                                          < j n
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            if (
                                                                                              equal? i j
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                set! dp_row (
                                                                                                  append dp_row (
                                                                                                    _list (
                                                                                                      list-ref freqs i
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                set! total_row (
                                                                                                  append total_row (
                                                                                                    _list (
                                                                                                      list-ref freqs i
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                set! root_row (
                                                                                                  append root_row (
                                                                                                    _list i
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                set! dp_row (
                                                                                                  append dp_row (
                                                                                                    _list 0
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                set! total_row (
                                                                                                  append total_row (
                                                                                                    _list 0
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                set! root_row (
                                                                                                  append root_row (
                                                                                                    _list 0
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
                                                                            set! dp (
                                                                              append dp (
                                                                                _list dp_row
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            set! total (
                                                                              append total (
                                                                                _list total_row
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            set! root (
                                                                              append root (
                                                                                _list root_row
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
                                            let (
                                              (
                                                interval_length 2
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    INF 2147483647
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
                                                                  <= interval_length n
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! i 0
                                                                  )
                                                                   (
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
                                                                                    + (
                                                                                      - n interval_length
                                                                                    )
                                                                                     1
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    let (
                                                                                      (
                                                                                        j (
                                                                                          - (
                                                                                            + i interval_length
                                                                                          )
                                                                                           1
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        list-set! (
                                                                                          list-ref dp i
                                                                                        )
                                                                                         j INF
                                                                                      )
                                                                                       (
                                                                                        list-set! (
                                                                                          list-ref total i
                                                                                        )
                                                                                         j (
                                                                                          + (
                                                                                            cond (
                                                                                              (
                                                                                                string? (
                                                                                                  list-ref total i
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                _substring (
                                                                                                  list-ref total i
                                                                                                )
                                                                                                 (
                                                                                                  - j 1
                                                                                                )
                                                                                                 (
                                                                                                  + (
                                                                                                    - j 1
                                                                                                  )
                                                                                                   1
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              (
                                                                                                hash-table? (
                                                                                                  list-ref total i
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                hash-table-ref (
                                                                                                  list-ref total i
                                                                                                )
                                                                                                 (
                                                                                                  - j 1
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              else (
                                                                                                list-ref (
                                                                                                  list-ref total i
                                                                                                )
                                                                                                 (
                                                                                                  - j 1
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            list-ref freqs j
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        let (
                                                                                          (
                                                                                            r (
                                                                                              cond (
                                                                                                (
                                                                                                  string? (
                                                                                                    list-ref root i
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  _substring (
                                                                                                    list-ref root i
                                                                                                  )
                                                                                                   (
                                                                                                    - j 1
                                                                                                  )
                                                                                                   (
                                                                                                    + (
                                                                                                      - j 1
                                                                                                    )
                                                                                                     1
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                (
                                                                                                  hash-table? (
                                                                                                    list-ref root i
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  hash-table-ref (
                                                                                                    list-ref root i
                                                                                                  )
                                                                                                   (
                                                                                                    - j 1
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                else (
                                                                                                  list-ref (
                                                                                                    list-ref root i
                                                                                                  )
                                                                                                   (
                                                                                                    - j 1
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
                                                                                                          <= r (
                                                                                                            cond (
                                                                                                              (
                                                                                                                string? (
                                                                                                                  list-ref root (
                                                                                                                    + i 1
                                                                                                                  )
                                                                                                                )
                                                                                                              )
                                                                                                               (
                                                                                                                _substring (
                                                                                                                  list-ref root (
                                                                                                                    + i 1
                                                                                                                  )
                                                                                                                )
                                                                                                                 j (
                                                                                                                  + j 1
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              (
                                                                                                                hash-table? (
                                                                                                                  list-ref root (
                                                                                                                    + i 1
                                                                                                                  )
                                                                                                                )
                                                                                                              )
                                                                                                               (
                                                                                                                hash-table-ref (
                                                                                                                  list-ref root (
                                                                                                                    + i 1
                                                                                                                  )
                                                                                                                )
                                                                                                                 j
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              else (
                                                                                                                list-ref (
                                                                                                                  list-ref root (
                                                                                                                    + i 1
                                                                                                                  )
                                                                                                                )
                                                                                                                 j
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          begin (
                                                                                                            let (
                                                                                                              (
                                                                                                                left (
                                                                                                                  if (
                                                                                                                    not (
                                                                                                                      equal? r i
                                                                                                                    )
                                                                                                                  )
                                                                                                                   (
                                                                                                                    cond (
                                                                                                                      (
                                                                                                                        string? (
                                                                                                                          list-ref dp i
                                                                                                                        )
                                                                                                                      )
                                                                                                                       (
                                                                                                                        _substring (
                                                                                                                          list-ref dp i
                                                                                                                        )
                                                                                                                         (
                                                                                                                          - r 1
                                                                                                                        )
                                                                                                                         (
                                                                                                                          + (
                                                                                                                            - r 1
                                                                                                                          )
                                                                                                                           1
                                                                                                                        )
                                                                                                                      )
                                                                                                                    )
                                                                                                                     (
                                                                                                                      (
                                                                                                                        hash-table? (
                                                                                                                          list-ref dp i
                                                                                                                        )
                                                                                                                      )
                                                                                                                       (
                                                                                                                        hash-table-ref (
                                                                                                                          list-ref dp i
                                                                                                                        )
                                                                                                                         (
                                                                                                                          - r 1
                                                                                                                        )
                                                                                                                      )
                                                                                                                    )
                                                                                                                     (
                                                                                                                      else (
                                                                                                                        list-ref (
                                                                                                                          list-ref dp i
                                                                                                                        )
                                                                                                                         (
                                                                                                                          - r 1
                                                                                                                        )
                                                                                                                      )
                                                                                                                    )
                                                                                                                  )
                                                                                                                   0
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                             (
                                                                                                              begin (
                                                                                                                let (
                                                                                                                  (
                                                                                                                    right (
                                                                                                                      if (
                                                                                                                        not (
                                                                                                                          equal? r j
                                                                                                                        )
                                                                                                                      )
                                                                                                                       (
                                                                                                                        cond (
                                                                                                                          (
                                                                                                                            string? (
                                                                                                                              list-ref dp (
                                                                                                                                + r 1
                                                                                                                              )
                                                                                                                            )
                                                                                                                          )
                                                                                                                           (
                                                                                                                            _substring (
                                                                                                                              list-ref dp (
                                                                                                                                + r 1
                                                                                                                              )
                                                                                                                            )
                                                                                                                             j (
                                                                                                                              + j 1
                                                                                                                            )
                                                                                                                          )
                                                                                                                        )
                                                                                                                         (
                                                                                                                          (
                                                                                                                            hash-table? (
                                                                                                                              list-ref dp (
                                                                                                                                + r 1
                                                                                                                              )
                                                                                                                            )
                                                                                                                          )
                                                                                                                           (
                                                                                                                            hash-table-ref (
                                                                                                                              list-ref dp (
                                                                                                                                + r 1
                                                                                                                              )
                                                                                                                            )
                                                                                                                             j
                                                                                                                          )
                                                                                                                        )
                                                                                                                         (
                                                                                                                          else (
                                                                                                                            list-ref (
                                                                                                                              list-ref dp (
                                                                                                                                + r 1
                                                                                                                              )
                                                                                                                            )
                                                                                                                             j
                                                                                                                          )
                                                                                                                        )
                                                                                                                      )
                                                                                                                       0
                                                                                                                    )
                                                                                                                  )
                                                                                                                )
                                                                                                                 (
                                                                                                                  begin (
                                                                                                                    let (
                                                                                                                      (
                                                                                                                        cost (
                                                                                                                          + (
                                                                                                                            + left (
                                                                                                                              cond (
                                                                                                                                (
                                                                                                                                  string? (
                                                                                                                                    list-ref total i
                                                                                                                                  )
                                                                                                                                )
                                                                                                                                 (
                                                                                                                                  _substring (
                                                                                                                                    list-ref total i
                                                                                                                                  )
                                                                                                                                   j (
                                                                                                                                    + j 1
                                                                                                                                  )
                                                                                                                                )
                                                                                                                              )
                                                                                                                               (
                                                                                                                                (
                                                                                                                                  hash-table? (
                                                                                                                                    list-ref total i
                                                                                                                                  )
                                                                                                                                )
                                                                                                                                 (
                                                                                                                                  hash-table-ref (
                                                                                                                                    list-ref total i
                                                                                                                                  )
                                                                                                                                   j
                                                                                                                                )
                                                                                                                              )
                                                                                                                               (
                                                                                                                                else (
                                                                                                                                  list-ref (
                                                                                                                                    list-ref total i
                                                                                                                                  )
                                                                                                                                   j
                                                                                                                                )
                                                                                                                              )
                                                                                                                            )
                                                                                                                          )
                                                                                                                           right
                                                                                                                        )
                                                                                                                      )
                                                                                                                    )
                                                                                                                     (
                                                                                                                      begin (
                                                                                                                        if (
                                                                                                                          > (
                                                                                                                            cond (
                                                                                                                              (
                                                                                                                                string? (
                                                                                                                                  list-ref dp i
                                                                                                                                )
                                                                                                                              )
                                                                                                                               (
                                                                                                                                _substring (
                                                                                                                                  list-ref dp i
                                                                                                                                )
                                                                                                                                 j (
                                                                                                                                  + j 1
                                                                                                                                )
                                                                                                                              )
                                                                                                                            )
                                                                                                                             (
                                                                                                                              (
                                                                                                                                hash-table? (
                                                                                                                                  list-ref dp i
                                                                                                                                )
                                                                                                                              )
                                                                                                                               (
                                                                                                                                hash-table-ref (
                                                                                                                                  list-ref dp i
                                                                                                                                )
                                                                                                                                 j
                                                                                                                              )
                                                                                                                            )
                                                                                                                             (
                                                                                                                              else (
                                                                                                                                list-ref (
                                                                                                                                  list-ref dp i
                                                                                                                                )
                                                                                                                                 j
                                                                                                                              )
                                                                                                                            )
                                                                                                                          )
                                                                                                                           cost
                                                                                                                        )
                                                                                                                         (
                                                                                                                          begin (
                                                                                                                            list-set! (
                                                                                                                              list-ref dp i
                                                                                                                            )
                                                                                                                             j cost
                                                                                                                          )
                                                                                                                           (
                                                                                                                            list-set! (
                                                                                                                              list-ref root i
                                                                                                                            )
                                                                                                                             j r
                                                                                                                          )
                                                                                                                        )
                                                                                                                         (
                                                                                                                          quote (
                                                                                                                            
                                                                                                                          )
                                                                                                                        )
                                                                                                                      )
                                                                                                                       (
                                                                                                                        set! r (
                                                                                                                          + r 1
                                                                                                                        )
                                                                                                                      )
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
                                                                                            set! i (
                                                                                              + i 1
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
                                                                    set! interval_length (
                                                                      + interval_length 1
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
                                                    _display (
                                                      if (
                                                        string? "Binary search tree nodes:"
                                                      )
                                                       "Binary search tree nodes:" (
                                                        to-str "Binary search tree nodes:"
                                                      )
                                                    )
                                                  )
                                                   (
                                                    newline
                                                  )
                                                   (
                                                    set! i 0
                                                  )
                                                   (
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
                                                                  < i n
                                                                )
                                                                 (
                                                                  begin (
                                                                    print_node (
                                                                      list-ref nodes i
                                                                    )
                                                                  )
                                                                   (
                                                                    set! i (
                                                                      + i 1
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
                                                    _display (
                                                      if (
                                                        string? (
                                                          string-append (
                                                            string-append "\nThe cost of optimal BST for given tree nodes is " (
                                                              to-str-space (
                                                                cond (
                                                                  (
                                                                    string? (
                                                                      list-ref dp 0
                                                                    )
                                                                  )
                                                                   (
                                                                    _substring (
                                                                      list-ref dp 0
                                                                    )
                                                                     (
                                                                      - n 1
                                                                    )
                                                                     (
                                                                      + (
                                                                        - n 1
                                                                      )
                                                                       1
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  (
                                                                    hash-table? (
                                                                      list-ref dp 0
                                                                    )
                                                                  )
                                                                   (
                                                                    hash-table-ref (
                                                                      list-ref dp 0
                                                                    )
                                                                     (
                                                                      - n 1
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  else (
                                                                    list-ref (
                                                                      list-ref dp 0
                                                                    )
                                                                     (
                                                                      - n 1
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                           "."
                                                        )
                                                      )
                                                       (
                                                        string-append (
                                                          string-append "\nThe cost of optimal BST for given tree nodes is " (
                                                            to-str-space (
                                                              cond (
                                                                (
                                                                  string? (
                                                                    list-ref dp 0
                                                                  )
                                                                )
                                                                 (
                                                                  _substring (
                                                                    list-ref dp 0
                                                                  )
                                                                   (
                                                                    - n 1
                                                                  )
                                                                   (
                                                                    + (
                                                                      - n 1
                                                                    )
                                                                     1
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                (
                                                                  hash-table? (
                                                                    list-ref dp 0
                                                                  )
                                                                )
                                                                 (
                                                                  hash-table-ref (
                                                                    list-ref dp 0
                                                                  )
                                                                   (
                                                                    - n 1
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                else (
                                                                  list-ref (
                                                                    list-ref dp 0
                                                                  )
                                                                   (
                                                                    - n 1
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                         "."
                                                      )
                                                       (
                                                        to-str (
                                                          string-append (
                                                            string-append "\nThe cost of optimal BST for given tree nodes is " (
                                                              to-str-space (
                                                                cond (
                                                                  (
                                                                    string? (
                                                                      list-ref dp 0
                                                                    )
                                                                  )
                                                                   (
                                                                    _substring (
                                                                      list-ref dp 0
                                                                    )
                                                                     (
                                                                      - n 1
                                                                    )
                                                                     (
                                                                      + (
                                                                        - n 1
                                                                      )
                                                                       1
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  (
                                                                    hash-table? (
                                                                      list-ref dp 0
                                                                    )
                                                                  )
                                                                   (
                                                                    hash-table-ref (
                                                                      list-ref dp 0
                                                                    )
                                                                     (
                                                                      - n 1
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  else (
                                                                    list-ref (
                                                                      list-ref dp 0
                                                                    )
                                                                     (
                                                                      - n 1
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                           "."
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    newline
                                                  )
                                                   (
                                                    print_binary_search_tree root keys 0 (
                                                      - n 1
                                                    )
                                                     (
                                                      - 1
                                                    )
                                                     #f
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
    )
     (
      define (
        main
      )
       (
        call/cc (
          lambda (
            ret23
          )
           (
            let (
              (
                nodes (
                  _list (
                    alist->hash-table (
                      _list (
                        cons "key" 12
                      )
                       (
                        cons "freq" 8
                      )
                    )
                  )
                   (
                    alist->hash-table (
                      _list (
                        cons "key" 10
                      )
                       (
                        cons "freq" 34
                      )
                    )
                  )
                   (
                    alist->hash-table (
                      _list (
                        cons "key" 20
                      )
                       (
                        cons "freq" 50
                      )
                    )
                  )
                   (
                    alist->hash-table (
                      _list (
                        cons "key" 42
                      )
                       (
                        cons "freq" 3
                      )
                    )
                  )
                   (
                    alist->hash-table (
                      _list (
                        cons "key" 25
                      )
                       (
                        cons "freq" 40
                      )
                    )
                  )
                   (
                    alist->hash-table (
                      _list (
                        cons "key" 37
                      )
                       (
                        cons "freq" 30
                      )
                    )
                  )
                )
              )
            )
             (
              begin (
                find_optimal_binary_search_tree nodes
              )
            )
          )
        )
      )
    )
     (
      main
    )
     (
      let (
        (
          end25 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur26 (
              quotient (
                * (
                  - end25 start24
                )
                 1000000
              )
               jps27
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur26
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
