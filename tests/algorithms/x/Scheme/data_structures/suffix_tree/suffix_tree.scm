;; Generated on 2025-08-07 08:20 +0700
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
      start17 (
        current-jiffy
      )
    )
     (
      jps20 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        new_node
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            ret1 (
              alist->hash-table (
                _list (
                  cons "children" (
                    alist->hash-table (
                      _list
                    )
                  )
                )
                 (
                  cons "is_end_of_string" #f
                )
                 (
                  cons "start" (
                    - 1
                  )
                )
                 (
                  cons "end" (
                    - 1
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
        has_key m k
      )
       (
        call/cc (
          lambda (
            ret2
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
                                  key (
                                    car xs
                                  )
                                )
                              )
                               (
                                begin (
                                  if (
                                    string=? key k
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
                              )
                            )
                             (
                              loop3 (
                                cdr xs
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                   (
                    loop3 (
                      hash-table-keys m
                    )
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
     (
      define (
        add_suffix tree suffix index
      )
       (
        call/cc (
          lambda (
            ret5
          )
           (
            let (
              (
                nodes (
                  hash-table-ref tree "nodes"
                )
              )
            )
             (
              begin (
                let (
                  (
                    node_idx 0
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
                                      < j (
                                        _len suffix
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            ch (
                                              _substring suffix j (
                                                + j 1
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                node (
                                                  list-ref nodes node_idx
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    children (
                                                      hash-table-ref node "children"
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    if (
                                                      not (
                                                        has_key children ch
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        set! nodes (
                                                          append nodes (
                                                            _list (
                                                              new_node
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        let (
                                                          (
                                                            new_idx (
                                                              - (
                                                                _len nodes
                                                              )
                                                               1
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            hash-table-set! children ch new_idx
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
                                                    hash-table-set! node "children" children
                                                  )
                                                   (
                                                    list-set! nodes node_idx node
                                                  )
                                                   (
                                                    set! node_idx (
                                                      hash-table-ref/default children ch (
                                                        quote (
                                                          
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
                        let (
                          (
                            node (
                              list-ref nodes node_idx
                            )
                          )
                        )
                         (
                          begin (
                            hash-table-set! node "is_end_of_string" #t
                          )
                           (
                            hash-table-set! node "start" index
                          )
                           (
                            hash-table-set! node "end" (
                              - (
                                + index (
                                  _len suffix
                                )
                              )
                               1
                            )
                          )
                           (
                            list-set! nodes node_idx node
                          )
                           (
                            hash-table-set! tree "nodes" nodes
                          )
                           (
                            ret5 tree
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
        build_suffix_tree tree
      )
       (
        call/cc (
          lambda (
            ret8
          )
           (
            let (
              (
                text (
                  hash-table-ref tree "text"
                )
              )
            )
             (
              begin (
                let (
                  (
                    n (
                      _len text
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
                        let (
                          (
                            t tree
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
                                                suffix ""
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    k i
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
                                                                  < k n
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! suffix (
                                                                      string-append suffix (
                                                                        _substring text k (
                                                                          + k 1
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    set! k (
                                                                      + k 1
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
                                                    set! t (
                                                      add_suffix t suffix i
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
                            ret8 t
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
        new_suffix_tree text
      )
       (
        call/cc (
          lambda (
            ret13
          )
           (
            let (
              (
                tree (
                  alist->hash-table (
                    _list (
                      cons "text" text
                    )
                     (
                      cons "nodes" (
                        _list
                      )
                    )
                  )
                )
              )
            )
             (
              begin (
                hash-table-set! tree "nodes" (
                  append (
                    hash-table-ref tree "nodes"
                  )
                   (
                    _list (
                      new_node
                    )
                  )
                )
              )
               (
                set! tree (
                  build_suffix_tree tree
                )
              )
               (
                ret13 tree
              )
            )
          )
        )
      )
    )
     (
      define (
        search tree pattern
      )
       (
        call/cc (
          lambda (
            ret14
          )
           (
            let (
              (
                node_idx 0
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
                    let (
                      (
                        nodes (
                          hash-table-ref tree "nodes"
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
                                      < i (
                                        _len pattern
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            ch (
                                              _substring pattern i (
                                                + i 1
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                node (
                                                  list-ref nodes node_idx
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    children (
                                                      hash-table-ref node "children"
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    if (
                                                      not (
                                                        has_key children ch
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        ret14 #f
                                                      )
                                                    )
                                                     (
                                                      quote (
                                                        
                                                      )
                                                    )
                                                  )
                                                   (
                                                    set! node_idx (
                                                      hash-table-ref/default children ch (
                                                        quote (
                                                          
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
                        ret14 #t
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
          st (
            new_suffix_tree "bananas"
          )
        )
      )
       (
        begin (
          _display (
            if (
              string? (
                to-str-space (
                  search st "ana"
                )
              )
            )
             (
              to-str-space (
                search st "ana"
              )
            )
             (
              to-str (
                to-str-space (
                  search st "ana"
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
                  search st "apple"
                )
              )
            )
             (
              to-str-space (
                search st "apple"
              )
            )
             (
              to-str (
                to-str-space (
                  search st "apple"
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
     (
      let (
        (
          end18 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur19 (
              quotient (
                * (
                  - end18 start17
                )
                 1000000
              )
               jps20
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur19
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
