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
      start13 (
        current-jiffy
      )
    )
     (
      jps16 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define OP_NODE 0
    )
     (
      define OP_LEAF 1
    )
     (
      define (
        merge_two_binary_trees t1 t2
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            ret1 (
              let (
                (
                  match2 t1
                )
              )
               (
                if (
                  equal? (
                    hash-table-ref match2 "__tag"
                  )
                   OP_LEAF
                )
                 t2 (
                  if (
                    equal? (
                      hash-table-ref match2 "__tag"
                    )
                     OP_NODE
                  )
                   (
                    let (
                      (
                        l1 (
                          hash-table-ref match2 "left"
                        )
                      )
                       (
                        v1 (
                          hash-table-ref match2 "value"
                        )
                      )
                       (
                        r1 (
                          hash-table-ref match2 "right"
                        )
                      )
                    )
                     (
                      let (
                        (
                          match3 t2
                        )
                      )
                       (
                        if (
                          equal? (
                            hash-table-ref match3 "__tag"
                          )
                           OP_LEAF
                        )
                         t1 (
                          if (
                            equal? (
                              hash-table-ref match3 "__tag"
                            )
                             OP_NODE
                          )
                           (
                            let (
                              (
                                l2 (
                                  hash-table-ref match3 "left"
                                )
                              )
                               (
                                v2 (
                                  hash-table-ref match3 "value"
                                )
                              )
                               (
                                r2 (
                                  hash-table-ref match3 "right"
                                )
                              )
                            )
                             (
                              alist->hash-table (
                                _list (
                                  cons "__tag" OP_NODE
                                )
                                 (
                                  cons "left" (
                                    merge_two_binary_trees l1 l2
                                  )
                                )
                                 (
                                  cons "value" (
                                    + v1 v2
                                  )
                                )
                                 (
                                  cons "right" (
                                    merge_two_binary_trees r1 r2
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
        )
      )
    )
     (
      define (
        is_leaf t
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            ret4 (
              let (
                (
                  match5 t
                )
              )
               (
                if (
                  equal? (
                    hash-table-ref match5 "__tag"
                  )
                   OP_LEAF
                )
                 #t #f
              )
            )
          )
        )
      )
    )
     (
      define (
        get_left t
      )
       (
        call/cc (
          lambda (
            ret6
          )
           (
            ret6 (
              let (
                (
                  match7 t
                )
              )
               (
                if (
                  equal? (
                    hash-table-ref match7 "__tag"
                  )
                   OP_NODE
                )
                 (
                  let (
                    (
                      l (
                        hash-table-ref match7 "left"
                      )
                    )
                  )
                   l
                )
                 (
                  alist->hash-table (
                    _list (
                      cons "__tag" OP_LEAF
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
        get_right t
      )
       (
        call/cc (
          lambda (
            ret8
          )
           (
            ret8 (
              let (
                (
                  match9 t
                )
              )
               (
                if (
                  equal? (
                    hash-table-ref match9 "__tag"
                  )
                   OP_NODE
                )
                 (
                  let (
                    (
                      r (
                        hash-table-ref match9 "right"
                      )
                    )
                  )
                   r
                )
                 (
                  alist->hash-table (
                    _list (
                      cons "__tag" OP_LEAF
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
        get_value t
      )
       (
        call/cc (
          lambda (
            ret10
          )
           (
            ret10 (
              let (
                (
                  match11 t
                )
              )
               (
                if (
                  equal? (
                    hash-table-ref match11 "__tag"
                  )
                   OP_NODE
                )
                 (
                  let (
                    (
                      v (
                        hash-table-ref match11 "value"
                      )
                    )
                  )
                   v
                )
                 0
              )
            )
          )
        )
      )
    )
     (
      define (
        print_preorder t
      )
       (
        call/cc (
          lambda (
            ret12
          )
           (
            if (
              not (
                is_leaf t
              )
            )
             (
              begin (
                let (
                  (
                    v (
                      get_value t
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        l (
                          get_left t
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            r (
                              get_right t
                            )
                          )
                        )
                         (
                          begin (
                            _display (
                              if (
                                string? v
                              )
                               v (
                                to-str v
                              )
                            )
                          )
                           (
                            newline
                          )
                           (
                            print_preorder l
                          )
                           (
                            print_preorder r
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
        )
      )
    )
     (
      let (
        (
          tree1 (
            alist->hash-table (
              _list (
                cons "__tag" OP_NODE
              )
               (
                cons "left" (
                  alist->hash-table (
                    _list (
                      cons "__tag" OP_NODE
                    )
                     (
                      cons "left" (
                        alist->hash-table (
                          _list (
                            cons "__tag" OP_NODE
                          )
                           (
                            cons "left" (
                              alist->hash-table (
                                _list (
                                  cons "__tag" OP_LEAF
                                )
                              )
                            )
                          )
                           (
                            cons "value" 4
                          )
                           (
                            cons "right" (
                              alist->hash-table (
                                _list (
                                  cons "__tag" OP_LEAF
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                     (
                      cons "value" 2
                    )
                     (
                      cons "right" (
                        alist->hash-table (
                          _list (
                            cons "__tag" OP_LEAF
                          )
                        )
                      )
                    )
                  )
                )
              )
               (
                cons "value" 1
              )
               (
                cons "right" (
                  alist->hash-table (
                    _list (
                      cons "__tag" OP_NODE
                    )
                     (
                      cons "left" (
                        alist->hash-table (
                          _list (
                            cons "__tag" OP_LEAF
                          )
                        )
                      )
                    )
                     (
                      cons "value" 3
                    )
                     (
                      cons "right" (
                        alist->hash-table (
                          _list (
                            cons "__tag" OP_LEAF
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
        begin (
          let (
            (
              tree2 (
                alist->hash-table (
                  _list (
                    cons "__tag" OP_NODE
                  )
                   (
                    cons "left" (
                      alist->hash-table (
                        _list (
                          cons "__tag" OP_NODE
                        )
                         (
                          cons "left" (
                            alist->hash-table (
                              _list (
                                cons "__tag" OP_LEAF
                              )
                            )
                          )
                        )
                         (
                          cons "value" 4
                        )
                         (
                          cons "right" (
                            alist->hash-table (
                              _list (
                                cons "__tag" OP_NODE
                              )
                               (
                                cons "left" (
                                  alist->hash-table (
                                    _list (
                                      cons "__tag" OP_LEAF
                                    )
                                  )
                                )
                              )
                               (
                                cons "value" 9
                              )
                               (
                                cons "right" (
                                  alist->hash-table (
                                    _list (
                                      cons "__tag" OP_LEAF
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
                    cons "value" 2
                  )
                   (
                    cons "right" (
                      alist->hash-table (
                        _list (
                          cons "__tag" OP_NODE
                        )
                         (
                          cons "left" (
                            alist->hash-table (
                              _list (
                                cons "__tag" OP_LEAF
                              )
                            )
                          )
                        )
                         (
                          cons "value" 6
                        )
                         (
                          cons "right" (
                            alist->hash-table (
                              _list (
                                cons "__tag" OP_NODE
                              )
                               (
                                cons "left" (
                                  alist->hash-table (
                                    _list (
                                      cons "__tag" OP_LEAF
                                    )
                                  )
                                )
                              )
                               (
                                cons "value" 5
                              )
                               (
                                cons "right" (
                                  alist->hash-table (
                                    _list (
                                      cons "__tag" OP_LEAF
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
            begin (
              _display (
                if (
                  string? "Tree1 is:"
                )
                 "Tree1 is:" (
                  to-str "Tree1 is:"
                )
              )
            )
             (
              newline
            )
             (
              print_preorder tree1
            )
             (
              _display (
                if (
                  string? "Tree2 is:"
                )
                 "Tree2 is:" (
                  to-str "Tree2 is:"
                )
              )
            )
             (
              newline
            )
             (
              print_preorder tree2
            )
             (
              let (
                (
                  merged_tree (
                    merge_two_binary_trees tree1 tree2
                  )
                )
              )
               (
                begin (
                  _display (
                    if (
                      string? "Merged Tree is:"
                    )
                     "Merged Tree is:" (
                      to-str "Merged Tree is:"
                    )
                  )
                )
                 (
                  newline
                )
                 (
                  print_preorder merged_tree
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
          end14 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur15 (
              quotient (
                * (
                  - end14 start13
                )
                 1000000
              )
               jps16
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur15
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
