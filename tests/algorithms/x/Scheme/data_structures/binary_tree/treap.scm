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
      start9 (
        current-jiffy
      )
    )
     (
      jps12 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      let (
        (
          NIL (
            - 0 1
          )
        )
      )
       (
        begin (
          let (
            (
              node_values (
                _list
              )
            )
          )
           (
            begin (
              let (
                (
                  node_priors (
                    _list
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      node_lefts (
                        _list
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          node_rights (
                            _list
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              seed 1
                            )
                          )
                           (
                            begin (
                              define (
                                random
                              )
                               (
                                call/cc (
                                  lambda (
                                    ret1
                                  )
                                   (
                                    begin (
                                      set! seed (
                                        _mod (
                                          + (
                                            * seed 13
                                          )
                                           7
                                        )
                                         100
                                      )
                                    )
                                     (
                                      ret1 (
                                        _div (
                                          + 0.0 seed
                                        )
                                         100.0
                                      )
                                    )
                                  )
                                )
                              )
                            )
                             (
                              define (
                                new_node value
                              )
                               (
                                call/cc (
                                  lambda (
                                    ret2
                                  )
                                   (
                                    begin (
                                      set! node_values (
                                        append node_values (
                                          _list value
                                        )
                                      )
                                    )
                                     (
                                      set! node_priors (
                                        append node_priors (
                                          _list (
                                            random
                                          )
                                        )
                                      )
                                    )
                                     (
                                      set! node_lefts (
                                        append node_lefts (
                                          _list NIL
                                        )
                                      )
                                    )
                                     (
                                      set! node_rights (
                                        append node_rights (
                                          _list NIL
                                        )
                                      )
                                    )
                                     (
                                      ret2 (
                                        - (
                                          _len node_values
                                        )
                                         1
                                      )
                                    )
                                  )
                                )
                              )
                            )
                             (
                              define (
                                split root value
                              )
                               (
                                call/cc (
                                  lambda (
                                    ret3
                                  )
                                   (
                                    begin (
                                      if (
                                        equal? root NIL
                                      )
                                       (
                                        begin (
                                          ret3 (
                                            alist->hash-table (
                                              _list (
                                                cons "left" NIL
                                              )
                                               (
                                                cons "right" NIL
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
                                      if (
                                        < value (
                                          list-ref node_values root
                                        )
                                      )
                                       (
                                        begin (
                                          let (
                                            (
                                              res (
                                                split (
                                                  list-ref node_lefts root
                                                )
                                                 value
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              list-set! node_lefts root (
                                                hash-table-ref res "right"
                                              )
                                            )
                                             (
                                              ret3 (
                                                alist->hash-table (
                                                  _list (
                                                    cons "left" (
                                                      hash-table-ref res "left"
                                                    )
                                                  )
                                                   (
                                                    cons "right" root
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
                                      let (
                                        (
                                          res (
                                            split (
                                              list-ref node_rights root
                                            )
                                             value
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          list-set! node_rights root (
                                            hash-table-ref res "left"
                                          )
                                        )
                                         (
                                          ret3 (
                                            alist->hash-table (
                                              _list (
                                                cons "left" root
                                              )
                                               (
                                                cons "right" (
                                                  hash-table-ref res "right"
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
                                merge left right
                              )
                               (
                                call/cc (
                                  lambda (
                                    ret4
                                  )
                                   (
                                    begin (
                                      if (
                                        equal? left NIL
                                      )
                                       (
                                        begin (
                                          ret4 right
                                        )
                                      )
                                       (
                                        quote (
                                          
                                        )
                                      )
                                    )
                                     (
                                      if (
                                        equal? right NIL
                                      )
                                       (
                                        begin (
                                          ret4 left
                                        )
                                      )
                                       (
                                        quote (
                                          
                                        )
                                      )
                                    )
                                     (
                                      if (
                                        < (
                                          list-ref node_priors left
                                        )
                                         (
                                          list-ref node_priors right
                                        )
                                      )
                                       (
                                        begin (
                                          list-set! node_rights left (
                                            merge (
                                              list-ref node_rights left
                                            )
                                             right
                                          )
                                        )
                                         (
                                          ret4 left
                                        )
                                      )
                                       (
                                        quote (
                                          
                                        )
                                      )
                                    )
                                     (
                                      list-set! node_lefts right (
                                        merge left (
                                          list-ref node_lefts right
                                        )
                                      )
                                    )
                                     (
                                      ret4 right
                                    )
                                  )
                                )
                              )
                            )
                             (
                              define (
                                insert root value
                              )
                               (
                                call/cc (
                                  lambda (
                                    ret5
                                  )
                                   (
                                    let (
                                      (
                                        node (
                                          new_node value
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            res (
                                              split root value
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            ret5 (
                                              merge (
                                                merge (
                                                  hash-table-ref res "left"
                                                )
                                                 node
                                              )
                                               (
                                                hash-table-ref res "right"
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
                                erase root value
                              )
                               (
                                call/cc (
                                  lambda (
                                    ret6
                                  )
                                   (
                                    let (
                                      (
                                        res1 (
                                          split root (
                                            - value 1
                                          )
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            res2 (
                                              split (
                                                hash-table-ref res1 "right"
                                              )
                                               value
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            ret6 (
                                              merge (
                                                hash-table-ref res1 "left"
                                              )
                                               (
                                                hash-table-ref res2 "right"
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
                                inorder i acc
                              )
                               (
                                call/cc (
                                  lambda (
                                    ret7
                                  )
                                   (
                                    begin (
                                      if (
                                        equal? i NIL
                                      )
                                       (
                                        begin (
                                          ret7 acc
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
                                          left_acc (
                                            inorder (
                                              list-ref node_lefts i
                                            )
                                             acc
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          let (
                                            (
                                              with_node (
                                                append left_acc (
                                                  _list (
                                                    list-ref node_values i
                                                  )
                                                )
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              ret7 (
                                                inorder (
                                                  list-ref node_rights i
                                                )
                                                 with_node
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
                                    ret8
                                  )
                                   (
                                    let (
                                      (
                                        root NIL
                                      )
                                    )
                                     (
                                      begin (
                                        set! root (
                                          insert root 1
                                        )
                                      )
                                       (
                                        _display (
                                          if (
                                            string? (
                                              to-str-space (
                                                inorder root (
                                                  _list
                                                )
                                              )
                                            )
                                          )
                                           (
                                            to-str-space (
                                              inorder root (
                                                _list
                                              )
                                            )
                                          )
                                           (
                                            to-str (
                                              to-str-space (
                                                inorder root (
                                                  _list
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
                                        set! root (
                                          insert root 3
                                        )
                                      )
                                       (
                                        set! root (
                                          insert root 5
                                        )
                                      )
                                       (
                                        set! root (
                                          insert root 17
                                        )
                                      )
                                       (
                                        set! root (
                                          insert root 19
                                        )
                                      )
                                       (
                                        set! root (
                                          insert root 2
                                        )
                                      )
                                       (
                                        set! root (
                                          insert root 16
                                        )
                                      )
                                       (
                                        set! root (
                                          insert root 4
                                        )
                                      )
                                       (
                                        set! root (
                                          insert root 0
                                        )
                                      )
                                       (
                                        _display (
                                          if (
                                            string? (
                                              to-str-space (
                                                inorder root (
                                                  _list
                                                )
                                              )
                                            )
                                          )
                                           (
                                            to-str-space (
                                              inorder root (
                                                _list
                                              )
                                            )
                                          )
                                           (
                                            to-str (
                                              to-str-space (
                                                inorder root (
                                                  _list
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
                                        set! root (
                                          insert root 4
                                        )
                                      )
                                       (
                                        set! root (
                                          insert root 4
                                        )
                                      )
                                       (
                                        set! root (
                                          insert root 4
                                        )
                                      )
                                       (
                                        _display (
                                          if (
                                            string? (
                                              to-str-space (
                                                inorder root (
                                                  _list
                                                )
                                              )
                                            )
                                          )
                                           (
                                            to-str-space (
                                              inorder root (
                                                _list
                                              )
                                            )
                                          )
                                           (
                                            to-str (
                                              to-str-space (
                                                inorder root (
                                                  _list
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
                                        set! root (
                                          erase root 0
                                        )
                                      )
                                       (
                                        _display (
                                          if (
                                            string? (
                                              to-str-space (
                                                inorder root (
                                                  _list
                                                )
                                              )
                                            )
                                          )
                                           (
                                            to-str-space (
                                              inorder root (
                                                _list
                                              )
                                            )
                                          )
                                           (
                                            to-str (
                                              to-str-space (
                                                inorder root (
                                                  _list
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
                                        set! root (
                                          erase root 4
                                        )
                                      )
                                       (
                                        _display (
                                          if (
                                            string? (
                                              to-str-space (
                                                inorder root (
                                                  _list
                                                )
                                              )
                                            )
                                          )
                                           (
                                            to-str-space (
                                              inorder root (
                                                _list
                                              )
                                            )
                                          )
                                           (
                                            to-str (
                                              to-str-space (
                                                inorder root (
                                                  _list
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
                            )
                             (
                              main
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
          end10 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur11 (
              quotient (
                * (
                  - end10 start9
                )
                 1000000
              )
               jps12
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur11
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
