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
      start10 (
        current-jiffy
      )
    )
     (
      jps13 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        new_node state value
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            begin (
              hash-table-set! state "nodes" (
                append (
                  hash-table-ref state "nodes"
                )
                 (
                  _list (
                    alist->hash-table (
                      _list (
                        cons "data" value
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
              ret1 (
                - (
                  _len (
                    hash-table-ref state "nodes"
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
      define (
        insert state value
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            begin (
              if (
                equal? (
                  hash-table-ref state "root"
                )
                 (
                  - 1
                )
              )
               (
                begin (
                  hash-table-set! state "root" (
                    new_node state value
                  )
                )
                 (
                  ret2 (
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
                  current (
                    hash-table-ref state "root"
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      nodes (
                        hash-table-ref state "nodes"
                      )
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
                                  if #t (
                                    begin (
                                      let (
                                        (
                                          node (
                                            list-ref nodes current
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          if (
                                            < value (
                                              hash-table-ref node "data"
                                            )
                                          )
                                           (
                                            begin (
                                              if (
                                                equal? (
                                                  hash-table-ref node "left"
                                                )
                                                 (
                                                  - 1
                                                )
                                              )
                                               (
                                                begin (
                                                  hash-table-set! node "left" (
                                                    new_node state value
                                                  )
                                                )
                                                 (
                                                  list-set! nodes current node
                                                )
                                                 (
                                                  hash-table-set! state "nodes" nodes
                                                )
                                                 (
                                                  ret2 (
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
                                              set! current (
                                                hash-table-ref node "left"
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              if (
                                                equal? (
                                                  hash-table-ref node "right"
                                                )
                                                 (
                                                  - 1
                                                )
                                              )
                                               (
                                                begin (
                                                  hash-table-set! node "right" (
                                                    new_node state value
                                                  )
                                                )
                                                 (
                                                  list-set! nodes current node
                                                )
                                                 (
                                                  hash-table-set! state "nodes" nodes
                                                )
                                                 (
                                                  ret2 (
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
                                              set! current (
                                                hash-table-ref node "right"
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
        inorder state idx
      )
       (
        call/cc (
          lambda (
            ret5
          )
           (
            begin (
              if (
                equal? idx (
                  - 1
                )
              )
               (
                begin (
                  ret5 (
                    _list
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
                    list-ref (
                      hash-table-ref state "nodes"
                    )
                     idx
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      result (
                        inorder state (
                          hash-table-ref node "left"
                        )
                      )
                    )
                  )
                   (
                    begin (
                      set! result (
                        append result (
                          _list (
                            hash-table-ref node "data"
                          )
                        )
                      )
                    )
                     (
                      let (
                        (
                          right_part (
                            inorder state (
                              hash-table-ref node "right"
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
                                            < i (
                                              _len right_part
                                            )
                                          )
                                           (
                                            begin (
                                              set! result (
                                                append result (
                                                  _list (
                                                    cond (
                                                      (
                                                        string? right_part
                                                      )
                                                       (
                                                        _substring right_part i (
                                                          + i 1
                                                        )
                                                      )
                                                    )
                                                     (
                                                      (
                                                        hash-table? right_part
                                                      )
                                                       (
                                                        hash-table-ref right_part i
                                                      )
                                                    )
                                                     (
                                                      else (
                                                        list-ref right_part i
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
                              ret5 result
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
        make_tree
      )
       (
        call/cc (
          lambda (
            ret8
          )
           (
            let (
              (
                state (
                  alist->hash-table (
                    _list (
                      cons "nodes" (
                        _list
                      )
                    )
                     (
                      cons "root" (
                        - 1
                      )
                    )
                  )
                )
              )
            )
             (
              begin (
                insert state 15
              )
               (
                insert state 10
              )
               (
                insert state 25
              )
               (
                insert state 6
              )
               (
                insert state 14
              )
               (
                insert state 20
              )
               (
                insert state 60
              )
               (
                ret8 state
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
            ret9
          )
           (
            let (
              (
                state (
                  make_tree
                )
              )
            )
             (
              begin (
                _display (
                  if (
                    string? "Printing values of binary search tree in Inorder Traversal."
                  )
                   "Printing values of binary search tree in Inorder Traversal." (
                    to-str "Printing values of binary search tree in Inorder Traversal."
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
                      inorder state (
                        hash-table-ref state "root"
                      )
                    )
                  )
                   (
                    inorder state (
                      hash-table-ref state "root"
                    )
                  )
                   (
                    to-str (
                      inorder state (
                        hash-table-ref state "root"
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
     (
      let (
        (
          end11 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur12 (
              quotient (
                * (
                  - end11 start10
                )
                 1000000
              )
               jps13
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur12
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
