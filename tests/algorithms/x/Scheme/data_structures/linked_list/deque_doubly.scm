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
      start12 (
        current-jiffy
      )
    )
     (
      jps15 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        new_deque
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                nodes (
                  _list
                )
              )
            )
             (
              begin (
                set! nodes (
                  append nodes (
                    _list (
                      alist->hash-table (
                        _list (
                          cons "data" ""
                        )
                         (
                          cons "prev" (
                            - 1
                          )
                        )
                         (
                          cons "next" 1
                        )
                      )
                    )
                  )
                )
              )
               (
                set! nodes (
                  append nodes (
                    _list (
                      alist->hash-table (
                        _list (
                          cons "data" ""
                        )
                         (
                          cons "prev" 0
                        )
                         (
                          cons "next" (
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
                  alist->hash-table (
                    _list (
                      cons "nodes" nodes
                    )
                     (
                      cons "header" 0
                    )
                     (
                      cons "trailer" 1
                    )
                     (
                      cons "size" 0
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
        is_empty d
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            ret2 (
              equal? (
                hash-table-ref d "size"
              )
               0
            )
          )
        )
      )
    )
     (
      define (
        front d
      )
       (
        call/cc (
          lambda (
            ret3
          )
           (
            begin (
              if (
                is_empty d
              )
               (
                begin (
                  panic "List is empty"
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
                  head (
                    list-ref (
                      hash-table-ref d "nodes"
                    )
                     (
                      hash-table-ref d "header"
                    )
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      idx (
                        hash-table-ref head "next"
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          node (
                            list-ref (
                              hash-table-ref d "nodes"
                            )
                             idx
                          )
                        )
                      )
                       (
                        begin (
                          ret3 (
                            hash-table-ref node "data"
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
        back d
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            begin (
              if (
                is_empty d
              )
               (
                begin (
                  panic "List is empty"
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
                  tail (
                    list-ref (
                      hash-table-ref d "nodes"
                    )
                     (
                      hash-table-ref d "trailer"
                    )
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      idx (
                        hash-table-ref tail "prev"
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          node (
                            list-ref (
                              hash-table-ref d "nodes"
                            )
                             idx
                          )
                        )
                      )
                       (
                        begin (
                          ret4 (
                            hash-table-ref node "data"
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
        insert d pred value succ
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
                  hash-table-ref d "nodes"
                )
              )
            )
             (
              begin (
                let (
                  (
                    new_idx (
                      _len nodes
                    )
                  )
                )
                 (
                  begin (
                    set! nodes (
                      append nodes (
                        _list (
                          alist->hash-table (
                            _list (
                              cons "data" value
                            )
                             (
                              cons "prev" pred
                            )
                             (
                              cons "next" succ
                            )
                          )
                        )
                      )
                    )
                  )
                   (
                    let (
                      (
                        pred_node (
                          list-ref nodes pred
                        )
                      )
                    )
                     (
                      begin (
                        hash-table-set! pred_node "next" new_idx
                      )
                       (
                        list-set! nodes pred pred_node
                      )
                       (
                        let (
                          (
                            succ_node (
                              list-ref nodes succ
                            )
                          )
                        )
                         (
                          begin (
                            hash-table-set! succ_node "prev" new_idx
                          )
                           (
                            list-set! nodes succ succ_node
                          )
                           (
                            hash-table-set! d "nodes" nodes
                          )
                           (
                            hash-table-set! d "size" (
                              + (
                                hash-table-ref d "size"
                              )
                               1
                            )
                          )
                           (
                            ret5 d
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
        delete d idx
      )
       (
        call/cc (
          lambda (
            ret6
          )
           (
            let (
              (
                nodes (
                  hash-table-ref d "nodes"
                )
              )
            )
             (
              begin (
                let (
                  (
                    node (
                      list-ref nodes idx
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        pred (
                          hash-table-ref node "prev"
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            succ (
                              hash-table-ref node "next"
                            )
                          )
                        )
                         (
                          begin (
                            let (
                              (
                                pred_node (
                                  list-ref nodes pred
                                )
                              )
                            )
                             (
                              begin (
                                hash-table-set! pred_node "next" succ
                              )
                               (
                                list-set! nodes pred pred_node
                              )
                               (
                                let (
                                  (
                                    succ_node (
                                      list-ref nodes succ
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    hash-table-set! succ_node "prev" pred
                                  )
                                   (
                                    list-set! nodes succ succ_node
                                  )
                                   (
                                    let (
                                      (
                                        val (
                                          hash-table-ref node "data"
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        hash-table-set! d "nodes" nodes
                                      )
                                       (
                                        hash-table-set! d "size" (
                                          - (
                                            hash-table-ref d "size"
                                          )
                                           1
                                        )
                                      )
                                       (
                                        ret6 (
                                          alist->hash-table (
                                            _list (
                                              cons "deque" d
                                            )
                                             (
                                              cons "value" val
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
        add_first d value
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            let (
              (
                head (
                  list-ref (
                    hash-table-ref d "nodes"
                  )
                   (
                    hash-table-ref d "header"
                  )
                )
              )
            )
             (
              begin (
                let (
                  (
                    succ (
                      hash-table-ref head "next"
                    )
                  )
                )
                 (
                  begin (
                    ret7 (
                      insert d (
                        hash-table-ref d "header"
                      )
                       value succ
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
        add_last d value
      )
       (
        call/cc (
          lambda (
            ret8
          )
           (
            let (
              (
                tail (
                  list-ref (
                    hash-table-ref d "nodes"
                  )
                   (
                    hash-table-ref d "trailer"
                  )
                )
              )
            )
             (
              begin (
                let (
                  (
                    pred (
                      hash-table-ref tail "prev"
                    )
                  )
                )
                 (
                  begin (
                    ret8 (
                      insert d pred value (
                        hash-table-ref d "trailer"
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
        remove_first d
      )
       (
        call/cc (
          lambda (
            ret9
          )
           (
            begin (
              if (
                is_empty d
              )
               (
                begin (
                  panic "remove_first from empty list"
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
                  head (
                    list-ref (
                      hash-table-ref d "nodes"
                    )
                     (
                      hash-table-ref d "header"
                    )
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      idx (
                        hash-table-ref head "next"
                      )
                    )
                  )
                   (
                    begin (
                      ret9 (
                        delete d idx
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
        remove_last d
      )
       (
        call/cc (
          lambda (
            ret10
          )
           (
            begin (
              if (
                is_empty d
              )
               (
                begin (
                  panic "remove_first from empty list"
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
                  tail (
                    list-ref (
                      hash-table-ref d "nodes"
                    )
                     (
                      hash-table-ref d "trailer"
                    )
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      idx (
                        hash-table-ref tail "prev"
                      )
                    )
                  )
                   (
                    begin (
                      ret10 (
                        delete d idx
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
            ret11
          )
           (
            let (
              (
                d (
                  new_deque
                )
              )
            )
             (
              begin (
                set! d (
                  add_first d "A"
                )
              )
               (
                _display (
                  if (
                    string? (
                      front d
                    )
                  )
                   (
                    front d
                  )
                   (
                    to-str (
                      front d
                    )
                  )
                )
              )
               (
                newline
              )
               (
                set! d (
                  add_last d "B"
                )
              )
               (
                _display (
                  if (
                    string? (
                      back d
                    )
                  )
                   (
                    back d
                  )
                   (
                    to-str (
                      back d
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
                    r (
                      remove_first d
                    )
                  )
                )
                 (
                  begin (
                    set! d (
                      hash-table-ref r "deque"
                    )
                  )
                   (
                    _display (
                      if (
                        string? (
                          hash-table-ref r "value"
                        )
                      )
                       (
                        hash-table-ref r "value"
                      )
                       (
                        to-str (
                          hash-table-ref r "value"
                        )
                      )
                    )
                  )
                   (
                    newline
                  )
                   (
                    set! r (
                      remove_last d
                    )
                  )
                   (
                    set! d (
                      hash-table-ref r "deque"
                    )
                  )
                   (
                    _display (
                      if (
                        string? (
                          hash-table-ref r "value"
                        )
                      )
                       (
                        hash-table-ref r "value"
                      )
                       (
                        to-str (
                          hash-table-ref r "value"
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
                            is_empty d
                          )
                        )
                      )
                       (
                        to-str-space (
                          is_empty d
                        )
                      )
                       (
                        to-str (
                          to-str-space (
                            is_empty d
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
      )
    )
     (
      main
    )
     (
      let (
        (
          end13 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur14 (
              quotient (
                * (
                  - end13 start12
                )
                 1000000
              )
               jps15
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur14
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
