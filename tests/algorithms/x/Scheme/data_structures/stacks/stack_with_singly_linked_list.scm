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
      start8 (
        current-jiffy
      )
    )
     (
      jps11 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        empty_stack
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
                  cons "nodes" (
                    _list
                  )
                )
                 (
                  cons "top" (
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
        is_empty stack
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            ret2 (
              equal? (
                hash-table-ref stack "top"
              )
               (
                - 1
              )
            )
          )
        )
      )
    )
     (
      define (
        push stack item
      )
       (
        call/cc (
          lambda (
            ret3
          )
           (
            let (
              (
                new_node (
                  alist->hash-table (
                    _list (
                      cons "value" item
                    )
                     (
                      cons "next" (
                        hash-table-ref stack "top"
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
                    new_nodes (
                      hash-table-ref stack "nodes"
                    )
                  )
                )
                 (
                  begin (
                    set! new_nodes (
                      append new_nodes (
                        _list new_node
                      )
                    )
                  )
                   (
                    let (
                      (
                        new_top (
                          - (
                            _len new_nodes
                          )
                           1
                        )
                      )
                    )
                     (
                      begin (
                        ret3 (
                          alist->hash-table (
                            _list (
                              cons "nodes" new_nodes
                            )
                             (
                              cons "top" new_top
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
        pop stack
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            begin (
              if (
                equal? (
                  hash-table-ref stack "top"
                )
                 (
                  - 1
                )
              )
               (
                begin (
                  panic "pop from empty stack"
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
                      hash-table-ref stack "nodes"
                    )
                     (
                      hash-table-ref stack "top"
                    )
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      new_top (
                        hash-table-ref node "next"
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          new_stack (
                            alist->hash-table (
                              _list (
                                cons "nodes" (
                                  hash-table-ref stack "nodes"
                                )
                              )
                               (
                                cons "top" new_top
                              )
                            )
                          )
                        )
                      )
                       (
                        begin (
                          ret4 (
                            alist->hash-table (
                              _list (
                                cons "stack" new_stack
                              )
                               (
                                cons "value" (
                                  hash-table-ref node "value"
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
        peek stack
      )
       (
        call/cc (
          lambda (
            ret5
          )
           (
            begin (
              if (
                equal? (
                  hash-table-ref stack "top"
                )
                 (
                  - 1
                )
              )
               (
                begin (
                  panic "peek from empty stack"
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
                      hash-table-ref stack "nodes"
                    )
                     (
                      hash-table-ref stack "top"
                    )
                  )
                )
              )
               (
                begin (
                  ret5 (
                    hash-table-ref node "value"
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
        clear stack
      )
       (
        call/cc (
          lambda (
            ret6
          )
           (
            ret6 (
              alist->hash-table (
                _list (
                  cons "nodes" (
                    _list
                  )
                )
                 (
                  cons "top" (
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
        main
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            let (
              (
                stack (
                  empty_stack
                )
              )
            )
             (
              begin (
                _display (
                  if (
                    string? (
                      is_empty stack
                    )
                  )
                   (
                    is_empty stack
                  )
                   (
                    to-str (
                      is_empty stack
                    )
                  )
                )
              )
               (
                newline
              )
               (
                set! stack (
                  push stack "5"
                )
              )
               (
                set! stack (
                  push stack "9"
                )
              )
               (
                set! stack (
                  push stack "python"
                )
              )
               (
                _display (
                  if (
                    string? (
                      is_empty stack
                    )
                  )
                   (
                    is_empty stack
                  )
                   (
                    to-str (
                      is_empty stack
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
                    res (
                      pop stack
                    )
                  )
                )
                 (
                  begin (
                    set! stack (
                      hash-table-ref res "stack"
                    )
                  )
                   (
                    _display (
                      if (
                        string? (
                          hash-table-ref res "value"
                        )
                      )
                       (
                        hash-table-ref res "value"
                      )
                       (
                        to-str (
                          hash-table-ref res "value"
                        )
                      )
                    )
                  )
                   (
                    newline
                  )
                   (
                    set! stack (
                      push stack "algorithms"
                    )
                  )
                   (
                    set! res (
                      pop stack
                    )
                  )
                   (
                    set! stack (
                      hash-table-ref res "stack"
                    )
                  )
                   (
                    _display (
                      if (
                        string? (
                          hash-table-ref res "value"
                        )
                      )
                       (
                        hash-table-ref res "value"
                      )
                       (
                        to-str (
                          hash-table-ref res "value"
                        )
                      )
                    )
                  )
                   (
                    newline
                  )
                   (
                    set! res (
                      pop stack
                    )
                  )
                   (
                    set! stack (
                      hash-table-ref res "stack"
                    )
                  )
                   (
                    _display (
                      if (
                        string? (
                          hash-table-ref res "value"
                        )
                      )
                       (
                        hash-table-ref res "value"
                      )
                       (
                        to-str (
                          hash-table-ref res "value"
                        )
                      )
                    )
                  )
                   (
                    newline
                  )
                   (
                    set! res (
                      pop stack
                    )
                  )
                   (
                    set! stack (
                      hash-table-ref res "stack"
                    )
                  )
                   (
                    _display (
                      if (
                        string? (
                          hash-table-ref res "value"
                        )
                      )
                       (
                        hash-table-ref res "value"
                      )
                       (
                        to-str (
                          hash-table-ref res "value"
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
                          is_empty stack
                        )
                      )
                       (
                        is_empty stack
                      )
                       (
                        to-str (
                          is_empty stack
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
          end9 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur10 (
              quotient (
                * (
                  - end9 start8
                )
                 1000000
              )
               jps11
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur10
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
