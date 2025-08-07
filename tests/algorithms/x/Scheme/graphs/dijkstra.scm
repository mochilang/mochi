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
      start25 (
        current-jiffy
      )
    )
     (
      jps28 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      let (
        (
          G (
            alist->hash-table (
              _list (
                cons "A" (
                  alist->hash-table (
                    _list (
                      cons "B" 2
                    )
                     (
                      cons "C" 5
                    )
                  )
                )
              )
               (
                cons "B" (
                  alist->hash-table (
                    _list (
                      cons "A" 2
                    )
                     (
                      cons "D" 3
                    )
                     (
                      cons "E" 1
                    )
                     (
                      cons "F" 1
                    )
                  )
                )
              )
               (
                cons "C" (
                  alist->hash-table (
                    _list (
                      cons "A" 5
                    )
                     (
                      cons "F" 3
                    )
                  )
                )
              )
               (
                cons "D" (
                  alist->hash-table (
                    _list (
                      cons "B" 3
                    )
                  )
                )
              )
               (
                cons "E" (
                  alist->hash-table (
                    _list (
                      cons "B" 4
                    )
                     (
                      cons "F" 3
                    )
                  )
                )
              )
               (
                cons "F" (
                  alist->hash-table (
                    _list (
                      cons "C" 3
                    )
                     (
                      cons "E" 3
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
              heap (
                _list (
                  alist->hash-table (
                    _list (
                      cons "node" "E"
                    )
                     (
                      cons "cost" 0
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
                  visited (
                    alist->hash-table (
                      _list
                    )
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      result (
                        - 1
                      )
                    )
                  )
                   (
                    begin (
                      call/cc (
                        lambda (
                          break2
                        )
                         (
                          letrec (
                            (
                              loop1 (
                                lambda (
                                  
                                )
                                 (
                                  if (
                                    > (
                                      _len heap
                                    )
                                     0
                                  )
                                   (
                                    begin (
                                      let (
                                        (
                                          best_idx 0
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
                                                              _len heap
                                                            )
                                                          )
                                                           (
                                                            begin (
                                                              if (
                                                                < (
                                                                  hash-table-ref (
                                                                    list-ref heap i
                                                                  )
                                                                   "cost"
                                                                )
                                                                 (
                                                                  hash-table-ref (
                                                                    list-ref heap best_idx
                                                                  )
                                                                   "cost"
                                                                )
                                                              )
                                                               (
                                                                begin (
                                                                  set! best_idx i
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
                                              let (
                                                (
                                                  best (
                                                    list-ref heap best_idx
                                                  )
                                                )
                                              )
                                               (
                                                begin (
                                                  let (
                                                    (
                                                      new_heap (
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
                                                              break6
                                                            )
                                                             (
                                                              letrec (
                                                                (
                                                                  loop5 (
                                                                    lambda (
                                                                      
                                                                    )
                                                                     (
                                                                      if (
                                                                        < j (
                                                                          _len heap
                                                                        )
                                                                      )
                                                                       (
                                                                        begin (
                                                                          if (
                                                                            not (
                                                                              equal? j best_idx
                                                                            )
                                                                          )
                                                                           (
                                                                            begin (
                                                                              set! new_heap (
                                                                                append new_heap (
                                                                                  _list (
                                                                                    list-ref heap j
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
                                                                         (
                                                                          loop5
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
                                                                loop5
                                                              )
                                                            )
                                                          )
                                                        )
                                                         (
                                                          set! heap new_heap
                                                        )
                                                         (
                                                          let (
                                                            (
                                                              u (
                                                                hash-table-ref best "node"
                                                              )
                                                            )
                                                          )
                                                           (
                                                            begin (
                                                              let (
                                                                (
                                                                  cost (
                                                                    hash-table-ref best "cost"
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                begin (
                                                                  if (
                                                                    cond (
                                                                      (
                                                                        string? visited
                                                                      )
                                                                       (
                                                                        if (
                                                                          string-contains visited u
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
                                                                          hash-table-exists? visited u
                                                                        )
                                                                         #t #f
                                                                      )
                                                                    )
                                                                     (
                                                                      else (
                                                                        if (
                                                                          member u visited
                                                                        )
                                                                         #t #f
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    begin (
                                                                      loop1
                                                                    )
                                                                  )
                                                                   (
                                                                    quote (
                                                                      
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  hash-table-set! visited u #t
                                                                )
                                                                 (
                                                                  if (
                                                                    string=? u "C"
                                                                  )
                                                                   (
                                                                    begin (
                                                                      set! result cost
                                                                    )
                                                                     (
                                                                      break2 (
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
                                                                  call/cc (
                                                                    lambda (
                                                                      break8
                                                                    )
                                                                     (
                                                                      letrec (
                                                                        (
                                                                          loop7 (
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
                                                                                      v (
                                                                                        car xs
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    begin (
                                                                                      if (
                                                                                        cond (
                                                                                          (
                                                                                            string? visited
                                                                                          )
                                                                                           (
                                                                                            if (
                                                                                              string-contains visited v
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
                                                                                              hash-table-exists? visited v
                                                                                            )
                                                                                             #t #f
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          else (
                                                                                            if (
                                                                                              member v visited
                                                                                            )
                                                                                             #t #f
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        begin (
                                                                                          loop7 (
                                                                                            cdr xs
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
                                                                                          next_cost (
                                                                                            + cost (
                                                                                              cond (
                                                                                                (
                                                                                                  string? (
                                                                                                    hash-table-ref/default G u (
                                                                                                      quote (
                                                                                                        
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  _substring (
                                                                                                    hash-table-ref/default G u (
                                                                                                      quote (
                                                                                                        
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                   v (
                                                                                                    + v 1
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                (
                                                                                                  hash-table? (
                                                                                                    hash-table-ref/default G u (
                                                                                                      quote (
                                                                                                        
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  hash-table-ref (
                                                                                                    hash-table-ref/default G u (
                                                                                                      quote (
                                                                                                        
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                   v
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                else (
                                                                                                  list-ref (
                                                                                                    hash-table-ref/default G u (
                                                                                                      quote (
                                                                                                        
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                   v
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        begin (
                                                                                          set! heap (
                                                                                            append heap (
                                                                                              _list (
                                                                                                alist->hash-table (
                                                                                                  _list (
                                                                                                    cons "node" v
                                                                                                  )
                                                                                                   (
                                                                                                    cons "cost" next_cost
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
                                                                                  loop7 (
                                                                                    cdr xs
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        loop7 (
                                                                          hash-table-keys (
                                                                            hash-table-ref/default G u (
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
                                      loop1
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
                            loop1
                          )
                        )
                      )
                    )
                     (
                      _display (
                        if (
                          string? result
                        )
                         result (
                          to-str result
                        )
                      )
                    )
                     (
                      newline
                    )
                     (
                      let (
                        (
                          G2 (
                            alist->hash-table (
                              _list (
                                cons "B" (
                                  alist->hash-table (
                                    _list (
                                      cons "C" 1
                                    )
                                  )
                                )
                              )
                               (
                                cons "C" (
                                  alist->hash-table (
                                    _list (
                                      cons "D" 1
                                    )
                                  )
                                )
                              )
                               (
                                cons "D" (
                                  alist->hash-table (
                                    _list (
                                      cons "F" 1
                                    )
                                  )
                                )
                              )
                               (
                                cons "E" (
                                  alist->hash-table (
                                    _list (
                                      cons "B" 1
                                    )
                                     (
                                      cons "F" 3
                                    )
                                  )
                                )
                              )
                               (
                                cons "F" (
                                  alist->hash-table (
                                    _list
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
                              heap2 (
                                _list (
                                  alist->hash-table (
                                    _list (
                                      cons "node" "E"
                                    )
                                     (
                                      cons "cost" 0
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
                                  visited2 (
                                    alist->hash-table (
                                      _list
                                    )
                                  )
                                )
                              )
                               (
                                begin (
                                  let (
                                    (
                                      result2 (
                                        - 1
                                      )
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
                                                    > (
                                                      _len heap2
                                                    )
                                                     0
                                                  )
                                                   (
                                                    begin (
                                                      let (
                                                        (
                                                          best2_idx 0
                                                        )
                                                      )
                                                       (
                                                        begin (
                                                          let (
                                                            (
                                                              i2 1
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
                                                                            < i2 (
                                                                              _len heap2
                                                                            )
                                                                          )
                                                                           (
                                                                            begin (
                                                                              if (
                                                                                < (
                                                                                  hash-table-ref (
                                                                                    list-ref heap2 i2
                                                                                  )
                                                                                   "cost"
                                                                                )
                                                                                 (
                                                                                  hash-table-ref (
                                                                                    list-ref heap2 best2_idx
                                                                                  )
                                                                                   "cost"
                                                                                )
                                                                              )
                                                                               (
                                                                                begin (
                                                                                  set! best2_idx i2
                                                                                )
                                                                              )
                                                                               (
                                                                                quote (
                                                                                  
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              set! i2 (
                                                                                + i2 1
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
                                                                  best2 (
                                                                    list-ref heap2 best2_idx
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                begin (
                                                                  let (
                                                                    (
                                                                      new_heap2 (
                                                                        _list
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    begin (
                                                                      let (
                                                                        (
                                                                          j2 0
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
                                                                                        < j2 (
                                                                                          _len heap2
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        begin (
                                                                                          if (
                                                                                            not (
                                                                                              equal? j2 best2_idx
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            begin (
                                                                                              set! new_heap2 (
                                                                                                append new_heap2 (
                                                                                                  _list (
                                                                                                    list-ref heap2 j2
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
                                                                                          set! j2 (
                                                                                            + j2 1
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
                                                                          set! heap2 new_heap2
                                                                        )
                                                                         (
                                                                          let (
                                                                            (
                                                                              u2 (
                                                                                hash-table-ref best2 "node"
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            begin (
                                                                              let (
                                                                                (
                                                                                  cost2 (
                                                                                    hash-table-ref best2 "cost"
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                begin (
                                                                                  if (
                                                                                    cond (
                                                                                      (
                                                                                        string? visited2
                                                                                      )
                                                                                       (
                                                                                        if (
                                                                                          string-contains visited2 u2
                                                                                        )
                                                                                         #t #f
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      (
                                                                                        hash-table? visited2
                                                                                      )
                                                                                       (
                                                                                        if (
                                                                                          hash-table-exists? visited2 u2
                                                                                        )
                                                                                         #t #f
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      else (
                                                                                        if (
                                                                                          member u2 visited2
                                                                                        )
                                                                                         #t #f
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    begin (
                                                                                      loop9
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    quote (
                                                                                      
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 (
                                                                                  hash-table-set! visited2 u2 #t
                                                                                )
                                                                                 (
                                                                                  if (
                                                                                    string=? u2 "F"
                                                                                  )
                                                                                   (
                                                                                    begin (
                                                                                      set! result2 cost2
                                                                                    )
                                                                                     (
                                                                                      break10 (
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
                                                                                  call/cc (
                                                                                    lambda (
                                                                                      break16
                                                                                    )
                                                                                     (
                                                                                      letrec (
                                                                                        (
                                                                                          loop15 (
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
                                                                                                      v2 (
                                                                                                        car xs
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    begin (
                                                                                                      if (
                                                                                                        cond (
                                                                                                          (
                                                                                                            string? visited2
                                                                                                          )
                                                                                                           (
                                                                                                            if (
                                                                                                              string-contains visited2 v2
                                                                                                            )
                                                                                                             #t #f
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          (
                                                                                                            hash-table? visited2
                                                                                                          )
                                                                                                           (
                                                                                                            if (
                                                                                                              hash-table-exists? visited2 v2
                                                                                                            )
                                                                                                             #t #f
                                                                                                          )
                                                                                                        )
                                                                                                         (
                                                                                                          else (
                                                                                                            if (
                                                                                                              member v2 visited2
                                                                                                            )
                                                                                                             #t #f
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        begin (
                                                                                                          loop15 (
                                                                                                            cdr xs
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
                                                                                                          next_cost2 (
                                                                                                            + cost2 (
                                                                                                              cond (
                                                                                                                (
                                                                                                                  string? (
                                                                                                                    hash-table-ref/default G2 u2 (
                                                                                                                      quote (
                                                                                                                        
                                                                                                                      )
                                                                                                                    )
                                                                                                                  )
                                                                                                                )
                                                                                                                 (
                                                                                                                  _substring (
                                                                                                                    hash-table-ref/default G2 u2 (
                                                                                                                      quote (
                                                                                                                        
                                                                                                                      )
                                                                                                                    )
                                                                                                                  )
                                                                                                                   v2 (
                                                                                                                    + v2 1
                                                                                                                  )
                                                                                                                )
                                                                                                              )
                                                                                                               (
                                                                                                                (
                                                                                                                  hash-table? (
                                                                                                                    hash-table-ref/default G2 u2 (
                                                                                                                      quote (
                                                                                                                        
                                                                                                                      )
                                                                                                                    )
                                                                                                                  )
                                                                                                                )
                                                                                                                 (
                                                                                                                  hash-table-ref (
                                                                                                                    hash-table-ref/default G2 u2 (
                                                                                                                      quote (
                                                                                                                        
                                                                                                                      )
                                                                                                                    )
                                                                                                                  )
                                                                                                                   v2
                                                                                                                )
                                                                                                              )
                                                                                                               (
                                                                                                                else (
                                                                                                                  list-ref (
                                                                                                                    hash-table-ref/default G2 u2 (
                                                                                                                      quote (
                                                                                                                        
                                                                                                                      )
                                                                                                                    )
                                                                                                                  )
                                                                                                                   v2
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        begin (
                                                                                                          set! heap2 (
                                                                                                            append heap2 (
                                                                                                              _list (
                                                                                                                alist->hash-table (
                                                                                                                  _list (
                                                                                                                    cons "node" v2
                                                                                                                  )
                                                                                                                   (
                                                                                                                    cons "cost" next_cost2
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
                                                                                                  loop15 (
                                                                                                    cdr xs
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        loop15 (
                                                                                          hash-table-keys (
                                                                                            hash-table-ref/default G2 u2 (
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
                                      _display (
                                        if (
                                          string? result2
                                        )
                                         result2 (
                                          to-str result2
                                        )
                                      )
                                    )
                                     (
                                      newline
                                    )
                                     (
                                      let (
                                        (
                                          G3 (
                                            alist->hash-table (
                                              _list (
                                                cons "B" (
                                                  alist->hash-table (
                                                    _list (
                                                      cons "C" 1
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                cons "C" (
                                                  alist->hash-table (
                                                    _list (
                                                      cons "D" 1
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                cons "D" (
                                                  alist->hash-table (
                                                    _list (
                                                      cons "F" 1
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                cons "E" (
                                                  alist->hash-table (
                                                    _list (
                                                      cons "B" 1
                                                    )
                                                     (
                                                      cons "G" 2
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                cons "F" (
                                                  alist->hash-table (
                                                    _list
                                                  )
                                                )
                                              )
                                               (
                                                cons "G" (
                                                  alist->hash-table (
                                                    _list (
                                                      cons "F" 1
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
                                              heap3 (
                                                _list (
                                                  alist->hash-table (
                                                    _list (
                                                      cons "node" "E"
                                                    )
                                                     (
                                                      cons "cost" 0
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
                                                  visited3 (
                                                    alist->hash-table (
                                                      _list
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                begin (
                                                  let (
                                                    (
                                                      result3 (
                                                        - 1
                                                      )
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
                                                                    > (
                                                                      _len heap3
                                                                    )
                                                                     0
                                                                  )
                                                                   (
                                                                    begin (
                                                                      let (
                                                                        (
                                                                          best3_idx 0
                                                                        )
                                                                      )
                                                                       (
                                                                        begin (
                                                                          let (
                                                                            (
                                                                              i3 1
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
                                                                                            < i3 (
                                                                                              _len heap3
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            begin (
                                                                                              if (
                                                                                                < (
                                                                                                  hash-table-ref (
                                                                                                    list-ref heap3 i3
                                                                                                  )
                                                                                                   "cost"
                                                                                                )
                                                                                                 (
                                                                                                  hash-table-ref (
                                                                                                    list-ref heap3 best3_idx
                                                                                                  )
                                                                                                   "cost"
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                begin (
                                                                                                  set! best3_idx i3
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                quote (
                                                                                                  
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             (
                                                                                              set! i3 (
                                                                                                + i3 1
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
                                                                              let (
                                                                                (
                                                                                  best3 (
                                                                                    list-ref heap3 best3_idx
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                begin (
                                                                                  let (
                                                                                    (
                                                                                      new_heap3 (
                                                                                        _list
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                   (
                                                                                    begin (
                                                                                      let (
                                                                                        (
                                                                                          j3 0
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
                                                                                                        < j3 (
                                                                                                          _len heap3
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        begin (
                                                                                                          if (
                                                                                                            not (
                                                                                                              equal? j3 best3_idx
                                                                                                            )
                                                                                                          )
                                                                                                           (
                                                                                                            begin (
                                                                                                              set! new_heap3 (
                                                                                                                append new_heap3 (
                                                                                                                  _list (
                                                                                                                    list-ref heap3 j3
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
                                                                                                          set! j3 (
                                                                                                            + j3 1
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
                                                                                          set! heap3 new_heap3
                                                                                        )
                                                                                         (
                                                                                          let (
                                                                                            (
                                                                                              u3 (
                                                                                                hash-table-ref best3 "node"
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            begin (
                                                                                              let (
                                                                                                (
                                                                                                  cost3 (
                                                                                                    hash-table-ref best3 "cost"
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                               (
                                                                                                begin (
                                                                                                  if (
                                                                                                    cond (
                                                                                                      (
                                                                                                        string? visited3
                                                                                                      )
                                                                                                       (
                                                                                                        if (
                                                                                                          string-contains visited3 u3
                                                                                                        )
                                                                                                         #t #f
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      (
                                                                                                        hash-table? visited3
                                                                                                      )
                                                                                                       (
                                                                                                        if (
                                                                                                          hash-table-exists? visited3 u3
                                                                                                        )
                                                                                                         #t #f
                                                                                                      )
                                                                                                    )
                                                                                                     (
                                                                                                      else (
                                                                                                        if (
                                                                                                          member u3 visited3
                                                                                                        )
                                                                                                         #t #f
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    begin (
                                                                                                      loop17
                                                                                                    )
                                                                                                  )
                                                                                                   (
                                                                                                    quote (
                                                                                                      
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                                 (
                                                                                                  hash-table-set! visited3 u3 #t
                                                                                                )
                                                                                                 (
                                                                                                  if (
                                                                                                    string=? u3 "F"
                                                                                                  )
                                                                                                   (
                                                                                                    begin (
                                                                                                      set! result3 cost3
                                                                                                    )
                                                                                                     (
                                                                                                      break18 (
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
                                                                                                  call/cc (
                                                                                                    lambda (
                                                                                                      break24
                                                                                                    )
                                                                                                     (
                                                                                                      letrec (
                                                                                                        (
                                                                                                          loop23 (
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
                                                                                                                      v3 (
                                                                                                                        car xs
                                                                                                                      )
                                                                                                                    )
                                                                                                                  )
                                                                                                                   (
                                                                                                                    begin (
                                                                                                                      if (
                                                                                                                        cond (
                                                                                                                          (
                                                                                                                            string? visited3
                                                                                                                          )
                                                                                                                           (
                                                                                                                            if (
                                                                                                                              string-contains visited3 v3
                                                                                                                            )
                                                                                                                             #t #f
                                                                                                                          )
                                                                                                                        )
                                                                                                                         (
                                                                                                                          (
                                                                                                                            hash-table? visited3
                                                                                                                          )
                                                                                                                           (
                                                                                                                            if (
                                                                                                                              hash-table-exists? visited3 v3
                                                                                                                            )
                                                                                                                             #t #f
                                                                                                                          )
                                                                                                                        )
                                                                                                                         (
                                                                                                                          else (
                                                                                                                            if (
                                                                                                                              member v3 visited3
                                                                                                                            )
                                                                                                                             #t #f
                                                                                                                          )
                                                                                                                        )
                                                                                                                      )
                                                                                                                       (
                                                                                                                        begin (
                                                                                                                          loop23 (
                                                                                                                            cdr xs
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
                                                                                                                          next_cost3 (
                                                                                                                            + cost3 (
                                                                                                                              cond (
                                                                                                                                (
                                                                                                                                  string? (
                                                                                                                                    hash-table-ref/default G3 u3 (
                                                                                                                                      quote (
                                                                                                                                        
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                )
                                                                                                                                 (
                                                                                                                                  _substring (
                                                                                                                                    hash-table-ref/default G3 u3 (
                                                                                                                                      quote (
                                                                                                                                        
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   v3 (
                                                                                                                                    + v3 1
                                                                                                                                  )
                                                                                                                                )
                                                                                                                              )
                                                                                                                               (
                                                                                                                                (
                                                                                                                                  hash-table? (
                                                                                                                                    hash-table-ref/default G3 u3 (
                                                                                                                                      quote (
                                                                                                                                        
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                )
                                                                                                                                 (
                                                                                                                                  hash-table-ref (
                                                                                                                                    hash-table-ref/default G3 u3 (
                                                                                                                                      quote (
                                                                                                                                        
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   v3
                                                                                                                                )
                                                                                                                              )
                                                                                                                               (
                                                                                                                                else (
                                                                                                                                  list-ref (
                                                                                                                                    hash-table-ref/default G3 u3 (
                                                                                                                                      quote (
                                                                                                                                        
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                   v3
                                                                                                                                )
                                                                                                                              )
                                                                                                                            )
                                                                                                                          )
                                                                                                                        )
                                                                                                                      )
                                                                                                                       (
                                                                                                                        begin (
                                                                                                                          set! heap3 (
                                                                                                                            append heap3 (
                                                                                                                              _list (
                                                                                                                                alist->hash-table (
                                                                                                                                  _list (
                                                                                                                                    cons "node" v3
                                                                                                                                  )
                                                                                                                                   (
                                                                                                                                    cons "cost" next_cost3
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
                                                                                                                  loop23 (
                                                                                                                    cdr xs
                                                                                                                  )
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                       (
                                                                                                        loop23 (
                                                                                                          hash-table-keys (
                                                                                                            hash-table-ref/default G3 u3 (
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
                                                      _display (
                                                        if (
                                                          string? result3
                                                        )
                                                         result3 (
                                                          to-str result3
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
          end26 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur27 (
              quotient (
                * (
                  - end26 start25
                )
                 1000000
              )
               jps28
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur27
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
