;; Generated on 2025-08-07 14:57 +0700
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
(define (_div a b) (if (and (integer? a) (integer? b) (exact? a) (exact? b)) (quotient a b) (/ a b)))
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
(define (list-ref-safe lst idx) (if (and (integer? idx) (>= idx 0) (< idx (length lst))) (list-ref lst idx) '()))
(
  let (
    (
      start61 (
        current-jiffy
      )
    )
     (
      jps64 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        load_data
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            ret1 (
              _list (
                _list "milk"
              )
               (
                _list "milk" "butter"
              )
               (
                _list "milk" "bread"
              )
               (
                _list "milk" "bread" "chips"
              )
            )
          )
        )
      )
    )
     (
      define (
        contains_string xs s
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
                           '(
                            
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
                                    string=? v s
                                  )
                                   (
                                    begin (
                                      ret2 #t
                                    )
                                  )
                                   '(
                                    
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
                    loop3 xs
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
        is_subset candidate transaction
      )
       (
        call/cc (
          lambda (
            ret5
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
                          xs
                        )
                         (
                          if (
                            null? xs
                          )
                           '(
                            
                          )
                           (
                            begin (
                              let (
                                (
                                  it (
                                    car xs
                                  )
                                )
                              )
                               (
                                begin (
                                  if (
                                    not (
                                      contains_string transaction it
                                    )
                                  )
                                   (
                                    begin (
                                      ret5 #f
                                    )
                                  )
                                   '(
                                    
                                  )
                                )
                              )
                            )
                             (
                              loop6 (
                                cdr xs
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                   (
                    loop6 candidate
                  )
                )
              )
            )
             (
              ret5 #t
            )
          )
        )
      )
    )
     (
      define (
        lists_equal a b
      )
       (
        call/cc (
          lambda (
            ret8
          )
           (
            begin (
              if (
                not (
                  equal? (
                    _len a
                  )
                   (
                    _len b
                  )
                )
              )
               (
                begin (
                  ret8 #f
                )
              )
               '(
                
              )
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
                                  _len a
                                )
                              )
                               (
                                begin (
                                  if (
                                    not (
                                      string=? (
                                        list-ref-safe a i
                                      )
                                       (
                                        list-ref-safe b i
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      ret8 #f
                                    )
                                  )
                                   '(
                                    
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
                               '(
                                
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
                  ret8 #t
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        contains_list itemset item
      )
       (
        call/cc (
          lambda (
            ret11
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
                          xs
                        )
                         (
                          if (
                            null? xs
                          )
                           '(
                            
                          )
                           (
                            begin (
                              let (
                                (
                                  l (
                                    car xs
                                  )
                                )
                              )
                               (
                                begin (
                                  if (
                                    lists_equal l item
                                  )
                                   (
                                    begin (
                                      ret11 #t
                                    )
                                  )
                                   '(
                                    
                                  )
                                )
                              )
                            )
                             (
                              loop12 (
                                cdr xs
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                   (
                    loop12 itemset
                  )
                )
              )
            )
             (
              ret11 #f
            )
          )
        )
      )
    )
     (
      define (
        count_list itemset item
      )
       (
        call/cc (
          lambda (
            ret14
          )
           (
            let (
              (
                c 0
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
                            xs
                          )
                           (
                            if (
                              null? xs
                            )
                             '(
                              
                            )
                             (
                              begin (
                                let (
                                  (
                                    l (
                                      car xs
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      lists_equal l item
                                    )
                                     (
                                      begin (
                                        set! c (
                                          + c 1
                                        )
                                      )
                                    )
                                     '(
                                      
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
                      loop15 itemset
                    )
                  )
                )
              )
               (
                ret14 c
              )
            )
          )
        )
      )
    )
     (
      define (
        slice_list xs start
      )
       (
        call/cc (
          lambda (
            ret17
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
                    i start
                  )
                )
                 (
                  begin (
                    call/cc (
                      lambda (
                        break19
                      )
                       (
                        letrec (
                          (
                            loop18 (
                              lambda (
                                
                              )
                               (
                                if (
                                  < i (
                                    _len xs
                                  )
                                )
                                 (
                                  begin (
                                    set! res (
                                      append res (
                                        _list (
                                          list-ref-safe xs i
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
                                    loop18
                                  )
                                )
                                 '(
                                  
                                )
                              )
                            )
                          )
                        )
                         (
                          loop18
                        )
                      )
                    )
                  )
                   (
                    ret17 res
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
        combinations_lists xs k
      )
       (
        call/cc (
          lambda (
            ret20
          )
           (
            let (
              (
                result (
                  _list
                )
              )
            )
             (
              begin (
                if (
                  equal? k 0
                )
                 (
                  begin (
                    set! result (
                      append result (
                        _list (
                          _list
                        )
                      )
                    )
                  )
                   (
                    ret20 result
                  )
                )
                 '(
                  
                )
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
                                  < i (
                                    _len xs
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        head (
                                          list-ref-safe xs i
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            tail (
                                              slice_list xs (
                                                + i 1
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                tail_combos (
                                                  combinations_lists tail (
                                                    - k 1
                                                  )
                                                )
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
                                                            xs
                                                          )
                                                           (
                                                            if (
                                                              null? xs
                                                            )
                                                             '(
                                                              
                                                            )
                                                             (
                                                              begin (
                                                                let (
                                                                  (
                                                                    combo (
                                                                      car xs
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    let (
                                                                      (
                                                                        new_combo (
                                                                          _list
                                                                        )
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        set! new_combo (
                                                                          append new_combo (
                                                                            _list head
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        call/cc (
                                                                          lambda (
                                                                            break26
                                                                          )
                                                                           (
                                                                            letrec (
                                                                              (
                                                                                loop25 (
                                                                                  lambda (
                                                                                    xs
                                                                                  )
                                                                                   (
                                                                                    if (
                                                                                      null? xs
                                                                                    )
                                                                                     '(
                                                                                      
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        let (
                                                                                          (
                                                                                            c (
                                                                                              car xs
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            set! new_combo (
                                                                                              append new_combo (
                                                                                                _list c
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        loop25 (
                                                                                          cdr xs
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              loop25 combo
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        set! result (
                                                                          append result (
                                                                            _list new_combo
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
                                                      loop23 tail_combos
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
                                    loop21
                                  )
                                )
                                 '(
                                  
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
                    ret20 result
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
        prune itemset candidates length
      )
       (
        call/cc (
          lambda (
            ret27
          )
           (
            let (
              (
                pruned (
                  _list
                )
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
                            xs
                          )
                           (
                            if (
                              null? xs
                            )
                             '(
                              
                            )
                             (
                              begin (
                                let (
                                  (
                                    candidate (
                                      car xs
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        is_subsequence #t
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
                                                    xs
                                                  )
                                                   (
                                                    if (
                                                      null? xs
                                                    )
                                                     '(
                                                      
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            item (
                                                              car xs
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            if (
                                                              or (
                                                                not (
                                                                  contains_list itemset item
                                                                )
                                                              )
                                                               (
                                                                _lt (
                                                                  count_list itemset item
                                                                )
                                                                 (
                                                                  - length 1
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                set! is_subsequence #f
                                                              )
                                                               (
                                                                break31 '(
                                                                  
                                                                )
                                                              )
                                                            )
                                                             '(
                                                              
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        loop30 (
                                                          cdr xs
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              loop30 candidate
                                            )
                                          )
                                        )
                                      )
                                       (
                                        if is_subsequence (
                                          begin (
                                            set! pruned (
                                              append pruned (
                                                _list candidate
                                              )
                                            )
                                          )
                                        )
                                         '(
                                          
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                               (
                                loop28 (
                                  cdr xs
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                     (
                      loop28 candidates
                    )
                  )
                )
              )
               (
                ret27 pruned
              )
            )
          )
        )
      )
    )
     (
      define (
        sort_strings xs
      )
       (
        call/cc (
          lambda (
            ret32
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
                call/cc (
                  lambda (
                    break34
                  )
                   (
                    letrec (
                      (
                        loop33 (
                          lambda (
                            xs
                          )
                           (
                            if (
                              null? xs
                            )
                             '(
                              
                            )
                             (
                              begin (
                                let (
                                  (
                                    s (
                                      car xs
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    set! res (
                                      append res (
                                        _list s
                                      )
                                    )
                                  )
                                )
                              )
                               (
                                loop33 (
                                  cdr xs
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                     (
                      loop33 xs
                    )
                  )
                )
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
                        break36
                      )
                       (
                        letrec (
                          (
                            loop35 (
                              lambda (
                                
                              )
                               (
                                if (
                                  < i (
                                    _len res
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        j (
                                          + i 1
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        call/cc (
                                          lambda (
                                            break38
                                          )
                                           (
                                            letrec (
                                              (
                                                loop37 (
                                                  lambda (
                                                    
                                                  )
                                                   (
                                                    if (
                                                      < j (
                                                        _len res
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        if (
                                                          string<? (
                                                            list-ref-safe res j
                                                          )
                                                           (
                                                            list-ref-safe res i
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                tmp (
                                                                  list-ref-safe res i
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                list-set! res i (
                                                                  list-ref-safe res j
                                                                )
                                                              )
                                                               (
                                                                list-set! res j tmp
                                                              )
                                                            )
                                                          )
                                                        )
                                                         '(
                                                          
                                                        )
                                                      )
                                                       (
                                                        set! j (
                                                          + j 1
                                                        )
                                                      )
                                                       (
                                                        loop37
                                                      )
                                                    )
                                                     '(
                                                      
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              loop37
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
                                    loop35
                                  )
                                )
                                 '(
                                  
                                )
                              )
                            )
                          )
                        )
                         (
                          loop35
                        )
                      )
                    )
                  )
                   (
                    ret32 res
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
        itemset_to_string xs
      )
       (
        call/cc (
          lambda (
            ret39
          )
           (
            let (
              (
                s "["
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
                        break41
                      )
                       (
                        letrec (
                          (
                            loop40 (
                              lambda (
                                
                              )
                               (
                                if (
                                  < i (
                                    _len xs
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      > i 0
                                    )
                                     (
                                      begin (
                                        set! s (
                                          string-append s ", "
                                        )
                                      )
                                    )
                                     '(
                                      
                                    )
                                  )
                                   (
                                    set! s (
                                      string-append (
                                        string-append (
                                          string-append s "'"
                                        )
                                         (
                                          list-ref-safe xs i
                                        )
                                      )
                                       "'"
                                    )
                                  )
                                   (
                                    set! i (
                                      + i 1
                                    )
                                  )
                                   (
                                    loop40
                                  )
                                )
                                 '(
                                  
                                )
                              )
                            )
                          )
                        )
                         (
                          loop40
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
                    ret39 s
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
        apriori data min_support
      )
       (
        call/cc (
          lambda (
            ret42
          )
           (
            let (
              (
                itemset (
                  _list
                )
              )
            )
             (
              begin (
                call/cc (
                  lambda (
                    break44
                  )
                   (
                    letrec (
                      (
                        loop43 (
                          lambda (
                            xs
                          )
                           (
                            if (
                              null? xs
                            )
                             '(
                              
                            )
                             (
                              begin (
                                let (
                                  (
                                    transaction (
                                      car xs
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        t (
                                          _list
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        call/cc (
                                          lambda (
                                            break46
                                          )
                                           (
                                            letrec (
                                              (
                                                loop45 (
                                                  lambda (
                                                    xs
                                                  )
                                                   (
                                                    if (
                                                      null? xs
                                                    )
                                                     '(
                                                      
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
                                                            set! t (
                                                              append t (
                                                                _list v
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        loop45 (
                                                          cdr xs
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              loop45 transaction
                                            )
                                          )
                                        )
                                      )
                                       (
                                        set! itemset (
                                          append itemset (
                                            _list t
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                               (
                                loop43 (
                                  cdr xs
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                     (
                      loop43 data
                    )
                  )
                )
              )
               (
                let (
                  (
                    frequent (
                      _list
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        length 1
                      )
                    )
                     (
                      begin (
                        call/cc (
                          lambda (
                            break48
                          )
                           (
                            letrec (
                              (
                                loop47 (
                                  lambda (
                                    
                                  )
                                   (
                                    if (
                                      > (
                                        _len itemset
                                      )
                                       0
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            counts (
                                              _list
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            let (
                                              (
                                                idx 0
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
                                                              < idx (
                                                                _len itemset
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                set! counts (
                                                                  append counts (
                                                                    _list 0
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                set! idx (
                                                                  + idx 1
                                                                )
                                                              )
                                                               (
                                                                loop49
                                                              )
                                                            )
                                                             '(
                                                              
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
                                                call/cc (
                                                  lambda (
                                                    break52
                                                  )
                                                   (
                                                    letrec (
                                                      (
                                                        loop51 (
                                                          lambda (
                                                            xs
                                                          )
                                                           (
                                                            if (
                                                              null? xs
                                                            )
                                                             '(
                                                              
                                                            )
                                                             (
                                                              begin (
                                                                let (
                                                                  (
                                                                    transaction (
                                                                      car xs
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
                                                                            break54
                                                                          )
                                                                           (
                                                                            letrec (
                                                                              (
                                                                                loop53 (
                                                                                  lambda (
                                                                                    
                                                                                  )
                                                                                   (
                                                                                    if (
                                                                                      < j (
                                                                                        _len itemset
                                                                                      )
                                                                                    )
                                                                                     (
                                                                                      begin (
                                                                                        let (
                                                                                          (
                                                                                            candidate (
                                                                                              list-ref-safe itemset j
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                         (
                                                                                          begin (
                                                                                            if (
                                                                                              is_subset candidate transaction
                                                                                            )
                                                                                             (
                                                                                              begin (
                                                                                                list-set! counts j (
                                                                                                  + (
                                                                                                    list-ref-safe counts j
                                                                                                  )
                                                                                                   1
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                             '(
                                                                                              
                                                                                            )
                                                                                          )
                                                                                           (
                                                                                            set! j (
                                                                                              + j 1
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                       (
                                                                                        loop53
                                                                                      )
                                                                                    )
                                                                                     '(
                                                                                      
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              loop53
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                loop51 (
                                                                  cdr xs
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      loop51 data
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                let (
                                                  (
                                                    new_itemset (
                                                      _list
                                                    )
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    let (
                                                      (
                                                        k 0
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        call/cc (
                                                          lambda (
                                                            break56
                                                          )
                                                           (
                                                            letrec (
                                                              (
                                                                loop55 (
                                                                  lambda (
                                                                    
                                                                  )
                                                                   (
                                                                    if (
                                                                      < k (
                                                                        _len itemset
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        if (
                                                                          >= (
                                                                            list-ref-safe counts k
                                                                          )
                                                                           min_support
                                                                        )
                                                                         (
                                                                          begin (
                                                                            set! new_itemset (
                                                                              append new_itemset (
                                                                                _list (
                                                                                  list-ref-safe itemset k
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                         '(
                                                                          
                                                                        )
                                                                      )
                                                                       (
                                                                        set! k (
                                                                          + k 1
                                                                        )
                                                                      )
                                                                       (
                                                                        loop55
                                                                      )
                                                                    )
                                                                     '(
                                                                      
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                             (
                                                              loop55
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        set! itemset new_itemset
                                                      )
                                                       (
                                                        let (
                                                          (
                                                            m 0
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            call/cc (
                                                              lambda (
                                                                break58
                                                              )
                                                               (
                                                                letrec (
                                                                  (
                                                                    loop57 (
                                                                      lambda (
                                                                        
                                                                      )
                                                                       (
                                                                        if (
                                                                          < m (
                                                                            _len itemset
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                sorted_item (
                                                                                  sort_strings (
                                                                                    list-ref-safe itemset m
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                set! frequent (
                                                                                  append frequent (
                                                                                    _list (
                                                                                      alist->hash-table (
                                                                                        _list (
                                                                                          cons "items" sorted_item
                                                                                        )
                                                                                         (
                                                                                          cons "support" (
                                                                                            list-ref-safe counts m
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                set! m (
                                                                                  + m 1
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                           (
                                                                            loop57
                                                                          )
                                                                        )
                                                                         '(
                                                                          
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  loop57
                                                                )
                                                              )
                                                            )
                                                          )
                                                           (
                                                            set! length (
                                                              + length 1
                                                            )
                                                          )
                                                           (
                                                            let (
                                                              (
                                                                combos (
                                                                  combinations_lists itemset length
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                set! itemset (
                                                                  prune itemset combos length
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
                                        loop47
                                      )
                                    )
                                     '(
                                      
                                    )
                                  )
                                )
                              )
                            )
                             (
                              loop47
                            )
                          )
                        )
                      )
                       (
                        ret42 frequent
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
          frequent_itemsets (
            apriori (
              load_data
            )
             2
          )
        )
      )
       (
        begin (
          call/cc (
            lambda (
              break60
            )
             (
              letrec (
                (
                  loop59 (
                    lambda (
                      xs
                    )
                     (
                      if (
                        null? xs
                      )
                       '(
                        
                      )
                       (
                        begin (
                          let (
                            (
                              fi (
                                car xs
                              )
                            )
                          )
                           (
                            begin (
                              _display (
                                if (
                                  string? (
                                    string-append (
                                      string-append (
                                        itemset_to_string (
                                          hash-table-ref fi "items"
                                        )
                                      )
                                       ": "
                                    )
                                     (
                                      to-str-space (
                                        hash-table-ref fi "support"
                                      )
                                    )
                                  )
                                )
                                 (
                                  string-append (
                                    string-append (
                                      itemset_to_string (
                                        hash-table-ref fi "items"
                                      )
                                    )
                                     ": "
                                  )
                                   (
                                    to-str-space (
                                      hash-table-ref fi "support"
                                    )
                                  )
                                )
                                 (
                                  to-str (
                                    string-append (
                                      string-append (
                                        itemset_to_string (
                                          hash-table-ref fi "items"
                                        )
                                      )
                                       ": "
                                    )
                                     (
                                      to-str-space (
                                        hash-table-ref fi "support"
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
                         (
                          loop59 (
                            cdr xs
                          )
                        )
                      )
                    )
                  )
                )
              )
               (
                loop59 frequent_itemsets
              )
            )
          )
        )
      )
    )
     (
      let (
        (
          end62 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur63 (
              quotient (
                * (
                  - end62 start61
                )
                 1000000
              )
               jps64
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur63
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
