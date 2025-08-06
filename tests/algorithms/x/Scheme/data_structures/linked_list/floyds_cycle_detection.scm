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
      start14 (
        current-jiffy
      )
    )
     (
      jps17 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      let (
        (
          NULL (
            - 0 1
          )
        )
      )
       (
        begin (
          define (
            empty_list
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
                      cons "next" (
                        _list
                      )
                    )
                     (
                      cons "head" NULL
                    )
                  )
                )
              )
            )
          )
        )
         (
          define (
            add_node list value
          )
           (
            call/cc (
              lambda (
                ret2
              )
               (
                let (
                  (
                    nexts (
                      hash-table-ref list "next"
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        new_index (
                          _len nexts
                        )
                      )
                    )
                     (
                      begin (
                        set! nexts (
                          append nexts (
                            _list NULL
                          )
                        )
                      )
                       (
                        if (
                          equal? (
                            hash-table-ref list "head"
                          )
                           NULL
                        )
                         (
                          begin (
                            ret2 (
                              alist->hash-table (
                                _list (
                                  cons "next" nexts
                                )
                                 (
                                  cons "head" new_index
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
                            last (
                              hash-table-ref list "head"
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
                                        if (
                                          not (
                                            equal? (
                                              list-ref nexts last
                                            )
                                             NULL
                                          )
                                        )
                                         (
                                          begin (
                                            set! last (
                                              list-ref nexts last
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
                                new_nexts (
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
                                                  < i (
                                                    _len nexts
                                                  )
                                                )
                                                 (
                                                  begin (
                                                    if (
                                                      equal? i last
                                                    )
                                                     (
                                                      begin (
                                                        set! new_nexts (
                                                          append new_nexts (
                                                            _list new_index
                                                          )
                                                        )
                                                      )
                                                    )
                                                     (
                                                      begin (
                                                        set! new_nexts (
                                                          append new_nexts (
                                                            _list (
                                                              list-ref nexts i
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
                                    ret2 (
                                      alist->hash-table (
                                        _list (
                                          cons "next" new_nexts
                                        )
                                         (
                                          cons "head" (
                                            hash-table-ref list "head"
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
            set_next list index next_index
          )
           (
            call/cc (
              lambda (
                ret7
              )
               (
                let (
                  (
                    nexts (
                      hash-table-ref list "next"
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        new_nexts (
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
                                break9
                              )
                               (
                                letrec (
                                  (
                                    loop8 (
                                      lambda (
                                        
                                      )
                                       (
                                        if (
                                          < i (
                                            _len nexts
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              equal? i index
                                            )
                                             (
                                              begin (
                                                set! new_nexts (
                                                  append new_nexts (
                                                    _list next_index
                                                  )
                                                )
                                              )
                                            )
                                             (
                                              begin (
                                                set! new_nexts (
                                                  append new_nexts (
                                                    _list (
                                                      list-ref nexts i
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
                                            loop8
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
                                  loop8
                                )
                              )
                            )
                          )
                           (
                            ret7 (
                              alist->hash-table (
                                _list (
                                  cons "next" new_nexts
                                )
                                 (
                                  cons "head" (
                                    hash-table-ref list "head"
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
            detect_cycle list
          )
           (
            call/cc (
              lambda (
                ret10
              )
               (
                begin (
                  if (
                    equal? (
                      hash-table-ref list "head"
                    )
                     NULL
                  )
                   (
                    begin (
                      ret10 #f
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
                      nexts (
                        hash-table-ref list "next"
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          slow (
                            hash-table-ref list "head"
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              fast (
                                hash-table-ref list "head"
                              )
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
                                            and (
                                              not (
                                                equal? fast NULL
                                              )
                                            )
                                             (
                                              not (
                                                equal? (
                                                  list-ref nexts fast
                                                )
                                                 NULL
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              set! slow (
                                                list-ref nexts slow
                                              )
                                            )
                                             (
                                              set! fast (
                                                list-ref nexts (
                                                  list-ref nexts fast
                                                )
                                              )
                                            )
                                             (
                                              if (
                                                equal? slow fast
                                              )
                                               (
                                                begin (
                                                  ret10 #t
                                                )
                                              )
                                               (
                                                quote (
                                                  
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
                              ret10 #f
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
                ret13
              )
               (
                let (
                  (
                    ll (
                      empty_list
                    )
                  )
                )
                 (
                  begin (
                    set! ll (
                      add_node ll 1
                    )
                  )
                   (
                    set! ll (
                      add_node ll 2
                    )
                  )
                   (
                    set! ll (
                      add_node ll 3
                    )
                  )
                   (
                    set! ll (
                      add_node ll 4
                    )
                  )
                   (
                    set! ll (
                      set_next ll 3 1
                    )
                  )
                   (
                    _display (
                      if (
                        string? (
                          detect_cycle ll
                        )
                      )
                       (
                        detect_cycle ll
                      )
                       (
                        to-str (
                          detect_cycle ll
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
     (
      let (
        (
          end15 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur16 (
              quotient (
                * (
                  - end15 start14
                )
                 1000000
              )
               jps17
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur16
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
