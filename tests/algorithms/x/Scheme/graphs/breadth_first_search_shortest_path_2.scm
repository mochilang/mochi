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
        contains xs x
      )
       (
        call/cc (
          lambda (
            ret1
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
                                _len xs
                              )
                            )
                             (
                              begin (
                                if (
                                  string=? (
                                    list-ref xs i
                                  )
                                   x
                                )
                                 (
                                  begin (
                                    ret1 #t
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
                ret1 #f
              )
            )
          )
        )
      )
    )
     (
      define (
        contains_key m key
      )
       (
        call/cc (
          lambda (
            ret4
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
                                  k (
                                    car xs
                                  )
                                )
                              )
                               (
                                begin (
                                  if (
                                    string=? k key
                                  )
                                   (
                                    begin (
                                      ret4 #t
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
                              loop5 (
                                cdr xs
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                   (
                    loop5 (
                      hash-table-keys m
                    )
                  )
                )
              )
            )
             (
              ret4 #f
            )
          )
        )
      )
    )
     (
      define (
        bfs_shortest_path graph start goal
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            let (
              (
                explored (
                  _list
                )
              )
            )
             (
              begin (
                let (
                  (
                    queue (
                      _list (
                        _list start
                      )
                    )
                  )
                )
                 (
                  begin (
                    if (
                      string=? start goal
                    )
                     (
                      begin (
                        ret7 (
                          _list start
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
                                  > (
                                    _len queue
                                  )
                                   0
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        path (
                                          list-ref queue 0
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        set! queue (
                                          slice queue 1 (
                                            _len queue
                                          )
                                        )
                                      )
                                       (
                                        let (
                                          (
                                            node (
                                              list-ref path (
                                                - (
                                                  _len path
                                                )
                                                 1
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              not (
                                                contains explored node
                                              )
                                            )
                                             (
                                              begin (
                                                let (
                                                  (
                                                    neighbours (
                                                      hash-table-ref/default graph node (
                                                        quote (
                                                          
                                                        )
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
                                                            break11
                                                          )
                                                           (
                                                            letrec (
                                                              (
                                                                loop10 (
                                                                  lambda (
                                                                    
                                                                  )
                                                                   (
                                                                    if (
                                                                      < i (
                                                                        _len neighbours
                                                                      )
                                                                    )
                                                                     (
                                                                      begin (
                                                                        let (
                                                                          (
                                                                            neighbour (
                                                                              list-ref neighbours i
                                                                            )
                                                                          )
                                                                        )
                                                                         (
                                                                          begin (
                                                                            let (
                                                                              (
                                                                                new_path path
                                                                              )
                                                                            )
                                                                             (
                                                                              begin (
                                                                                set! new_path (
                                                                                  append new_path (
                                                                                    _list neighbour
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                set! queue (
                                                                                  append queue (
                                                                                    _list new_path
                                                                                  )
                                                                                )
                                                                              )
                                                                               (
                                                                                if (
                                                                                  string=? neighbour goal
                                                                                )
                                                                                 (
                                                                                  begin (
                                                                                    ret7 new_path
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
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        loop10
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
                                                              loop10
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        set! explored (
                                                          append explored (
                                                            _list node
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
                      _list
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
        bfs_shortest_path_distance graph start target
      )
       (
        call/cc (
          lambda (
            ret12
          )
           (
            begin (
              if (
                or (
                  eq? (
                    contains_key graph start
                  )
                   #f
                )
                 (
                  eq? (
                    contains_key graph target
                  )
                   #f
                )
              )
               (
                begin (
                  ret12 (
                    - 1
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
                string=? start target
              )
               (
                begin (
                  ret12 0
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
                  queue (
                    _list start
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      visited (
                        _list start
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          dist (
                            alist->hash-table (
                              _list
                            )
                          )
                        )
                      )
                       (
                        begin (
                          hash-table-set! dist start 0
                        )
                         (
                          hash-table-set! dist target (
                            - 1
                          )
                        )
                         (
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
                                        > (
                                          _len queue
                                        )
                                         0
                                      )
                                       (
                                        begin (
                                          let (
                                            (
                                              node (
                                                list-ref queue 0
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              set! queue (
                                                slice queue 1 (
                                                  _len queue
                                                )
                                              )
                                            )
                                             (
                                              if (
                                                string=? node target
                                              )
                                               (
                                                begin (
                                                  if (
                                                    or (
                                                      equal? (
                                                        hash-table-ref/default dist target (
                                                          quote (
                                                            
                                                          )
                                                        )
                                                      )
                                                       (
                                                        - 1
                                                      )
                                                    )
                                                     (
                                                      < (
                                                        hash-table-ref/default dist node (
                                                          quote (
                                                            
                                                          )
                                                        )
                                                      )
                                                       (
                                                        hash-table-ref/default dist target (
                                                          quote (
                                                            
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    begin (
                                                      hash-table-set! dist target (
                                                        hash-table-ref/default dist node (
                                                          quote (
                                                            
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
                                               (
                                                quote (
                                                  
                                                )
                                              )
                                            )
                                             (
                                              let (
                                                (
                                                  adj (
                                                    hash-table-ref/default graph node (
                                                      quote (
                                                        
                                                      )
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
                                                                      _len adj
                                                                    )
                                                                  )
                                                                   (
                                                                    begin (
                                                                      let (
                                                                        (
                                                                          next (
                                                                            list-ref adj i
                                                                          )
                                                                        )
                                                                      )
                                                                       (
                                                                        begin (
                                                                          if (
                                                                            not (
                                                                              contains visited next
                                                                            )
                                                                          )
                                                                           (
                                                                            begin (
                                                                              set! visited (
                                                                                append visited (
                                                                                  _list next
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              set! queue (
                                                                                append queue (
                                                                                  _list next
                                                                                )
                                                                              )
                                                                            )
                                                                             (
                                                                              hash-table-set! dist next (
                                                                                + (
                                                                                  hash-table-ref/default dist node (
                                                                                    quote (
                                                                                      
                                                                                    )
                                                                                  )
                                                                                )
                                                                                 1
                                                                              )
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
                          ret12 (
                            hash-table-ref/default dist target (
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
     (
      let (
        (
          demo_graph (
            alist->hash-table (
              _list (
                cons "A" (
                  _list "B" "C" "E"
                )
              )
               (
                cons "B" (
                  _list "A" "D" "E"
                )
              )
               (
                cons "C" (
                  _list "A" "F" "G"
                )
              )
               (
                cons "D" (
                  _list "B"
                )
              )
               (
                cons "E" (
                  _list "A" "B" "D"
                )
              )
               (
                cons "F" (
                  _list "C"
                )
              )
               (
                cons "G" (
                  _list "C"
                )
              )
            )
          )
        )
      )
       (
        begin
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
