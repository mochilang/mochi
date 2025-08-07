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
      start23 (
        current-jiffy
      )
    )
     (
      jps26 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        topology_sort graph vert visited
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            begin (
              list-set! visited vert #t
            )
             (
              let (
                (
                  order (
                    _list
                  )
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
                                      neighbour (
                                        car xs
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      if (
                                        not (
                                          list-ref visited neighbour
                                        )
                                      )
                                       (
                                        begin (
                                          set! order (
                                            append order (
                                              topology_sort graph neighbour visited
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
                                 (
                                  loop2 (
                                    cdr xs
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                       (
                        loop2 (
                          list-ref graph vert
                        )
                      )
                    )
                  )
                )
                 (
                  set! order (
                    append order (
                      _list vert
                    )
                  )
                )
                 (
                  ret1 order
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        find_component graph vert visited
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            begin (
              list-set! visited vert #t
            )
             (
              let (
                (
                  comp (
                    _list vert
                  )
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
                                      neighbour (
                                        car xs
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      if (
                                        not (
                                          list-ref visited neighbour
                                        )
                                      )
                                       (
                                        begin (
                                          set! comp (
                                            append comp (
                                              find_component graph neighbour visited
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
                          list-ref graph vert
                        )
                      )
                    )
                  )
                )
                 (
                  ret4 comp
                )
              )
            )
          )
        )
      )
    )
     (
      define (
        strongly_connected_components graph
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            let (
              (
                n (
                  _len graph
                )
              )
            )
             (
              begin (
                let (
                  (
                    visited (
                      _list
                    )
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
                                _
                              )
                               (
                                if (
                                  < _ n
                                )
                                 (
                                  begin (
                                    begin (
                                      set! visited (
                                        append visited (
                                          _list #f
                                        )
                                      )
                                    )
                                  )
                                   (
                                    loop8 (
                                      + _ 1
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
                          loop8 0
                        )
                      )
                    )
                  )
                   (
                    let (
                      (
                        reversed (
                          _list
                        )
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
                                    _
                                  )
                                   (
                                    if (
                                      < _ n
                                    )
                                     (
                                      begin (
                                        begin (
                                          set! reversed (
                                            append reversed (
                                              _list (
                                                _list
                                              )
                                            )
                                          )
                                        )
                                      )
                                       (
                                        loop10 (
                                          + _ 1
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
                              loop10 0
                            )
                          )
                        )
                      )
                       (
                        call/cc (
                          lambda (
                            break13
                          )
                           (
                            letrec (
                              (
                                loop12 (
                                  lambda (
                                    i
                                  )
                                   (
                                    if (
                                      < i n
                                    )
                                     (
                                      begin (
                                        begin (
                                          call/cc (
                                            lambda (
                                              break15
                                            )
                                             (
                                              letrec (
                                                (
                                                  loop14 (
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
                                                              neighbour (
                                                                car xs
                                                              )
                                                            )
                                                          )
                                                           (
                                                            begin (
                                                              list-set! reversed neighbour (
                                                                append (
                                                                  list-ref reversed neighbour
                                                                )
                                                                 (
                                                                  _list i
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                         (
                                                          loop14 (
                                                            cdr xs
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                loop14 (
                                                  list-ref graph i
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                       (
                                        loop12 (
                                          + i 1
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
                              loop12 0
                            )
                          )
                        )
                      )
                       (
                        let (
                          (
                            order (
                              _list
                            )
                          )
                        )
                         (
                          begin (
                            call/cc (
                              lambda (
                                break17
                              )
                               (
                                letrec (
                                  (
                                    loop16 (
                                      lambda (
                                        i
                                      )
                                       (
                                        if (
                                          < i n
                                        )
                                         (
                                          begin (
                                            begin (
                                              if (
                                                not (
                                                  list-ref visited i
                                                )
                                              )
                                               (
                                                begin (
                                                  set! order (
                                                    append order (
                                                      topology_sort graph i visited
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
                                            loop16 (
                                              + i 1
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
                                  loop16 0
                                )
                              )
                            )
                          )
                           (
                            set! visited (
                              _list
                            )
                          )
                           (
                            call/cc (
                              lambda (
                                break19
                              )
                               (
                                letrec (
                                  (
                                    loop18 (
                                      lambda (
                                        _
                                      )
                                       (
                                        if (
                                          < _ n
                                        )
                                         (
                                          begin (
                                            begin (
                                              set! visited (
                                                append visited (
                                                  _list #f
                                                )
                                              )
                                            )
                                          )
                                           (
                                            loop18 (
                                              + _ 1
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
                                  loop18 0
                                )
                              )
                            )
                          )
                           (
                            let (
                              (
                                components (
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
                                        break21
                                      )
                                       (
                                        letrec (
                                          (
                                            loop20 (
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
                                                        v (
                                                          list-ref order (
                                                            - (
                                                              - n i
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
                                                            list-ref visited v
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            let (
                                                              (
                                                                comp (
                                                                  find_component reversed v visited
                                                                )
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                set! components (
                                                                  append components (
                                                                    _list comp
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
                                                        set! i (
                                                          + i 1
                                                        )
                                                      )
                                                    )
                                                  )
                                                   (
                                                    loop20
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
                                          loop20
                                        )
                                      )
                                    )
                                  )
                                   (
                                    ret7 components
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
        main
      )
       (
        call/cc (
          lambda (
            ret22
          )
           (
            let (
              (
                test_graph_1 (
                  _list (
                    _list 2 3
                  )
                   (
                    _list 0
                  )
                   (
                    _list 1
                  )
                   (
                    _list 4
                  )
                   (
                    _list
                  )
                )
              )
            )
             (
              begin (
                let (
                  (
                    test_graph_2 (
                      _list (
                        _list 1 2 3
                      )
                       (
                        _list 2
                      )
                       (
                        _list 0
                      )
                       (
                        _list 4
                      )
                       (
                        _list 5
                      )
                       (
                        _list 3
                      )
                    )
                  )
                )
                 (
                  begin (
                    _display (
                      if (
                        string? (
                          to-str-space (
                            strongly_connected_components test_graph_1
                          )
                        )
                      )
                       (
                        to-str-space (
                          strongly_connected_components test_graph_1
                        )
                      )
                       (
                        to-str (
                          to-str-space (
                            strongly_connected_components test_graph_1
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
                            strongly_connected_components test_graph_2
                          )
                        )
                      )
                       (
                        to-str-space (
                          strongly_connected_components test_graph_2
                        )
                      )
                       (
                        to-str (
                          to-str-space (
                            strongly_connected_components test_graph_2
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
          end24 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur25 (
              quotient (
                * (
                  - end24 start23
                )
                 1000000
              )
               jps26
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur25
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
