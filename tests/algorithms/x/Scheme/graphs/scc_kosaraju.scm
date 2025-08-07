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
      define (
        dfs u graph visit stack
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            begin (
              if (
                list-ref visit u
              )
               (
                begin (
                  ret1 stack
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              list-set! visit u #t
            )
             (
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
                                  v (
                                    car xs
                                  )
                                )
                              )
                               (
                                begin (
                                  set! stack (
                                    dfs v graph visit stack
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
                      list-ref graph u
                    )
                  )
                )
              )
            )
             (
              set! stack (
                append stack (
                  _list u
                )
              )
            )
             (
              ret1 stack
            )
          )
        )
      )
    )
     (
      define (
        dfs2 u reversed_graph visit component
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            begin (
              if (
                list-ref visit u
              )
               (
                begin (
                  ret4 component
                )
              )
               (
                quote (
                  
                )
              )
            )
             (
              list-set! visit u #t
            )
             (
              set! component (
                append component (
                  _list u
                )
              )
            )
             (
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
                                  v (
                                    car xs
                                  )
                                )
                              )
                               (
                                begin (
                                  set! component (
                                    dfs2 v reversed_graph visit component
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
                      list-ref reversed_graph u
                    )
                  )
                )
              )
            )
             (
              ret4 component
            )
          )
        )
      )
    )
     (
      define (
        kosaraju graph
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
                    reversed_graph (
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
                                      < i n
                                    )
                                     (
                                      begin (
                                        set! reversed_graph (
                                          append reversed_graph (
                                            _list (
                                              _list
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
                        set! i 0
                      )
                       (
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
                                      < i n
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
                                                            list-set! reversed_graph v (
                                                              append (
                                                                list-ref reversed_graph v
                                                              )
                                                               (
                                                                _list i
                                                              )
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
                                              loop12 (
                                                list-ref graph i
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
                        let (
                          (
                            visit (
                              _list
                            )
                          )
                        )
                         (
                          begin (
                            set! i 0
                          )
                           (
                            call/cc (
                              lambda (
                                break15
                              )
                               (
                                letrec (
                                  (
                                    loop14 (
                                      lambda (
                                        
                                      )
                                       (
                                        if (
                                          < i n
                                        )
                                         (
                                          begin (
                                            set! visit (
                                              append visit (
                                                _list #f
                                              )
                                            )
                                          )
                                           (
                                            set! i (
                                              + i 1
                                            )
                                          )
                                           (
                                            loop14
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
                                  loop14
                                )
                              )
                            )
                          )
                           (
                            let (
                              (
                                stack (
                                  _list
                                )
                              )
                            )
                             (
                              begin (
                                set! i 0
                              )
                               (
                                call/cc (
                                  lambda (
                                    break17
                                  )
                                   (
                                    letrec (
                                      (
                                        loop16 (
                                          lambda (
                                            
                                          )
                                           (
                                            if (
                                              < i n
                                            )
                                             (
                                              begin (
                                                if (
                                                  eq? (
                                                    list-ref visit i
                                                  )
                                                   #f
                                                )
                                                 (
                                                  begin (
                                                    set! stack (
                                                      dfs i graph visit stack
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
                                               (
                                                loop16
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
                                      loop16
                                    )
                                  )
                                )
                              )
                               (
                                set! i 0
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
                                            
                                          )
                                           (
                                            if (
                                              < i n
                                            )
                                             (
                                              begin (
                                                list-set! visit i #f
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
                                             (
                                              quote (
                                                
                                              )
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
                                let (
                                  (
                                    scc (
                                      _list
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        idx (
                                          - (
                                            _len stack
                                          )
                                           1
                                        )
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
                                                      >= idx 0
                                                    )
                                                     (
                                                      begin (
                                                        let (
                                                          (
                                                            node (
                                                              list-ref stack idx
                                                            )
                                                          )
                                                        )
                                                         (
                                                          begin (
                                                            if (
                                                              eq? (
                                                                list-ref visit node
                                                              )
                                                               #f
                                                            )
                                                             (
                                                              begin (
                                                                let (
                                                                  (
                                                                    component (
                                                                      _list
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  begin (
                                                                    set! component (
                                                                      dfs2 node reversed_graph visit component
                                                                    )
                                                                  )
                                                                   (
                                                                    set! scc (
                                                                      append scc (
                                                                        _list component
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
                                                            set! idx (
                                                              - idx 1
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
                                        ret7 scc
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
                graph (
                  _list (
                    _list 1
                  )
                   (
                    _list 2
                  )
                   (
                    _list 0 3
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
                    comps (
                      kosaraju graph
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
                            break24
                          )
                           (
                            letrec (
                              (
                                loop23 (
                                  lambda (
                                    
                                  )
                                   (
                                    if (
                                      < i (
                                        _len comps
                                      )
                                    )
                                     (
                                      begin (
                                        _display (
                                          if (
                                            string? (
                                              cond (
                                                (
                                                  string? comps
                                                )
                                                 (
                                                  _substring comps i (
                                                    + i 1
                                                  )
                                                )
                                              )
                                               (
                                                (
                                                  hash-table? comps
                                                )
                                                 (
                                                  hash-table-ref comps i
                                                )
                                              )
                                               (
                                                else (
                                                  list-ref comps i
                                                )
                                              )
                                            )
                                          )
                                           (
                                            cond (
                                              (
                                                string? comps
                                              )
                                               (
                                                _substring comps i (
                                                  + i 1
                                                )
                                              )
                                            )
                                             (
                                              (
                                                hash-table? comps
                                              )
                                               (
                                                hash-table-ref comps i
                                              )
                                            )
                                             (
                                              else (
                                                list-ref comps i
                                              )
                                            )
                                          )
                                           (
                                            to-str (
                                              cond (
                                                (
                                                  string? comps
                                                )
                                                 (
                                                  _substring comps i (
                                                    + i 1
                                                  )
                                                )
                                              )
                                               (
                                                (
                                                  hash-table? comps
                                                )
                                                 (
                                                  hash-table-ref comps i
                                                )
                                              )
                                               (
                                                else (
                                                  list-ref comps i
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
                                        set! i (
                                          + i 1
                                        )
                                      )
                                       (
                                        loop23
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
                              loop23
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
      main
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
