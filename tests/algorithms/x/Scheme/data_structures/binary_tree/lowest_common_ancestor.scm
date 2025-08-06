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
      start26 (
        current-jiffy
      )
    )
     (
      jps29 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        pow2 exp
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                res 1
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
                                  < i exp
                                )
                                 (
                                  begin (
                                    set! res (
                                      * res 2
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
                    ret1 res
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
        create_sparse max_node parent
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            let (
              (
                j 1
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
                              _lt (
                                pow2 j
                              )
                               max_node
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
                                        break8
                                      )
                                       (
                                        letrec (
                                          (
                                            loop7 (
                                              lambda (
                                                
                                              )
                                               (
                                                if (
                                                  <= i max_node
                                                )
                                                 (
                                                  begin (
                                                    list-set! (
                                                      list-ref parent j
                                                    )
                                                     i (
                                                      cond (
                                                        (
                                                          string? (
                                                            list-ref parent (
                                                              - j 1
                                                            )
                                                          )
                                                        )
                                                         (
                                                          _substring (
                                                            list-ref parent (
                                                              - j 1
                                                            )
                                                          )
                                                           (
                                                            cond (
                                                              (
                                                                string? (
                                                                  list-ref parent (
                                                                    - j 1
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                _substring (
                                                                  list-ref parent (
                                                                    - j 1
                                                                  )
                                                                )
                                                                 i (
                                                                  + i 1
                                                                )
                                                              )
                                                            )
                                                             (
                                                              (
                                                                hash-table? (
                                                                  list-ref parent (
                                                                    - j 1
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                hash-table-ref (
                                                                  list-ref parent (
                                                                    - j 1
                                                                  )
                                                                )
                                                                 i
                                                              )
                                                            )
                                                             (
                                                              else (
                                                                list-ref (
                                                                  list-ref parent (
                                                                    - j 1
                                                                  )
                                                                )
                                                                 i
                                                              )
                                                            )
                                                          )
                                                           (
                                                            + (
                                                              cond (
                                                                (
                                                                  string? (
                                                                    list-ref parent (
                                                                      - j 1
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  _substring (
                                                                    list-ref parent (
                                                                      - j 1
                                                                    )
                                                                  )
                                                                   i (
                                                                    + i 1
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                (
                                                                  hash-table? (
                                                                    list-ref parent (
                                                                      - j 1
                                                                    )
                                                                  )
                                                                )
                                                                 (
                                                                  hash-table-ref (
                                                                    list-ref parent (
                                                                      - j 1
                                                                    )
                                                                  )
                                                                   i
                                                                )
                                                              )
                                                               (
                                                                else (
                                                                  list-ref (
                                                                    list-ref parent (
                                                                      - j 1
                                                                    )
                                                                  )
                                                                   i
                                                                )
                                                              )
                                                            )
                                                             1
                                                          )
                                                        )
                                                      )
                                                       (
                                                        (
                                                          hash-table? (
                                                            list-ref parent (
                                                              - j 1
                                                            )
                                                          )
                                                        )
                                                         (
                                                          hash-table-ref (
                                                            list-ref parent (
                                                              - j 1
                                                            )
                                                          )
                                                           (
                                                            cond (
                                                              (
                                                                string? (
                                                                  list-ref parent (
                                                                    - j 1
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                _substring (
                                                                  list-ref parent (
                                                                    - j 1
                                                                  )
                                                                )
                                                                 i (
                                                                  + i 1
                                                                )
                                                              )
                                                            )
                                                             (
                                                              (
                                                                hash-table? (
                                                                  list-ref parent (
                                                                    - j 1
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                hash-table-ref (
                                                                  list-ref parent (
                                                                    - j 1
                                                                  )
                                                                )
                                                                 i
                                                              )
                                                            )
                                                             (
                                                              else (
                                                                list-ref (
                                                                  list-ref parent (
                                                                    - j 1
                                                                  )
                                                                )
                                                                 i
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                       (
                                                        else (
                                                          list-ref (
                                                            list-ref parent (
                                                              - j 1
                                                            )
                                                          )
                                                           (
                                                            cond (
                                                              (
                                                                string? (
                                                                  list-ref parent (
                                                                    - j 1
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                _substring (
                                                                  list-ref parent (
                                                                    - j 1
                                                                  )
                                                                )
                                                                 i (
                                                                  + i 1
                                                                )
                                                              )
                                                            )
                                                             (
                                                              (
                                                                hash-table? (
                                                                  list-ref parent (
                                                                    - j 1
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                hash-table-ref (
                                                                  list-ref parent (
                                                                    - j 1
                                                                  )
                                                                )
                                                                 i
                                                              )
                                                            )
                                                             (
                                                              else (
                                                                list-ref (
                                                                  list-ref parent (
                                                                    - j 1
                                                                  )
                                                                )
                                                                 i
                                                              )
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
                                                    loop7
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
                                          loop7
                                        )
                                      )
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
                ret4 parent
              )
            )
          )
        )
      )
    )
     (
      define (
        lowest_common_ancestor u v level parent
      )
       (
        call/cc (
          lambda (
            ret9
          )
           (
            begin (
              if (
                < (
                  list-ref level u
                )
                 (
                  list-ref level v
                )
              )
               (
                begin (
                  let (
                    (
                      temp u
                    )
                  )
                   (
                    begin (
                      set! u v
                    )
                     (
                      set! v temp
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
                  i 18
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
                                >= i 0
                              )
                               (
                                begin (
                                  if (
                                    _ge (
                                      - (
                                        list-ref level u
                                      )
                                       (
                                        pow2 i
                                      )
                                    )
                                     (
                                      list-ref level v
                                    )
                                  )
                                   (
                                    begin (
                                      set! u (
                                        cond (
                                          (
                                            string? (
                                              list-ref parent i
                                            )
                                          )
                                           (
                                            _substring (
                                              list-ref parent i
                                            )
                                             u (
                                              + u 1
                                            )
                                          )
                                        )
                                         (
                                          (
                                            hash-table? (
                                              list-ref parent i
                                            )
                                          )
                                           (
                                            hash-table-ref (
                                              list-ref parent i
                                            )
                                             u
                                          )
                                        )
                                         (
                                          else (
                                            list-ref (
                                              list-ref parent i
                                            )
                                             u
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
                                    - i 1
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
                  if (
                    equal? u v
                  )
                   (
                    begin (
                      ret9 u
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  set! i 18
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
                              
                            )
                             (
                              if (
                                >= i 0
                              )
                               (
                                begin (
                                  let (
                                    (
                                      pu (
                                        cond (
                                          (
                                            string? (
                                              list-ref parent i
                                            )
                                          )
                                           (
                                            _substring (
                                              list-ref parent i
                                            )
                                             u (
                                              + u 1
                                            )
                                          )
                                        )
                                         (
                                          (
                                            hash-table? (
                                              list-ref parent i
                                            )
                                          )
                                           (
                                            hash-table-ref (
                                              list-ref parent i
                                            )
                                             u
                                          )
                                        )
                                         (
                                          else (
                                            list-ref (
                                              list-ref parent i
                                            )
                                             u
                                          )
                                        )
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      let (
                                        (
                                          pv (
                                            cond (
                                              (
                                                string? (
                                                  list-ref parent i
                                                )
                                              )
                                               (
                                                _substring (
                                                  list-ref parent i
                                                )
                                                 v (
                                                  + v 1
                                                )
                                              )
                                            )
                                             (
                                              (
                                                hash-table? (
                                                  list-ref parent i
                                                )
                                              )
                                               (
                                                hash-table-ref (
                                                  list-ref parent i
                                                )
                                                 v
                                              )
                                            )
                                             (
                                              else (
                                                list-ref (
                                                  list-ref parent i
                                                )
                                                 v
                                              )
                                            )
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          if (
                                            and (
                                              not (
                                                equal? pu 0
                                              )
                                            )
                                             (
                                              not (
                                                equal? pu pv
                                              )
                                            )
                                          )
                                           (
                                            begin (
                                              set! u pu
                                            )
                                             (
                                              set! v pv
                                            )
                                          )
                                           (
                                            quote (
                                              
                                            )
                                          )
                                        )
                                         (
                                          set! i (
                                            - i 1
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                                 (
                                  loop12
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
                        loop12
                      )
                    )
                  )
                )
                 (
                  ret9 (
                    cond (
                      (
                        string? (
                          list-ref parent 0
                        )
                      )
                       (
                        _substring (
                          list-ref parent 0
                        )
                         u (
                          + u 1
                        )
                      )
                    )
                     (
                      (
                        hash-table? (
                          list-ref parent 0
                        )
                      )
                       (
                        hash-table-ref (
                          list-ref parent 0
                        )
                         u
                      )
                    )
                     (
                      else (
                        list-ref (
                          list-ref parent 0
                        )
                         u
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
        breadth_first_search level parent max_node graph root
      )
       (
        call/cc (
          lambda (
            ret14
          )
           (
            begin (
              list-set! level root 0
            )
             (
              let (
                (
                  q (
                    _list
                  )
                )
              )
               (
                begin (
                  set! q (
                    append q (
                      _list root
                    )
                  )
                )
                 (
                  let (
                    (
                      head 0
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
                                    < head (
                                      _len q
                                    )
                                  )
                                   (
                                    begin (
                                      let (
                                        (
                                          u (
                                            list-ref q head
                                          )
                                        )
                                      )
                                       (
                                        begin (
                                          set! head (
                                            + head 1
                                          )
                                        )
                                         (
                                          let (
                                            (
                                              adj (
                                                hash-table-ref/default graph u (
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
                                                  j 0
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
                                                                < j (
                                                                  _len adj
                                                                )
                                                              )
                                                               (
                                                                begin (
                                                                  let (
                                                                    (
                                                                      v (
                                                                        list-ref adj j
                                                                      )
                                                                    )
                                                                  )
                                                                   (
                                                                    begin (
                                                                      if (
                                                                        equal? (
                                                                          list-ref level v
                                                                        )
                                                                         (
                                                                          - 0 1
                                                                        )
                                                                      )
                                                                       (
                                                                        begin (
                                                                          list-set! level v (
                                                                            + (
                                                                              list-ref level u
                                                                            )
                                                                             1
                                                                          )
                                                                        )
                                                                         (
                                                                          list-set! (
                                                                            list-ref parent 0
                                                                          )
                                                                           v u
                                                                        )
                                                                         (
                                                                          set! q (
                                                                            append q (
                                                                              _list v
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
      )
    )
     (
      define (
        main
      )
       (
        call/cc (
          lambda (
            ret19
          )
           (
            let (
              (
                max_node 13
              )
            )
             (
              begin (
                let (
                  (
                    parent (
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
                                      < i 20
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            row (
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
                                                    break23
                                                  )
                                                   (
                                                    letrec (
                                                      (
                                                        loop22 (
                                                          lambda (
                                                            
                                                          )
                                                           (
                                                            if (
                                                              < j (
                                                                + max_node 10
                                                              )
                                                            )
                                                             (
                                                              begin (
                                                                set! row (
                                                                  append row (
                                                                    _list 0
                                                                  )
                                                                )
                                                              )
                                                               (
                                                                set! j (
                                                                  + j 1
                                                                )
                                                              )
                                                               (
                                                                loop22
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
                                                      loop22
                                                    )
                                                  )
                                                )
                                              )
                                               (
                                                set! parent (
                                                  append parent (
                                                    _list row
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
                        let (
                          (
                            level (
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
                                break25
                              )
                               (
                                letrec (
                                  (
                                    loop24 (
                                      lambda (
                                        
                                      )
                                       (
                                        if (
                                          < i (
                                            + max_node 10
                                          )
                                        )
                                         (
                                          begin (
                                            set! level (
                                              append level (
                                                _list (
                                                  - 0 1
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
                                            loop24
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
                                  loop24
                                )
                              )
                            )
                          )
                           (
                            let (
                              (
                                graph (
                                  alist->hash-table (
                                    _list
                                  )
                                )
                              )
                            )
                             (
                              begin (
                                hash-table-set! graph 1 (
                                  _list 2 3 4
                                )
                              )
                               (
                                hash-table-set! graph 2 (
                                  _list 5
                                )
                              )
                               (
                                hash-table-set! graph 3 (
                                  _list 6 7
                                )
                              )
                               (
                                hash-table-set! graph 4 (
                                  _list 8
                                )
                              )
                               (
                                hash-table-set! graph 5 (
                                  _list 9 10
                                )
                              )
                               (
                                hash-table-set! graph 6 (
                                  _list 11
                                )
                              )
                               (
                                hash-table-set! graph 7 (
                                  _list
                                )
                              )
                               (
                                hash-table-set! graph 8 (
                                  _list 12 13
                                )
                              )
                               (
                                hash-table-set! graph 9 (
                                  _list
                                )
                              )
                               (
                                hash-table-set! graph 10 (
                                  _list
                                )
                              )
                               (
                                hash-table-set! graph 11 (
                                  _list
                                )
                              )
                               (
                                hash-table-set! graph 12 (
                                  _list
                                )
                              )
                               (
                                hash-table-set! graph 13 (
                                  _list
                                )
                              )
                               (
                                breadth_first_search level parent max_node graph 1
                              )
                               (
                                set! parent (
                                  create_sparse max_node parent
                                )
                              )
                               (
                                _display (
                                  if (
                                    string? (
                                      string-append "LCA of node 1 and 3 is: " (
                                        to-str-space (
                                          lowest_common_ancestor 1 3 level parent
                                        )
                                      )
                                    )
                                  )
                                   (
                                    string-append "LCA of node 1 and 3 is: " (
                                      to-str-space (
                                        lowest_common_ancestor 1 3 level parent
                                      )
                                    )
                                  )
                                   (
                                    to-str (
                                      string-append "LCA of node 1 and 3 is: " (
                                        to-str-space (
                                          lowest_common_ancestor 1 3 level parent
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
                                _display (
                                  if (
                                    string? (
                                      string-append "LCA of node 5 and 6 is: " (
                                        to-str-space (
                                          lowest_common_ancestor 5 6 level parent
                                        )
                                      )
                                    )
                                  )
                                   (
                                    string-append "LCA of node 5 and 6 is: " (
                                      to-str-space (
                                        lowest_common_ancestor 5 6 level parent
                                      )
                                    )
                                  )
                                   (
                                    to-str (
                                      string-append "LCA of node 5 and 6 is: " (
                                        to-str-space (
                                          lowest_common_ancestor 5 6 level parent
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
                                _display (
                                  if (
                                    string? (
                                      string-append "LCA of node 7 and 11 is: " (
                                        to-str-space (
                                          lowest_common_ancestor 7 11 level parent
                                        )
                                      )
                                    )
                                  )
                                   (
                                    string-append "LCA of node 7 and 11 is: " (
                                      to-str-space (
                                        lowest_common_ancestor 7 11 level parent
                                      )
                                    )
                                  )
                                   (
                                    to-str (
                                      string-append "LCA of node 7 and 11 is: " (
                                        to-str-space (
                                          lowest_common_ancestor 7 11 level parent
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
                                _display (
                                  if (
                                    string? (
                                      string-append "LCA of node 6 and 7 is: " (
                                        to-str-space (
                                          lowest_common_ancestor 6 7 level parent
                                        )
                                      )
                                    )
                                  )
                                   (
                                    string-append "LCA of node 6 and 7 is: " (
                                      to-str-space (
                                        lowest_common_ancestor 6 7 level parent
                                      )
                                    )
                                  )
                                   (
                                    to-str (
                                      string-append "LCA of node 6 and 7 is: " (
                                        to-str-space (
                                          lowest_common_ancestor 6 7 level parent
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
                                _display (
                                  if (
                                    string? (
                                      string-append "LCA of node 4 and 12 is: " (
                                        to-str-space (
                                          lowest_common_ancestor 4 12 level parent
                                        )
                                      )
                                    )
                                  )
                                   (
                                    string-append "LCA of node 4 and 12 is: " (
                                      to-str-space (
                                        lowest_common_ancestor 4 12 level parent
                                      )
                                    )
                                  )
                                   (
                                    to-str (
                                      string-append "LCA of node 4 and 12 is: " (
                                        to-str-space (
                                          lowest_common_ancestor 4 12 level parent
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
                                _display (
                                  if (
                                    string? (
                                      string-append "LCA of node 8 and 8 is: " (
                                        to-str-space (
                                          lowest_common_ancestor 8 8 level parent
                                        )
                                      )
                                    )
                                  )
                                   (
                                    string-append "LCA of node 8 and 8 is: " (
                                      to-str-space (
                                        lowest_common_ancestor 8 8 level parent
                                      )
                                    )
                                  )
                                   (
                                    to-str (
                                      string-append "LCA of node 8 and 8 is: " (
                                        to-str-space (
                                          lowest_common_ancestor 8 8 level parent
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
          end27 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur28 (
              quotient (
                * (
                  - end27 start26
                )
                 1000000
              )
               jps29
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur28
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
