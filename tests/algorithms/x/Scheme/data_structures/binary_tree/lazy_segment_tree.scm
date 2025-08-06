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
      start15 (
        current-jiffy
      )
    )
     (
      jps18 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define (
        init_int_array n
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                arr (
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
                                    + (
                                      * 4 n
                                    )
                                     5
                                  )
                                )
                                 (
                                  begin (
                                    set! arr (
                                      append arr (
                                        _list 0
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
                    ret1 arr
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
        init_bool_array n
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            let (
              (
                arr (
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
                                    + (
                                      * 4 n
                                    )
                                     5
                                  )
                                )
                                 (
                                  begin (
                                    set! arr (
                                      append arr (
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
                    ret4 arr
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
        left idx
      )
       (
        call/cc (
          lambda (
            ret7
          )
           (
            ret7 (
              * idx 2
            )
          )
        )
      )
    )
     (
      define (
        right idx
      )
       (
        call/cc (
          lambda (
            ret8
          )
           (
            ret8 (
              + (
                * idx 2
              )
               1
            )
          )
        )
      )
    )
     (
      define (
        build segment_tree idx l r a
      )
       (
        call/cc (
          lambda (
            ret9
          )
           (
            if (
              equal? l r
            )
             (
              begin (
                list-set! segment_tree idx (
                  list-ref a (
                    - l 1
                  )
                )
              )
            )
             (
              begin (
                let (
                  (
                    mid (
                      _div (
                        + l r
                      )
                       2
                    )
                  )
                )
                 (
                  begin (
                    build segment_tree (
                      left idx
                    )
                     l mid a
                  )
                   (
                    build segment_tree (
                      right idx
                    )
                     (
                      + mid 1
                    )
                     r a
                  )
                   (
                    let (
                      (
                        lv (
                          list-ref segment_tree (
                            left idx
                          )
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            rv (
                              list-ref segment_tree (
                                right idx
                              )
                            )
                          )
                        )
                         (
                          begin (
                            if (
                              > lv rv
                            )
                             (
                              begin (
                                list-set! segment_tree idx lv
                              )
                            )
                             (
                              begin (
                                list-set! segment_tree idx rv
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
        update segment_tree lazy flag idx l r a b val
      )
       (
        call/cc (
          lambda (
            ret10
          )
           (
            begin (
              if (
                list-ref flag idx
              )
               (
                begin (
                  list-set! segment_tree idx (
                    list-ref lazy idx
                  )
                )
                 (
                  list-set! flag idx #f
                )
                 (
                  if (
                    not (
                      equal? l r
                    )
                  )
                   (
                    begin (
                      list-set! lazy (
                        left idx
                      )
                       (
                        list-ref lazy idx
                      )
                    )
                     (
                      list-set! lazy (
                        right idx
                      )
                       (
                        list-ref lazy idx
                      )
                    )
                     (
                      list-set! flag (
                        left idx
                      )
                       #t
                    )
                     (
                      list-set! flag (
                        right idx
                      )
                       #t
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
              if (
                or (
                  < r a
                )
                 (
                  > l b
                )
              )
               (
                begin (
                  ret10 (
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
              if (
                and (
                  >= l a
                )
                 (
                  <= r b
                )
              )
               (
                begin (
                  list-set! segment_tree idx val
                )
                 (
                  if (
                    not (
                      equal? l r
                    )
                  )
                   (
                    begin (
                      list-set! lazy (
                        left idx
                      )
                       val
                    )
                     (
                      list-set! lazy (
                        right idx
                      )
                       val
                    )
                     (
                      list-set! flag (
                        left idx
                      )
                       #t
                    )
                     (
                      list-set! flag (
                        right idx
                      )
                       #t
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  ret10 (
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
                  mid (
                    _div (
                      + l r
                    )
                     2
                  )
                )
              )
               (
                begin (
                  update segment_tree lazy flag (
                    left idx
                  )
                   l mid a b val
                )
                 (
                  update segment_tree lazy flag (
                    right idx
                  )
                   (
                    + mid 1
                  )
                   r a b val
                )
                 (
                  let (
                    (
                      lv (
                        list-ref segment_tree (
                          left idx
                        )
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          rv (
                            list-ref segment_tree (
                              right idx
                            )
                          )
                        )
                      )
                       (
                        begin (
                          if (
                            > lv rv
                          )
                           (
                            begin (
                              list-set! segment_tree idx lv
                            )
                          )
                           (
                            begin (
                              list-set! segment_tree idx rv
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
          NEG_INF (
            - 1000000000
          )
        )
      )
       (
        begin (
          define (
            query segment_tree lazy flag idx l r a b
          )
           (
            call/cc (
              lambda (
                ret11
              )
               (
                begin (
                  if (
                    list-ref flag idx
                  )
                   (
                    begin (
                      list-set! segment_tree idx (
                        list-ref lazy idx
                      )
                    )
                     (
                      list-set! flag idx #f
                    )
                     (
                      if (
                        not (
                          equal? l r
                        )
                      )
                       (
                        begin (
                          list-set! lazy (
                            left idx
                          )
                           (
                            list-ref lazy idx
                          )
                        )
                         (
                          list-set! lazy (
                            right idx
                          )
                           (
                            list-ref lazy idx
                          )
                        )
                         (
                          list-set! flag (
                            left idx
                          )
                           #t
                        )
                         (
                          list-set! flag (
                            right idx
                          )
                           #t
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
                  if (
                    or (
                      < r a
                    )
                     (
                      > l b
                    )
                  )
                   (
                    begin (
                      ret11 NEG_INF
                    )
                  )
                   (
                    quote (
                      
                    )
                  )
                )
                 (
                  if (
                    and (
                      >= l a
                    )
                     (
                      <= r b
                    )
                  )
                   (
                    begin (
                      ret11 (
                        list-ref segment_tree idx
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
                      mid (
                        _div (
                          + l r
                        )
                         2
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          q1 (
                            query segment_tree lazy flag (
                              left idx
                            )
                             l mid a b
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              q2 (
                                query segment_tree lazy flag (
                                  right idx
                                )
                                 (
                                  + mid 1
                                )
                                 r a b
                              )
                            )
                          )
                           (
                            begin (
                              if (
                                _gt q1 q2
                              )
                               (
                                begin (
                                  ret11 q1
                                )
                              )
                               (
                                begin (
                                  ret11 q2
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
            segtree_to_string segment_tree lazy flag n
          )
           (
            call/cc (
              lambda (
                ret12
              )
               (
                let (
                  (
                    res "["
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
                                      <= i n
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            v (
                                              query segment_tree lazy flag 1 1 n i i
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            set! res (
                                              string-append res (
                                                to-str-space v
                                              )
                                            )
                                          )
                                           (
                                            if (
                                              < i n
                                            )
                                             (
                                              begin (
                                                set! res (
                                                  string-append res ", "
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
                        set! res (
                          string-append res "]"
                        )
                      )
                       (
                        ret12 res
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
              A (
                _list 1 2 (
                  - 4
                )
                 7 3 (
                  - 5
                )
                 6 11 (
                  - 20
                )
                 9 14 15 5 2 (
                  - 8
                )
              )
            )
          )
           (
            begin (
              let (
                (
                  n 15
                )
              )
               (
                begin (
                  let (
                    (
                      segment_tree (
                        init_int_array n
                      )
                    )
                  )
                   (
                    begin (
                      let (
                        (
                          lazy (
                            init_int_array n
                          )
                        )
                      )
                       (
                        begin (
                          let (
                            (
                              flag (
                                init_bool_array n
                              )
                            )
                          )
                           (
                            begin (
                              build segment_tree 1 1 n A
                            )
                             (
                              _display (
                                if (
                                  string? (
                                    query segment_tree lazy flag 1 1 n 4 6
                                  )
                                )
                                 (
                                  query segment_tree lazy flag 1 1 n 4 6
                                )
                                 (
                                  to-str (
                                    query segment_tree lazy flag 1 1 n 4 6
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
                                    query segment_tree lazy flag 1 1 n 7 11
                                  )
                                )
                                 (
                                  query segment_tree lazy flag 1 1 n 7 11
                                )
                                 (
                                  to-str (
                                    query segment_tree lazy flag 1 1 n 7 11
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
                                    query segment_tree lazy flag 1 1 n 7 12
                                  )
                                )
                                 (
                                  query segment_tree lazy flag 1 1 n 7 12
                                )
                                 (
                                  to-str (
                                    query segment_tree lazy flag 1 1 n 7 12
                                  )
                                )
                              )
                            )
                             (
                              newline
                            )
                             (
                              update segment_tree lazy flag 1 1 n 1 3 111
                            )
                             (
                              _display (
                                if (
                                  string? (
                                    query segment_tree lazy flag 1 1 n 1 15
                                  )
                                )
                                 (
                                  query segment_tree lazy flag 1 1 n 1 15
                                )
                                 (
                                  to-str (
                                    query segment_tree lazy flag 1 1 n 1 15
                                  )
                                )
                              )
                            )
                             (
                              newline
                            )
                             (
                              update segment_tree lazy flag 1 1 n 7 8 235
                            )
                             (
                              _display (
                                if (
                                  string? (
                                    segtree_to_string segment_tree lazy flag n
                                  )
                                )
                                 (
                                  segtree_to_string segment_tree lazy flag n
                                )
                                 (
                                  to-str (
                                    segtree_to_string segment_tree lazy flag n
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
     (
      let (
        (
          end16 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur17 (
              quotient (
                * (
                  - end16 start15
                )
                 1000000
              )
               jps18
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur17
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
