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
      start16 (
        current-jiffy
      )
    )
     (
      jps19 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      define OP_NODE 0
    )
     (
      define OP_EMPTY 1
    )
     (
      define (
        digit ch
      )
       (
        call/cc (
          lambda (
            ret1
          )
           (
            let (
              (
                digits "0123456789"
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
                                    _len digits
                                  )
                                )
                                 (
                                  begin (
                                    if (
                                      string=? (
                                        _substring digits i (
                                          + i 1
                                        )
                                      )
                                       ch
                                    )
                                     (
                                      begin (
                                        ret1 i
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
                    ret1 0
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
        to_int s
      )
       (
        call/cc (
          lambda (
            ret4
          )
           (
            let (
              (
                i 0
              )
            )
             (
              begin (
                let (
                  (
                    sign 1
                  )
                )
                 (
                  begin (
                    if (
                      and (
                        > (
                          _len s
                        )
                         0
                      )
                       (
                        string=? (
                          _substring s 0 1
                        )
                         "-"
                      )
                    )
                     (
                      begin (
                        set! sign (
                          - 1
                        )
                      )
                       (
                        set! i 1
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
                        num 0
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
                                        _len s
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            ch (
                                              _substring s i (
                                                + i 1
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            set! num (
                                              _add (
                                                * num 10
                                              )
                                               (
                                                digit ch
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
                        ret4 (
                          * sign num
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
        split s sep
      )
       (
        call/cc (
          lambda (
            ret7
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
                    current ""
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
                                        _len s
                                      )
                                    )
                                     (
                                      begin (
                                        let (
                                          (
                                            ch (
                                              _substring s i (
                                                + i 1
                                              )
                                            )
                                          )
                                        )
                                         (
                                          begin (
                                            if (
                                              string=? ch sep
                                            )
                                             (
                                              begin (
                                                set! res (
                                                  append res (
                                                    _list current
                                                  )
                                                )
                                              )
                                               (
                                                set! current ""
                                              )
                                            )
                                             (
                                              begin (
                                                set! current (
                                                  string-append current ch
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
                        set! res (
                          append res (
                            _list current
                          )
                        )
                      )
                       (
                        ret7 res
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
        serialize node
      )
       (
        call/cc (
          lambda (
            ret10
          )
           (
            ret10 (
              let (
                (
                  match11 node
                )
              )
               (
                if (
                  equal? (
                    hash-table-ref match11 "__tag"
                  )
                   OP_EMPTY
                )
                 "null" (
                  if (
                    equal? (
                      hash-table-ref match11 "__tag"
                    )
                     OP_NODE
                  )
                   (
                    let (
                      (
                        l (
                          hash-table-ref match11 "left"
                        )
                      )
                       (
                        v (
                          hash-table-ref match11 "value"
                        )
                      )
                       (
                        r (
                          hash-table-ref match11 "right"
                        )
                      )
                    )
                     (
                      string-append (
                        string-append (
                          string-append (
                            string-append (
                              to-str-space v
                            )
                             ","
                          )
                           (
                            serialize l
                          )
                        )
                         ","
                      )
                       (
                        serialize r
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
      )
    )
     (
      define (
        build nodes idx
      )
       (
        call/cc (
          lambda (
            ret12
          )
           (
            let (
              (
                value (
                  list-ref nodes idx
                )
              )
            )
             (
              begin (
                if (
                  string=? value "null"
                )
                 (
                  begin (
                    ret12 (
                      alist->hash-table (
                        _list (
                          cons "node" (
                            alist->hash-table (
                              _list (
                                cons "__tag" OP_EMPTY
                              )
                            )
                          )
                        )
                         (
                          cons "next" (
                            + idx 1
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
                let (
                  (
                    left_res (
                      build nodes (
                        + idx 1
                      )
                    )
                  )
                )
                 (
                  begin (
                    let (
                      (
                        right_res (
                          build nodes (
                            hash-table-ref left_res "next"
                          )
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            node (
                              alist->hash-table (
                                _list (
                                  cons "__tag" OP_NODE
                                )
                                 (
                                  cons "left" (
                                    hash-table-ref left_res "node"
                                  )
                                )
                                 (
                                  cons "value" (
                                    to_int value
                                  )
                                )
                                 (
                                  cons "right" (
                                    hash-table-ref right_res "node"
                                  )
                                )
                              )
                            )
                          )
                        )
                         (
                          begin (
                            ret12 (
                              alist->hash-table (
                                _list (
                                  cons "node" node
                                )
                                 (
                                  cons "next" (
                                    hash-table-ref right_res "next"
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
        deserialize data
      )
       (
        call/cc (
          lambda (
            ret13
          )
           (
            let (
              (
                nodes (
                  split data ","
                )
              )
            )
             (
              begin (
                let (
                  (
                    res (
                      build nodes 0
                    )
                  )
                )
                 (
                  begin (
                    ret13 (
                      hash-table-ref res "node"
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
        five_tree
      )
       (
        call/cc (
          lambda (
            ret14
          )
           (
            let (
              (
                left_child (
                  alist->hash-table (
                    _list (
                      cons "__tag" OP_NODE
                    )
                     (
                      cons "left" 2
                    )
                     (
                      cons "value" (
                        alist->hash-table (
                          _list (
                            cons "__tag" OP_EMPTY
                          )
                        )
                      )
                    )
                     (
                      cons "right" (
                        alist->hash-table (
                          _list (
                            cons "__tag" OP_EMPTY
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
                    right_left (
                      alist->hash-table (
                        _list (
                          cons "__tag" OP_NODE
                        )
                         (
                          cons "left" 4
                        )
                         (
                          cons "value" (
                            alist->hash-table (
                              _list (
                                cons "__tag" OP_EMPTY
                              )
                            )
                          )
                        )
                         (
                          cons "right" (
                            alist->hash-table (
                              _list (
                                cons "__tag" OP_EMPTY
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
                        right_right (
                          alist->hash-table (
                            _list (
                              cons "__tag" OP_NODE
                            )
                             (
                              cons "left" 5
                            )
                             (
                              cons "value" (
                                alist->hash-table (
                                  _list (
                                    cons "__tag" OP_EMPTY
                                  )
                                )
                              )
                            )
                             (
                              cons "right" (
                                alist->hash-table (
                                  _list (
                                    cons "__tag" OP_EMPTY
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
                            right_child (
                              alist->hash-table (
                                _list (
                                  cons "__tag" OP_NODE
                                )
                                 (
                                  cons "left" 3
                                )
                                 (
                                  cons "value" right_left
                                )
                                 (
                                  cons "right" right_right
                                )
                              )
                            )
                          )
                        )
                         (
                          begin (
                            ret14 (
                              alist->hash-table (
                                _list (
                                  cons "__tag" OP_NODE
                                )
                                 (
                                  cons "left" 1
                                )
                                 (
                                  cons "value" left_child
                                )
                                 (
                                  cons "right" right_child
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
            ret15
          )
           (
            let (
              (
                root (
                  five_tree
                )
              )
            )
             (
              begin (
                let (
                  (
                    serial (
                      serialize root
                    )
                  )
                )
                 (
                  begin (
                    _display (
                      if (
                        string? serial
                      )
                       serial (
                        to-str serial
                      )
                    )
                  )
                   (
                    newline
                  )
                   (
                    let (
                      (
                        rebuilt (
                          deserialize serial
                        )
                      )
                    )
                     (
                      begin (
                        let (
                          (
                            serial2 (
                              serialize rebuilt
                            )
                          )
                        )
                         (
                          begin (
                            _display (
                              if (
                                string? serial2
                              )
                               serial2 (
                                to-str serial2
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
                                  if (
                                    equal? serial serial2
                                  )
                                   #t #f
                                )
                              )
                               (
                                if (
                                  equal? serial serial2
                                )
                                 #t #f
                              )
                               (
                                to-str (
                                  if (
                                    equal? serial serial2
                                  )
                                   #t #f
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
     (
      main
    )
     (
      let (
        (
          end17 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur18 (
              quotient (
                * (
                  - end17 start16
                )
                 1000000
              )
               jps19
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur18
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
