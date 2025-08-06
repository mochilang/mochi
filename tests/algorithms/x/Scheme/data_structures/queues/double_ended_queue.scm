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
        empty_deque
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
                  cons "data" (
                    _list
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
        push_back dq value
      )
       (
        call/cc (
          lambda (
            ret2
          )
           (
            ret2 (
              alist->hash-table (
                _list (
                  cons "data" (
                    append (
                      hash-table-ref dq "data"
                    )
                     (
                      _list value
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
        push_front dq value
      )
       (
        call/cc (
          lambda (
            ret3
          )
           (
            let (
              (
                res (
                  _list value
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
                        break5
                      )
                       (
                        letrec (
                          (
                            loop4 (
                              lambda (
                                
                              )
                               (
                                if (
                                  < i (
                                    _len (
                                      hash-table-ref dq "data"
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    set! res (
                                      append res (
                                        _list (
                                          list-ref (
                                            hash-table-ref dq "data"
                                          )
                                           i
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
                                    loop4
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
                          loop4
                        )
                      )
                    )
                  )
                   (
                    ret3 (
                      alist->hash-table (
                        _list (
                          cons "data" res
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
        extend_back dq values
      )
       (
        call/cc (
          lambda (
            ret6
          )
           (
            let (
              (
                res (
                  hash-table-ref dq "data"
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
                                  < i (
                                    _len values
                                  )
                                )
                                 (
                                  begin (
                                    set! res (
                                      append res (
                                        _list (
                                          list-ref values i
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
                    ret6 (
                      alist->hash-table (
                        _list (
                          cons "data" res
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
        extend_front dq values
      )
       (
        call/cc (
          lambda (
            ret9
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
                    i (
                      - (
                        _len values
                      )
                       1
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
                                
                              )
                               (
                                if (
                                  >= i 0
                                )
                                 (
                                  begin (
                                    set! res (
                                      append res (
                                        _list (
                                          list-ref values i
                                        )
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
                    let (
                      (
                        j 0
                      )
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
                                    
                                  )
                                   (
                                    if (
                                      < j (
                                        _len (
                                          hash-table-ref dq "data"
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        set! res (
                                          append res (
                                            _list (
                                              list-ref (
                                                hash-table-ref dq "data"
                                              )
                                               j
                                            )
                                          )
                                        )
                                      )
                                       (
                                        set! j (
                                          + j 1
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
                          alist->hash-table (
                            _list (
                              cons "data" res
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
        pop_back dq
      )
       (
        call/cc (
          lambda (
            ret14
          )
           (
            begin (
              if (
                equal? (
                  _len (
                    hash-table-ref dq "data"
                  )
                )
                 0
              )
               (
                begin (
                  panic "pop from empty deque"
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
                  res (
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
                                      - (
                                        _len (
                                          hash-table-ref dq "data"
                                        )
                                      )
                                       1
                                    )
                                  )
                                   (
                                    begin (
                                      set! res (
                                        append res (
                                          _list (
                                            list-ref (
                                              hash-table-ref dq "data"
                                            )
                                             i
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
                     (
                      ret14 (
                        alist->hash-table (
                          _list (
                            cons "deque" (
                              alist->hash-table (
                                _list (
                                  cons "data" res
                                )
                              )
                            )
                          )
                           (
                            cons "value" (
                              list-ref (
                                hash-table-ref dq "data"
                              )
                               (
                                - (
                                  _len (
                                    hash-table-ref dq "data"
                                  )
                                )
                                 1
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
        pop_front dq
      )
       (
        call/cc (
          lambda (
            ret17
          )
           (
            begin (
              if (
                equal? (
                  _len (
                    hash-table-ref dq "data"
                  )
                )
                 0
              )
               (
                begin (
                  panic "popleft from empty deque"
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
                  res (
                    _list
                  )
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
                                      _len (
                                        hash-table-ref dq "data"
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      set! res (
                                        append res (
                                          _list (
                                            list-ref (
                                              hash-table-ref dq "data"
                                            )
                                             i
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
                      ret17 (
                        alist->hash-table (
                          _list (
                            cons "deque" (
                              alist->hash-table (
                                _list (
                                  cons "data" res
                                )
                              )
                            )
                          )
                           (
                            cons "value" (
                              list-ref (
                                hash-table-ref dq "data"
                              )
                               0
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
        is_empty dq
      )
       (
        call/cc (
          lambda (
            ret20
          )
           (
            ret20 (
              equal? (
                _len (
                  hash-table-ref dq "data"
                )
              )
               0
            )
          )
        )
      )
    )
     (
      define (
        length dq
      )
       (
        call/cc (
          lambda (
            ret21
          )
           (
            ret21 (
              _len (
                hash-table-ref dq "data"
              )
            )
          )
        )
      )
    )
     (
      define (
        to_string dq
      )
       (
        call/cc (
          lambda (
            ret22
          )
           (
            begin (
              if (
                equal? (
                  _len (
                    hash-table-ref dq "data"
                  )
                )
                 0
              )
               (
                begin (
                  ret22 "[]"
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
                  s (
                    string-append "[" (
                      to-str-space (
                        list-ref (
                          hash-table-ref dq "data"
                        )
                         0
                      )
                    )
                  )
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
                                      _len (
                                        hash-table-ref dq "data"
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      set! s (
                                        string-append (
                                          string-append s ", "
                                        )
                                         (
                                          to-str-space (
                                            list-ref (
                                              hash-table-ref dq "data"
                                            )
                                             i
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
                     (
                      ret22 (
                        string-append s "]"
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
            ret25
          )
           (
            let (
              (
                dq (
                  empty_deque
                )
              )
            )
             (
              begin (
                set! dq (
                  push_back dq 2
                )
              )
               (
                set! dq (
                  push_front dq 1
                )
              )
               (
                set! dq (
                  extend_back dq (
                    _list 3 4
                  )
                )
              )
               (
                set! dq (
                  extend_front dq (
                    _list 0
                  )
                )
              )
               (
                _display (
                  if (
                    string? (
                      to_string dq
                    )
                  )
                   (
                    to_string dq
                  )
                   (
                    to-str (
                      to_string dq
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
                    r (
                      pop_back dq
                    )
                  )
                )
                 (
                  begin (
                    set! dq (
                      hash-table-ref r "deque"
                    )
                  )
                   (
                    _display (
                      if (
                        string? (
                          hash-table-ref r "value"
                        )
                      )
                       (
                        hash-table-ref r "value"
                      )
                       (
                        to-str (
                          hash-table-ref r "value"
                        )
                      )
                    )
                  )
                   (
                    newline
                  )
                   (
                    set! r (
                      pop_front dq
                    )
                  )
                   (
                    set! dq (
                      hash-table-ref r "deque"
                    )
                  )
                   (
                    _display (
                      if (
                        string? (
                          hash-table-ref r "value"
                        )
                      )
                       (
                        hash-table-ref r "value"
                      )
                       (
                        to-str (
                          hash-table-ref r "value"
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
                          to_string dq
                        )
                      )
                       (
                        to_string dq
                      )
                       (
                        to-str (
                          to_string dq
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
                          is_empty (
                            empty_deque
                          )
                        )
                      )
                       (
                        is_empty (
                          empty_deque
                        )
                      )
                       (
                        to-str (
                          is_empty (
                            empty_deque
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
