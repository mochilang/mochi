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
      start13 (
        current-jiffy
      )
    )
     (
      jps16 (
        jiffies-per-second
      )
    )
  )
   (
    begin (
      let (
        (
          NIL (
            - 0 1
          )
        )
      )
       (
        begin (
          let (
            (
              seed 1
            )
          )
           (
            begin (
              define (
                set_seed s
              )
               (
                call/cc (
                  lambda (
                    ret1
                  )
                   (
                    set! seed s
                  )
                )
              )
            )
             (
              define (
                randint a b
              )
               (
                call/cc (
                  lambda (
                    ret2
                  )
                   (
                    begin (
                      set! seed (
                        _mod (
                          + (
                            * seed 1103515245
                          )
                           12345
                        )
                         2147483648
                      )
                    )
                     (
                      ret2 (
                        + (
                          _mod seed (
                            + (
                              - b a
                            )
                             1
                          )
                        )
                         a
                      )
                    )
                  )
                )
              )
            )
             (
              define (
                rand_bool
              )
               (
                call/cc (
                  lambda (
                    ret3
                  )
                   (
                    ret3 (
                      equal? (
                        randint 0 1
                      )
                       1
                    )
                  )
                )
              )
            )
             (
              let (
                (
                  nodes (
                    _list
                  )
                )
              )
               (
                begin (
                  let (
                    (
                      root NIL
                    )
                  )
                   (
                    begin (
                      define (
                        new_heap
                      )
                       (
                        call/cc (
                          lambda (
                            ret4
                          )
                           (
                            begin (
                              set! nodes (
                                _list
                              )
                            )
                             (
                              set! root NIL
                            )
                          )
                        )
                      )
                    )
                     (
                      define (
                        merge r1 r2
                      )
                       (
                        call/cc (
                          lambda (
                            ret5
                          )
                           (
                            begin (
                              if (
                                equal? r1 NIL
                              )
                               (
                                begin (
                                  ret5 r2
                                )
                              )
                               (
                                quote (
                                  
                                )
                              )
                            )
                             (
                              if (
                                equal? r2 NIL
                              )
                               (
                                begin (
                                  ret5 r1
                                )
                              )
                               (
                                quote (
                                  
                                )
                              )
                            )
                             (
                              if (
                                > (
                                  cond (
                                    (
                                      string? (
                                        list-ref nodes r1
                                      )
                                    )
                                     (
                                      _substring (
                                        list-ref nodes r1
                                      )
                                       "value" (
                                        + "value" 1
                                      )
                                    )
                                  )
                                   (
                                    (
                                      hash-table? (
                                        list-ref nodes r1
                                      )
                                    )
                                     (
                                      hash-table-ref (
                                        list-ref nodes r1
                                      )
                                       "value"
                                    )
                                  )
                                   (
                                    else (
                                      list-ref (
                                        list-ref nodes r1
                                      )
                                       "value"
                                    )
                                  )
                                )
                                 (
                                  cond (
                                    (
                                      string? (
                                        list-ref nodes r2
                                      )
                                    )
                                     (
                                      _substring (
                                        list-ref nodes r2
                                      )
                                       "value" (
                                        + "value" 1
                                      )
                                    )
                                  )
                                   (
                                    (
                                      hash-table? (
                                        list-ref nodes r2
                                      )
                                    )
                                     (
                                      hash-table-ref (
                                        list-ref nodes r2
                                      )
                                       "value"
                                    )
                                  )
                                   (
                                    else (
                                      list-ref (
                                        list-ref nodes r2
                                      )
                                       "value"
                                    )
                                  )
                                )
                              )
                               (
                                begin (
                                  let (
                                    (
                                      tmp r1
                                    )
                                  )
                                   (
                                    begin (
                                      set! r1 r2
                                    )
                                     (
                                      set! r2 tmp
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
                                rand_bool
                              )
                               (
                                begin (
                                  let (
                                    (
                                      tmp (
                                        cond (
                                          (
                                            string? (
                                              list-ref nodes r1
                                            )
                                          )
                                           (
                                            _substring (
                                              list-ref nodes r1
                                            )
                                             "left" (
                                              + "left" 1
                                            )
                                          )
                                        )
                                         (
                                          (
                                            hash-table? (
                                              list-ref nodes r1
                                            )
                                          )
                                           (
                                            hash-table-ref (
                                              list-ref nodes r1
                                            )
                                             "left"
                                          )
                                        )
                                         (
                                          else (
                                            list-ref (
                                              list-ref nodes r1
                                            )
                                             "left"
                                          )
                                        )
                                      )
                                    )
                                  )
                                   (
                                    begin (
                                      hash-table-set! (
                                        list-ref nodes r1
                                      )
                                       "left" (
                                        cond (
                                          (
                                            string? (
                                              list-ref nodes r1
                                            )
                                          )
                                           (
                                            _substring (
                                              list-ref nodes r1
                                            )
                                             "right" (
                                              + "right" 1
                                            )
                                          )
                                        )
                                         (
                                          (
                                            hash-table? (
                                              list-ref nodes r1
                                            )
                                          )
                                           (
                                            hash-table-ref (
                                              list-ref nodes r1
                                            )
                                             "right"
                                          )
                                        )
                                         (
                                          else (
                                            list-ref (
                                              list-ref nodes r1
                                            )
                                             "right"
                                          )
                                        )
                                      )
                                    )
                                     (
                                      hash-table-set! (
                                        list-ref nodes r1
                                      )
                                       "right" tmp
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
                              hash-table-set! (
                                list-ref nodes r1
                              )
                               "left" (
                                merge (
                                  cond (
                                    (
                                      string? (
                                        list-ref nodes r1
                                      )
                                    )
                                     (
                                      _substring (
                                        list-ref nodes r1
                                      )
                                       "left" (
                                        + "left" 1
                                      )
                                    )
                                  )
                                   (
                                    (
                                      hash-table? (
                                        list-ref nodes r1
                                      )
                                    )
                                     (
                                      hash-table-ref (
                                        list-ref nodes r1
                                      )
                                       "left"
                                    )
                                  )
                                   (
                                    else (
                                      list-ref (
                                        list-ref nodes r1
                                      )
                                       "left"
                                    )
                                  )
                                )
                                 r2
                              )
                            )
                             (
                              ret5 r1
                            )
                          )
                        )
                      )
                    )
                     (
                      define (
                        insert value
                      )
                       (
                        call/cc (
                          lambda (
                            ret6
                          )
                           (
                            let (
                              (
                                node (
                                  alist->hash-table (
                                    _list (
                                      cons "value" value
                                    )
                                     (
                                      cons "left" NIL
                                    )
                                     (
                                      cons "right" NIL
                                    )
                                  )
                                )
                              )
                            )
                             (
                              begin (
                                set! nodes (
                                  append nodes (
                                    _list node
                                  )
                                )
                              )
                               (
                                let (
                                  (
                                    idx (
                                      - (
                                        _len nodes
                                      )
                                       1
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    set! root (
                                      merge root idx
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
                        top
                      )
                       (
                        call/cc (
                          lambda (
                            ret7
                          )
                           (
                            begin (
                              if (
                                equal? root NIL
                              )
                               (
                                begin (
                                  ret7 0
                                )
                              )
                               (
                                quote (
                                  
                                )
                              )
                            )
                             (
                              ret7 (
                                cond (
                                  (
                                    string? (
                                      list-ref nodes root
                                    )
                                  )
                                   (
                                    _substring (
                                      list-ref nodes root
                                    )
                                     "value" (
                                      + "value" 1
                                    )
                                  )
                                )
                                 (
                                  (
                                    hash-table? (
                                      list-ref nodes root
                                    )
                                  )
                                   (
                                    hash-table-ref (
                                      list-ref nodes root
                                    )
                                     "value"
                                  )
                                )
                                 (
                                  else (
                                    list-ref (
                                      list-ref nodes root
                                    )
                                     "value"
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
                        pop
                      )
                       (
                        call/cc (
                          lambda (
                            ret8
                          )
                           (
                            let (
                              (
                                result (
                                  top
                                )
                              )
                            )
                             (
                              begin (
                                let (
                                  (
                                    l (
                                      cond (
                                        (
                                          string? (
                                            list-ref nodes root
                                          )
                                        )
                                         (
                                          _substring (
                                            list-ref nodes root
                                          )
                                           "left" (
                                            + "left" 1
                                          )
                                        )
                                      )
                                       (
                                        (
                                          hash-table? (
                                            list-ref nodes root
                                          )
                                        )
                                         (
                                          hash-table-ref (
                                            list-ref nodes root
                                          )
                                           "left"
                                        )
                                      )
                                       (
                                        else (
                                          list-ref (
                                            list-ref nodes root
                                          )
                                           "left"
                                        )
                                      )
                                    )
                                  )
                                )
                                 (
                                  begin (
                                    let (
                                      (
                                        r (
                                          cond (
                                            (
                                              string? (
                                                list-ref nodes root
                                              )
                                            )
                                             (
                                              _substring (
                                                list-ref nodes root
                                              )
                                               "right" (
                                                + "right" 1
                                              )
                                            )
                                          )
                                           (
                                            (
                                              hash-table? (
                                                list-ref nodes root
                                              )
                                            )
                                             (
                                              hash-table-ref (
                                                list-ref nodes root
                                              )
                                               "right"
                                            )
                                          )
                                           (
                                            else (
                                              list-ref (
                                                list-ref nodes root
                                              )
                                               "right"
                                            )
                                          )
                                        )
                                      )
                                    )
                                     (
                                      begin (
                                        set! root (
                                          merge l r
                                        )
                                      )
                                       (
                                        ret8 result
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
                        is_empty
                      )
                       (
                        call/cc (
                          lambda (
                            ret9
                          )
                           (
                            ret9 (
                              equal? root NIL
                            )
                          )
                        )
                      )
                    )
                     (
                      define (
                        to_sorted_list
                      )
                       (
                        call/cc (
                          lambda (
                            ret10
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
                                              not (
                                                is_empty
                                              )
                                            )
                                             (
                                              begin (
                                                set! res (
                                                  append res (
                                                    _list (
                                                      pop
                                                    )
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
                                ret10 res
                              )
                            )
                          )
                        )
                      )
                    )
                     (
                      set_seed 1
                    )
                     (
                      new_heap
                    )
                     (
                      insert 2
                    )
                     (
                      insert 3
                    )
                     (
                      insert 1
                    )
                     (
                      insert 5
                    )
                     (
                      insert 1
                    )
                     (
                      insert 7
                    )
                     (
                      _display (
                        if (
                          string? (
                            to_sorted_list
                          )
                        )
                         (
                          to_sorted_list
                        )
                         (
                          to-str (
                            to_sorted_list
                          )
                        )
                      )
                    )
                     (
                      newline
                    )
                     (
                      new_heap
                    )
                     (
                      insert 1
                    )
                     (
                      insert (
                        - 1
                      )
                    )
                     (
                      insert 0
                    )
                     (
                      _display (
                        if (
                          string? (
                            to_sorted_list
                          )
                        )
                         (
                          to_sorted_list
                        )
                         (
                          to-str (
                            to_sorted_list
                          )
                        )
                      )
                    )
                     (
                      newline
                    )
                     (
                      new_heap
                    )
                     (
                      insert 3
                    )
                     (
                      insert 1
                    )
                     (
                      insert 3
                    )
                     (
                      insert 7
                    )
                     (
                      _display (
                        if (
                          string? (
                            pop
                          )
                        )
                         (
                          pop
                        )
                         (
                          to-str (
                            pop
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
                            pop
                          )
                        )
                         (
                          pop
                        )
                         (
                          to-str (
                            pop
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
                            pop
                          )
                        )
                         (
                          pop
                        )
                         (
                          to-str (
                            pop
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
                            pop
                          )
                        )
                         (
                          pop
                        )
                         (
                          to-str (
                            pop
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
     (
      let (
        (
          end14 (
            current-jiffy
          )
        )
      )
       (
        let (
          (
            dur15 (
              quotient (
                * (
                  - end14 start13
                )
                 1000000
              )
               jps16
            )
          )
        )
         (
          begin (
            _display (
              string-append "{\n  \"duration_us\": " (
                number->string dur15
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
